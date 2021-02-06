;;; -*- Mode:Lisp -*-

;;; =======================================================================
;;; Construction Engine
;;;
;;; This file contains the main construction engine which is based on a 
;;; modified earley chart parser. 
;;;
;;; Constructions are specified via the (define-construction ...) macro. 
;;;

(provide "scone-constructions")
(require "scone" "scone-loader.lisp")
(require "scone-general" "general.lisp")
(require "scone-gen-obj" "gen-obj.lisp")

;; Set Scone Options
(setf *no-checking* t)

;; The Scone Construction
(new-type {scone-construction} *thing*)
(defvar *scone-construction* (lookup-element-predicate {scone-construction}))

;; Some Parameters
(defparameter *c-max-constructions-per-chart-element* 1024)
(defparameter *c-target-constructions* *scone-construction*)

;; Construction Types
(new-type {c-variable-type} *thing*)
(new-type {c-trigger-type} *thing*)
(new-type {c-trigger-option} *thing*)

;; Roles and Relations for Constructions
(new-type-role {c-variable} {scone-construction}
               {c-variable-type})
(new-relation {c-trigger} :a-inst-of {scone-construction} 
              :b-inst-of {c-trigger-type} :c-inst-of {c-trigger-option})

;; Trigger Definitions
(new-indv {c-rep-element} {c-trigger-type})
(new-indv {c-rep-element-fn} {c-trigger-type})
(new-indv {c-head} {c-trigger-type})
(new-indv {c-run-function} {c-trigger-type})
(new-indv {c-potential-subject} {c-trigger-type})

;; Built-In Options
(new-indv {c-exp-eval} {c-trigger-option})

;; Short-Cuts
(defvar *c-variable-type* (lookup-element-predicate {c-variable-type}))
(defvar *c-variable* (lookup-element-predicate {c-variable}))
(defvar *c-trigger* (lookup-element-predicate {c-trigger}))
(defvar *c-head* (lookup-element-predicate {c-head}))
(defvar *c-exp-eval* (lookup-element-predicate {c-exp-eval}))
(defvar *c-rep-element* (lookup-element-predicate {c-rep-element}))
(defvar *c-rep-element-fn* (lookup-element-predicate {c-rep-element-fn}))


;; Variable Keys
(defparameter *v-g-node-key* 'g-node)
(defmacro get-v-g-node (v) `(get-element-property ,v *v-g-node-key*))
(defmacro set-v-g-node (v val) `(set-element-property ,v *v-g-node-key* ,val))

;; Pattern Keys
(defparameter *runtime-eval-tag* 'r-eval)
(defparameter *c-code-key* 'c-code)
(defparameter *c-pattern-key* 'c-pattern)

(defmacro get-c-pattern (c) `(get-element-property ,c *c-pattern-key*))
(defmacro set-c-pattern (c val) `(set-element-property ,c *c-pattern-key* ,val))
(defmacro new-c-pattern (&optional (len 16)) `(make-array ,len 
                                                          :initial-element nil :adjustable t))

;; Variable-List Keys
(defparameter *c-varlist-key* 'var-list)
(defmacro get-c-varlist (c) `(get-element-property ,c *c-varlist-key*))
(defmacro set-c-varlist (c val) `(set-element-property ,c *c-varlist-key* ,val))

;; Scoring  Keys
(defparameter *c-base-score-key* 'base-score)
(defmacro get-c-base-score (v) `(get-element-property ,v *c-base-score-key*))
(defmacro set-c-base-score (v val) `(set-element-property ,v *c-base-score-key* ,val))

;; Pattern Var Struct
;; The pattern variable structure is the structure that each element of a
;; pattern uses. It contains the Gen-Obj node and other information which is
;; used during the parsing of the input.
(defstruct (pattern-var
             (:constructor make-pattern-var)
             (:type list)) 
  "Each construction is associated with a pattern - an ordered array of restrictions.
   The pattern-var struct provides an interface to specify the restrictions.

   v-name - Name of the variable, this is used in the scone-code
   g-node - The gen-obj associated with this pattern-var
   head-var - Binary value to indicate whether this variable is the head. 
   m-constructions - A list of constructions that can produce elements which directly match this restriction
   ss-m-constructions - Same as m-constructions, however, we need to check whether the element produced matched"
  (v-name nil :type symbol)
  (g-node nil :type gen-obj)
  (head-var  nil)                  ;; Boolean to represent head var
  (m-constructions nil :type list) ;; Matching Constructions
  (ss-m-constructions nil :type list)) ;; SuperSet-Matching Constructions

;; Construction Creation Macro
(defmacro define-construction 
    ((name 
      parent
      var-list 
      pattern 
      trigger-list
      &key
      (base-score 0.0))
     &body scone-ops)

  "Helper to setup a construction. 

   NAME - The name of the construction
   PARENT - A parent construction, can be nil
   VAR-LIST - A list of variables with restrictions
   PATTERN - A pattern comprising of variables from the var-list
   TRIGGER-LIST - Triggers for the construction
   BASE-SCORE - The base score for the construction "


  ;; The following sets up and returns a huge let statement which builds
  ;; the construction

  (let* ((nc (gensym)) ;; New-Construction
         (pc (lookup-element-predicate parent))

         ;; Full-Variable List
         (fvlist nil)  

         ;; Variable Restrictions
         (v-res-symbols (let ((arr (make-array (length var-list))))
                          (dotimes (i (length var-list))
                            (setf (aref arr i) (gensym)))
                          arr))  

         ;; Head Variable Name (if any)
         (h-var (second
                 (find-if (lambda (t-entry) 
                            (or
                             (equal *c-head* 
                                    (lookup-element-predicate
                                     (symbol-value (first t-entry))))
                             (equal *c-head* 
                                    (lookup-element-predicate
                                     (first t-entry)))))
                          trigger-list)))
         
         ;; Parent's PATtern Array 
         (ppata (when pc 
                  (get-c-pattern pc)))
         (pattern (if (not pattern)
                      (when ppata
                        (map 'list 'car ppata))
                      pattern))

         ;; My own PAT-tern A-rray
         (pata (gensym))

         (pvar-list (when pc
                      (get-element-property pc 'var-list))))

    `(let* ,(delete-if
	     'null ;; So as to remove any empty stuff here that might
	           ;; cause errors

	     `((,nc (new-indv (gen-iname 
			       ,(if name name "scone-construction"))
		     ,(if (element-p pc)
			  pc
			  *scone-construction*)
		     :english ,name))

               ;; Setup all the variables that I am using
               ;; Merge any variables from my parent with those specified

	       ;; Setup parent's variables if defined
	       ,@(when (element-p pc)
		       (mapcar (lambda (pv-entry)
				 (progn
				   (push (first pv-entry) fvlist)
				   `(,(first pv-entry)
				     (new-indv nil ,(second pv-entry)))))
			       pvar-list))

	       ;; Setup my own variables
	       ,@(mapcar (lambda (v-entry)
			   (unless (find (first v-entry) fvlist)
			     ;; Store variable!
			     (push (first v-entry) fvlist)
			     `(,(first v-entry)
			       (new-indv nil *c-variable-type*))))
			 var-list)


	       ;; Setup Variable Restrictions
	       ,@(loop for v-entry in var-list 
		       for i = 0 then (+ i 1)
		       for v-res-sym = (aref v-res-symbols i)
		       collect 
		       (multiple-value-bind (string-list 
					     predicate-list
					     is-a-list is-not-a-list
					     role-list rel-list x-of-y-list
					     option-list)
			   (make-gen-obj-process-list (cdr v-entry))
						

			 (setf (cdr v-entry) 
			       (list v-res-sym 
				     `(list ,@option-list)))

			 `(,v-res-sym (make-gen-obj
				       :is-a (list ,@is-a-list)
				       :is-not-a (list ,@is-not-a-list)
				       :role (list ,@role-list)
				       :rel (list ,@rel-list)
				       :x-of-y (list ,@x-of-y-list)
				       :string (list ,@string-list)
				       :predicates (list ,@predicate-list)
				       :match-set ,(if (find-if 
							(lambda (x)
							  (when (eql (first x) 'quote)
							    (setf x (cadr x)))
							  (equal 'match-set x))
							option-list)
						       t nil)))))
							
	       ,(when pattern
		      `(,pata (new-c-pattern ,(length pattern))))))

       ;; Associate Variables
       ,@(mapcar (lambda (fv-entry)
                   `(x-is-a-y-of-z ,fv-entry *c-variable* ,nc))
                 fvlist)

       ;; Create all the is-a links based on the matching information
       ;; For strings, we just say string
       ,@(loop for v-entry in var-list
            collect (let ((v-is-a (gensym)))
                      `(loop for ,v-is-a in (gen-obj-is-a ,(second v-entry))
                          do (new-is-a ,(first v-entry) ,v-is-a))))

       ;; Do is-a creation for roles as well. 
       ,@(loop for v-entry in var-list
            collect (let ((v-role (gensym))
                          (v-is-a (gensym))
                          (v-is-a-list (gensym))
                          (v-role-res (gensym)))
                      `(loop for ,v-role-res in (gen-obj-role ,(second v-entry))
                          for ,v-role = (first ,v-role-res)
                          for ,v-is-a-list = (gen-obj-is-a (second ,v-role-res))
                          do (dolist (,v-is-a ,v-is-a-list)
                               (new-is-a 
                                (get-the-x-role-of-y ,v-role 
                                                     ,(first v-entry))
                                ,v-is-a)))))

       ;; Set property for variables (actual elems) here
       ;; use fvlist, referencing pattern
       ,@(loop for fv-entry in fvlist
            collect 
	    `(set-v-g-node ,fv-entry 
              ;; Following checks if we need to fill it using information from parent
              ;; Note ppata belongs to the parent (parent pata)
	      ,(let ((v-entry 
		      (find-if (lambda (e) (eq (first e) fv-entry)) 
			       var-list)))
		    (if v-entry
			(when v-entry
			  (second v-entry))
			(pattern-var-g-node 
			 (find-if 
			  (lambda (e) 
			    (eql (pattern-var-v-name e)  fv-entry))
			  ppata))))))

       ;; Fill Pattern Array
       ,@(when pattern
               (loop for ps in pattern
                  for i = 0 then (+ i 1)
                  collect
                  `(setf (aref ,pata ,i) 
                         (make-pattern-var 
                          :head-var (equal ,ps ,h-var)
                          :v-name (quote ,ps) 
                          :g-node 
                          ;; Following checks if we need to fill it using information from parent
                          ;; Note ppata belongs to the parent (parent pata)
                          ,(let ((v-entry 
                                  (find-if (lambda (e) (eq (first e) ps)) 
                                           var-list)))

                                (if v-entry
                                    (when v-entry
                                      (second v-entry))
                                    (pattern-var-g-node 
                                     (find-if 
                                      (lambda (e) 
                                        (eql (pattern-var-v-name e) ps))
                                      ppata))))))))

       ;; Set pattern to construction
       ,(when pattern
              `(set-c-pattern ,nc ,pata))

       ;; Save mine AND parents variable list
       (set-element-property ,nc 'var-list
                             (list 
                              ,@(mapcar 
				 (lambda (fv-entry) 
				   `(list

				     (quote ,fv-entry)
				     ,fv-entry

				     ;; Save options here
                                     ;; Again, checking if we need parent info 
				     ,(let ((v-entry 
					     (find-if 
					      (lambda (e)
						(eq (first e) fv-entry))
					      var-list)))
							 
					   (if v-entry
					       (when v-entry
						 (third v-entry))
					       `(quote
						 ,(third 
						   (find-if 
						    (lambda (e) 
						      (eql (first e) fv-entry))
						    pvar-list)))))))
				 fvlist)))
			
       ;; Set Triggers
       ,@(mapcar (lambda (f-entry)
                   (if (second f-entry)
                       `(new-statement ,nc *c-trigger* ,(first f-entry) 
                                       :c ,(second f-entry))
                       `(new-statement ,nc *c-trigger* ,(first f-entry))))
                 trigger-list)
	  
       ;; Save OPs to *c-code-key*, stripping r-evals
       (set-element-property ,nc *c-code-key*
                             (quote ,(loop for s-op in scone-ops
                                        append 
					(if (eql (first s-op) 
						 *runtime-eval-tag*)
					    (cdr s-op)
					    (list s-op)))))

       ;; Perform Scone-Ops 
       ;; Except those which have r-eval
       (set-element-property ,nc *c-exp-eval*
                             (progn ,@(remove-if (lambda (s-op) 
                                                   (eql (first s-op)
							*runtime-eval-tag*)) 
                                                 scone-ops)))

       (set-c-base-score ,nc ,base-score)

       ;; Return the Construction
       ,nc)))


;; This Function Needs Some Cleanup, Efficiency Wise etc
(defun get-construction-trigger-option (construction trigger)
  "Obtains the trigger for an option

   construction - The construction element we are looking at
   trigger - The trigger element"

  (setq trigger (lookup-element-predicate trigger))
  (setq construction (lookup-element-predicate construction))

  ;; Trigger Options are stored on the c-wire of statements
  ;; One important note is that these options should also be inherited
  ;; by child constructions, this gives rise to the following code
  ;; which does some simple marker walking across the wires to look
  ;; statements that should be inherited


  ;; Point to ponder: May be this should be "done" at instantiation time
  ;; That is, at instantiation, we look at all the parents/triggers again

  (when (and trigger construction)
    (with-temp-markers (m1 m2 m3)
      (dolist (e (incoming-b-wires trigger))
        (mark e m3))
      (dolist (e-a (incoming-a-wires construction))
        (mark e-a m2))
      (dolist (e (list-superiors construction))
        (dolist (e-a (incoming-a-wires e))
          (mark e-a m2)))
      (mark-boolean m1 (list m2 m3) nil)
      (when (most-specific m1)
        (clear-marker m2)
        (let ((e (c-wire (most-specific m1))))
          (when e

            (downscan (c-wire (most-specific m1)) m2)
            (if (is-x-a-y? e *c-variable-type*)
                (progn
				  
                  (clear-marker m3)
				  
                  (clear-marker m1)
                  (mark-the-x-of-y *c-variable* construction m1)
                  (do-marked (m-e m1)
                    (dolist (c-e (list-children m-e))
                      (mark c-e m3)))

                  (dolist (s-e (list-superiors construction))
                    (when (can-x-have-a-y? s-e *c-variable*)
                      (clear-marker m1)
                      (mark-the-x-of-y *c-variable* s-e m1)
                      (do-marked (m-e m1)
                        (dolist (c-e (list-children m-e))
                          (mark c-e m3)))))
				  
                  (clear-marker m1)

                  (mark-boolean m1 (list m2 m3) nil)
                  (most-specific m1))

                e)))))))
	

;; Construction Operations
(defun get-construction-head-rep (construction)

  "Given a construction, get an element that essentially represents the head"

  (let ((c-head-opt (get-construction-trigger-option construction
                                                     *c-head*))
        (c-rep-element-opt (get-construction-trigger-option construction
                                                            *c-rep-element*)))
    (if c-rep-element-opt
        c-rep-element-opt
        (if (equal c-head-opt *c-exp-eval*)
            (get-element-property construction 
                                  *c-exp-eval*)
            c-head-opt))))


	  

(defun get-constructions-by-head-gtype (head-gtype)
  "Get all constructions that match the gtype (a Gen-Obj)
   Returns both all directly matching construcions and all potentially matching constructions (General -> Specific case)

   head-gtype - A gen-node that we are trying to find constructions that match"
  (when head-gtype
    (let (m-constructions ss-m-constructions)
      (loop for construction in (list-inferiors *scone-construction*)
         do (let ((c-head-opt (get-construction-head-rep construction)))
              (when (or (element-p c-head-opt) (stringp c-head-opt))
                (if (check-gen-satisfied-all c-head-opt
                                             head-gtype)
                    (push construction m-constructions)
                    (let ((c-head-g-node (get-v-g-node c-head-opt))) 
                      (when (element-p c-head-opt)
                        (setf c-head-g-node (get-v-g-node c-head-opt)))
                      (when (and c-head-g-node
                                 (check-gen-x-subset-of-y head-gtype
                                                          c-head-g-node))
                        (push construction ss-m-constructions)))))))
      (values m-constructions ss-m-constructions))))
  
(defun build-matching-constructions-to-pattern (construction)
  "Given a construction, this function finds all other matching constructions 
   that fits something it is looking out for.

   construction - The construction element to build the matches for"
  (setq construction (lookup-element-predicate construction))
  (let* ((parray (get-c-pattern construction)))
    (when parray
      (loop for i = 0 then (+ i 1)
         for pentry across parray
         do 
         (multiple-value-bind (m-constructions ss-m-constructions)
             (get-constructions-by-head-gtype (pattern-var-g-node pentry))
           (setf (pattern-var-ss-m-constructions pentry) ss-m-constructions)
           (setf (pattern-var-m-constructions pentry) m-constructions))))))


;; Note: construction patterns are arrays of lists, each list in the form of
;;       (v1 gen-node (opt opt) (list of extended constructions))

;; ---- Pattern Matching Functions ----

;; Earley/Chart Parsing Algorithm
;; Store in a n^2 array 

(defstruct (parse-element
             (:constructor make-parse-element)
             (:type list))
  "An element in the 2D array representing the parse-chart"
  (construction *scone-construction* :type element)
  (pos 0 :type integer))

(defstruct (pe-extra-info
             (:constructor make-pe-extra))
  "Additional information tagged onto elements in the parse-chart
   Note: These are stored in the hash table with keys of the form
         (end-token-pos start-token-pos parse-elem)"
  (do-not-complete nil)
  (rep-element nil)
  (prior-pe nil :type list)
  (current-skip-penalty 1.0 :type short-float)
  (matching-score 0.0 :type short-float)
  (last-matched-item nil))

(defun pe-completed? (pe)
  "Tests whether a construction (in a parse-element) is complete"
  (let ((c-pat (get-c-pattern (parse-element-construction pe))))
    (when c-pat
      (>= (parse-element-pos pe) (array-dimension c-pat 0)))))

;; ---- Scoring Functions ----

;; Skip Scoring Parameters
(defparameter *score-pe-skip-base-penalty* 1.5)

;; Prefer Completed Constructions
(defparameter *score-pe-pdone-w* 0.0)
(defun score-pe-pdone (pe)
  "Returns a score [0-1] for the construction length"
  (let ((c-pat (get-c-pattern (parse-element-construction pe))))
    (if (and c-pat (> (array-dimension c-pat 0) 0))
        (/ (parse-element-pos pe)
           (array-dimension c-pat 0))
        0.0)))

;; Prefer Longer Constructions (take n^1.5, cap at max 10^1.5)
;; * only if completed
(defparameter *score-pe-length-w* 1)
(defparameter *score-pe-length-max-exp* 1.5)
(defparameter *score-pe-length-max-score* (expt 6 *score-pe-length-max-exp*))
(defun score-pe-length (pe)
  "Returns a score [0-1] for the construction length"

  (* (score-pe-pdone pe)
     (let ((c-pat (get-c-pattern (parse-element-construction pe))))
       (if c-pat
           (/ (min (expt (array-dimension c-pat 0) 
                         *score-pe-length-max-exp*)
                   *score-pe-length-max-score*)
                *score-pe-length-max-score*)
           0.0))))

;; Prefer Constructions with a Higher Base Score
(defparameter *score-pe-base-w* 2)
(defun score-pe-base (pe)
  "Returns a score [0-1] for the construction's base score"
  (let* ((construction (parse-element-construction pe))
         (base-score (get-c-base-score construction)))
    (if (typep base-score 'single-float)
        base-score
        0.0)))

;; Matching Score
(defparameter *score-pe-matching-w* 1)
(defun score-pe-matching (pe-ei &optional (norm-factor 1.0))
  "Returns a score [0-1] based on how well the underlying tokens match"
  (/ (pe-extra-info-matching-score pe-ei)
     norm-factor))

;; Skip Score (Negative)
(defparameter *score-pe-skip-w* 1)
(defun score-pe-skip (pe-ei &optional (norm-factor 1.0))
  "Returns a score [0-1] based on how well the underlying tokens match"

  ;; The norm-factor is usually set to the # of tokens being processed.

  (- (/ (/ (1- (pe-extra-info-current-skip-penalty pe-ei))
           (1- *score-pe-skip-base-penalty*))
        norm-factor)))

;; Auxillary Score 
(defparameter *score-pe-aux-score-w* 1)
(defun score-pe (pe pe-ei aux-score &optional (match-norm-factor 1.0))
  "Returns a weighted score for a construction. This score is not normalized."
  (/
   (+ (* *score-pe-base-w* (score-pe-base pe))
      (* *score-pe-matching-w* (score-pe-matching pe-ei match-norm-factor))
      (* *score-pe-skip-w* (score-pe-skip pe-ei match-norm-factor))
      (* *score-pe-aux-score-w* aux-score)
      (* *score-pe-length-w* (score-pe-length pe))
      (* *score-pe-pdone-w* (score-pe-pdone pe)))
   (+ *score-pe-base-w* *score-pe-matching-w* *score-pe-aux-score-w* 
      *score-pe-skip-w* *score-pe-length-w* *score-pe-pdone-w*)))

;; Parse Charts is  (n+1)^2 array  of sets (hash-table)
;; First  Index represents the end token
;; Second Index represents the start token
;; I.E. 

(defun cp-completer-print-chain (pc-extra chain-c-list &optional (depth ""))
  "Debugging function"
  (format t "~ACHAINPRINT~%~%" depth)
  (loop 
     until (null chain-c-list)
     for pe-extra = (gethash chain-c-list
                             pc-extra)
     for last-matched-item = (when pe-extra
                               (pe-extra-info-last-matched-item pe-extra))
     for prior-pe = (when pe-extra
                        (pe-extra-info-prior-pe pe-extra))
     do (progn (format t "~A~%~A~%~%" last-matched-item chain-c-list)
               (when (and (not (null last-matched-item)) (listp last-matched-item))
                 (cp-completer-print-chain pc-extra chain-c-list (strcat depth " ")))
               (setf chain-c-list prior-pe))))

(defun cp-completer-update-check-chain (pc-extra find-c-list chain-c-list)
  "During a completer score update, we want to ensure that no cycles are formed. 
   E.g. This can happen when a construction matches itself. This function verifies
   that the 'update chain' does not already contain the current construction

   pc-extra - extra info hash table for the parsechart
   find-c-list - construction to be searching for in the list
   chain-c-list - the construction we are looking at now"

  (loop 
     until (null chain-c-list)
     for pe-extra = (gethash chain-c-list
                             pc-extra)
     for last-matched-item = (when pe-extra
                               (pe-extra-info-last-matched-item pe-extra))
     for prior-pe = (when pe-extra
                      (pe-extra-info-prior-pe pe-extra))
     do (progn

          ;; Found a match
          (when (or (equal find-c-list last-matched-item)
                    (equal find-c-list chain-c-list))
            (return-from cp-completer-update-check-chain nil))

          ;; Check the rest of the chain
          (when (and (not (null last-matched-item)) (listp last-matched-item))
            (unless (cp-completer-update-check-chain 
                     pc-extra find-c-list last-matched-item)
              (return-from cp-completer-update-check-chain nil)))
          (setf chain-c-list prior-pe)))
  t)
  

(defun cp-completer (pc-extra parse-chart e-check-pos tok-pos completed-pe token-list)
  "At check-pos position, check to see if any constructions have completed due completed-elem. 
   tok-pos should never change as we recuse. 

   pc-extra - extra information hash-table
   parse-chart - the 2d array representing the parse
   e-check-pos - end position of the constructions to check (or start position of the completed construction)
   tok-pos - where the token is currently
   completed-pe - the parse element that has been completed
   token-list - the list of actual input tokens"

  (if (null completed-pe)

      ;; If the end and the start is the same, this is a special indicator to check tok-pos
      ;; for any constructions that have completed at that point

      (when (= e-check-pos tok-pos)	
        (dotimes (i (1+ e-check-pos))
          ;; Since the heap is modified by the completer, we have to use heap-copy here
          (heap-copy-do (pe pe-priority (aref parse-chart e-check-pos i))
            pe-priority
            (when (pe-completed? pe)
              (cp-completer pc-extra parse-chart i tok-pos pe token-list)))))

      ;; Possible to Optimize Following
      
      (let ((cpe-ei-elem (gethash `(,tok-pos 
                                    ,e-check-pos 
                                    ,completed-pe) pc-extra)))
        
        ;; This was added after skips were allowed, makes it faster
        ;; So if a construction was completed and we're now just skipping stuff, we 
        ;; do not go back and try to complete it again (since the start-pos of the
        ;; construction is always the same, completing it again does nothing)
        (unless (pe-extra-info-do-not-complete cpe-ei-elem)
        
          (unless (pe-extra-info-rep-element cpe-ei-elem)
          
            (setf (pe-extra-info-rep-element cpe-ei-elem)
                  (get-construction-trigger-option
                   (parse-element-construction completed-pe)
                   *c-rep-element*)))
        
          (dotimes (i (1+ e-check-pos))
             
            (heap-do (pe nil (aref parse-chart e-check-pos i))
            
              ;; For each candidate that we want to try to use the completed
              ;; construction with. In essence, doing something like scanner
              ;; with the exception that the new 'token' is a completed
              ;; construction
              
              (unless (pe-completed? pe)
              
                (let* ((c-pat (get-c-pattern (parse-element-construction pe)))
                       (npe (make-parse-element :construction 
                                                (parse-element-construction pe)
                                                :pos
                                                (1+ (parse-element-pos pe)))))

                  (when (and c-pat
                             ;; not recreating ourself for some reason
                             (not (equal `(,tok-pos ,i ,npe)
                                         `(,tok-pos ,e-check-pos ,completed-pe))))

                    ;; Get all the necessary nodes/elements. This makes it 
                    ;; easier to work with
                    (let* ((pe-ei-elem (gethash `(,e-check-pos ,i ,pe)
                                                pc-extra))
                           (cpe-rep (pe-extra-info-rep-element
                                     cpe-ei-elem))
                           (c-g-node (pattern-var-g-node
                                      (aref c-pat (parse-element-pos pe))))
                           (rep-elem-fn-se (get-construction-trigger-option
                                            (parse-element-construction pe)
                                            *c-rep-element-fn*))
                           (rep-elem-fn (when (element-p rep-elem-fn-se)
                                          (internal-name rep-elem-fn-se))))
				  
                      ;; Two possible ways to match
                      (when (or 
                             (find (parse-element-construction 
                                    completed-pe)
                                   (pattern-var-m-constructions
                                    (aref c-pat (parse-element-pos pe))))
                             (and 
                              (find (parse-element-construction 
                                     completed-pe)
                                    (pattern-var-ss-m-constructions
                                     (aref c-pat (parse-element-pos pe))))
                              (check-gen-satisfied-all cpe-rep c-g-node)))


                        ;; Create the new element in anycase
                        ;; The scoring functions need the new elements to operate over
                        ;; Can be optimized!
                        (let ((npe-ei 
                               (make-pe-extra :rep-element
                                              ;; The representative element is determine
                                              ;; by the head-var variable
                                              (if (pattern-var-head-var 
                                                   (aref c-pat (parse-element-pos 
                                                                pe)))

                                                  (return-as-gen-type cpe-rep
                                                                      c-g-node)

                                                  ;; These rep elements can be created by 
                                                  ;; arbitary functions as well, see 
                                                  ;; 'Merge Neighboring Strings' for ex.
                                                  (if (functionp rep-elem-fn)
                                                      (funcall rep-elem-fn
                                                               (pe-extra-info-rep-element
                                                                pe-ei-elem)
                                                               cpe-rep)
                                                      (pe-extra-info-rep-element
                                                       pe-ei-elem)))

                                              :current-skip-penalty 
                                              ;; The skip penalty is multiplied together to account
                                              ;; for all the skips at one go
                                              (* (pe-extra-info-current-skip-penalty pe-ei-elem)
                                                 (pe-extra-info-current-skip-penalty cpe-ei-elem))

                                              :prior-pe `(,e-check-pos ,i ,pe)

                                              :matching-score 
                                              ;; Matching score is simply added up together, this is ok
                                              ;; since it will later be normalized by # of tokens.
                                              (+ (pe-extra-info-matching-score 
                                                  pe-ei-elem)
                                                 (pe-extra-info-matching-score 
                                                  cpe-ei-elem))

                                              :last-matched-item `(,tok-pos 
                                                                   ,e-check-pos 
                                                                   ,completed-pe))))
					  
                          ;; We add the new element and recurse only if it either does not exist
                          ;; or has a better score.
                          (when (or (heap-not-exists (aref parse-chart tok-pos i) 
                                                     npe)
				
                                    ;; nth-value 1 since heap-get returns a values pair of 
                                    ;; the actual heap element and the score
                                    (and (< (nth-value 1
                                                       (heap-get 
                                                        (aref parse-chart tok-pos i) npe))
                                            (score-pe npe npe-ei 0.0 (1+ (- tok-pos i))))

                                         ;; scoring updates require a special chain check
                                         ;; as described above
                                         
                                         ;; note that this might not always give a global
                                         ;; optimum. one should look at (Stolcke 1995)
                                         ;; on probablistic earley parsing for a better solution
                                         (or (not (pe-completed? npe))
                                             (cp-completer-update-check-chain
                                              pc-extra 
                                              `(,e-check-pos ,i ,pe)
                                              `(,tok-pos ,e-check-pos ,completed-pe)))))
                          
                            (setf (gethash `(,tok-pos ,i ,npe) pc-extra)
                                  npe-ei)
						
                            (heap-insert (aref parse-chart tok-pos i) npe 
                                         (score-pe npe npe-ei
                                                   0.0 (1+ (- tok-pos i))))
						
                            (when (pe-completed? npe)
                              (cp-completer pc-extra parse-chart i tok-pos npe token-list)))))))))))))))

	  

(defun cp-predictor (pc-extra parse-chart tok-pos)
  "At tok-pos, add new constructions that is predicted - that is, these constructions
   are expected or can result in elements that can be used to match another construction's pattern

   pc-extra - extra information hash-table
   parse-chart - the 2d array representing the parse
   tok-pos - where the token is currently"
  (dotimes (i (1+ tok-pos))

    ;; We maintain two sets because we do not want to be iterating over a set
    ;; that is actively modified.

    (let ((check-set (aref parse-chart tok-pos i))
          (new-check-set (make-heap)))

      (loop until (heap-is-empty? check-set)

         do (progn

              ;; go through check-set and add stuff to new-check-set
              (heap-do (pe nil check-set)
                
                (let* ((c-pat (get-c-pattern (parse-element-construction pe)))

                       (n-plist (when c-pat

                                  (unless (>= (parse-element-pos pe) 
                                              (array-dimension c-pat 0))
	   
                                    (append ;; Optimization Note - append is slow, but 
                                            ;; makes the code clean here and nconc is 
                                            ;; dangerous here - do not want to change the lists
                                            ;; Alternative, write a macro to take in multiple lists
                                            ;; and do the same operations!
                                     (pattern-var-m-constructions 
                                      (aref c-pat (parse-element-pos pe)))
                                     (pattern-var-ss-m-constructions
                                      (aref c-pat (parse-element-pos pe))))))))

                  (dolist (npc n-plist)

                    (let ((npe (make-parse-element :construction npc
                                                   :pos 0))
                          (npe-ei (make-pe-extra)))

                      ;; we replace the element if the score is better or does not exist
                      (when (or (heap-not-exists (aref parse-chart tok-pos tok-pos)
                                                 npe)
                                (< (nth-value 1 (heap-get (aref parse-chart tok-pos tok-pos) npe))
                                   (score-pe npe npe-ei 0.0 1.0)))
                        
                        (setf (gethash `(,tok-pos ,tok-pos ,npe) pc-extra) 
                              npe-ei)
                        
                        (heap-insert new-check-set npe 
                                     (score-pe npe npe-ei 0.0 1.0)))))))
              
			 

              (heap-meld (aref parse-chart tok-pos tok-pos) new-check-set)

              (setf check-set new-check-set)
              (setf new-check-set (make-heap)))))))
			 

(defun cp-scanner (pc-extra parse-chart tok-pos new-token)
  "Move pos forward for relevant constructions (starting at start-pos). 
   In the process add these constructions to 1+tok-pos

   pc-extra - extra information hash-table
   parse-chart - the 2d array representing the parse
   tok-pos - where the token is currently
   new-token - the actual new input token" 

  (dotimes (i (1+ tok-pos))

    (heap-do (pe nil (aref parse-chart tok-pos i))
      ;; Check if pe can be moved forward by one based on new-token
      (let* ((c-pat (get-c-pattern (parse-element-construction pe)))
             (pe-ei-elem (gethash `(,tok-pos ,i ,pe) pc-extra))
             (c-g-node (when (and c-pat
                                  (not (pe-completed? pe)))
                         (pattern-var-g-node (aref c-pat (parse-element-pos pe))))))

        ;; CHeck if the new-token satisfies the next var for this construction
        (when (and c-g-node
                   (check-gen-satisfied-all new-token c-g-node))
		  
          ;; Create the new parse-element
          (let* ((new-pe (make-parse-element :construction 
                                             (parse-element-construction pe)
                                             :pos
                                             (1+ (parse-element-pos pe))))
                 (rep-elem-fn-se (get-construction-trigger-option
                                  (parse-element-construction pe)
                                  *c-rep-element-fn*))
                 (rep-elem-fn (when (element-p rep-elem-fn-se)
                                (internal-name rep-elem-fn-se)))
                 (npe-ei (make-pe-extra :rep-element
                                        (if (pattern-var-head-var 
                                             (aref c-pat (parse-element-pos pe)))
                                            (return-as-gen-type new-token 
                                                                c-g-node)
                                            (if (functionp rep-elem-fn)
                                                (funcall rep-elem-fn
                                                         (pe-extra-info-rep-element pe-ei-elem)
                                                         (return-as-gen-type new-token 
                                                                             c-g-node))
                                                (pe-extra-info-rep-element pe-ei-elem)))
                                        :prior-pe `(,tok-pos ,i ,pe)
                                        :matching-score (+ (pe-extra-info-matching-score 
                                                            pe-ei-elem)
                                                           (get-gen-match-value new-token 
                                                                                c-g-node))
                                        :last-matched-item tok-pos)))
			
            (setf (gethash `(,(1+ tok-pos) ,i ,new-pe) pc-extra)
                  npe-ei)

            (heap-insert (aref parse-chart (1+ tok-pos) i)
                         new-pe
                         (score-pe new-pe npe-ei 0.0 (- tok-pos i)))))))))

(defun cp-skip (pc-extra parse-chart tok-pos)
  "Skips over the current token for existing constructions

   pc-extra - extra information hash-table
   parse-chart - the 2d array representing the parse
   tok-pos - where the token is currently"
  (dotimes (i (1+ tok-pos))

    (heap-do (pe nil (aref parse-chart tok-pos i))
             
      (let ((pe-ei-elem (gethash `(,tok-pos ,i ,pe) pc-extra)))
        
        (when (heap-not-exists (aref parse-chart (1+ tok-pos) i) pe)

          (let* ((new-pe (make-parse-element :construction 
                                             (parse-element-construction pe)
                                             :pos
                                             (parse-element-pos pe)))
                 (skip-penalty (* *score-pe-skip-base-penalty*
                                  (pe-extra-info-current-skip-penalty pe-ei-elem)))
                 (npe-ei (make-pe-extra :rep-element (pe-extra-info-rep-element pe-ei-elem)
                                        :do-not-complete (pe-completed? pe)
                                        :prior-pe (pe-extra-info-prior-pe pe-ei-elem)
                                        :current-skip-penalty skip-penalty
                                        :matching-score (pe-extra-info-matching-score 
                                                         pe-ei-elem)
                                        :last-matched-item (pe-extra-info-last-matched-item
                                                            pe-ei-elem))))

            (setf (gethash `(,(1+ tok-pos) ,i ,new-pe) pc-extra)
                  npe-ei)
              
            (heap-insert (aref parse-chart (1+ tok-pos) i)
                         new-pe
                         (score-pe new-pe npe-ei 0.0 (- tok-pos i)))))))))

(defun cp-manage-size (parse-chart tok-pos)
  "Limit size of chart elements indicated at tok-pos. This calls the heap function
   to resize the heap at that point if necessary, keeping only the highest scoring elements."
  (dotimes (i (1+ tok-pos))
    (heap-downsize (aref parse-chart tok-pos i)
                   *c-max-constructions-per-chart-element*)))


(defun cp-build-parse-chart (tokens &optional
                             (start-constructions
                              (list-inferiors 
                               (lookup-element *scone-construction*))))
  "Build a parse chart given an input list of tokens"
  (let* ((token-list tokens)
         (n (length tokens))
         (parse-chart (make-array (list (1+ n) (1+ n))))
         (pc-extra (make-hash-table :test 'equal :size 512)))

    ;; Initialize the array
    (dotimes (i (1+ n))
      (dotimes (j (1+ n))
        (setf (aref parse-chart i j) (make-heap))))

    ;; Seed start-constructions into the array
    (dolist (c start-constructions)
      (let ((npe (make-parse-element :construction c
                                     :pos 0))
            (npe-ei (make-pe-extra)))
        (setf (gethash `(0 0 ,npe) pc-extra) npe-ei)
        (heap-insert (aref parse-chart 0 0) 
                     npe
                     (score-pe npe npe-ei 0.0 1.0))))

    ;; Start the processing cycle!
    (dotimes (tok-pos n)

      ;; We are now at tok-pos

      ;; Predictor first
      (cp-predictor pc-extra parse-chart tok-pos)

      ;; Manage Size to ensure finite running time
      (cp-manage-size parse-chart tok-pos)
  
      ;; Scanner - will add stuff to tok-pos+1
      (cp-scanner pc-extra parse-chart tok-pos (nth tok-pos token-list))

      ;; Skipper - either skip token or allow construction to skip
      (cp-skip pc-extra parse-chart tok-pos)

      ;; For every completed construction, call completer
      (cp-completer pc-extra parse-chart (1+ tok-pos) (1+ tok-pos) nil token-list))

    ;; Output completed stuff

    (when *verbose*
      (format t "Parse Chart (end-pos start-pos: constructions)~%")
      (dotimes (i (1+ n))
        (format t "~%")
        (dotimes (j (1+ n))
          (format t "~A ~A:~%" i j)
          (heap-do (e p (aref parse-chart i j))
            (format t "    ~A ~A ~A~%" e p
                    (pe-extra-info-current-skip-penalty (gethash `(,i ,j ,e) pc-extra))))))

      (format t "~%-----------------~%Complete Matches~%")
      (heap-do (e p (aref parse-chart n 0))
        (when (pe-completed? e)
          (format t "~A ~A~%" e p))))

    (values parse-chart pc-extra)))

;; Functions for Instantiating!

;; General Idea
;; 1. Build translation table from vX to matched element 
;;                                       [an actual scone thing or a string]
;;    1a. The parser above will already have included information on 
;;        which elements to link back to obtain the score mentioned
;; 2. Run the c-code associated with the pattern
;;    - create a let () with the table and then do the c-code
;;    - eval!
;; 3. associate the triggers with the variables
;;    - return a list of trigger and option pairs

(defun cp-got-pe (parse-chart s-tok e-tok construction pos)
  "Checks if a particular parse position has a construction completed to position pos."
  (heap-exists 
   (aref parse-chart e-tok s-tok)
   (make-parse-element :construction construction
                       :pos pos)))

(defstruct var-match-info 
  "var-match-info allows for a reconstruction of the match-tree"
  pe tok-pos var-ht var-minfo-ht)

(defun cp-build-inst-table (tokens 
                            pc-extra
                            parse-chart 
                            start-tok
                            end-tok 
                            parse-elem
                            &optional 
                            (t-info nil))

  "Builds and returns a table with two objectives:
      Translating from variables to actual scone objects or strings AND 
      a trigger/trigger option pairing (note that the actual objects/strings
      actually come from the trigger {c-head} )

  Calls cp-instantiate on things to further evaluate.
  This function decides on the partitioning if there are multiple matches.

  Normal calls should have end-tok = (length-tokens)

   tokens -  the list of actual input tokens
   pc-extra - extra information hash-table
   parse-chart - the 2d array representing the parse
   start-tok - starting position of the parse-elem
   end-tok - ending position of the parse-elem
   parse-elem - the parse element that was completed
   t-info - token information, additional if you want to reconstruction the
            match tree"

  ;; Create var-hashtable, base on var-list
  ;; maps VX -> hashtable that maps
  ;;            triggers -> value filler

  (let ((var-ht (make-hash-table :test 'equal :size 16))
        (var-minfo-ht (make-hash-table :test 'equal :size 16)))
	
    ;; Scan back parse-elem
    ;; Basically, we want to look at last-matched-item and instantiate that
    ;; Then, we can backtrack by using prior-pe.
    (loop 
       named build-table-scan-construction
       for construction = (parse-element-construction parse-elem)
       for pos = (1- (parse-element-pos parse-elem)) 
       for match-elem = (when (>= pos 0)
                          (aref (get-c-pattern construction) pos))
       for pe-extra = (gethash `(,end-tok ,start-tok ,parse-elem)
                               pc-extra)
       for last-matched-item = (when pe-extra
                                 (pe-extra-info-last-matched-item pe-extra))
       for prior-pe = (when pe-extra
                        (pe-extra-info-prior-pe pe-extra))
       until (or (= 0 (parse-element-pos parse-elem)) 
                 (null parse-elem))
       do (progn

            (cond 

              ;; Skip Case
              ((equal last-matched-item
                      'skip-var)
               nil)
              
              ((listp last-matched-item)
               
               ;; Recurse Case
               ;; This is a list when another construction was used
               ;; to fulfill this variable
               (let* ((e-tok (first last-matched-item))
                      (s-tok (second last-matched-item))
                      (mpe (third last-matched-item)))

                 ;; Actually instantiate this other construction
                 (multiple-value-bind (t-ht v-ht v-mi-ht)
                     (cp-instantiate tokens pc-extra parse-chart
                                     s-tok e-tok
                                     mpe t-info)

                   ;; Pick out the values we want
                   (setf (gethash (pattern-var-v-name match-elem)
                                  var-minfo-ht)
                         (make-var-match-info :pe mpe
                                              :var-ht v-ht
                                              :var-minfo-ht v-mi-ht))
		   
                   (setf (gethash
                          (pattern-var-v-name match-elem)
                          var-ht)
                         (return-as-gen-type (gethash *c-head* t-ht)
                                             (pattern-var-g-node match-elem))))))
              
              ;; Token Case
              (t (progn
                   (setf (gethash (pattern-var-v-name match-elem) var-minfo-ht)
                         (make-var-match-info :tok-pos (if t-info
                                                           (nth last-matched-item t-info)
                                                           last-matched-item)))
                   (setf (gethash (pattern-var-v-name match-elem) var-ht)
                         (return-as-gen-type (nth last-matched-item tokens)
                                             (pattern-var-g-node
                                              match-elem))))))

			 
            ;; Backtrack!
            (if (and prior-pe
                     (listp prior-pe))

                (progn
                  (setf end-tok (first prior-pe))
                  (setf start-tok (second prior-pe))
                  (setf parse-elem (third prior-pe)))

                (progn
                  (setf parse-elem nil)
                  (return-from build-table-scan-construction)))))
	
    (values var-ht var-minfo-ht)))


(defun cp-obtain-all-c-code (construction)
  "This function retrieves the c-code of a construction and all its parents, 
   putting them together such that the parents code is executed first"
  `(progn
     ,@(nreverse 
        (loop
           for c = construction then (parent-wire c)
           until (or (equal c *thing*) (equal c *scone-construction*))
           collect `(progn 
                      ,@(get-element-property c *c-code-key*))))))

(defun cp-perform-inst (iv-ht construction)

  "Run (eval) the actual stuff as in *c-code*. 

  iv-ht - the variable hash table obtained from build-inst-table
  construction - the construction to run the code from"

  ;; Build the variables in another hash-table
  ;; Go through the varlist, and either
  ;;   1. call inst-code
  ;;   2a. create new indv (if option is set)
  ;;       -parent is the person in var-ht
  ;;   2b. direct reference to var-ht thing

  (let* (var-ht
         (vlist (get-c-varlist construction))
         (let-list (list `(var-ht (make-hash-table :test 'equal :size 16))))
         (c-code (cp-obtain-all-c-code construction))
         eval-list)

    (dolist (v vlist) 
	  
      (let* ((v-name (first v))
             (v-opts (third v))
             (i-code (find-if (lambda (x) (equal 
                                           (when (listp x)
                                             (first x))
                                           'inst-code))
                              v-opts)))
        (cond
          ;; There is some inst-code to use already
          (i-code 
           (push `(,v-name ,@(cdr i-code)) let-list))

          ;; This is supposed to be a generic indv
          ((find 'generic v-opts) 
           (push `(,v-name 
                   (if (generic-indv-node? 
                        (lookup-element-predicate
                         ,(gethash v-name iv-ht)))
                       (lookup-element-predicate
                        ,(gethash v-name iv-ht))
                       (new-indv nil 
                                 (lookup-element-predicate
                                  ,(gethash v-name iv-ht))
                                 :proper nil))) let-list))

          ;; Supposed to be a proper indv
          ((find 'instance v-opts) 
           (push `(,v-name 
                   (if (proper-indv-node? 
                        (lookup-element-predicate
                         ,(gethash v-name iv-ht)))
                       (lookup-element-predicate
                        ,(gethash v-name iv-ht))
                       (new-indv nil 
                                 (lookup-element-predicate
                                  ,(gethash v-name iv-ht))))) let-list))

          ;; All other cases, we just take the value from iv-ht
          (t 
           (push `(,v-name ,(gethash v-name iv-ht)) let-list)))

        ;; Make sure to store the result in var-ht
        (push `(setf (gethash (quote ,v-name) var-ht) ,v-name) eval-list)))

    
    
    ;; Run c-code, build the let statement over it and eval the whole thing
    (when *verbose*
      (format t "-----------------~%Evaluating~%~S~%-----------------~%" 
              `(let* (,@let-list) ,@eval-list 
                     (setf (gethash *c-exp-eval* var-ht) ,c-code)
                     var-ht)))
	  
    ;; Actual Evaluation
    (setf var-ht
          (eval `(let* (,@let-list) ,@eval-list 
                       (setf (gethash *c-exp-eval* var-ht) ,c-code)
                       var-ht)))
    
    (when *verbose*
      (format t "var-ht: ~%" )
      (print-ht var-ht))

    var-ht))

;; So cp-instantiate takes in parse-elem, which is basically *exactly* which
;; element to use, start-tok and end-tok simply indicates the position in the
;; parse-chart and tokens its refering to
(defun cp-instantiate (tokens pc-extra parse-chart start-tok end-tok parse-elem
                       &optional (t-info nil))
  "Main function, calls build-inst table, performs-inst, then proceeds to
   do anything else necessary as indicated by the triggers. Returns a list of 
   trigger-opt pairs

   tokens -  the list of actual input tokens
   pc-extra - extra information hash-table
   parse-chart - the 2d array representing the parse
   start-tok - starting position of the parse-elem
   end-tok - ending position of the parse-elem
   parse-elem - the parse element that was completed
   t-info - token information, additional if you want to reconstruction the
            match tree"
  

  ;; First build the inst-table
  (multiple-value-bind (iv-ht v-mi-ht)
      (cp-build-inst-table 
       tokens pc-extra parse-chart 
       start-tok end-tok parse-elem
       t-info)

    ;; Perform the instantiation
    (let* ((v-ht (cp-perform-inst iv-ht
                                 (parse-element-construction
                                  parse-elem)))
           (t-ht (make-hash-table :test 'equal :size 16))
           (c-head-opt (get-construction-trigger-option 
                        (parse-element-construction parse-elem)
                        *c-head*))
           (v-c-head-name (first (when c-head-opt
                                   (find-if 
                                    (lambda (x) (equal (second x) c-head-opt))
                                    (get-c-varlist
                                     (parse-element-construction parse-elem)))))))

      ;; Set triggers here
      (when (equal c-head-opt *c-exp-eval*)
        (setf v-c-head-name *c-exp-eval*))

      (when v-c-head-name
        (setf (gethash *c-head* t-ht)
              (gethash v-c-head-name v-ht)))
	  
      (values t-ht v-ht v-mi-ht))))
	  

(defun cp-instantiate-simple (tokens pc-extra parse-chart)
  "Instantiation helper to pick the highest scoring construction and use it.

   tokens -  the list of actual input tokens
   pc-extra - extra information hash-table
   parse-chart - the 2d array representing the parse"

  (let* ((s-tok 0)
         (e-tok (length tokens))
         (target-constructions (list-inferiors 
                                (lookup-element *c-target-constructions*)))
         (pe (heap-get-max (aref parse-chart e-tok s-tok)
                           (lambda (x) (and (pe-completed? x)
                                            (find (parse-element-construction x)
                                                  target-constructions))))))
	
    (if pe
        (progn

          (format t "Instantiating with ~%~A~%" pe)
          
          (multiple-value-bind (t-ht v-ht v-mi-ht) (cp-instantiate tokens 
                                                                   pc-extra
                                                                   parse-chart 
                                                                   s-tok e-tok pe)
            (values t-ht v-ht v-mi-ht pe)))
        (format t "No constructions completed!"))))



(defun cp-init ()
  "Initializes all the constructions by chaining them appropriately."
  (dolist (e (list-inferiors *scone-construction*))
    (build-matching-constructions-to-pattern e)))

(defun cp-interpret (tokens-or-string 
                     &optional (start-constructions  nil))
  "Helper function to interpret a string (which will be broken into a series
   of tokens) or simply a list of tokens"

  ;; Set initial constructions
  (unless start-constructions
    (setf start-constructions
          (list-inferiors 
           (lookup-element *c-target-constructions*))))

  ;; Pre-process the tokens
  (let* ((tokens (if (stringp tokens-or-string)
                     (str-explode tokens-or-string)
                     tokens-or-string)))

    ;; Build the parse-chart
    (multiple-value-bind (pc pc-extra) 
        (cp-build-parse-chart tokens 
                              start-constructions)

      ;; Do a simple instantiation
      (multiple-value-bind (t-ht v-ht v-mi-ht pe) (cp-instantiate-simple tokens 
                                                                         pc-extra pc)
        
        (when (and t-ht v-ht)

          (let* ((c-head-opt (get-construction-trigger-option
                              (parse-element-construction pe)
                              *c-head*))
                 (v-c-head-name (first (when c-head-opt
                                         (find-if 
                                          (lambda (x) (equal (second x) c-head-opt))
                                          (get-c-varlist
                                           (parse-element-construction pe)))))))

            (when (equal c-head-opt *c-exp-eval*)
              (setf v-c-head-name *c-exp-eval*))

            (format t "Overall Construction used for Interpretation is ~A ~%" pe)

            (format t "Trigger HT: ~%")
            (print-ht t-ht)
		
            (format t "Variable HT: ~%")
            (print-ht v-ht)

            (values (gethash v-c-head-name v-ht)
                    pe t-ht v-ht v-mi-ht)))))))

 



(defun cp-set-target-constructions (target-construction)
  (setf *c-target-constructions* target-construction))


;; Additional Helpers

(defun remove-all-constructions ()
  "Developer function, removes all constructions defined"
  (dolist (e (list-inferiors *scone-construction*) )
    (remove-element e)))

(defun cp-build-parse-hierarchy (pc-extra
                                 parse-chart 
                                 start-tok
                                 end-tok 
                                 parse-elem
                                 &optional
                                 (t-info nil))

  "Variant of build-inst without the inst"
  (let ((match (list (parse-element-construction parse-elem))))
	
    ;; Scan back parse-elem
    (loop 
       named build-table-scan-construction
       for construction = (parse-element-construction parse-elem)
       for pos = (1- (parse-element-pos parse-elem)) 
       for match-elem = (when (>= pos 0)
                          (aref (get-c-pattern construction) pos))
       for pe-extra = (gethash `(,end-tok ,start-tok ,parse-elem)
                               pc-extra)
       for last-matched-item = (when pe-extra
                                 (pe-extra-info-last-matched-item pe-extra))
       for prior-pe = (when pe-extra
                        (pe-extra-info-prior-pe pe-extra))
       until (or (= 0 (parse-element-pos parse-elem)) 
                 (null parse-elem))
       do (progn

            (cond 

              ;; Skip Case
              ((equal last-matched-item
                      'skip-var)
               nil)
              
              ((listp last-matched-item)
               ;; Recurse Case
               (push 
                (let* ((e-tok (first last-matched-item))
                       (s-tok (second last-matched-item))
                       (mpe (third last-matched-item)))
                  
                  (cp-build-parse-hierarchy pc-extra
                                            parse-chart 
                                            s-tok
                                            e-tok 
                                            mpe
                                            t-info))
                match))
              
              ;; Token Case
              (t (push (if t-info
                           (nth last-matched-item t-info)
                           last-matched-item)
                       match)))
			 
            (if (and prior-pe
                     (listp prior-pe))
                
                (progn
                  (setf end-tok (first prior-pe))
                  (setf start-tok (second prior-pe))
                  (setf parse-elem (third prior-pe)))

                (progn
                  (setf parse-elem nil)
                  (return-from build-table-scan-construction)))))
	
    (nreverse match)))

(defun cp-build-match-hierarchy (head-elem var-ht var-minfo-ht pe)
  (let* ((construction (parse-element-construction pe))
         (vlist (get-c-varlist construction))
         (m-hierarchy (list construction head-elem)))
    (dolist (v vlist)
      (let ((v-minfo (gethash (first v) var-minfo-ht))
            (v-head-elem (gethash (first v) var-ht)))
        (when v-minfo
          (push 
           (if (var-match-info-tok-pos v-minfo)
               (var-match-info-tok-pos v-minfo)
               (cp-build-match-hierarchy v-head-elem
                (var-match-info-var-ht v-minfo)
                (var-match-info-var-minfo-ht v-minfo)
                (var-match-info-pe v-minfo)))
           m-hierarchy))))
    (nreverse m-hierarchy)))

(defun cp-test-get-structure (tokens-or-string 
                      &optional (start-constructions nil))

  (multiple-value-bind (head-elem pe t-ht v-ht v-mi-ht)
      (cp-interpret tokens-or-string start-constructions)
    t-ht ;; don't like unused warnings
    (when head-elem
      (cp-build-match-hierarchy head-elem v-ht v-mi-ht pe))))
 
