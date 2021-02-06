;;; -*- Mode:Lisp -*-

;;; =======================================================================
;;; Gen-Obj Structure
;;;
;;; The Gen-Obj structure provides functionality to represent
;;; sets and specify these sets in a compact and efficient way.
;;; The representation is embodied in a structure called a gen-obj.

(provide "scone-gen-obj")
(require "scone" "scone-loader.lisp")
(require "scone-general" "general.lisp")
(require "lisp-wordnet-interface" "lisp-wordnet-interface.lisp")

;; Special scone-specific variable used in some cases
(defparameter *set-member* (lookup-element-pred {common:member}))

;; Gen-Obj - The generalized object structure
(defstruct (gen-obj (:constructor internal-make-gen-obj)) 
  "Gen-obj provides the basic functionality for matching"
  (string nil :type list)
  (predicates nil :type list)
  (is-a nil :type list)
  (is-not-a nil :type list)
  (rel nil :type list)
  (role nil :type list)
  (x-of-y nil :type list)

  ;; Alternative nodes to match (gives us a OR of ANDs, think DNF)
  (gen-objs nil :type list)

  ;; Set option, this node can also match a set
  (match-set nil))


(defun make-gen-obj-process-list (input-list &key no-quote)
  "Eases the process of defining gen-obj by allowing flexible arguments"
  (let ((string-opt (find 'string input-list))
        string-list predicate-list
        is-a-list is-not-a-list
        role-list rel-list x-of-y-list
        option-list)
	
    (when string-opt
      (push *string* is-a-list))

    (dolist (v-res input-list)

      ;; Process 
      (cond

        ;; Following processes special cases 
        ((and (listp v-res)
              (eql (first v-res) 'inst-code))
         (push (if no-quote
                   v-res
                   `(quote ,v-res)) option-list))

        ((and (listp v-res)
              (eql (first v-res) 'role))
         (push (if no-quote
                   (cdr v-res)
                   `(quote ,(cdr v-res))) role-list))

        ((and (listp v-res)
              (eql (first v-res) 'rel))
         (push (if no-quote
                   (cdr v-res)
                   `(quote ,(cdr v-res))) rel-list))

        ((and (listp v-res)
              (eql (first v-res) 'x-of-y))
         (push (if no-quote
                   (cdr v-res)
                   `(quote ,(cdr v-res))) x-of-y-list))

        ((and (listp v-res)
              (eql (first v-res) 'is-not-a))
         (let ((v-res-e 
                (lookup-element-predicate (cadr v-res))))
           (when v-res-e
             (push v-res-e
                   is-not-a-list))))
			

        ;; If it is not a special case, attempt to decide what it should be

        ((symbolp v-res)
         ;; Add to option list
         (push (if no-quote
                   v-res
                   `(quote ,v-res)) option-list))

        ((stringp v-res)
         (if string-opt
             (push v-res
                   string-list)
             (let ((v-res-e 
                    (lookup-element-predicate v-res)))
               (when v-res-e
                 (push v-res-e
                       is-a-list)))))
										
        ((element-iname-p v-res)
         (let ((v-res-e 
                (lookup-element-predicate v-res)))
           (when v-res-e
             (push v-res-e
                   is-a-list))))

        ((functionp v-res)
         (push v-res
               predicate-list))))

    (values string-list predicate-list
            is-a-list is-not-a-list
            role-list rel-list x-of-y-list
            option-list)))

(defun make-gen-obj (&key is-a is-not-a string 
                     predicates role rel x-of-y
                     gen-objs match-set)
  "Creates a Gen-Obj"
  (internal-make-gen-obj 
   :string (make-list-if-needed string)
   :predicates (make-list-if-needed predicates)
   :is-a (make-list-if-needed is-a)
   :is-not-a (make-list-if-needed is-not-a)
   :rel (make-gen-obj-process-rel (make-list-if-needed rel))
   :role (make-gen-obj-process-role-or-x-of-y (make-list-if-needed role))
   :x-of-y (make-gen-obj-process-role-or-x-of-y (make-list-if-needed x-of-y))
   :gen-objs (make-list-if-needed gen-objs)
   :match-set match-set))

(defun make-gen-obj-from-list (l)
  "Creates a Gen-Obj given a list of properties specified by an inputl list.
   This is meant to be user-friendly."
  (multiple-value-bind (string-list
                        predicate-list
                        is-a-list is-not-a-list
                        role-list rel-list x-of-y-list
                        option-list)
 
      (make-gen-obj-process-list (make-list-if-needed l))
	
    (make-gen-obj
     :is-a is-a-list
     :is-not-a is-not-a-list
     :role role-list
     :rel rel-list
     :x-of-y x-of-y-list
     :string string-list
     :predicates predicate-list
     :match-set (if (find 'match-set option-list)
                    t nil))))

(defun make-gen-obj-process-role-or-x-of-y (role-list)
  "Helps process role lists"
  (mapcar (lambda (role-entry)

            (when (eql (first role-entry) 'quote)
              (setf role-entry (cadr role-entry)))

            (setf (first role-entry) 
                  (lookup-element-pred (first role-entry)))

            (if (gen-obj-p (second role-entry))
                role-entry
                (list (first role-entry)
                      (make-gen-obj-from-list
                       (cdr role-entry))))) role-list))

(defun make-gen-obj-process-rel (rel-list)
  "Helps process relation lists. 
   Assume correct input: i.e. each entry looks like
      {relation-iname} :a (restrictions ..)
                       :b (...) 
                       :c (...) // note that param c is not usable"
  (mapcar (lambda (rel-entry)

            (when (eql (first rel-entry) 'quote)
              (setf rel-entry (cadr rel-entry)))

            (setf (first rel-entry) (lookup-element-pred (first rel-entry)))
            (dolist (check-pos `(2 4 6))
              (when (and 
                     (> (length rel-entry) check-pos)
                     (not (gen-obj-p (nth check-pos rel-entry))))
                (setf (nth check-pos rel-entry)
                      (make-gen-obj-from-list (nth check-pos rel-entry)))))
            rel-entry)
          rel-list))

(defun lookup-element-extended (name 
                                &key
                                syntax-tags
                                hints
                                disallow-list
                                force-return)
  "Extended version of lookup-element with morphy support"

  ;; Force a particular value to be returned
  (when force-return
    (return-from lookup-element-extended force-return))
    
  (if (stringp name)

      ;; Attempt to use morphy if we do not have a def lookup
      (let ((defs (lookup-definitions name syntax-tags)))
        (when (and *wn-interface-loaded*
                   (null defs))
          (let ((new-name (wordnet-get-base-form name)))
            (when (not (equal new-name name))
              (setf name new-name)
              (setf defs (lookup-definitions (wordnet-get-base-form name)
                                             syntax-tags)))))

        ;; Determine how to handle the disambiguation
        ;; Basically, we want just to use heuristics all the time
        (cond ((null defs) nil)
              ((null (cdr defs))
               (values (car (car defs))
                       (cdr (car defs))))
              (t (let ((old-disam *disambiguate-policy*)
                       ret)
                   (setf *disambiguate-policy* :heuristic)
                   (dolist (del disallow-list)
                     (setf defs (delete del defs)))
                   (setf ret (disambiguate name defs
                                           syntax-tags hints))
                   (setf *disambiguate-policy* old-disam)
                   ret))))

      (lookup-element-predicate name 
                                :syntax-tags syntax-tags
                                :hints hints)))

(defun return-as-gen-type (e gen-e)
  "Assumes that e already satisfies gen-e, then returns e as the type expected - string or actual scone-element. This is important when e is a string type input token, while gen-e expects scone-elements. Then, this function will find the correct type to return, in the process looking up the element."

  ;; String case
  (when (gen-obj-string gen-e)
    (dolist (cs (gen-obj-string gen-e))
      (when (not (string-equal cs ""))
        (return-from return-as-gen-type (getstr e)))))
  
  ;; Element case
  (let ((elem (lookup-element-extended (if (stringp e)
                                           e
                                           (if (and (element-p e) 
                                                    (is-x-a-y? e *string*))
                                               (internal-name e)
                                               e))
                                       :hints (gen-obj-is-a gen-e))))

    (when (and
           (stringp e)
           (check-is-a (gen-obj-is-a gen-e) *number*))

      (let ((test-elem (parse-integer e :junk-allowed t)))
        (when (numberp test-elem)
          (setf elem (new-number test-elem)))))

    (when (and (gen-obj-is-a gen-e)
               elem
               (not (equal (gen-obj-is-a gen-e) (list *string*))))
      (return-from return-as-gen-type elem)))
  
  (when (gen-obj-string gen-e)
    (return-from return-as-gen-type (getstr e)))

  e)

(defun get-gen-match-value (e gen-e)
  "Assumes all matched up according to check-gen-satisfied-all
   Returns a score for this match 0-1.

  If the distance (in the is-a hierarchy) to gen-e is x, 
  the score is computed by the formula 1/ln(x+e-1). 
  Since x >= 1, this always gives a value between 0-1 and approaches 0
  as x tends to infinity. 

  e - the element being considered
  gen-e - the gen-obj used to score"

  (let ((ret-elem (return-as-gen-type e gen-e)))
    (cond 
      ((stringp ret-elem) 0.9)
      ((element-p ret-elem) 
       (let ((is-a-list (gen-obj-is-a gen-e)))
         (if is-a-list
             ;; Upscan by step till we get there
             ;; Basically count how far up the element is 
             (let ((m-v (progn
                          (push *thing* is-a-list)
                          (with-temp-marker (m)
                            (mark ret-elem m)
                            (loop for i = 1.0 then (1+ i)
                               until (find-if (lambda (is-e)
                                                (marker-on? is-e m)) is-a-list)
                               do (upscan nil m
                                          :augment t
                                          :one-step t)
                               finally (return i))))))

               ;; Computations, see doc-string for explanation
               (/ 1.0 (log (1- (+ m-v (exp 1))))))
             1.0)))
      (t 0.7))))

(defun check-gen-satisfied-all (e gen-e)
  "Check if the element e satisfies all requirements in gen-e"

  ;; Check for match-set first
  (when (and (gen-obj-match-set gen-e)
             (element-p e)
             (is-x-a-y? e *set*)
             (check-gen-satisfied-all 
              (get-the-x-role-of-y *set-member* e)
              gen-e))
    (return-from check-gen-satisfied-all t))
	
  ;; String Match - match one
  (block string-match-check
    (when (gen-obj-string gen-e)
      (let ((es (if (stringp e)
                    e
                    (if (and (element-p e) (is-x-a-y? e *string*))
                        (internal-name e)
                        nil))))
        (if es
            (progn
              (dolist (cs (gen-obj-string gen-e))
                (when (or (string-equal es cs) (string-equal cs ""))
                  (return-from string-match-check)))
              (return-from check-gen-satisfied-all nil))
            (return-from check-gen-satisfied-all nil)))))

  ;; Predicate
  (dolist (cp (gen-obj-predicates gen-e))
    (unless (funcall cp e)
      (return-from check-gen-satisfied-all nil)))

  ;; IS-A  and  IS-NOT-A
  (let ((elem (lookup-element-extended (if (stringp e)
                                           e
                                           (if (and (element-p e) 
                                                    (is-x-a-y? e *string*))
                                               (internal-name e)
                                               e))
                                       :hints (gen-obj-is-a gen-e))))
    
    (when (and (stringp e)
               (check-is-a (gen-obj-is-a gen-e) *number*))

      (let ((test-elem (parse-integer e :junk-allowed t)))
        (when (numberp test-elem)
          (setf elem (new-number test-elem)))))
	
    ;; We exclude *string* specially
    (when (and (gen-obj-is-a gen-e)
               (not (equal (gen-obj-is-a gen-e) (list *string*)))
               (or (not elem) (equal elem *string*)))
      (return-from check-gen-satisfied-all nil))

    ;; Get Markers and marked all the parents
    (when (and (or (gen-obj-is-a gen-e) 
                   (gen-obj-is-not-a gen-e))
               (not (equal (gen-obj-is-a gen-e) (list *string*)))
               elem)

      (with-temp-markers (m) 
		
        (upscan elem m)

        ;; Marked, now check memberships	
        (dolist (ce (gen-obj-is-a gen-e))
          (when (marker-off? ce m)
            (return-from check-gen-satisfied-all nil)))
		
        (dolist (ce (gen-obj-is-not-a gen-e))
          (when (marker-on? ce m)
            (return-from check-gen-satisfied-all nil))))))

  ;; Roles
  (dolist (crole (gen-obj-role gen-e))
    (let ((erole (find-the-x-role-of-y (first crole)
                                       e)))
      (when (or (null erole)
                (not (check-gen-satisfied-all erole (second crole))))
        (return-from check-gen-satisfied-all nil))))

  ;; x-of-y
  (when (gen-obj-x-of-y gen-e)
    (dolist (cx-of-y (gen-obj-x-of-y gen-e))
      (block x-of-y-check
        (dolist (e-filler (list-all-x-of-y (first cx-of-y) e))
          (when (check-gen-satisfied-all e-filler (second cx-of-y))
            (return-from x-of-y-check)))
        (return-from check-gen-satisfied-all nil))))

  ;; Relations
  ;; Tricky one. if A and B specified, will need to do one run for each
  ;; list-rel and list-rel-inverse
  (dolist (crel (gen-obj-rel gen-e))

    (let (calist
          (rel (first crel)))
	  
      (do ((vlist (cdr crel) (cddr vlist)))
          ((null vlist))
        (setf calist
              (acons (first vlist) (second vlist) calist)))
	  
      (let ((aparam (cdr (assoc :a calist)))
            (bparam (cdr (assoc :b calist)))
            (cparam (cdr (assoc :c calist))))

        ;; NOTE: There is no easy way to retrieve param C now 
        ;;       so it shall be ignored
        cparam ;; hide warning
		
        ;; Do for param a (i.e. find some X rel E s.t. X satisfies A)
        (when aparam
          (block check-aparam
            (dolist (ce (list-rel-inverse rel e))
              (when (check-gen-satisfied-all ce aparam)
                (return-from check-aparam)))
            (return-from check-gen-satisfied-all nil)))

        (when bparam
          (block check-bparam
            (dolist (ce (list-rel e rel))
              (when (check-gen-satisfied-all ce bparam)
                (return-from check-bparam)))
            (return-from check-gen-satisfied-all nil))))))
	  	   
  ;; Gen-Objs, enter only when set
  ;; does an OR over the gen-objs 
  (when (gen-obj-gen-objs gen-e)
    (dolist (cgo (gen-obj-gen-objs gen-e))
      (when (check-gen-satisfied-all e cgo)
        (return-from check-gen-satisfied-all t)))
    (return-from check-gen-satisfied-all nil))

  ;; All Checks Passed
  t)



(defun check-gen-x-subset-of-y (gen-x gen-y)
  "Check if nodes matched by GEN-X are definitely a subset of those matched by GEN-Y."

  ;; Check string
  ;; If y has strings, then 
  ;; find if there exists a string in X that is eql to Y
  ;; use set functions
  (block check-strings
    (when (gen-obj-string gen-y)
      (let ((y-strs (set-create)))
        (dolist (es (gen-obj-string gen-y))
          (set-add-member y-strs es)
          (when (equal es "")
            (return-from check-strings t)))
        (dolist (cs (gen-obj-string gen-x))
          (when (set-is-not-member y-strs cs)
            (return-from check-gen-x-subset-of-y nil))))))
  
  ;; Check is-a
  ;; For each is-a in Y, check
  ;; if there exist an is-a in X such that it satisfies Y
  (when (gen-obj-is-a gen-y)
    (with-temp-markers (m)
      (dolist (xe (gen-obj-is-a gen-x))
        (upscan xe m :augment t))
      (dolist (ye (gen-obj-is-a gen-y))
        (when (and (not (equal *string* ye)) (marker-off? ye m))
          (return-from check-gen-x-subset-of-y nil)))))
  
  ;; Check role
  ;; For every role in Y, 
  ;; make sure X has some role that satisfies it 
  (when (gen-obj-role gen-y)
    (dolist (y-r (gen-obj-role gen-y))
      (block check-one-y-r
        (dolist (x-r (gen-obj-role gen-x))
          (when (and (eql (first y-r)
                          (first x-r))
                     (check-gen-x-subset-of-y (second x-r)
                                              (second y-r)))
            (return-from check-one-y-r t)))
        (return-from check-gen-x-subset-of-y nil))))
	   
  T) 



;; Naive Generalization Algorithm
(defun set-generalize (elist initial-act) 

  "Generalize a set of elements"

  (let (set-general-elements)
    (loop until (null elist)
       for max-act-e = nil
       do (progn
		   
            ;; Activate
            (reset-activation-levels)
            (activate-element-list elist initial-act)
		   
            ;; Pick maximum activated element
            (setq max-act-e (get-max-act-element))
            (push max-act-e set-general-elements)

            (when *verbose*
              (show-activated-elements)
              (format t "Picking element ~A with act ~A~%" 
                      max-act-e (get-activation-fast max-act-e)))

            (let (new-elist)

              ;; Down-scan from this element and remove all list elements
              (with-temp-marker (m)
                (downscan max-act-e m)
                (dolist (e elist)
                  (unless (marker-on? e m)
                    (push e new-elist))))
			 
              (setf elist new-elist))))

    set-general-elements))
