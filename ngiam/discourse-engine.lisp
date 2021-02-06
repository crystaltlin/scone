;;; -*- Mode:Lisp -*-

;;; =======================================================================
;;; Dialogue Engine

(provide "discourse-engine")
(require "scone-constructions" "construction-engine.lisp")

(defvar *processed-sentences* (make-hash-table :test 'equal
                                               :size 64))

(defstruct (token-info
             (:type list))
  offset
  length)

(defstruct s-mem
  tokens
  t-info
  parse-chart
  pc-extra)

(defun cp-discourse-str-explode-1 (str 
                                  &key
                                  (delimiter-discard '(#\ ))
                                  (delimiter-ignore '(#\-))
                                  (current-offset 0))

  (let* ((pos (position-if (lambda (x) (not (or (alphanumericp x)
                                                (find x delimiter-ignore))))
                           str))
         (ch (when pos (elt str pos))))

    (if (null pos)
        (values (list str)
                (list (make-token-info :offset current-offset
                                       :length (length str))))
        (multiple-value-bind (tokens t-info)
            (cp-discourse-str-explode-1 (subseq str (1+ pos))
                                       :delimiter-discard delimiter-discard
                                       :delimiter-ignore delimiter-ignore
                                       :current-offset (+ current-offset pos 1))
          (if (find ch delimiter-discard)
              (values
               (cons (subseq str 0 pos) tokens)
               (cons (make-token-info :offset current-offset
                                      :length pos) t-info))
              (values
               (cons (subseq str 0 pos) (cons (string ch) tokens))
               (cons (make-token-info :offset current-offset
                                      :length pos)
                     (cons (make-token-info :offset (+ current-offset pos)
                                            :length 1)
                           t-info))))))))

(defun cp-discourse-str-explode (str 
                                &key
                                (delimiter-discard '(#\ ))
                                (delimiter-ignore '(#\-))
                                (current-offset 0))

  (multiple-value-bind (tokens t-info)
      (cp-discourse-str-explode-1 str
                                 :delimiter-discard delimiter-discard
                                 :delimiter-ignore delimiter-ignore
                                 :current-offset current-offset)
    (let ((tokens (delete "" tokens :test 'equal))
          (t-info (delete-if (lambda (x) (equal 0 (cadr x))) t-info)))
      (values tokens t-info))))

(defun cp-discourse-get-unused-id ()
  (loop 
     for id = (random 2147483647)
     while (gethash id *processed-sentences*)
     finally (return id)))

(defun cp-discourse-process-sentence (string
                                     &optional (start-constructions nil))
  (unless start-constructions
    (setf start-constructions
          (list-inferiors 
           (lookup-element *c-target-constructions*))))
  
  (multiple-value-bind (tokens t-info)
      (cp-discourse-str-explode string)

    (let ((n (length tokens))
          (complete-matches))

      (multiple-value-bind (pc pc-extra)
          (cp-build-parse-chart tokens start-constructions)

        (heap-do (e p (aref pc n 0))
          (when (and (pe-completed? e)
                     (find (parse-element-construction e)
                           start-constructions))
            (push (list (parse-element-construction e) p)
                  complete-matches)))

        (let ((s (make-s-mem :tokens tokens :t-info t-info
                             :parse-chart pc :pc-extra pc-extra))
              (sentence-id (cp-discourse-get-unused-id)))

          (setf (gethash sentence-id *processed-sentences*) s)
        
          (cons sentence-id complete-matches))))))


(defun cp-discourse-get-structure (sentence-id construction)
  (let ((construction (lookup-element-pred construction))
        (s (gethash sentence-id *processed-sentences*)))

    (when (and (element-p construction)
               s)
      (let* ((tokens (s-mem-tokens s))
             (n (length tokens))
             (t-info (s-mem-t-info s))
             (pc (s-mem-parse-chart s))
             (pc-extra (s-mem-pc-extra s))

             (pe (heap-find (aref pc n 0) 
                            (lambda (x) 
                              (and (pe-completed? x)
                                   (equal (parse-element-construction x)
                                          construction))))))
             (when pe
               (cp-build-parse-hierarchy pc-extra pc 0
                                         n pe
                                         t-info))))))

(defun cp-discourse-instantiate (sentence-id construction)
   (let ((construction (lookup-element-pred construction))
        (s (gethash sentence-id *processed-sentences*)))

    (when (and (element-p construction)
               s)
      (let* ((tokens (s-mem-tokens s))
             (n (length tokens))
             (t-info (s-mem-t-info s))
             (pc (s-mem-parse-chart s))
             (pc-extra (s-mem-pc-extra s))

             (pe (heap-find (aref pc n 0) 
                            (lambda (x) 
                              (and (pe-completed? x)
                                   (equal (parse-element-construction x)
                                          construction))))))
             (when pe

               (multiple-value-bind (t-ht v-ht v-mi-ht)
                   (cp-instantiate tokens 
                                   pc-extra
                                   pc
                                   0 n pe t-info)

                 (when (and t-ht v-ht)

                   (let* ((c-head-opt (get-construction-trigger-option
                                       (parse-element-construction pe)
                                       *c-head*))
                          (v-c-head-name (first 
                                          (when c-head-opt
                                            (find-if 
                                             (lambda (x) (equal (second x)
                                                                c-head-opt))
                                             (get-c-varlist
                                              (parse-element-construction 
                                               pe))))))
                          head-elem)

                     (when (equal c-head-opt *c-exp-eval*)
                       (setf v-c-head-name *c-exp-eval*))
                     
                     (setf head-elem (gethash v-c-head-name v-ht))
                   
                     (when head-elem
                       (cp-build-match-hierarchy head-elem v-ht v-mi-ht pe))))))))))
