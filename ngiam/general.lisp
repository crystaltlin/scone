;;; -*- Mode:Lisp -*-

;; General Support Functions 

(require "scone" "scone-loader.lisp")
(provide "scone-general")
(defvar *verbose* nil)

;; --- Functions operating over properties ---

(defun get-num-property-fast (e prop-key)
  (let ((act (if e
                 (get-element-property e prop-key)
                 0)))
    (if (numberp act)
        act
        0)))

(defun get-num-property (e prop-key)
  (let ((elem (lookup-element e)))
    (if elem
        (get-num-property-fast elem prop-key)
        0)))

(defun set-num-property-fast (e prop-key prop-value)
  (set-element-property e prop-key prop-value))

(defun set-num-property (e prop-key prop-value)
  (let ((elem (lookup-element e)))
    (when elem
      (set-num-property-fast elem prop-key prop-value))))

;; --- Pseudo-set functions ---

(defun set-create ()
  (make-hash-table :test 'equal :size 256))

(defun set-add-member (s element)
  (setf (gethash element s) t) 
  element)

(defun set-is-member (s element)
  (gethash element s))

(defun set-is-not-member (s element)
  (not (gethash element s)))

(defun set-rem-member (s element)
  (remhash element s))

(defun set-clear (s)
  (clrhash s))

(defun set-is-empty? (s)
  (= 0 (hash-table-count s)))

(defun set-size (s)
  (hash-table-size s))

(defun set-print (s)
  (loop for key being the hash-keys of s
     do (format t "~A~%" key)))

(defmacro set-do ((element s) &body body)
  `(loop for ,element being the hash-keys of ,s
      do (progn
           ,@body)))

(defun set-find (s predicate)
  (set-do (e s)
          (when (funcall predicate e)
            (return-from set-find e))))

(defun set-union (s s-add)
  (set-do (e s-add)
          (set-add-member s e)))


;; --- Queue Wrapper ---
;; Simple queue. use two stacks (lists) to make a queue
;; Amortized O(1) time

(defun queue-create ()
  (cons nil nil))

(defun queue-add (q element)
  (push element (cdr q)))

(defun queue-transfer (q)
  (setf (car q) (nreverse (cdr q)))
  (setf (cdr q) nil))
 
(defun queue-empty (q)
  (when (null (car q))
    (queue-transfer q))
  (null (car q)))

(defun queue-pop (q)
  (unless (queue-empty q)
    (pop (car q))))

;; --- Min-Heap with delete,update ---

(defstruct (heap-element
             (:constructor make-heap-element))
  (key nil)
  (priority 0.0 :type single-float)
  (index 0 :type integer))

(defstruct (heap
             (:constructor make-heap))
  (array (make-array 128
                     :initial-element nil
                     :adjustable t
                     :fill-pointer 1) ;; 1-index array, easier calc
         :type vector)
  (key-to-he (make-hash-table :size 128
                              :test 'equal)
             :type hash-table))

(defun heap-get-smaller-child-index (heap index)
  ;; Children are 2*index and 2*index+1
  (let* ((c1 (* 2 index))
         (c2 (1+ c1)))
    (when (> (fill-pointer (heap-array heap)) c1)
      (let ((child c1))
        (when (and (> (fill-pointer (heap-array heap)) c2)
                   (> (heap-element-priority 
                       (aref (heap-array heap) c1))
                      (heap-element-priority 
                       (aref (heap-array heap) c2))))
          (setf child c2))
        child))))

(defun heap-swap-elements (heap i1 i2)
  (let ((e1 (aref (heap-array heap) i1))
        (e2 (aref (heap-array heap) i2)))
    (setf (aref (heap-array heap) i2) e1
          (aref (heap-array heap) i1) e2
          (heap-element-index e1) i2
          (heap-element-index e2) i1)))

(defun heap-float (heap index)
  (do ((p-index (floor index 2) (floor index 2)))
      ((or (>= 1 index)
           (> (heap-element-priority (aref (heap-array heap) index))
              (heap-element-priority (aref (heap-array heap) p-index)))))
    (heap-swap-elements heap index p-index)
    (setf index p-index))
  index)

(defun heap-sink (heap index)
  (do ((c-index (heap-get-smaller-child-index heap index) 
                (heap-get-smaller-child-index heap index)))
      ((or (null c-index)
           (> (heap-element-priority (aref (heap-array heap) c-index))
              (heap-element-priority (aref (heap-array heap) index)))))
    (heap-swap-elements heap index c-index)
    (setf index c-index))
  index)
  
(defmacro heap-do ((element priority heap) &body body)
  (let ((he (gensym)))
    `(loop 
        for ,he across (heap-array ,heap)
        for ,element = (when (heap-element-p ,he)
                         (heap-element-key ,he))
        for ,priority = (when (heap-element-p ,he)
                          (heap-element-priority ,he))
        when (heap-element-p ,he)
        do (progn
             ,@body))))

(defmacro heap-copy-do ((element priority heap) &body body)
  (let ((he (gensym))
        (h-copy (gensym))
        (te (gensym))
        (tp (gensym)))
    `(let ((,h-copy (set-create)))
       (heap-do (,te ,tp ,heap)
                (set-add-member ,h-copy (cons ,te ,tp)))
       (set-do (,he ,h-copy)
               (let ((,element (car ,he))
                     (,priority (cdr ,he)))
                 ,@body)))))

(defun heap-find (heap predicate)
  (heap-do (k p heap)
           (when (funcall predicate k)
             (return-from heap-find (values k p))))
  (values nil 0.0))

(defun heap-get-min (heap)
  (when (> (fill-pointer (heap-array heap)) 1)
    (let ((mhe (aref (heap-array heap) 1)))
      (values (heap-element-key mhe) (heap-element-priority mhe)))))

(defun heap-get-max (heap &optional (predicate (lambda (x) t)))
  (when (> (fill-pointer (heap-array heap)) 1)
    (multiple-value-bind (curr-max-he curr-max-priority) (heap-find heap 
                                                                    predicate)
      (heap-do (e p heap)
               (when (and (> p curr-max-priority)
                          (funcall predicate e))
                 (setf curr-max-priority p)
                 (setf curr-max-he e)))
      (values curr-max-he curr-max-priority))))
	

(defun heap-get (heap key)
  (let ((h-elem (gethash key (heap-key-to-he heap))))
    (when h-elem
      (values (heap-element-key h-elem)
              (heap-element-priority h-elem)
              (heap-element-index h-elem)))))

(defun heap-delete-min (heap)
  (when (> (fill-pointer (heap-array heap)) 1)
    (let ((mhe (aref (heap-array heap) 1)))
      (heap-swap-elements heap 1 (1- (fill-pointer (heap-array heap))))
      (decf (fill-pointer (heap-array heap)))
      (remhash (heap-element-key mhe) (heap-key-to-he heap))
      (heap-sink heap 1)
      (values (heap-element-key mhe) (heap-element-priority mhe)))))

(defun heap-pop (heap)
  (heap-delete-min heap))


(defun heap-size (heap)
  (1- (fill-pointer (heap-array heap))))


(defun heap-downsize (heap desired-size)
  (when (> desired-size 0)
    (loop while (> (heap-size heap) desired-size)
       do (heap-pop heap))))
	  
(defun heap-update (heap key new-priority &optional (perform-exist-check t))
  (let ((h-elem (gethash key (heap-key-to-he heap))))
    (if (and perform-exist-check 
             (null h-elem))
        (heap-insert heap key new-priority)
        (let ((old-priority (heap-element-priority h-elem))
              (index (heap-element-index h-elem)))
          (setf (heap-element-priority h-elem) new-priority)
          (if (> new-priority old-priority)
              (heap-sink heap index)
              (heap-float heap index)))))
  (values key new-priority))
  
(defun heap-insert (heap key priority &optional (perform-exist-check t))
  (if (and perform-exist-check 
           (gethash key (heap-key-to-he heap)))
      (heap-update heap key priority)
      (let* ((index (fill-pointer (heap-array heap)))
             (new-element (make-heap-element :key key 
                                             :priority priority 
                                             :index index)))
        (vector-push-extend new-element (heap-array heap))
        (setf (gethash key (heap-key-to-he heap)) new-element)
        (heap-float heap index)))
  (values key priority))
  
(defun heap-delete (heap key)
  (let ((h-elem (gethash key (heap-key-to-he heap)))
        (h-size (heap-size heap)))

    (when h-elem
      (if (= h-size
             (heap-element-index h-elem))
		  
          ;; Last Element
          (progn
            (decf (fill-pointer (heap-array heap)))
            (remhash (heap-element-key h-elem)
                     (heap-key-to-he heap)))
		  
          ;; Move the last element there
          (let ((h-last-elem (aref (heap-array heap) h-size))
                (index (heap-element-index h-elem)))
            (heap-swap-elements heap h-size index)
            (decf (fill-pointer (heap-array heap)))
            (remhash (heap-element-key h-elem) 
                     (heap-key-to-he heap))
            (if (> (heap-element-priority h-last-elem)
                   (heap-element-priority h-elem))
                (heap-sink heap index)
                (heap-float heap index))))
      
      (values (heap-element-key h-elem)
              (heap-element-priority h-elem)))))

(defun heap-exists (heap element)
  (gethash element (heap-key-to-he heap)))

(defun heap-not-exists (heap element)
  (not (gethash element (heap-key-to-he heap))))

(defun heap-is-empty? (heap)
  (= 0 (hash-table-count (heap-key-to-he heap))))

(defun heap-meld (h h-add)
  (heap-do (k p h-add)
           (heap-insert h k p)))

;; Additional Functions

(defmacro aconsf (key datum alist)
  `(setf ,alist (acons ,key ,datum, alist)))

(defmacro make-list-if-needed (x)
  `(if (listp ,x) ,x (list ,x)))

(defmacro strcat (&rest s)
  "String Concatenate"
  `(concatenate 'string ,@s))

(defun getnum (e)
  (cond 
    ((numberp e) e)
    ((and
      (element-p e)
      (is-x-a-y? e *number*)) (internal-name e))
    (t "")))

(defun getstr (e)
  (cond 
    ((stringp e) e)
    ((and
      (element-p e)
      (is-x-a-y? e *string*)) (internal-name e))
    (t "")))

(defun check-is-a (list-e is-a-e)
  (with-temp-marker (m)
    (dolist (e list-e)
      (upscan e m :augment t)
      (when (marker-on? is-a-e m)
        (return-from check-is-a t))))
  nil)

(defun print-ht (ht) 
  "Pretty-prints a hash-table with its keys and values"
  (loop for h being the hash-keys of ht
     using (hash-value v)
     do (format t "~S: ~S~%" h v)))


;; Explode modified from http://abhishek.geek.nz/lisp-features
(defun str-explode-dep (string &optional (delimiter #\Space))
  (let ((pos (position delimiter string)))
    (if (null pos)
        (list string)
        (cons (subseq string 0 pos)
              (str-explode-dep (subseq string (1+ pos))
                               delimiter)))))

(defun str-explode (str 
                    &key
                    (delimiter-discard '(#\ ))
                    (delimiter-ignore '(#\-)))

  (let* ((pos (position-if (lambda (x) (not (or (alphanumericp x)
                                                (find x delimiter-ignore))))
                           str))
         (ch (when pos (elt str pos))))

    (if (null pos)
        (list str)
        (let ((tokens (str-explode (subseq str (1+ pos))
                                   :delimiter-discard delimiter-discard
                                   :delimiter-ignore delimiter-ignore)))
          (if (find ch delimiter-discard)
              (cons (subseq str 0 pos) tokens)
              (cons (subseq str 0 pos) (cons (string ch) tokens)))))))
