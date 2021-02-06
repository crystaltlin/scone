;;; -*- Mode:Lisp -*-

;;; =======================================================================
;;; LISP Wordnet Interface


;;; Usage:
;;;
;;; 1) Look at lisp_wordnet_interface.h
;;;    and compile the lisp_wordnet_interface.so as stated there
;;;
;;; 2) Place those files in the same directory as this one and you can
;;;    use the following functions to interact with wordnet

(provide "lisp-wordnet-interface")

(use-package '("CL" "SB-ALIEN" "SB-C-CALL"))

(defparameter *wn-shared-object-file* "lisp-wordnet-interface.so")
(defparameter *wn-base-form-cache* (make-hash-table :test 'equal :size 128))
(defparameter *wn-interface-loaded* nil)

(defmacro load-wordnet-interface ()
  `(progn
     (load-shared-object *wn-shared-object-file*)
     (define-alien-routine c-wn-init int)
     (define-alien-routine c-wn-get-base-form c-string (s c-string))
     (when (> (c-wn-init) 0) ;; Should return non-zero value
       (setf *wn-interface-loaded* t))))
  
(defun wordnet-get-base-form (str &key (use-cache t))
  (when use-cache
    (let ((cache-res (gethash str *wn-base-form-cache*)))
      (when cache-res
        (return-from wordnet-get-base-form cache-res))))
  (if *wn-interface-loaded*
      (let ((res (funcall 'c-wn-get-base-form str)))
        (when use-cache
          (setf (gethash str *wn-base-form-cache*) res))
        res)
      str))
  
