

(in-context {t.0})
(setq *current-time* 0)


(defvar old-room)
(defvar adjacent-rooms)
(defun set-shakey-location (room)
    "set the location of shakey and store the eq link for future use"
    (setq old-room (car (list-rel {is located at} {shakey})))
    (setq adjacent-rooms (list-rel {adjacent to} old-room))
    (if (member room adjacent-rooms :test #'simple-is-x-a-y?)
     (progn (new-cancel *context*
                (get-statement-link {shakey} {is located at} 
                 (car (list-rel {is located at} {shakey}))))
            (new-statement {shakey} {is located at} room)
    ) (print (format nil "cannot move to ~a" room)))
)

(defvar i)
(defun visit-next-room ()
    (setq old-room (car (list-rel {is located at} {shakey})))
    (setq adjacent-rooms (list-rel {adjacent to} old-room))
    (setq i (random (- (length adjacent-rooms) 1 )))
    (set-shakey-location (nth i adjacent-rooms))
)


(defun current-location-flower (room)
    "return the flower of the current room"
    (car (remove-if-not (lambda (x) (simple-is-x-a-y? x {flower}))
               (list-rel-inverse {is located at} room)))
)



(defvar new-context-name)
(defvar flower)
(defvar current-room)
(defvar old-context-name)

(defun next-step (time)
    (print (format nil "current time: ~a" time))
    (print (format nil "current room: ~a" (car (list-rel {is located at} {shakey}))))
    (setq new-context-name (concatenate 'string "t." (write-to-string time )))
    (new-context new-context-name *context*)
    (setq old-context-name *context*)
    (in-context new-context-name)
    (print (format nil "current context: ~a" *context*))
    (visit-next-room)
    (setq current-room (car (list-rel {is located at} {shakey})))
    (setq flower (current-location-flower current-room))
    (print (format nil "current flower: ~a" flower))
    (if (simple-is-x-a-y? (car (list-rel {water status is} flower)) {thirsty}) 
     (progn
        (new-cancel *context* (get-statement-link flower {water status is} {thirsty}))
        (new-statement flower {water status is} {watered}))
    )
    (in-context old-context-name)
    (in-context new-context-name)
    )


(dotimes (x 10)
    (next-step (+ x 1))
)

(list-rel-inverse {water status is} {watered})
(list-rel-inverse {water status is} {thirsty})
    