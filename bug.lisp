; load-kb "core" before this file

(new-context {t.0} {universal})
(in-context {t.0})

(new-type {room} {physical object})
(new-indv {shakey} {physical object})
(new-members {room} 
            '({room.0} {room.1} {room.2}))

(new-relation {is located at} :a {physical object} :b {room})
(new-statement {shakey} {is located at} {room.0})

(new-context {t.1} {t.0})
(in-context {t.1})
(print (format nil "before cancelling: ~a" (list-rel {is located at} {shakey})))
(new-cancel *context* (get-statement-link {shakey} {is located at} {room.0}))
(new-statement {shakey} {is located at} {room.1})
(print (format nil "after cancelling: ~a" (list-rel {is located at} {shakey})))
(in-context {t.0})
(in-context {t.1})
(print (format nil "after switching context: ~a" (list-rel {is located at} {shakey})))
