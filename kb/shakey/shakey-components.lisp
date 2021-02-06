; (in-namespace "common")
; (new-context {shakey})
; (in-context {shakey})

(new-type {room} {thing})
(new-type {door} {thing})
(new-type {flower} {thing})
(new-indv {shakey} {thing})


(new-members {room} 
            '({room.0} {room.1} {room.2} 
              {room.3} {room.4} {room.5}
              {room.6} {room.7} {room.8}))

(new-members {flower}
             '({flower.0} {flower.2} {flower.4}
               {flower.6} {flower.8}))

(set-element-property {flower} 'watered )

(new-relation {adjacent to} :a {room}
                :b {room}
                :symmetric T)

(new-relation {location at} :a {thing} :b {room})

(new-has {room.0} {flower.0})
(new-has {room.2} {flower.2})
(new-has {room.4} {flower.4})
(new-has {room.6} {flower.6})
(new-has {room.8} {flower.8})

(new-statement {room.0} {adjacent to} {room.1})
(new-statement {room.0} {adjacent to} {room.3})
(new-statement {room.1} {adjacent to} {room.2})
(new-statement {room.1} {adjacent to} {room.4})
(new-statement {room.2} {adjacent to} {room.5})
(new-statement {room.3} {adjacent to} {room.4})
(new-statement {room.3} {adjacent to} {room.6})
(new-statement {room.4} {adjacent to} {room.5})
(new-statement {room.4} {adjacent to} {room.7})
(new-statement {room.5} {adjacent to} {room.8})
(new-statement {room.6} {adjacent to} {room.7})
(new-statement {room.7} {adjacent to} {room.8})

(new-statement {shakey} {location at} {room.4})

; (defun )

; mark adjacent rooms
; mark rooms
; doors
; - 










