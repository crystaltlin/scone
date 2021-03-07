; (in-namespace "common")
; (new-context {shakey})
; (in-context {shakey})

(defvar *current-time* 0)
(new-context {t.0} {universal})
(in-context {t.0})

(new-type {room} {physical object})
(new-type {robot} {physical object})

(new-type {door} {physical object})
(new-complete-split-subtypes {door}
 '({opened door} {closed door} {no door}))
(new-complete-split-subtypes {closed door}
 '({locked door} {unlocked door}))

(new-type {flower} {plant})
(new-type {water status} {thing})
(new-complete-split-subtypes {water status}
 '({watered} {thirsty}))
;(new-indv-role {water status} {plant} {water status adj})


(new-indv {shakey} {robot})


(new-members {room} 
            '({room.0} {room.1} {room.2} 
              {room.3} {room.4} {room.5}
              {room.6} {room.7} {room.8}))

(new-members {flower}
             '({flower.0} {flower.1} {flower.2} {flower.3} {flower.4}
               {flower.5} {flower.6} {flower.7} {flower.8}))


(new-relation {adjacent to} :a {room} :b {room}
                :c {door}
                :symmetric T)


(new-members {door}
            '({door.0.1} {door.0.3} {door.1.2}
              {door.1.4} {door.2.5} {door.3.4}
              {door.3.6} {door.4.5} {door.4.7}
              {door.5.8} {door.6.7} {door.7.8}))

(new-statement {room.0} {adjacent to} {room.1} :c {door.0.1})
(new-statement {room.0} {adjacent to} {room.3} :c {door.0.3})
(new-statement {room.1} {adjacent to} {room.2} :c {door.1.2})
(new-statement {room.1} {adjacent to} {room.4} :c {door.1.4})
(new-statement {room.2} {adjacent to} {room.5} :c {door.2.5})
(new-statement {room.3} {adjacent to} {room.4} :c {door.3.4})
(new-statement {room.3} {adjacent to} {room.6} :c {door.3.6})
(new-statement {room.4} {adjacent to} {room.5} :c {door.4.5})
(new-statement {room.4} {adjacent to} {room.7} :c {door.4.7})
(new-statement {room.5} {adjacent to} {room.8} :c {door.5.8})
(new-statement {room.6} {adjacent to} {room.7} :c {door.6.7})
(new-statement {room.7} {adjacent to} {room.8} :c {door.7.8})


(new-relation {is located at} :a {physical object} :b {room} :c {door})
(new-statement {flower.0} {is located at} {room.0})
(new-statement {flower.1} {is located at} {room.1})
(new-statement {flower.2} {is located at} {room.2})
(new-statement {flower.3} {is located at} {room.3})
(new-statement {flower.4} {is located at} {room.4})
(new-statement {flower.5} {is located at} {room.5})
(new-statement {flower.6} {is located at} {room.6})
(new-statement {flower.7} {is located at} {room.7})
(new-statement {flower.8} {is located at} {room.8})
(new-statement {shakey}   {is located at} {room.0})

(new-relation {water status is} :a {flower} :b {water status})
(new-statement {flower.0} {water status is} {thirsty})
(new-statement {flower.1} {water status is} {thirsty})
(new-statement {flower.2} {water status is} {thirsty})
(new-statement {flower.3} {water status is} {thirsty})
(new-statement {flower.4} {water status is} {thirsty})
(new-statement {flower.5} {water status is} {thirsty})
(new-statement {flower.6} {water status is} {thirsty})
(new-statement {flower.7} {water status is} {thirsty})
(new-statement {flower.8} {water status is} {thirsty})








