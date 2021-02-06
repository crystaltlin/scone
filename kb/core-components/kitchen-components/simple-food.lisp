;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; Minimal intuitive knowledge of food and ingredients for
;;; Scone's knowledge base.
;;;
;;; Author: Crystal Lin
;;; ***************************************************************************

(new-type {food} {inanimate})

(new-complete-split-subtypes {food}
                    '({plant-based} {animal-based} {fungi} {mineral}))

(new-complete-split-subtypes {food}
                    '({processed} {unprocessed}))

(new-split-subtypes {food}
                    '({protein} {carbohydrates} {fat} 
                         {vegetable} {fruit} {seasoning}))

;; plants classified by their parts
;; but maybe this should go to plants instead of plant-based food
; (new-complete-split-subtypes {plant-based}
;                     '({root-vegetable} {fruit} {stem-vegetable} 
;                         {flower-vegetable} {bulb-vegetable} {leaf-vegetable}
;                         {tuber} {seed}))


(new-complete-split-subtypes {animal-based}
                    '({meat} {egg} {seafood} {dairy}))

(new-is-a {meat} {protein})
(new-is-a {egg} {protein})
(new-is-a {seafood} {protein})
(new-is-a {fruit} {plant-based})
(new-is-a {fungi} {vegetable})

(new-split-subtypes {meat}
                    '({beef} {pork} {lamb} {poultry}))

(new-split-subtypes {poultry}
                    '({chicken} {duck} {goose} {turkey}))

(new-split-subtypes {poultry}
                    '({breast} {leg} {wing}))

(new-split-subtypes {beef}
                    '({chuck} {brisket} {rib} {plate} 
                    {short loin} {flank} {sirloin} 
                    {top sirloin} {bottom sirloin}
                    {tenderloin} {round} {shank}))


(new-split-subtypes {dairy}
                    '({milk} {yogurt} {cheese} {cream}))


(new-split-subtypes {seafood}
                    '({fish(meat)} {shellfish}))

(new-complete-split-subtypes {shellfish}
                            '({crustacean} {mollusk} {cephalapod}))

(new-split-subtypes {crustacean}
                    '({crab} {lobster} {shrimp}))

(new-split-subtypes {mollusk}
                    '({clam} {oyster} {scallop}))

(new-split-subtypes {cephalapod}
                    '({octopus} {squid}))

(new-split-subtypes {carbohydrates}
                    '({rice} {potato} {noodle}))

(new-split-subtypes {vegetable}
                    '({spinach} {cabbage} {carrot} {kale}))

(new-split-subtypes {fruit}
                    '({berry} {citrus} {melon} {tropical fruit}))

(new-members {citrus}
          '({orange} {lemon} {lime} {grapefruit}))



(new-members {berry} '({strawberry} {cranberry} {blueberry}))

;; salt is weird
(new-split-subtypes {seasoning}
                    '({sweetener} {spice} {salt}))

(new-indv {sugar} {sweetener})
(new-indv {pepper} {spice})



