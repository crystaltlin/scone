;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; Minimal intuitive knowledge of kitchen utility for
;;; Scone's knowledge base.
;;;
;;; Author: Crystal Lin
;;; ***************************************************************************

(new-type {kitchen tool} {inanimate})

(new-split-subtypes {kitchen tool}
                    '({kitchen utensil} {kitchen appliance}
                      {eating utensil} {cutlery} {measuring utensil}
                      {cookware} {bakeware} {dinnerware}))

(new-split-subtypes {kitchen utensil}
                    '({spatula} {brush} {egg beater} {tong} 
                      {can opener} {grater}))

(new-split-subtypes {kitchen appliance}
                    '({microwave} {oven} {fryer} {rice cooker} {toaster}
                      {blender} {fodd processor} {coffee appliance}))

(new-split-subtypes {coffee appliance}
                    '({coffee grinder} {espresso machine} {coffee maker}))

(new-split-subtypes {eating utensil}
                    '({spoon} {fork} {chopstick}))

(new-split-subtypes {cutlery}
                    '({knife} {knife sharpener} {shear} {cutting board}))

(new-split-subtypes {measuring utensil}
                    '({measuring cup} {measuring spoon} {scale}))

(new-split-subtypes {cookware} 
                    '({pot} {pan} {wok} {skillet} {dutch oven}))


(new-split-subtypes {pan}
                    '({frying pan} {sauce pan}))

(new-split-subtypes {bakeware}
                    '({tart pan} {pie pan} {cake pan} {cupcake pan} 
                      {loaf pan} {sheet pan}))

(new-split-subtypes {dinnerware}
                    '({plate} {bowl} {cup.n.01}))



