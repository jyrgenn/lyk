(require 'regtests)

;;; struct types

;; low-level primitives

(test-is "def sailor"
         (sys:def-struct-type sailor "a sailor and his assets"
                               earring
                               pipe
                               home-port
                               bag)
         'sailor)

(test-is "read literal"
         (read
          "#s(sailor pipe hemp earring golden bag large home-port Hamburg)")
         "#S(sailor earring golden pipe hemp home-port Hamburg bag large)")

(test-is "def hom-coord"
         (sys:def-struct-type hom-coord "set of homogenous coordinates"
                               x y z m)
         'hom-coord)

(test-is "var hom-coord"
         (defvar c1 #s(hom-coord x 23.4 y 1 z 222 m 5.89))
         'c1)

(test-is "tag hom-coord" (get-struct-tag c1) 'hom-coord)
(test-is "type hom-coord" (get-struct-type c1)
         '(hom-coord "set of homogenous coordinates" x y z m))

(test-is "get hom-coord 0" (sys:get-struct-slot c1 0) 23.4)
(test-is "get hom-coord 1" (sys:get-struct-slot c1 1) 1)
(test-is "get hom-coord 2" (sys:get-struct-slot c1 2) 222)
(test-is "get hom-coord 3" (sys:get-struct-slot c1 3) 5.89)
(test-err "get hom-coord 4" (sys:get-struct-slot c1 4)
         #/index . out of bounds/)

(test-is "set hom-coord 0" (progn (sys:set-struct-slot c1 0 "halleluja") c1)
         #s(hom-coord x "halleluja" y 1 z 222 m 5.89))
(test-is "set hom-coord 1" (progn (sys:set-struct-slot c1 1 t) c1)
         #s(hom-coord x "halleluja" y t z 222 m 5.89))
(test-is "set hom-coord 2" (progn (sys:set-struct-slot c1 2 #(3 4 5)) c1)
         #s(hom-coord x "halleluja" y t z #(3 4 5) m 5.89))
(test-is "set hom-coord 3" (progn (sys:set-struct-slot c1 3 #/^.?$/) c1)
         #s(hom-coord x "halleluja" y t z #(3 4 5) m #/^.?$/))

(test-is "make hom-coord 0" (sys:make-struct 'hom-coord)
         #s(hom-coord x nil y nil z nil m nil))
(test-is "make hom-coord 1" (sys:make-struct 'hom-coord :y "rangdangdang")
         #s(hom-coord x nil y "rangdangdang" z nil m nil))
(test-is "make hom-coord 2" (sys:make-struct 'hom-coord :y 5 :x 7 :m 'm :z 9)
         #s(hom-coord x 7 y 5 z 9 m m))
(test-err "make hom-coord 3" (sys:make-struct 'hom-coord :y 5 :f 7 :m 'm :z 9)
          #/ has extra slot: .*/)
(test-err "make hom-coord 4" (sys:make-struct 'hom-coord :du 5 :f 7 :m 'm :z 9) 
          #/ has extra slots: /)

;; now for the real thing: defstruct

(test-is "def the struct" (defstruct hcoord "homogenous coordinates" x y z m)
         'hcoord)
;; there was a problem with defstruct without doc string (the first
;; arg was tentatively evaluated and then checked if it is a string,
;; which of course failed with the (often unbound) first component
;; name); now a string literal is need in that place, which is okay
(test "defstruct sans doc" (defstruct hooga x y z m))

(test-is "make-hcoord 0" (make-hcoord)
         #s(hcoord x nil y nil z nil m nil))
(test-is "make-hcoord 1" (make-hcoord :x 3 :y 5 :z 6 :m 7)
         #s(hcoord x 3 y 5 z 6 m 7))
(test-is "make-hcoord 2" (make-hcoord :z 6 :x 3 :y 5 :m 7)
         #s(hcoord x 3 y 5 z 6 m 7))
(test-is "make-hcoord 3" (make-hcoord :z 5)
         #s(hcoord x nil y nil z 5 m nil))
(test-err "make-hcoord 4" (make-hcoord :z 5 :x 3 :v 6)
          #/has extra slot: \(./)

(test-is "defvar hc1" (defvar hc1 (make-hcoord :x 3 :y 5 :z 6 :m 7)) 'hc1)

(test-is "hcoord get 1" (hcoord-x hc1) 3)
(test-is "hcoord get 2" (hcoord-y hc1) 5)
(test-is "hcoord get 3" (hcoord-z hc1) 6)
(test-is "hcoord get 4" (hcoord-m hc1) 7)

(test-is "hcoord set 1" (progn (setf (hcoord-x hc1) 119)
                                hc1)
         #s(hcoord x 119 y 5 z 6 m 7))
(test-is "hcoord set 2" (progn (setf (hcoord-y hc1) 993)
                                hc1)
         #s(hcoord x 119 y 993 z 6 m 7))
(test-is "hcoord set 3" (progn (setf (hcoord-z hc1) 8081)
                                hc1)
         #s(hcoord x 119 y 993 z 8081 m 7))
(test-is "hcoord set 4" (progn (setf (hcoord-m hc1) 465)
                                hc1)
         #s(hcoord x 119 y 993 z 8081 m 465))

;; bug: defstruct does not pass docstring
(test-is "get struct type doc" (cadr (get-struct-type 'hcoord))
         "homogenous coordinates")

;; bug: get-struct-type of something that is not a struct or a valid
;; struct tag raises a misleading error; should rather return nil
(test-not "get struct type 119" (get-struct-type 119))

(done-testing)
