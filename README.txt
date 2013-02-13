                        CL-ALGEBRAIC-DATA-TYPE
                        ----------------------
                        
                             Robert Smith

CL-ALGEBRAIC-DATA-TYPE, or ADT, is a library for defining algebraic
data types in a similar spirit to Haskell or Standard ML, as well as
for operating on them.

We can define ADTs using DEFDATA:

(defdata maybe
  (just t)
  nothing)

which will define a new type MAYBE, with a unary constructor JUST, and
a nullary constructor (a simple symbol) NOTHING. The T represents the
data type of that field.

We can define our own version of a list via

(defdata lst
  (kons t lst)
  knil)

which defines the binary constructor KONS and the nullary constructor
KNIL.

For efficiency, we might specify the types more exactly. For a POINT
type that supports rectangular and polar coordinates, which is also
mutable, we might have:

(defdata (point :mutable)
  (rectangular float float)
  (polar float float))

The :MUTABLE keyword signifies that the data is mutable.

When we have constructed a value, we can extract data out of it using MATCH:

> (let ((pt (rectangular 1.0 2.0)))
    (match point pt
      ((rectangular x y) (+ x y))
      ((polar _ _) nil)))

=> 3.0
      
If we did not include the POLAR case, we would get a warning.

> (let ((pt (rectangular 1.0 2.0)))
    (match point pt
      ((rectangular x y) (+ x y))))
; caught WARNING:
;   Non-exhaustive match. Missing cases: (POLAR)
=> 3.0

We can also specify a fall-through:

> (let ((pt (rectangular 1.0 2.0)))
    (match point pt
      ((rectangular x y) (+ x y))
      (_ nil)))

=> 3.0

Since POINT is mutable, we can efficiently modify its fields using
SET-DATA.

> (defun mirror-point! (pt)
    (with-data (rectangular x y) pt
      (set-data pt (rectangular y x))))

> (let ((pt (rectangular 1.0 2.0)))
   (mirror-point! pt)
   (match point pt
     ((rectangular x y) (format t "point is (~A, ~A)" x y))
     (_ nil))

will print "point is (2.0, 1.0)".

See examples.txt for examples.
