# CL-ALGEBRAIC-DATA-TYPE
*by Robert Smith*

CL-ALGEBRAIC-DATA-TYPE, or ADT, is a library for defining algebraic
data types in a similar spirit to Haskell or Standard ML, as well as
for operating on them.

We can define ADTs using `defdata`:

``` common-lisp
(adt:defdata maybe
  (just t)
  nothing)
```

which will define a new type `maybe`, with a unary constructor `just`,
and a nullary constructor `nothing`. The `t` represents the data type
of that field.

``` common-lisp
> (just 5)
#.(JUST 5)
> nothing
#.NOTHING
```

Note that the `#.` are printed so that they can be read back. This
allows them to be used literally in quoted lists, for example.

``` common-lisp
> '(#.(just 1) #.nothing)
(#.(JUST 1) #.NOTHING)
> (typep (first *) 'maybe)
T
```

If this is annoying to you, you can set the variable
`adt:*print-adt-readably*` to `nil`.

We can define our own version of a list via

``` common-lisp
(adt:defdata liszt
  (kons t liszt)
  knil)
```

which defines the binary constructor `kons` and the nullary constructor
`knil`.

``` common-lisp
> (kons 1 (kons 2 knil))
#.(KONS 1 #.(KONS 2 #.KNIL))
```


At the end we will define `kar` and `kdr`.

For efficiency, we might specify the types more exactly. For a `point`
type that supports rectangular and polar coordinates, which is also
mutable, we might have:

``` common-lisp
(adt:defdata (point :mutable t)
  (rectangular float float)
  (polar float float))
```

The `:mutable` option signifies that the data is mutable.

When we have constructed a value, we can extract data out of it using `match`:

``` common-lisp
> (let ((pt (rectangular 1.0 2.0)))
    (adt:match point pt
      ((rectangular x y) (+ x y))
      ((polar _ _) nil)))
3.0
```

If we did not include the `polar` case, we would get a warning.

``` common-lisp
> (let ((pt (rectangular 1.0 2.0)))
    (adt:match point pt
      ((rectangular x y) (+ x y))))
; caught WARNING:
;   Non-exhaustive match. Missing cases: (POLAR)
3.0
```

We can also specify a fall-through:

``` common-lisp
> (let ((pt (rectangular 1.0 2.0)))
    (adt:match point pt
      ((rectangular x y) (+ x y))
      (_ nil)))
3.0
```

Since `point` is mutable, we can efficiently modify its fields using
`set-data`.

``` common-lisp
> (defun mirror-point! (pt)
    (adt:with-data (rectangular x y) pt
      (adt:set-data pt (rectangular y x))))

> (let ((pt (rectangular 1.0 2.0)))
   (mirror-point! pt)
   (adt:match point pt
     ((rectangular x y) (format t "point is (~A, ~A)" x y))
     (_ nil))
```

will print `point is (2.0, 1.0)`.

See [examples.txt](examples.txt) for examples.


## Frequently Asked Questions

**Q.** How do we define `kar` and `kdr` for `liszt`?

**A.** Easy.

``` common-lisp
(defun kar (l)
  (adt:match liszt l
    ((kons a _) a)
    (knil knil)))

(defun kdr (l)
  (adt:match liszt l
    ((kons _ b) b)
    (knil knil)))
```

**Q.** Can I get the constructors dynamically for a particular ADT?

**A.** Yes. You can get the constructors and associated arity by
calling the `get-constructors` function, which will return a list of
`(<constructor> <arity>)` pairs. For example, given the `liszt`
example above, we have

``` common-lisp
> (adt:get-constructors 'liszt)
((KONS 2) (KNIL 0))
T
```

The second value `t` represents the fact that the ADT is known and
exists.

**Q.** I have an ADT defined, and I'd like to extend it with another
ADT. How can I do that?

**A.** You can define a new ADT which includes another one. For
example, consider the following Boolean ADT.

``` common-lisp
(adt:defdata bool
  true
  false)
```

Suppose you wanted to extend this to have a "fuzzy" option, a
probability between true and false, specifically a `real` between `0`
and `1` exclusive. We can create a `fuzzy-bool` which includes the
`bool` type, as well as a unary `fuzzy` constructor. This is done by
the `:include` option to `defdata`.

``` common-lisp
(adt:defdata (fuzzy-bool :include bool)
  (fuzzy (real (0) (1))))
```

Note that `true` and `false` are constructors for *both* `bool` and
`fuzzy-bool`, as we can see with `get-constructors`.

``` common-lisp
> (adt:get-constructors 'bool)
((TRUE 0) (FALSE 0))
T
> (adt:get-constructors 'fuzzy-bool)
((TRUE 0) (FALSE 0) (FUZZY 1))
T
```

**Q.** Can we do parametric ADTs like I can in Haskell?

**A.** There is no support for it because Lisp doesn't have any useful
notion of definable parametric types that aren't aliases of another
existing parametric type.


**Q.** Why doesn't deeper pattern matching work?

**A.** It's not implemented, but it could be implemented for fields
which are themselves algebraic data types. Patches welcome!
