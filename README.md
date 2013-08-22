matchure
============

Matchure is pattern matching for clojure.

* sequence destructuring
* map destructuring
* equality checks
* regexp matches
* variable binding
* instance checking
* arbitrary boolean expressions
* boolean operators (and, or, not)
* if, when, cond, fn, and defn variants

It features 
    fn-match
, which dynamically yields a (lambda) matching function, 
and 
    defn-match
which provides for _à la Erlang_ function definitions (see below).

Matchure is pretty fast too - all patterns matches are compiled to nested if statements at compile time.

Changes since matchure 0.10.1:
- compiled and tested against Clojure 1.5.1 
- big number types (M suffix) do not match anymore against other numeric types 
- the main namespace is now matchure.core

Usage
--------

Add a Leiningen depency to your project.clj:
```clojure
(defproject your-project "0.0.1"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [matchure "0.13.0-SNAPSHOT"]])
````

Add a :use clause to your namespace:

```clojure
(ns your-project.core
  (:use matchure.core))
````

### Equality

Basic values check for equality

    (if-match [nil nil] true) ;=> true
    (if-match [1 1] true) ;=> true
    (if-match ["asdf" "asdf"] true) ;=> true
    (let [s "asdf"]
      (if-match ["asdf" s] true)) ;=> true

### Wildcards

`_` and `?` are both wildcards and will match anything. `_` is idiomatic for clojure.

`?` has special meaning in matchure. `?` can
be thought of as the thing being matched against, and so by itself always succeeds. It is also used to store the matched value in a variable
and is substituted into function calls for arbitrary tests (examples below).

### Regular expressions

Regular expression literals check for a match

    (if-match [#"hello" "hello world"] true) ;=> true

### Type checking

Fully qualified class/interface names check `instance?`.

    (if-match [java.lang.String "foo"] true) ;=> true
    (if-match [java.lang.Comparable "foo"] true) ;=> true

### Variable binding

The form `?var` is a pattern which always succeeds and has the side effect of binding the matched-against value to the variable `var`.

    (if-match [?foo "bar"] foo) ;=> "bar"

### Sequence destructuring

Literal vectors destructure and match sequences.

    (if-match [[?fst & ?rst] [1 2 3]] [fst rst]) ;=> [1 (2 3)]
    (if-match [[:message ?value] [:message "foo"]] value) ;=> "foo"
    (if-match [[java.lang.String java.lang.String] (list "hello" "world")] true) ;=> true
    (if-match [[[?a] ?b & ?rest] [[1] 2 3 4]] (list a b rest)) ;=> (1 2 (3 4))
    
    ; can also destructure maps
    (if-match [[[?key ?value] & ?rest] (sorted-map 1 2)] [key value rest]) ;=> (1 2 ())
    (if-match [[[?key ?value] & ?rest] (sorted-map 1 2, 3 4)] [key value rest]) ;=> (1 2 ([3 4]))

Failing matches
    
    (if-match [[?fst & ?rst] []] [fst rst] :failed-match) ;=> :failed-match
    (if-match [[[?key ?value] & ?rest] (sorted-map)] [key value rest]) ;=> nil
    
### Destructuring Maps

Map literals look up corresponding values by key and check the value of the given map against the pattern value of the pattern map.

    (if-match [{:foo java.lang.String} {:foo "bar"}] true) ;=> true
    (if-match [{:foo java.lang.String} (sorted-map :foo "bar")] true) ;=>

Keys that aren't pattern matched are ignored

    (if-match [{:foo java.lang.String} {:foo "bar", :baz "qux"}] true) ;=> true

Assoc lists aren't currently supported

    (if-match [{:foo java.lang.String} [[:foo "bar"]]] true) ;=> nil
    
### Expressions

Lists are evaluated as clojure expressions, with `?` being substituted for the matched-against value. For example, to check for an odd integer, you would use

    (if-match [(odd? ?) 1] true) ;=> true


### Special forms

Not all lists are left as-is. Matchure has an extensible set of special forms. Right now, the special forms just include `quote` and boolean operators, `and`, `or`, and `not`.

One common use of `and` is to test a value and bind it to a variable if the test succeeds:

    (if-match [(and ?foo #"hello") "hello world"] foo) ;=> "hello world"
    (if-match [(and ?foo #"hello") "goodbye world"] foo) ;=> nil

Or and not also supported. To assert the matched value is a string either doesn't match `#"hello"` or matches both `#"hello"` and `#"world"`:

    (if-match [(or (not #"hello") #"world") "hello world"] true) ;=> true
    (if-match [(or (not #"hello") #"world") "whatever"] true) ;=> true
    (if-match [(or (not #"hello") #"world") "hello everyone"] true) ;=> nil

#### Quote

Quote allows you to escape what would otherwise be a pattern so it's
tested for equality instead. For example, the pattern`'foo` tests for
equality to the symbol `foo`.

### when-match and cond-match

You can also use `when-match`

    (when-match [[?fst & ?rst] (list 1 2)]
      (prn "asdf")
      (prn "ghjkl"))

`cond-match` allows you to either test one value against multiple patterns

    (cond-match "hello, world"
      #"foo" "matches foo"
      #"hello" "matches hello"
      ? "doesn't match either") ;=> "matches hello"

Or match multiple values against multiple patterns

    (let [s "hello world"]
      (cond-match
        [#"foo" s] "matches foo"
        [#"hello" s] "matches hello"
        [? s] "doesn't match either")) ;=> "matches hello"

### fn-match and defn-match

`fn-match` and `defn-match` work like the corresponding builtins, but
match their patterns. They support both the (fn [& args] body) and (fn
([& arglist1] body1)...) variants. An IllegalArgumentException is
raised if either the number of arguments does not fit any provided
pattern or the particular specified arguments do not fit any
pattern. 

`fn-match` defines anonymous functions that pattern match on
arguments:

    (fn-match this
      ([0] 1)
      ([1] 1)
      ([?n] (+ (this (dec n)) (this (dec (dec n))))))

`defn-match` works similarly:

    (defn-match fib
      ([0] 1)
      ([1] 1)
      ([?n] (+ (fib (dec n)) (fib (dec (dec n))))))

`fn-match` and `defn-match` are intended to work as though the
provided arguments are matched against each provided pattern in
order. For example, consider the function

    (defn-match example-fn
      ([_ & _] :wildcard)
      ([] :no-args)
      ([1] :one)
      ([:a :b] :a-b))

`fn-match` first tests its arguments against `[_ & _]`, which will
match any sequence having at least one element. It then tests its
arguments against `[]` which matches an empty sequence.  finally `[1]`
and `[:a :b]` are tested. Because `[_ & _]` will match any non-empty
set of arguments, `example-fn` only has two return values in practice:
`:no-args` is returned when `example-fn` is called with no arguments,
and `:wildcard` is returned for any other set of arguments. This is
true even for `(example-fn 1)` because `[_ & _]` is tested first.

Performance Characteristics
-------------------

An efforts been made to have patterns compile down into efficient
code. One goal was to be able to use patterns in almost any context
without having to worry about overhead. In general, every set of
patterns compiles down into a tree of `if` and `let`
statements. Pattern matches short circuit, and redundant checks are
avoided. For example, when `[:a, :b, :c]` is compiled, you end up with
a code tree that checks the value matched against is seqable, then
that the first element is equal to `:a`, then the second is equal to
`:b`, etc. If any of these checks fails, the match short circuits and
the failure branch is taken.

Because of this, the success and failure branches passed to `if-let`
(which all the other macros compile down to) can be repeated a large
number of times. If they contain other macro calls, the whole thing
can generate into a large amount of code. For this reason, if either
the true or the false branch is going to be repeated more than once,
and that branch is not atomic, it is wrapped in an anonymous function
which is called any place that case can occur. This compramise can
make simple cases a tiny bit slower but hopefully avoids more drastic
consequences for complex use cases and increases the chance the
resulting function is below the JVM's inline limit.

`fn-match` and `defn-match` define multi-arity functions behind the
scenes and only test patterns that compatible with the provided number
of arguments. Furthermore, varargs are only used if varargs patterns
are provided, and then only for argument counts that don't match any
non-variadic patterns. In practice, this means matching functions that
use simple patterns are about as fast as the function you probably
would have written yourself.

As an example, here's the `fn` `example-fn` ends up expanding
to.

    (clojure.core/fn
     ([]
        (matchure/cond-match
         [] (do :no-args)
         [] (throw (java.lang.IllegalArgumentException. "Failed to match arguments"))))
     ([arg02655]
        (matchure/cond-match
         [_ arg02655 _ (list)] (do :wildcard)
         [1 arg02655] (do :one)
         [_ arg02655] (throw (java.lang.IllegalArgumentException. "Failed to match arguments"))))
     ([arg02656 arg12657]
        (matchure/cond-match
         [_ arg02656 _ (list arg12657)] (do :wildcard)
         [:a arg02656 :b arg12657] (do :a-b)
         [_ arg02656 _ arg12657] (throw (java.lang.IllegalArgumentException. "Failed to match arguments"))))
     ([arg02653 arg12654 & rest2651]
        (matchure/cond-match (clojure.core/list* arg02653 arg12654 rest2651)
         [_ & _] (do :wildcard)
         _ (throw (java.lang.IllegalArgumentException. "Failed to match arguments")))))

Notice that that `[_ & _]` is not tested when no arguments are passed
in, and `[]` is not tested when any arguments are passed in, because
these patterns' arities don't match those cases. Also note that
variadic patterns introduce a small amount of overhead to test (to
combine the extra arguments into a sequence), so putting variadic
patterns toward the end of a function definition can reduce overhead.

### More examples

For more examples, see the [tests](http://github.com/dcolthorp/matchure/blob/master/test/matchure_test.clj).

## Installation

See [http://clojars.org/matchure](http://clojars.org/matchure).

## Todo

* Add public extension mechanism.

## License

Copyright (c) 2011 Drew Colthorp

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.


Author
======
* Drew Colthorp (colthorp@atomicobject.com) and [Atomic Object](http://www.atomicobject.com/)
* More Atomic Object [open source](http://www.atomicobject.com/pages/Software+Commons) projects
