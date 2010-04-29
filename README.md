matchure
============

Matchure is pattern matching for clojure.

* sequence destructuring
* Map destructuring
* equality checks
* regexp matches
* variable binding
* instance checking
* arbitrary boolean expressions
* boolean operators (and, or, not)
* if, when, cond, fn, and defn variants

Matchure is pretty fast too – all patterns matches are compiled to nested if statements at compile time. 

Usage
--------

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

Quote allows you to escape what would otherwise be a pattern so it's tested for equality instead.

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
pattern. `fn-match` and `defn-match` define multi-arity functions
behind the scenes and only test patterns that could conceivably match
a call of a given arity.

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

### More examples

For more examples, see the [tests](http://github.com/dcolthorp/matchure/blob/master/test/matchure_test.clj).

## Installation

See [http://clojars.org/matchure](http://clojars.org/matchure).

## Todo

* Add public extension mechanism.

## License

Copyright (c) 2010 Drew Colthorp

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


