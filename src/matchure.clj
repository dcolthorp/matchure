(ns matchure
  (:require [clojure.zip :as zip]
            [clojure.walk :as walk]))

(import '[java.util Map])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
(defmacro with-gensyms "Create gensyms bound to names." [names & code]
  (let [syms (into []  (map (fn [name] `(gensym '~name)) names))]
    `(let [~names ~syms]
       ~@code)))

(defn- variable-binding? "Decide whether the given thing is a directive to bind a variable" [v]
  (and (symbol? v) 
       (= \? (.charAt (name v) 0))
       (< 1 (count (name v)))))

(defn- bound-variable-name "Returns the name of the variable to bind to. I.e. the bound-variable-name of '?foo is 'foo." [v]
  (assert (variable-binding? v))
  (let [var-name (name v)
	name-rest (.substring var-name 1 (.length var-name))]
    (symbol name-rest)))

(defn- class-name? [class]
  (re-find #"\A(?:[a-z0-9]+\.)+[A-Z]\w*\Z" (name class)))

(defn- all-zip [root]
  (zip/zipper #(instance? clojure.lang.Seqable %)
              seq
              #(into (empty %1) %2)
              root))

(defn- deep-count [root atom]
  (loop [zipper (all-zip root) n 0]
    (if (zip/end? zipper)
      n
      (if (= (zip/node zipper)
             atom)
        (recur (zip/next zipper) (inc n))
        (recur (zip/next zipper)
               n)))))



(defn- extract-variables-from-pattern [pattern]
  (if (symbol? pattern)
    (if (variable-binding? pattern)
      [(bound-variable-name pattern)]
      [])
    (if (instance? clojure.lang.Seqable pattern)
      (loop [zipper (all-zip pattern)
            variables []]
       (if (zip/end? zipper)
         (distinct variables)
         (recur (zip/next zipper)
                (if (variable-binding? (zip/node zipper))
                  (conj variables (bound-variable-name (zip/node zipper)))
                  variables)))))))

(defn- extract-variables-list-from-matches [matches]
  (loop [matches matches variables []]
    (if (empty? matches)
      variables
      (let [[pattern _ & rest-matches] matches]
        (recur rest-matches
               (concat variables (extract-variables-from-pattern pattern)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; state manipulation/usage helpers
(defn- success "Do whatever a successful match entails. Returns code for a successful match." [state]
  ((state :success) state))

(defn- failure "Do whatever a failed match entails. Returns code for a failed match." [state]
  ((state :failure) state))

(defn- simple-if "Return an if statement testing condition that uses the default success and failure"
  ([state condition] (simple-if state condition (success state)))
  ([state condition true-case] (simple-if state condition true-case (failure state)))
  ([state condition true-case false-case]
     `(if ~condition ~true-case ~false-case)))

(defn- default-match "Returns the code for the default runtime match behavior."
  [pattern matching-name state]
  (simple-if state (list '= matching-name pattern)))


(defn- wrap-result "Wrap succuss and/or failure behavior with the functions specified. Passes the wrapping
functions the current state with the original success/failure behavior restored when that result is signaled
in a submatch."
  [state & key-wrappers]
  (let [original-results (select-keys state [:success :failure])]
    (loop [state state, key-wrappers key-wrappers]
      (if (empty? key-wrappers)
	state
	(let [[key wrapper & remaining] key-wrappers]
	  (recur 
	   (assoc state key (fn [current-state] (wrapper (merge current-state original-results))))
	   remaining))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern match compilation
(declare compile-pattern)


(defn- compile-matches "Compiles a list of pattern, match pairs into a block of code. If the first match fails,
the following match functions are never evaluated. "
  [matches success-code failure-code options]
  (if (empty? matches)
    success-code
    (let [[pattern matching & remainder] matches
	  matching-name (gensym 'matching)]
      `(let [~matching-name ~matching]
	 ~(compile-pattern 
	   pattern
	   matching-name 
	   (merge options 
		  {:success (fn [state] (compile-matches remainder success-code failure-code options)),
		   :failure  (fn [_] failure-code)}))))))

(defn- wrap-code? [compiled placeholder code]
  (and (> 1 (deep-count compiled placeholder)) (list? code)))


(defn- compile-top-level-match "Compiles a match at the very top level."
  [matches success-code failure-code]
  (assert (vector? matches))
  (with-gensyms [successn failuren]
    (let [variables (extract-variables-list-from-matches matches)
          variable-assigns (mapcat #(vector % nil) variables)
          success-placeholder (gensym "success")
          failure-placeholder (gensym "failure")
          compiled (compile-matches matches success-placeholder failure-placeholder {})
          wrap-success (wrap-code? compiled success-placeholder success-code)
          wrap-failure (wrap-code? compiled failure-placeholder failure-code)]
      `(let [~@variable-assigns
             ~@(if wrap-success (list successn `(fn [~@variables] ~success-code)))
             ~@(if wrap-failure (list failuren `(fn [~@variables] ~failure-code)))]
         ~(walk/prewalk-replace {success-placeholder (if wrap-success `(~successn ~@variables) success-code)
                                  failure-placeholder (if wrap-failure `(~failuren ~@variables) failure-code)}
                                 compiled

                   )))))

(defmacro if-match
  ([matches true-case] 
     (compile-top-level-match matches true-case nil))
  ([matches true-case false-case]
     (compile-top-level-match matches true-case false-case)))

(defmacro when-match [matches & code]
  `(if-match ~matches (do ~@code)))


(defmacro cond-match [& patterns]
  (letfn [(compile-conditions [patterns]
			      (if (empty? patterns)
				nil
				(let [[pattern, code & rest] patterns]
				  `(if-match ~pattern ~code ~(compile-conditions rest)))))]
    (if (odd? (count patterns))
      	(with-gensyms [whatn]
	  (let [[what & patterns] patterns
		wrapped-patterns (apply concat
				       (map (fn [[a b]] 
					      [(if (= true a) a [a whatn]) b])
					    (partition 2 patterns)))]
	    `(let [~whatn ~what]
	       ~(compile-conditions wrapped-patterns))))
	(compile-conditions patterns))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fn-match

(defn- varargs?
  [form]
  (some #(= '& %) (first form)))

(defn- max-non-varargs-length
  "Returns the maximum number of args any non-varargs form in the form can handle"
  [forms]
  (apply max (map #(count (first %)) (remove varargs? forms))))

(defn- min-varargs
  "Returns the minimum number of args a varargs form can take"
  [form]
  (count (take-while #(not (= '& %)) (first form))))

(defn group-fn-forms-by-count
  "Given a sequence of ([args] body) forms, group them into an ordered map of {count, [forms]}"
  [forms]
  (let [max-size (max-non-varargs-length forms)]
    (reduce
     (fn [groups form]
       (if (varargs? form)
         (loop [i (min-varargs form)
                groups groups]
           (if (<= i max-size)
             (let [collection (or (get groups i) [])]
              (recur (inc i)
                     (assoc groups i (conj collection form))))
             groups))
         (let [size (count (first form))
              collection (or (get groups size) [])]
          (assoc groups size (conj collection form)))))
     (sorted-map)
     forms)))

(defn- cond-form-for-fn
  "Return a cond-match pattern/body pair for a form, handling normal/varargs forms appropriately."
  [argnames form]
  (if (varargs? form)
    (let [n (min-varargs form)
          first-patterns (take n (first form))
          rest-pattern (last (first form))
          [first-argnames rest-argnames] (split-at n argnames)
          first-matches (into [] (interleave first-patterns first-argnames))
          all-matches (into []  (concat first-matches [rest-pattern (cons 'list rest-argnames)]))]
      `(~all-matches (do ~@(rest form))))
    `(~(into [] (interleave (first form) argnames))
      (do ~@(rest form)))))

(defn- fn-form-for-match
  "Given a number of arguments and a sequence of ([args] forms) values, generate a ([args] forms) form that can be used in a standard fn. "
  [arg-count forms]
  (let [argnames (map gensym (map #(str "arg" %) (range arg-count)))]
    `([~@argnames]
        (cond-match
          ~@(mapcat (partial cond-form-for-fn argnames) forms)
          ~(into [] (interleave (repeat arg-count '_) argnames))
          (throw (IllegalArgumentException. "Failed to match arguments"))))))


(defn- varargs-fn-form-for-match
  "Return a ([& args] body) form that uses all of the provided fn-match varargs forms to pattern match the arguments."
  [min-length forms]
  (let [argnames (map gensym (map #(str "arg" %) (range min-length)))
        restname (gensym "rest")
        args (gensym "args")]
    `([~@argnames & ~restname]
        (cond-match (list* ~@argnames ~restname)
                    ~@(mapcat
                       (fn [form] `(~(first form) (do ~@(rest form))))
                       forms)
                    ~'_ (throw (IllegalArgumentException. "Failed to match arguments"))))))

(defmacro fn-match
  "Works like clojure.core/fn, but argument lists are patterns. Any failed match raises IllegalArgumentException.

Example:
  (fn-match this
          ([0] 1)
          ([1] 1)
          ([?n] (+ (this (dec n)) (this (dec (dec n))))))"
  [& forms]
  (let [name-container (if (symbol? (first forms))
                         (list (first forms))
                         (list))
        forms (if (symbol? (first forms))
                (next forms)
                forms)
        forms (if (vector? (first forms))
                `((~@forms))
                forms)]
    (let [varargs (filter varargs? forms)
          groups (group-fn-forms-by-count forms)]
     `(fn ~@name-container
        ~@(map #(apply fn-form-for-match %) groups)
        ~@(if (empty? varargs)
            (list)
            (list (varargs-fn-form-for-match (max-non-varargs-length forms) varargs)))))))

(defmacro defn-match
  "Works like clojure.core/defn, but argument lists are patterns. Any failed match raises IllegalArgumentException.

Example:
(defn-match fib
          ([0] 1)
          ([1] 1)
          ([?n] (+ (fib (dec n)) (fib (dec (dec n))))))"
  [name & forms]
  (let [m (or (meta name) {})
        m (if (string? (first forms))
            (conj m {:doc (first forms)})
            m)
        forms (if (string? (first forms))
                (next forms)
                forms)
        m (if (map? (first forms))
            (conj m (first forms))
            m)
        forms (if (map? (first forms))
                (next forms)
                forms)
        m (if (map? (last forms))
            (conj m (last forms))
            m)
        forms (if (map? (last forms))
                (butlast forms)
                forms)]

    `(def ~(with-meta name m) (fn-match ~@forms))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern compilation

;; ad-hoc hierarchies for dispatching compilation
(derive clojure.lang.IPersistentList ::function-call)
(derive clojure.lang.Cons ::function-call)

(defmulti compile-pattern (fn [pattern _ _]
			    (if (nil? pattern)
			      ::nil
			      (class pattern))))

;;; Default compilation matching - use the pattern matcher stored in state
(defmethod compile-pattern :default [pattern matching-name state]
  (default-match pattern matching-name state))

;;; Regular Expressions
(defmethod compile-pattern java.util.regex.Pattern [pattern matching-name state]
  (simple-if state `(and (string? ~matching-name) (re-find ~pattern ~matching-name))))

;;; Variables
(defmethod compile-pattern clojure.lang.Symbol [pattern matching-name state]
  (cond
    (some #(= % pattern) '(_ ?)) (success state)
    (variable-binding? pattern) `(let [~(bound-variable-name pattern) ~matching-name]
                                   ~(success state))
    (class-name? pattern) (simple-if state `(instance? ~pattern ~matching-name))
    true (default-match pattern matching-name state)))

;;; Vectors - the sequence type that is destructured, a la let
(defmethod compile-pattern clojure.lang.IPersistentVector [pattern matching-name state]
  (letfn [(compile-rest [pattern matching-name state]
	   (cond
	     (empty? pattern) (simple-if state `(empty? ~matching-name))
	     (= '& (first pattern)) (compile-pattern (second pattern) matching-name state)
	     true (with-gensyms [seqn firstn restn]
				`(if-let [~seqn (seq ~matching-name)]
				   (let [~firstn (first ~seqn) ~restn (rest ~seqn)]
				     ~(compile-pattern (first pattern) firstn 
						       (wrap-result state :success #(compile-rest (rest pattern) restn %))))
				   ~(failure state)))))]
    `(if (or
          (instance? clojure.lang.Seqable ~matching-name)
          (instance? Iterable ~matching-name)
          (.isArray (class ~matching-name)))
       ~(compile-rest pattern matching-name state)
       ~(failure state))))

;;; Maps - pattern match against values with corresponding keys 
(defmethod compile-pattern clojure.lang.IPersistentMap [pattern matching-name state]
  (letfn [(compile-rest [pattern matching-name state]
	   (if (empty? pattern)
	     (success state)
	     (let [[key subpattern] (first pattern)]
	       (with-gensyms [keyn valuen]
	         `(let [~keyn ~key]
		    (if (contains? ~matching-name ~keyn)
		      (let [~valuen (get ~matching-name ~keyn)]
			~(compile-pattern subpattern valuen
					  (wrap-result state :success #(compile-rest (dissoc pattern key) matching-name %))))
		      ~(failure state)))))))]
    `(if (map? ~matching-name)
       ~(compile-rest pattern matching-name state)
       ~(failure state))))

;; Lists - function calls
(defmulti compile-list (fn [pat _ _] (first pat)))
(defmethod compile-pattern ::function-call [pattern matching-name state]
  (compile-list pattern matching-name state))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function evaluation

(defmethod compile-list :default [pattern matching-name state]
  (simple-if state (walk/postwalk-replace {'? matching-name} pattern)))


;;; special forms
(defmethod compile-list 'if [[_ test then else] matching-name state]
  (compile-pattern test matching-name
		   (wrap-result state
				:success #(compile-pattern then matching-name %)
				:failure #(compile-pattern else matching-name %))))

(defmethod compile-list 'and [[_ & patterns] matching-name state]
  (letfn [(compile-and [patterns matching-name state]
		       (if (empty? patterns)
			 (success state)
			 (compile-pattern (first patterns) matching-name
					  (wrap-result state :success #(compile-and (rest patterns) matching-name %)))))]
    (compile-and patterns matching-name state)))

(defmethod compile-list 'or [[_ & patterns] matching-name state]
  (letfn [(compile-or [patterns matching-name state]
		       (if (empty? patterns)
			 (failure state)
			 (compile-pattern (first patterns) matching-name
					  (wrap-result state :failure #(compile-or (rest patterns) matching-name %)))))]
    (compile-or patterns matching-name state)))

(defmethod compile-list 'not [[_ pattern] matching-name state]
  (compile-pattern pattern matching-name
		   (wrap-result state 
				:success failure
				:failure success)))

(defmethod compile-list 'quote [pattern matching-name state]
  (simple-if state `(= ~pattern ~matching-name)))
