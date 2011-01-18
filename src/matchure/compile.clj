(ns matchure.compile
  (:require [clojure.zip :as zip]
            [clojure.walk :as walk]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
(defmacro with-gensyms "Create gensyms bound to names." [names & code]
  (let [syms (into []  (map (fn [name] `(gensym '~name)) names))]
    `(let [~names ~syms]
       ~@code)))

(defn variable-binding? "Decide whether the given thing is a directive to bind a variable"
  [v]
  (and (symbol? v)
       (= \? (.charAt (name v) 0))
       (< 1 (count (name v)))))

(defn bound-variable-name "Returns the name of the variable to bind to. I.e. the bound-variable-name of '?foo is 'foo." [v]
  (assert (variable-binding? v))
  (let [var-name (name v)
        name-rest (.substring var-name 1 (.length var-name))]
    (symbol name-rest)))

(defn class-name? [class]
  (re-find #"\A(?:[a-z0-9\-]+\.)+[A-Z]\w*\Z" (name class)))

(defn all-zip [root]
  (zip/zipper #(instance? clojure.lang.Seqable %)
              seq
              #(into (empty %1) %2)
              root))

(defn deep-count [root atom]
  (loop [zipper (all-zip root) n 0]
    (if (zip/end? zipper)
      n
      (if (= (zip/node zipper)
             atom)
        (recur (zip/next zipper) (inc n))
        (recur (zip/next zipper)
               n)))))



(defn extract-variables-from-pattern [pattern]
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

(defn extract-variables-list-from-matches [matches]
  (loop [matches matches variables []]
    (if (empty? matches)
      variables
      (let [[pattern _ & rest-matches] matches]
        (recur rest-matches
               (concat variables (extract-variables-from-pattern pattern)))))))

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


(defn compile-matches "Compiles a list of pattern, match pairs into a block of code. If the first match fails
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
                  {:success (fn [state] (compile-matches remainder success-code failure-code options))
                   :failure  (fn [_] failure-code)}))))))

(defn wrap-code? [compiled placeholder code]
  (and (< 1 (deep-count compiled placeholder)) (coll? code)))


(defn compile-top-level-match "Compiles a match at the very top level."
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
          (and ~matching-name (.isArray (class ~matching-name))))
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
                         (compile-top-level-match [(first patterns) matching-name]
                                                  (compile-and (rest patterns) matching-name state)
                                                  (failure state))))]
    (compile-and patterns matching-name state)))

(defmethod compile-list 'or [[_ & patterns] matching-name state]
  (letfn [(compile-or [patterns matching-name state]
                      (if (empty? patterns)
                        (failure state)
                        (compile-top-level-match [(first patterns) matching-name]
                                                 (success state)
                                                 (compile-or (rest patterns) matching-name state))))]
    (compile-or patterns matching-name state)))

(defmethod compile-list 'not [[_ pattern] matching-name state]
  (compile-pattern pattern matching-name
                   (wrap-result state
                                :success failure
                                :failure success)))

(defmethod compile-list 'quote [pattern matching-name state]
  (simple-if state `(= ~pattern ~matching-name)))
