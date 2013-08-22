(ns matchure.core                         ;; Moved and renamed matchure to matchure.core
  (:use matchure.compile))                ;; to follow lein (new) good practices. Maybe picky?


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

(defn- group-fn-forms-by-count
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
  "Works like clojure.core/defn, but argument lists are patterns. Any failed match raises IllegalArgumentException."
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


