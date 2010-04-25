(ns matchure-test
  (:use clojure.test
	matchure))

(import '(java.util ArrayList TreeMap))


(deftest test-if-match
  (testing "matching simple values"
    ; regular expressions test for matches
    (is (if-match [#"world" "Hello, world!"] true))
    
    ; basic data types use equality checking
    (is (if-match [true true] true))
    (is (if-match [false false] true))
    (is (not (if-match [true false] true)))

    (is (if-match [nil nil] true))
    (is (if-match [1234 1234] true))
    (is (if-match [1234M 1234] true))
    (is (if-match [1234M 1234M] true))
    (is (if-match [1.234 1.234] true))
    (is (if-match [1.234M 1.234] true))
    (is (if-match [1.234M 1.234M] true))

    (is (if-match ["abc 123" "abc 123"] true))

    (is (if-match [:foo :foo] true)))

  (testing "instance type checking"
    (is (if-match [java.lang.String "asdf"] true))

    ; require fully qualified class name
    (is (not (let [String "asdf"] (if-match [String ""] true))))
    (is (not (if-match [java.lang.String nil] true))))


  (testing "destructuring/binding sequences"
    (is (= {:a 1, :b 2}
           (if-match [?a 1, ?b 2] {:a a, :b b})))

    (is (= {:a 1, :b 2}
           (if-match [[?a ?b] [1 2]] {:a a, :b b})))

    (is (not (if-match [[?a ?b] [1]] true)))
    (is (= {:a 1, :b 2}
           (if-match [[[?a] ?b] [[1] 2]] {:a a, :b b})))

    (is (= {:a [1], :b 2}
           (if-match [[?a ?b] [[1] 2]] {:a a, :b b} 'fail)))

    (let [seq [1 2 3 4 5]]
      (is (= [1 (rest seq)]
             (if-match [[?fst & ?rst] seq]
                       [fst rst]))))

    (is (not (if-match [[[?a] ?b] [1 2]] true)))
    
    (testing "matches lists"
      (is (if-match [[1 2 3] '(1 2 3)] true)))
    (testing "matching arrays"
      (is (if-match [[0 0] (make-array (. Integer TYPE) 2)] true)))
    (testing "matching iterables"
      (let [list (ArrayList.)]
        (do
          (.add list 0)
          (.add list 1)
          (is (if-match [[0 1] list] true))))))
  
  (testing "ignored symbols"
    (is (if-match [[_ _] [1 2]] true)))

  (testing "rematching variables"
    (is (= 2 (if-match [?a 1 ?a 2] a))))

  (testing "destructures sorted maps as sequences"
    (is  (if-match [[[:a 1] [ :b 2]] (sorted-map :a 1 :b 2)] true)))

  (testing "destructures sorted sets as sequences"
    (is (if-match [[1 2 3] (sorted-set 3 2 1)] true)))

  (testing "matching hashes"
    (is (= [1 2]
           (if-match [{:a ?a, :b ?b} {:a 1, :b 2, :c 3}] [a b])))
    (is (= :fail (if-match [{:a ?a, :d ?b} {:a 1, :b 2, :c 3}] [a b] :fail)))
    (is (if-match [{:a ?a} {:a false}] true)))


  (testing "list patterns (function calls)"
    (is (if-match [(odd? ?) 1] true))
    (is (not (if-match [(odd? ?) 2] true)))

    (testing "and"
      (is (= 1 
             (if-match [(and ?a 1 (odd? ?)) 1] a))))
    (testing "not"
      (is (not (if-match [(not 1) 1] true)))
      (is (if-match [(not 1) 2] true)))
    
    (testing "or"
      (is (if-match [(or #"hello", #"world") "hello"] true))
      (is (if-match [(or #"hello", #"world") "world"] true))
      (is (if-match [(or #"hello", #"world") "world"] true))
      (is (not (if-match [(or #"hello", #"world") "FAIL"] true))))

    (testing "complex patterns"
      (is (= "hello, world" 
             (if-match [(and ?a #"hello" #"world" (not #"goodbye")) "hello, world"] a)))
      (is (if-match [(and ?a #"hello" #"world" (not #"goodbye")) "To world: hello"] true))
      (is (if-match [(or #"success" (and ?a #"hello" #"world" (not #"goodbye"))) "hello, world"] true))
      (is (if-match [(or #"success" (and ?a #"hello" #"world" (not #"goodbye"))) "success"] true))
      (is (not (if-match [(and ?a #"hello" #"world" (not #"goodbye")) "To world: hello. goodbye."] true)))
      (is (not (if-match [(and ?a #"hello" #"world" (not #"goodbye")) "Aw, hell, world"] true)))
      (is (not (if-match [(and ?a #"hello" #"world" (not #"goodbye")) "hello, give it a worl, d"] true)))))
  )


(deftest test-when-match
  (is (= :second (when-match [[1 2] [1 2]] :first :second)))
  (is (nil? (when-match [[1 2] [3 2]] true))))

(deftest test-cond-match
  (testing "with a constant thing to match against"
    (is (nil? (cond-match 0 1 1 2 2 3 3)))
    (is (= :match0
	   (cond-match 0, 0 :match0, 1 :match1, _ :match-true)))
    (is (= :match1
	   (cond-match 1, 0 :match0, 1 :match1, _ :match-true)))
    (is (= :match-true
	   (cond-match 3, 0 :match0, 1  :match1, _ :match-true)))

    (is (= {:a 1, :b 2}
	   (cond-match [1 2 3] 
		       [?a ?b 4] :miss
		       [?a ?b 3] {:a a, :b b}))))

  (testing "with the match specified in each clause"
    (is (nil? (cond-match [1 0] [2 0] 2 [3 0] 3)))
    (is (= :match0
	   (cond-match [0 0] :match0, [1 1] :match1, [] :match-true)))
    (is (= :match1
	   (cond-match [0 1] :match0, [1 1] :match1, [] :match-true)))
    (is (= :match-true
	   (cond-match [0 1] :match0, [1 2] :match1, [] :match-true)))

    (is (= {:a 1, :b 2}
	   (cond-match [[?a ?b 4] [2 3 nil]] :miss
		       [[?a ?b 3] [1 2 3]] {:a a, :b b})))))

