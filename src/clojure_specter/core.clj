#_{:clj-kondo/ignore [:refer-all]}
(ns clojure-specter.core
  (:require [com.rpl.specter :refer :all]))

(comment
  (declare MAP-VALS ALL END filterer compact srange selected? view collect-one putval if-path subselect ; from specter
           AccountPath TreeWalker p))                                                                   ; custom stuff kondo can't resolve

;; ;;;;;;;;;
;; README ;;
;; ;;;;;;;;;

;; https://github.com/redplanetlabs/specter?tab=readme-ov-file#specter

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 1: Increment every even number nested within map of vector of maps
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def data {:a [{:aa 1 :bb 2} {:cc 3}]
           :b [{:dd 4}]})

;; Manual Clojure
(defn map-vals [m afn]
  (->> m
       (map (fn [[k v]] [k (afn v)]))
       (into (empty m))))

(map-vals
 data
 (fn [v]
   (mapv
    (fn [m]
      (map-vals
       m
       (fn [v] (if (even? v) (inc v) v))))
    v)))
; {:a [{:aa 1, :bb 3} {:cc 3}],
;  :b [{:dd 5}]))

;; Specter
(transform [MAP-VALS ALL MAP-VALS even?] inc data)
; {:a [{:aa 1, :bb 3} {:cc 3}],
;  :b [{:dd 5}])

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 2: Append a sequence of elements to a nested vector
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def data1 {:a [1 2 3]})

;; Manual Clojure
(update data1 :a (fn [v] (into (if v v []) [4 5]))) ; {:a [1 2 3 4 5]}

;; Specter
(setval [:a END] [4 5] data1)                     ; {:a [1 2 3 4 5]}

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 3: Increment the last odd number in a sequence
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def data2 [1 2 3 4 5 6 7 8])

;; Manual Clojure
(let [idx (reduce-kv (fn [res k v] (if (odd? v) k res)) nil data2)]
  (if idx
    (update data2 idx inc)
    data2))
; [1 2 3 4 5 6 8 8]

;; Specter
(transform [(filterer odd?) LAST] inc data2)
; [1 2 3 4 5 6 8 8]

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 4: Map a function over a sequence without changing the type or order of the sequence
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Manual Clojure
(map inc data2)                       ; (2 3 4 5 6 7 8 9)   (doesn't work, becomes a lazy sequence)
(into (empty data2) (map inc data2))  ; [2 3 4 5 6 7 8 9]   (doesn't work, reverses the order of lists)

;; Specter
(transform ALL inc data2)             ; [2 3 4 5 6 7 8 9]   (works for all Clojure datatypes with near-optimal efficiency)

;; ;;;;;;;;;;;
;; Examples ;;
;; ;;;;;;;;;;;

;; https://github.com/redplanetlabs/specter?tab=readme-ov-file#examples

;; Increment all the values in maps of maps:
(transform [MAP-VALS MAP-VALS]
           inc
           {:a {:aa 1} :b {:ba -1 :bb 2}})
; {:a {:aa 2}, :b {:ba 0, :bb 3}}

;; Increment all the even values for :a keys in a sequence of maps:
(transform [ALL :a even?]
           inc
           [{:a 1} {:a 2} {:a 4} {:a 3}])
; [{:a 1} {:a 3} {:a 5} {:a 3}]

;; Retrieve every number divisible by 3 out of a sequence of sequences:
(select [ALL ALL #(= 0 (mod % 3))]
        [[1 2 3 4] [] [5 3 2 18] [2 4 6] [12]])
; [3 3 18 6 12]

;; Increment the last odd number in a sequence:
(transform [(filterer odd?) LAST]
           inc
           [2 1 3 6 9 4 8])
; [2 1 3 6 10 4 8]

;; Remove nils from a nested sequence:
(setval [:a ALL nil?] NONE {:a [1 2 nil 3 nil]})
; {:a [1 2 3]}

;; Remove key/value pair from nested map:
(setval [:a :b :c] NONE {:a {:b {:c 1}}})
; {:a {:b {}}}

;; Remove key/value pair from nested map, removing maps that become empty along the way:
(setval [:a (compact :b :c)] NONE {:a {:b {:c 1}}})
; {}

;; Increment all the odd numbers between indices 1 (inclusive) and 4 (exclusive):

(transform [(srange 1 4) ALL odd?] inc [0 1 2 3 4 5 6 7])
; [0 2 2 4 4 5 6 7]

;; Replace the subsequence from indices 2 to 4 with [:a :b :c :d :e]:
(setval (srange 2 4) [:a :b :c :d :e] [0 1 2 3 4 5 6 7 8 9])
; [0 1 :a :b :c :d :e 4 5 6 7 8 9]

;; Concatenate the sequence [:a :b] to every nested sequence of a sequence:
(setval [ALL END] [:a :b] [[1] '(1 2) [:c]])
; [[1 :a :b] (1 2 :a :b) [:c :a :b]]

;; Get all the numbers out of a data structure, no matter how they're nested:
(select (walker number?)
        {2 [1 2 [6 7]] :a 4 :c {:a 1 :d [2 nil]}})
; [2 1 2 6 7 4 1 2]

;; Navigate with string keys:
(select ["a" "b"]
        {"a" {"b" 10}})
; [10]

;; Reverse the positions of all even numbers between indices 4 and 11:
(transform [(srange 4 11) (filterer even?)]
           reverse
           [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15])
; [0 1 2 3 10 5 8 7 6 9 4 11 12 13 14 15]

;; Append [:c :d] to every subsequence that has at least two even numbers:
(setval [ALL
         (selected? (filterer even?) (view count) (pred>= 2))
         END]
        [:c :d]
        [[1 2 3 4 5 6] [7 0 -1] [8 8] []])
; [[1 2 3 4 5 6 :c :d] [7 0 -1] [8 8 :c :d] []]

(transform [ALL (collect-one :b) :a even?]
           +
           [{:a 1 :b 3} {:a 2 :b -10} {:a 4 :b 10} {:a 3}])
; [{:a 1, :b 3} {:a -8, :b -10} {:a 14, :b 10} {:a 3}]

;; Increment the value for :a key by 10:
(transform [:a (putval 10)]
           +
           {:a 1 :b 3})
; {:a 11, :b 3}

;; For every map in a sequence, increment every number in :c's value if :a is even or increment :d if :a is odd:
(transform [ALL (if-path [:a even?] [:c ALL] :d)]
           inc
           [{:a 2 :c [1 2] :d 4} {:a 4 :c [0 10 -1]} {:a -1 :c [1 1 1] :d 1}])
; [{:a 2, :c [2 3], :d 4} {:a 4, :c [1 11 0]} {:a -1, :c [1 1 1], :d 2}]

;; "Protocol paths" can be used to navigate on polymorphic data.
;; For example, if you have two ways of storing "account" information:
(defrecord Account [funds])
(defrecord User    [account])
(defrecord Family  [accounts-list])

;; You can make an "AccountPath" that dynamically chooses its path based on the type of element it is currently navigated to:
(defprotocolpath     AccountPath [])
(extend-protocolpath AccountPath
                     User    :account
                     Family [:accounts-list ALL])

;; Then, here is how to select all the funds out of a list of User and Family:
(select [ALL AccountPath :funds]
        [(->User    (->Account 50))
         (->User    (->Account 51))
         (->Family [(->Account 1)
                    (->Account 2)])])
; [50 51 1 2]

;; The next examples demonstrate recursive navigation.
;; Here's one way to double all the even numbers in a tree:
(defprotocolpath TreeWalker [])

(extend-protocolpath TreeWalker
                     Object nil
                     clojure.lang.PersistentVector [ALL TreeWalker])

(transform [TreeWalker number? even?] #(* 2 %) [:a 1 [2 [[[3]]] :e] [4 5 [6 7]]])
; [:a 1 [4 [[[3]]] :e] [8 5 [12 7]]]

;; Here's how to reverse the positions of all even numbers in a tree (with order based on a depth first search).
;; This example uses conditional navigation instead of protocol paths to do the walk:
(def TreeValues
  (recursive-path [] p
                  (if-path vector?
                           [ALL p]
                           STAY)))

(transform (subselect TreeValues even?)
           reverse
           [1 2 [3 [[4]] 5] [6 [7 8] 9 [[10]]]])
; [1 10 [3 [[8]] 5] [6 [7 4] 9 [[2]]]]
