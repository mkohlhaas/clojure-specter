#_{:clj-kondo/ignore [:refer-all]}
(ns clojure-specter.core
  (:require [com.rpl.specter :refer :all]))

(comment
  (declare MAP-VALS ALL END multi-path filterer compact srange selected? view collect-one putval if-path subselect ; specter stuff
           AccountPath TreeWalker p))                                                                   ; custom  stuff kondo can't resolve

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

(comment
  (select [MAP-VALS ALL MAP-VALS even?] data)) ; [2 4]

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 2: Append a sequence of elements to a nested vector
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def data1 {:a [1 2 3]})

;; Manual Clojure
(update data1 :a (fn [v] (into (if v v []) [4 5]))) ; {:a [1 2 3 4 5]}

;; Specter
(setval [:a END] [4 5] data1)                       ; {:a [1 2 3 4 5]}

(comment
  (select [:a END] data1)) ; [[]]

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

(comment
  (select [(filterer odd?) LAST] data2)) ; [7]

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example 4: Map a function over a sequence without changing the type or order of the sequence
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Manual Clojure
(map inc data2)                       ; (2 3 4 5 6 7 8 9)   (doesn't work, becomes a lazy sequence)
(into (empty data2) (map inc data2))  ; [2 3 4 5 6 7 8 9]   (doesn't work, reverses the order of lists)

;; Specter
(transform ALL inc data2)             ; [2 3 4 5 6 7 8 9]   (works for all Clojure datatypes with near-optimal efficiency)

(comment
  (type data2)                     ; clojure.lang.PersistentVector
  (type (transform ALL inc data2)) ; clojure.lang.PersistentVector
  (select ALL data2))              ; [1 2 3 4 5 6 7 8]

;; ;;;;;;;;;;;
;; Examples ;;
;; ;;;;;;;;;;;

;; https://github.com/redplanetlabs/specter?tab=readme-ov-file#examples

;; Increment all the values in maps of maps:
(transform [MAP-VALS MAP-VALS]
           inc
           {:a {:aa 1} :b {:ba -1 :bb 2}})
; {:a {:aa 2}, :b {:ba 0, :bb 3}}

(comment
  (select [MAP-VALS MAP-VALS]
          {:a {:aa 1} :b {:ba -1 :bb 2}}))
  ; [1 -1 2]

;; Increment all the even values for :a keys in a sequence of maps:
(transform [ALL :a even?]
           inc
           [{:a 1} {:a 2} {:a 4} {:a 3}])
; [{:a 1} {:a 3} {:a 5} {:a 3}]

(comment
  (select [ALL :a even?] [{:a 1} {:a 2} {:a 4} {:a 3}])) ; [2 4]

;; Retrieve every number divisible by 3 out of a sequence of sequences:
(select [ALL ALL #(= 0 (mod % 3))]
        [[1 2 3 4] [] [5 3 2 18] [2 4 6] [12]])
; [3 3 18 6 12]

(comment
  (select [ALL ALL] [[1 2 3 4] [] [5 3 2 18] [2 4 6] [12]])) ; [1 2 3 4 5 3 2 18 2 4 6 12]

;; Increment the last odd number in a sequence:
(transform [(filterer odd?) LAST]
           inc
           [2 1 3 6 9 4 8])
; [2 1 3 6 10 4 8]

(comment
  (select [(filterer odd?) LAST] [2 1 3 6 9 4 8])) ; [9]

;; Remove nils from a nested sequence:
(setval [:a ALL nil?] NONE {:a [1 2 nil 3 nil]})
; {:a [1 2 3]}

(comment
  (select [:a ALL nil?] {:a [1 2 nil 3 nil]})  ; [nil nil]
  (select [:a ALL]      {:a [1 2 nil 3 nil]})) ; [1 2 nil 3 nil]

;; Remove key/value pair from nested map:
(setval [:a :b :c] NONE {:a {:b {:c 1}}})
; {:a {:b {}}}

(comment
  (select [:a :b :c] {:a {:b {:c 1}}})) ; [1]

;; Remove key/value pair from nested map, removing maps that become empty along the way:
(setval [:a (compact :b :c)] NONE {:a {:b {:c 1}}})
; {}

;; Increment all the odd numbers between indices 1 (inclusive) and 4 (exclusive):
(transform [(srange 1 4) ALL odd?] inc [0 1 2 3 4 5 6 7])
; [0 2 2 4 4 5 6 7]

(comment
  (select [(srange 1 4) ALL odd?] [0 1 2 3 4 5 6 7])) ; [1 3]

;; Replace the subsequence from indices 2 to 4 with [:a :b :c :d :e]:
(setval (srange 2 4) [:a :b :c :d :e] [0 1 2 3 4 5 6 7 8 9])
; [0 1 :a :b :c :d :e 4 5 6 7 8 9]

;; Concatenate the sequence [:a :b] to every nested sequence of a sequence:
(setval [ALL END] [:a :b] [[1] '(1 2) [:c]])
; [[1 :a :b] (1 2 :a :b) [:c :a :b]]

;; Get all the numbers out of a data structure, no matter how they're nested:
(select (walker number?) {2 [1 2 [6 7]] :a 4 :c {:a 1 :d [2 nil]}})
; [2 1 2 6 7 4 1 2]

;; Navigate with string keys:
(select ["a" "b"] {"a" {"b" 10}})
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

;; Transforms a sequence of maps by adding the value of the :b key to the value of the :a key, but only if the :a key is even
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
        [(->User   (->Account 50))
         (->User   (->Account 51))
         (->Family [(->Account 1) (->Account 2)])])
; [50 51 1 2]

;; ;;;;;;;;;;;;;;;;;;;;
;; Recursive Navigation
;; ;;;;;;;;;;;;;;;;;;;;

;; 1. Using Protocol Paths

(defprotocolpath TreeWalker [])

(extend-protocolpath TreeWalker
                     Object nil
                     clojure.lang.PersistentVector [ALL TreeWalker])

;; Double all the even numbers in a tree:
(transform [TreeWalker number? even?] #(* 2 %) [:a 1 [2 [[[3]]] :e] [4 5 [6 7]]])
; [:a 1 [4 [[[3]]] :e] [8 5 [12 7]]]

(comment
  (select [TreeWalker number? even?] [:a 1 [2 [[[3]]] :e] [4 5 [6 7]]])) ; [2 4 6]

;; 2. Using Conditional Navigation

;; Reverse the positions of all even numbers in a tree (with order based on a depth first search).
(def TreeValues
  (recursive-path [] p
                  (if-path vector?
                           [ALL p]
                           STAY)))

(transform (subselect TreeValues even?)
           reverse
           [1 2 [3 [[4]] 5] [6 [7 8] 9 [[10]]]])
; [1 10 [3 [[8]] 5] [6 [7 4] 9 [[2]]]]

(comment
  (select (subselect TreeValues even?)
          [1 2 [3 [[4]] 5] [6 [7 8] 9 [[10]]]]))
  ; [[2 4 6 8 10]]

;; following outline taken from
;; [Cheatsheet](https://github.com/redplanetlabs/specter/wiki/Cheat-Sheet)

;; ;;;;;;;;;;;;;;;;
;; I. Operations ;;
;; ;;;;;;;;;;;;;;;;

;; ;;;;;;;;
;; 1. Query
;; ;;;;;;;;

;; ;;;;;;
;; select
;; ;;;;;;

(select [ALL even?] (range 10))  ; [0 2 4 6 8]
(select :a          {:a 0 :b 1}) ; [0]
(select ALL         {:a 0 :b 1}) ; [[:a 0] [:b 1]]

;; ;;;;;;;;;;
;; select-any
;; ;;;;;;;;;;

(select-any STAY :a) ; :a
(select-any even? 3) ; :com.rpl.specter.impl/NONE

(comment
  (select STAY :a)  ; [:a]
  (select even? 3)) ; []

;; ;;;;;;;;;;;;
;; select-first
;; ;;;;;;;;;;;;

(select-first ALL   (range 10)) ; 0
(select-first FIRST (range 10)) ; 0 (returns the result itself if the result is not a sequence)

;; ;;;;;;;;;;
;; select-one
;; ;;;;;;;;;;

(select     (srange 2 7) (range 10)) ; [[2 3 4 5 6]]
(select-one (srange 2 7) (range 10)) ; [2 3 4 5 6]

(comment
  (select-one ALL (range 10)))
  ; (err) Execution error (ExceptionInfo)
  ; (err) More than one element found in structure

;; ;;;;;;;;;;;
;; select-one!
;; ;;;;;;;;;;;

(select-one! FIRST (range 5)) ; 0

(comment
  (select-one! [ALL even? odd?] (range 10))
  ; (err) Execution error (ExceptionInfo)
  ; (err) Found no elements for select-one!

  (select-one! [ALL even?] (range 10)))
  ; (err) Execution error (ExceptionInfo)
  ; (err) More than one element found in structure

;; ;;;;;;;;;;;;;
;; selected-any?
;; ;;;;;;;;;;;;;

(selected-any? STAY :a)          ; true
(selected-any? even? 3)          ; false
(selected-any? ALL   (range 10)) ; true
(selected-any? ALL   [])         ; false

;; ;;;;;;;;
;; traverse
;; ;;;;;;;;

;; Creates Reducibles!

(traverse (walker integer?) [[[1 2]] 3 [4 [[5 6 7]] 8] 9])              ; returns an object implementing clojure.lang.IReduce

(reduce + 0 (traverse ALL (range 10)))                                  ; 45
(reduce + 0 (traverse (walker integer?) [[[1 2]] 3 [4 [[5 6 7]] 8] 9])) ; 45
(into #{}   (traverse (walker integer?) [[1 2] 1 [[3 [4 4 [2]]]]]))     ; #{1 4 3 2}

;; ;;;;;;;;;;;;
;; traverse-all
;; ;;;;;;;;;;;;

;; Creates Transducers!

;; Many common transducer use cases can be expressed more elegantly with `traverse-all`.

;; using vanilla Clojure
(transduce
 (comp (map :a) (mapcat identity) (filter odd?))
 +
 [{:a [1 2]} {:a [3]} {:a [4 5]}])
; 9

(transduce
 (traverse-all [:a ALL odd?])
 +
 [{:a [1 2]} {:a [3]} {:a [4 5]}])
; 9

;; ;;;;;;;;;;;;
;; 2. Transform
;; ;;;;;;;;;;;;

;; ;;;;;;;;;
;; transform
;; ;;;;;;;;;

(transform ALL #(* % 2) (range 10))       ; (0 2 4 6 8 10 12 14 16 18)
(transform [(putval 2) ALL] * (range 10)) ; (0 2 4 6 8 10 12 14 16 18)

(transform [(putval 2) (walker #(and (integer? %) (even? %)))] * [[[[1] 2]] 3 4 [5 6] [7 [[8]]]])
; [[[[1] 4]] 3 8 [5 12] [7 [[16]]]]

(comment
  (select [(walker #(and (integer? %) (even? %)))] [[[[1] 2]] 3 4 [5 6] [7 [[8]]]])
  ; [2 4 6 8]

  (select [(putval 2) (walker #(and (integer? %) (even? %)))] [[[[1] 2]] 3 4 [5 6] [7 [[8]]]]))
  ; [[2 2] [2 4] [2 6] [2 8]]

(transform [ALL] (fn [[k v]] [k {:key k :val v}]) {:a 0 :b 1})
; {:a {:key :a, :val 0}, :b {:key :b, :val 1}}

;; ;;;;;;;;;;;;;;;
;; multi-transform
;; ;;;;;;;;;;;;;;;

(multi-transform [:a :b (multi-path [:c (terminal-val :done)]
                                    [:d (terminal inc)]
                                    [:e (putval 3) (terminal +)])]
                 {:a {:b {:c :working :d 0 :e 1.5}}})
; {:a {:b {:c :done, :d 1, :e 4.5}}}

;; ;;;;;;;;;;
;; replace-in
;; ;;;;;;;;;;

;; `replace-in` is useful for situations where you need to know the specific values of what was transformed in the data structure.

;; double and save evens
(replace-in [ALL even?] (fn [x] [(* 2 x) [x]]) (range 10))
; [(0 1 4 3 8 5 12 7 16 9) (0 2 4 6 8)]
;  (0 1 2 3 4 5  6 7  8 9) (evens are transformed `(*2 x)`, odds are left alone `[x]`)

;; double evens and save largest even
(replace-in [ALL even?] (fn [x] [(* 2 x) x]) [3 2 8 5 6]
            :merge-fn (fn [curr new] (if (nil? curr) new (max curr new))))
; [[3 4 16 5 12] 8]

;; ;;;;;;
;; setval
;; ;;;;;;

(setval [ALL even?] :even (range 10))
; (:even 1 :even 3 :even 5 :even 7 :even 9)

;; ;;;;;;;;;;
;; vtransform
;; ;;;;;;;;;;

(vtransform ALL #(conj %1 %2) (range 10)) ; ([0] [1] [2] [3] [4] [5] [6] [7] [8] [9])

;; Navigates to each value specified by the path and replaces it by the result of running
;; the transform-fn on two arguments: the collected values as a vector, and the navigated value.

;; ;;;;;;;;;;;;;;;;;
;; II. Navigators ;;
;; ;;;;;;;;;;;;;;;;;

;; ;;;;;;;;
;; 1. Maps
;; ;;;;;;;;

;; ;;;
;; ALL
;; ;;;

;; ;;;;;;;;
;; MAP-KEYS
;; ;;;;;;;;

;; ;;;;;;;;
;; MAP-VALS
;; ;;;;;;;;

;; ;;;;;;;
;; compact
;; ;;;;;;;

;; ;;;;;;;
;; keypath
;; ;;;;;;;

;; ;;;;;;;
;; map-key
;; ;;;;;;;

;; ;;;;;;
;; submap
;; ;;;;;;

;; ;;;;
;; must
;; ;;;;

;; ;;;;;;;;;;;;
;; 2. Sequences
;; ;;;;;;;;;;;;

;; ;;;
;; ALL
;; ;;;

;; ;;;;;;;;;;;;;
;; ALL-WITH-META
;; ;;;;;;;;;;;;;

;; ;;;;;;;;;;
;; AFTER-ELEM
;; ;;;;;;;;;;

;; ;;;;;;;;;;;
;; BEFORE-ELEM
;; ;;;;;;;;;;;

;; ;;;;;;;;;
;; BEGINNING
;; ;;;;;;;;;

;; ;;;
;; END
;; ;;;

;; ;;;;;
;; FIRST
;; ;;;;;

;; ;;;;;;;;;;;;
;; INDEXED-VALS
;; ;;;;;;;;;;;;

;; ;;;;
;; LAST
;; ;;;;

;; ;;;;;;;;;;;;
;; before-index
;; ;;;;;;;;;;;;

;; ;;;;;;;
;; compact
;; ;;;;;;;

;; ;;;;;;;;;;;;;;;;;;
;; continuous-subseqs
;; ;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;
;; filterer
;; ;;;;;;;;

;; ;;;;;;;;;
;; index-nav
;; ;;;;;;;;;

;; ;;;;;;;
;; nthpath
;; ;;;;;;;

;; ;;;;;;
;; srange
;; ;;;;;;

;; ;;;;;;;;;;;;;;
;; srange-dynamic
;; ;;;;;;;;;;;;;;

;; ;;;;;;;
;; 3. Sets
;; ;;;;;;;

;; ;;;
;; ALL
;; ;;;

;; ;;;;;;;;;
;; NONE-ELEM
;; ;;;;;;;;;

;; ;;;;;;;
;; compact
;; ;;;;;;;

;; ;;;;;;;;
;; set-elem
;; ;;;;;;;;

;; ;;;;;;
;; subset
;; ;;;;;;

;; ;;;;;;;;;;;;;;;;;;;
;; 4. Keywords/Symbols
;; ;;;;;;;;;;;;;;;;;;;

;; ;;;;
;; NAME
;; ;;;;

;; ;;;;;;;;;
;; NAMESPACE
;; ;;;;;;;;;

;; ;;;;;;;;
;; 5. Atoms
;; ;;;;;;;;

;; ;;;;
;; ATOM
;; ;;;;

;; ;;;;;;;;;;
;; 6. Strings
;; ;;;;;;;;;;

;; ;;;;;;;;;
;; BEGINNING
;; ;;;;;;;;;

;; ;;;
;; END
;; ;;;

;; ;;;;;
;; FIRST
;; ;;;;;

;; ;;;;
;; LAST
;; ;;;;

;; ;;;;;;;;;
;; regex-nav
;; ;;;;;;;;;

;; ;;;;;;
;; srange
;; ;;;;;;

;; ;;;;;;;;;;;
;; 7. Metadata
;; ;;;;;;;;;;;

;; ;;;;;;;;;;;;;
;; ALL-WITH-META
;; ;;;;;;;;;;;;;

;; ;;;;
;; META
;; ;;;;

;; ;;;;;;;;
;; 8. Views
;; ;;;;;;;;

;; ;;;;;;;;;
;; NIL->LIST
;; ;;;;;;;;;

;; ;;;;;;;;
;; NIL->SET
;; ;;;;;;;;

;; ;;;;;;;;;;;
;; NIL->VECTOR
;; ;;;;;;;;;;;

;; ;;;;;;;;
;; nil->val
;; ;;;;;;;;

;; ;;;;;;
;; parser
;; ;;;;;;

;; ;;;;;;;;;;;
;; transformed
;; ;;;;;;;;;;;

;; ;;;;;;;;;
;; traversed
;; ;;;;;;;;;

;; ;;;;
;; view
;; ;;;;

;; ;;;;;;;;;;;;;;;;;;;
;; 9. Value collection
;; ;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;
;; DISPENSE
;; ;;;;;;;;

;; ;;;
;; VAL
;; ;;;

;; ;;;;;;;
;; collect
;; ;;;;;;;

;; ;;;;;;;;;;;
;; collect-one
;; ;;;;;;;;;;;

;; ;;;;;;;;;;
;; collected?
;; ;;;;;;;;;;

;; ;;;;;;
;; putval
;; ;;;;;;

;; ;;;;;;;;;;;;;;;;;;;;
;; with-fresh-collected
;; ;;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;;;;
;; 10. Control
;; ;;;;;;;;;;;

;; ;;;;
;; STAY
;; ;;;;

;; ;;;;
;; STOP
;; ;;;;

;; ;;;;;;;;;
;; cond-path
;; ;;;;;;;;;

;; ;;;;;;;;;;;;;;;;;;
;; continue-then-stay
;; ;;;;;;;;;;;;;;;;;;

;; ;;;;;;;
;; if-path
;; ;;;;;;;

;; ;;;;;;;;;;
;; multi-path
;; ;;;;;;;;;;

;; ;;;;;;;;;;;;;;;;;;
;; stay-then-continue
;; ;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;;
;; subselect
;; ;;;;;;;;;

;; ;;;;;;;;;;;
;; 11. Filters
;; ;;;;;;;;;;;

;; ;;;;
;; pred
;; ;;;;

;; ;;;;;
;; pred=
;; ;;;;;

;; ;;;;;
;; pred<
;; ;;;;;

;; ;;;;;
;; pred>
;; ;;;;;

;; ;;;;;;
;; pred<=
;; ;;;;;;

;; ;;;;;;
;; pred>=
;; ;;;;;;

;; ;;;;;;;;;;;;;
;; not-selected?
;; ;;;;;;;;;;;;;

;; ;;;;;;;;;
;; selected?
;; ;;;;;;;;;

;; ;;;;;;;;;;;
;; 12. Walking
;; ;;;;;;;;;;;

;; ;;;;;;;;;;
;; codewalker
;; ;;;;;;;;;;

;; ;;;;;;
;; walker
;; ;;;;;;

;; ;;;;;;;;;;;;;;;;;;;
;; 13. Multi-transform
;; ;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;
;; terminal
;; ;;;;;;;;

;; ;;;;;;;;;;;;
;; terminal-val
;; ;;;;;;;;;;;;

;; ;;;;;;;;;
;; vterminal
;; ;;;;;;;;;

;; ;;;;;;;;;;;;;;;;;;;;;
;; 14. Custom navigators
;; ;;;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;;;;
;; declarepath
;; ;;;;;;;;;;;

;; ;;;;;;;;;;;;;;;
;; defprotocolpath
;; ;;;;;;;;;;;;;;;

;; ;;;;;;;;;;;;;;;;;;;
;; extend-protocolpath
;; ;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;;;;;;;;;;
;; local-declarepath
;; ;;;;;;;;;;;;;;;;;

;; ;;;;
;; path
;; ;;;;

;; ;;;;;;;;;;;
;; providepath
;; ;;;;;;;;;;;

;; ;;;;;;;;;;;;;;
;; recursive-path
;; ;;;;;;;;;;;;;;

;; ;;;;;;;;;;;;
;; defcollector
;; ;;;;;;;;;;;;

;; ;;;;;;;;;;;;;
;; defdynamicnav
;; ;;;;;;;;;;;;;

;; ;;;;;;
;; defnav
;; ;;;;;;

;; ;;;;;;;
;; eachnav
;; ;;;;;;;

;; ;;;
;; nav
;; ;;;

;; ;;;;;;;;
;; 15. Misc
;; ;;;;;;;;

;; TODO: go through all macros and navigators to see what hasn't been covered

;; ;;;;;;;;;;
;; comp-paths
;; ;;;;;;;;;;

