#_{:clj-kondo/ignore [:refer-all]}
(ns clojure-specter.core
  (:require [com.rpl.specter :refer :all]
            [clojure.string :as str])) ; nil

(comment
  (declare ATOM AFTER-ELEM BEFORE-ELEM BEGINNING ALL-WITH-META MAP-KEYS MAP-VALS ALL END META NAME NAMESPACE NONE-ELEM VAL DISPENSE with-fresh-collected collect traversed transformed parser regex-nav subset set-elem srange-dynamic index-nav continuous-subseqs before-index submap map-key nil->val multi-path filterer compact srange selected? view collect-one putval if-path subselect ; specter stuff
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

(vtransform ALL #(conj %1 %2) (range 10))
; ([0] [1] [2] [3] [4] [5] [6] [7] [8] [9])

(comment
  (conj [] 0)  ; [0]
  (conj [] 1)  ; [1]
  ; …
  (conj [] 9)) ; [9]

;; Navigates to each value specified by the path and replaces it by the result of running
;; the transform-fn on two arguments: the collected values as a vector, and the navigated value.

;; https://github.com/redplanetlabs/specter/issues/238#issue-277999536

(vtransform [:a (putval 2) (putval 3)] (fn [vs v] (+ v (reduce + vs))) {:a 1})
; {:a 6}

(comment
  (+ 2 3 1)) ; 6

;; ;;;;;;;;;;;;;;;;;
;; II. Navigators ;;
;; ;;;;;;;;;;;;;;;;;

;; ;;;;;;;;
;; 1. Maps
;; ;;;;;;;;

;; ;;;
;; ALL
;; ;;;

(select ALL [0 1 2 3])      ; [0 1 2 3]
(select ALL (list 0 1 2 3)) ; [0 1 2 3]

;; in a map it'll navigate to each key-value pair [key value]
(select ALL {:a :b, :c :d}) ; [[:a :b] [:c :d]]

(transform ALL identity {:a :b, :c :d}) ; {:a :b, :c :d}

(comment
  (identity [:a :b])) ; [:a :b]

;; ALL can transform to NONE to remove elements
(setval [ALL nil?] NONE [1 2 nil 3 nil]) ; [1 2 3]

;; ;;;;;;;;
;; MAP-KEYS
;; ;;;;;;;;

;; more efficient than [ALL FIRST]

(select [MAP-KEYS] {:a 3 :b 4}) ; [:a :b]

;; ;;;;;;;;
;; MAP-VALS
;; ;;;;;;;;

;; more efficient than [ALL LAST]

(select MAP-VALS {:a :b, :c :d}) ; [:b :d]

(select [MAP-VALS MAP-VALS] {:a {:b :c}, :d {:e :f}}) ; [:c :f]

;; MAP-VALS can transform to NONE to remove elements
(setval [MAP-VALS even?] NONE {:a 1 :b 2 :c 3 :d 4}) ; {:a 1, :c 3}

;; ;;;;;;;
;; compact
;; ;;;;;;;

(setval [:a (compact :b :c)] NONE {:a {:b {:c 1}}}) ; {}

(setval [:a :b (compact :c)] NONE {:a {:b {:c 1}}}) ; {:a {}}

(setval [1 (compact 0)] NONE [1 [2] 3]) ; [1 3]

(comment
  ;; not sure about the upside of `compact`
  (setval [:a] NONE {:a {:b {:c 1}}})    ; {}
  (setval [:a :b] NONE {:a {:b {:c 1}}}) ; {:a {}}
  (setval [1 0] NONE [1 [2] 3])          ; [1 [] 3]
  (setval [1] NONE [1 [2] 3]))           ; [1 3]

;; ;;;;;;;
;; keypath
;; ;;;;;;;

(select-one (keypath :a) {:a 0})                            ; 0
(select-one (keypath :a :b) {:a {:b 1}})                    ; 1
(select [ALL (keypath :a)] [{:a 0} {:b 1}])                 ; [0 nil]
(select [ALL (keypath :a) (nil->val :boo)] [{:a 0} {:b 1}]) ; [0 :boo] (does not stop navigation)

(comment
  (select-one :a {:a 0})                                    ; 0
  (select-one [:a :b] {:a {:b 1}})                          ; 1
  (select [ALL :a] [{:a 0} {:b 1}])                         ; [0 nil]
  (select [ALL :a (nil->val :boo)] [{:a 0} {:b 1}]))        ; [0 :boo]

;; ;;;;;;;
;; map-key
;; ;;;;;;;

(select [(map-key :a)] {:a 2 :b 3})    ; [:a]

(setval [(map-key :a)] :c {:a 2 :b 3}) ; {:b 3, :c 2}

;; ;;;;;;
;; submap
;; ;;;;;;

(select-one (submap [:a :b]) {:a 0, :b 1, :c 2}) ; {:a 0, :b 1}

(comment
  (select (submap [:a :b]) {:a 0, :b 1, :c 2})) ; [{:a 0, :b 1}]

(select-one (submap [:c]) {:a 0}) ; {}

(comment
  (select (submap [:c]) {:a 0})) ; [{}]

(transform [(submap [:a :c]) MAP-VALS] ; (submap [:a :c]) returns {:a 0} with no :c
           inc
           {:a 0, :b 1})
; {:b 1, :a 1}

;; we replace the empty submap with {:c 2} and merge with the original structure
(transform (submap []) #(assoc % :c 2) {:a 0, :b 1}) ; {:a 0, :b 1, :c 2}

(comment
  (select-one (submap []) {:a 0, :b 1})  ; {}
  (assoc {} :c 2))                       ; {:c 2}

;; ;;;;
;; must
;; ;;;;

(select-one (must :a)    {:a 0})           ; 0
(select-one (must :a)    {:b 1})           ; nil
(select-any (must :a)    {:a {:b 2} :c 3}) ; {:b 2}
(select-any (must :a :b) {:a {:b 2} :c 3}) ; 2

(setval (must :a) NONE {:a 1 :b 2}) ; {:b 2}

(comment
  (setval (must :a) NONE {:b 2})) ; {:b 2}

;; ;;;;;;;;;;;;
;; 2. Sequences
;; ;;;;;;;;;;;;

;; ;;;
;; ALL
;; ;;;

;; see above

;; ;;;;;;;;;;;;;
;; ALL-WITH-META
;; ;;;;;;;;;;;;;

;; ALL-WITH-META is the same as ALL, except it maintains metadata on the structure in transforms.
;; This navigator exists solely for transforms, especially for codewalker.
;; There's no metadata to maintain on select, since it navigates into the subvalues.

(select ALL ^{:purpose "Count"} [0 1 2 3])                                    ; [0 1 2 3]
(meta (select ALL ^{:purpose "Count"} [0 1 2 3]))                             ; nil

(select ALL-WITH-META ^{:purpose "Count"} [0 1 2 3])                          ; [0 1 2 3]
(meta (select ALL-WITH-META ^{:purpose "Count"} [0 1 2 3]))                   ; nil

(transform ALL-WITH-META inc ^{:purpose "Count"} [0 1 2 3])                   ; [1 2 3 4]
(meta (transform ALL-WITH-META inc ^{:purpose "Count"} [0 1 2 3]))            ; {:purpose "Count"}

(setval [ALL-WITH-META nil?] NONE ^{:purpose "Count"} [1 2 nil 3 nil])        ; [1 2 3]
(meta (setval [ALL-WITH-META nil?] NONE ^{:purpose "Count"} [1 2 nil 3 nil])) ; {:purpose "Count"}

;; ;;;;;;;;;;
;; AFTER-ELEM
;; ;;;;;;;;;;

(setval AFTER-ELEM 3 [1 2])  ; [1 2 3]

;; ;;;;;;;;;;;
;; BEFORE-ELEM
;; ;;;;;;;;;;;

(setval BEFORE-ELEM 1 [2 3]) ; [1 2 3]

;; ;;;;;;;;;
;; BEGINNING
;; ;;;;;;;;;

(setval BEGINNING '(0 1) (range 2 7)) ; (0 1 2 3 4 5 6)
(setval BEGINNING  [0 1] (range 2 7)) ; (0 1 2 3 4 5 6)
(setval BEGINNING  {0 1} (range 2 7)) ; ([0 1] 2 3 4 5 6)
(setval BEGINNING '(0 1) [2 3 4])     ; [0 1 2 3 4]

(setval BEGINNING {:foo :baz} {:foo :bar}) ; ([:foo :baz] [:foo :bar])

;; works with strings
(select-any BEGINNING "abc") ; ""
(setval BEGINNING "b" "a")   ; "ba"

;; ;;;
;; END
;; ;;;

(setval END '(5 6) (range 5)) ; (0 1 2 3 4 5 6)
(setval END  [5 6] (range 5)) ; (0 1 2 3 4 5 6)
(setval END  {5 6} (range 5)) ; (0 1 2 3 4 [5 6])
(setval END '(5 6) [1 2 3 4]) ; [1 2 3 4 5 6]

(setval END {:foo :baz} {:foo :bar}) ; ([:foo :bar] [:foo :baz])

;; works with strings
(select-any END "abc") ; ""
(setval END "b" "a")   ; "ab"

;; ;;;;;
;; FIRST
;; ;;;;;

(select-one FIRST (range 5))              ; 0
(select-one FIRST (sorted-map 0 :a 1 :b)) ; [0 :a]
(select-one FIRST (sorted-set 0 1 2 3))   ; 0
(select-one FIRST '())                    ; nil
(select     FIRST '())                    ; []

(setval FIRST NONE [:a :b :c :d :e]) ; [:b :c :d :e]

;; works with strings
(select-any FIRST    "abc") ; \a
(setval     FIRST \q "abc") ; "qbc"

;; ;;;;;;;;;;;;
;; INDEXED-VALS
;; ;;;;;;;;;;;;

;; INDEXED-VALS navigates to [index elem] pairs for each element in a sequence.
;; Transforms of index move element at that index to the new index, shifting other elements in the sequence.
;; Indices seen during transform take into account any shifting from prior sequence elements changing indices.

;; TODO: no idea how this works (opened an issue; https://github.com/redplanetlabs/specter/issues/338)

;; see also test cases https://github.com/redplanetlabs/specter/blob/6119462a4d959834f2d78a6183d74608bf08ab52/test/com/rpl/specter/core_test.cljc#L1657

(select [INDEXED-VALS] [1 2 3 4 5])         ; [[0 1] [1 2] [2 3] [3 4] [4 5]]
(setval [INDEXED-VALS FIRST] 0 [1 2 3 4 5]) ; [5 4 3 2 1]
(setval [INDEXED-VALS FIRST] 1 [1 2 3 4 5]) ; [1 5 4 3 2]

(comment
  (select [INDEXED-VALS FIRST] [1 2 3 4 5])) ; [0 1 2 3 4]

;; ;;;;
;; LAST
;; ;;;;

(select-one LAST (range 5))              ; 4
(select-one LAST (sorted-map 0 :a 1 :b)) ; [1 :b]
(select-one LAST (sorted-set 0 1 2 3))   ; 3
(select-one LAST '())                    ; nil
(select     LAST '())                    ; []

(setval LAST NONE [:a :b :c :d :e]) ; [:a :b :c :d]

;; works with strings
(select-any LAST "abc") ; \c
(setval LAST "q" "abc") ; "abq"

;; ;;;;;;;;;;;;
;; before-index
;; ;;;;;;;;;;;;

(select-any (before-index 0) [1 2 3]) ; :com.rpl.specter.impl/NONE

(setval (before-index 0) :a [1 2 3]) ; [:a 1 2 3]
(setval (before-index 1) NONE [1 2 3]) ; [1 2 3]
(setval (before-index 1) :a [1 2 3]) ; [1 :a 2 3]
(setval (before-index 3) :a [1 2 3]) ; [1 2 3 :a]

;; ;;;;;;;
;; compact
;; ;;;;;;;

;; see above

;; ;;;;;;;;;;;;;;;;;;
;; continuous-subseqs
;; ;;;;;;;;;;;;;;;;;;

(select (continuous-subseqs #(< % 10)) [5 6 11 11 3 12 2 5])  ; [[5 6] [3] [2 5]]
(select (continuous-subseqs #(< % 10)) [12 13])               ; []
(setval (continuous-subseqs #(< % 10)) [] [3 2 5 11 12 5 20]) ; [11 12 20]

(comment
  (select (continuous-subseqs #(< % 10)) [3 2 5 11 12 5 20])) ; [[3 2 5] [5]]

;; ;;;;;;;;
;; filterer
;; ;;;;;;;;

(select-one (filterer even?) (range 10)) ; [0 2 4 6 8]

;; removes falsy values (`false` and `nil`)
(select-one (filterer identity) ['() [] #{} {} "" true false nil]) ; [() [] #{} {} "" true]

;; ;;;;;;;;;
;; index-nav
;; ;;;;;;;;;

(select [(index-nav 0)] [1 2 3 4 5]) ; [0]
(select [(index-nav 7)] [1 2 3 4 5]) ; []
(setval (index-nav 2) 0 [1 2 3 4 5]) ; [3 1 2 4 5]

;; ;;;;;;;
;; nthpath
;; ;;;;;;;

(select [(nthpath 0)]        [1 2 3])  ; [1]
(select [(nthpath 2)]        [1 2 3])  ; [3]
(setval [(nthpath 2)]   NONE [1 2 3])  ; [1 2]
(select [(nthpath 0)]        [1 2 3])  ; [1]

(select [(nthpath 0)]   [[0 1 2] 2 3]) ; [[0 1 2]]
(select [(nthpath 0 0)] [[0 1 2] 2 3]) ; [0]

;; ;;;;;;
;; srange
;; ;;;;;;

(select-one (srange 2 4)    (range 5)) ; [2 3]
(setval     (srange 2 4) [] (range 5)) ; (0 1 4)

(comment
  (select-one (srange 0 10) (range 5)))
  ; (err) Execution error (IndexOutOfBoundsException)

;; works with strings
(select-any (srange 1 3)          "abcd") ; "bc"
(setval     (srange 1 3) ""       "abcd") ; "ad"
(setval    [(srange 1 3) END] "x" "abcd") ; "abcxd"

;; ;;;;;;;;;;;;;;
;; srange-dynamic
;; ;;;;;;;;;;;;;;

(select-one (srange-dynamic #(.indexOf % 2) #(.indexOf % 4)) (range 5)) ; [2 3]
(select-one (srange-dynamic (fn [_] 0) #(quot (count %) 2)) (range 10)) ; [0 1 2 3 4]

;; ;;;;;;;
;; 3. Sets
;; ;;;;;;;

;; ;;;
;; ALL
;; ;;;

;; see above

;; ;;;;;;;;;
;; NONE-ELEM
;; ;;;;;;;;;

(setval NONE-ELEM 3 #{1 2}) ; #{1 3 2}

(setval NONE-ELEM 1 nil) ; #{1}

;; ;;;;;;;
;; compact
;; ;;;;;;;

;; see above

;; ;;;;;;;;
;; set-elem
;; ;;;;;;;;

(select [(set-elem 3)] #{3 4 5})      ; [3]
(select [(set-elem 3)] #{4 5})        ; []
(setval [(set-elem 3)] NONE #{3 4 5}) ; #{4 5}

;; ;;;;;;
;; subset
;; ;;;;;;

(select-one (subset #{:a :b}) #{:b :c})   ; #{:b}

;; replaces the #{:a} subset with #{:a :c} and unions back into the original structure
(setval (subset #{:a}) #{:a :c} #{:a :b}) ; #{:c :b :a}

;; ;;;;;;;;;;;;;;;;;;;
;; 4. Keywords/Symbols
;; ;;;;;;;;;;;;;;;;;;;

;; ;;;;
;; NAME
;; ;;;;

(select [NAME] :key)                                ; ["key"]
(select [MAP-KEYS NAME] {:a 3 :b 4 :c 5})           ; ["a" "b" "c"]
(setval [MAP-KEYS NAME] "q" {'a/b 3 'bbb/c 4 'd 5}) ; {a/q 3, bbb/q 4, q 5}

;; ;;;;;;;;;
;; NAMESPACE
;; ;;;;;;;;;

(select [ALL NAMESPACE]     [::test ::fun]) ; ["clojure-specter.core" "clojure-specter.core"]
(select [ALL NAMESPACE]     [::test  :fun]) ; ["clojure-specter.core" nil]
(setval [ALL NAMESPACE] "a" [::test  :fun]) ; [:a/test :a/fun]

;; ;;;;;;;;
;; 5. Atoms
;; ;;;;;;;;

;; ;;;;
;; ATOM
;; ;;;;

(let [a (atom 0)]
  (select-one ATOM a))
; 0

(let [a (atom 0)]
  (swap! a inc)
  (select-one ATOM a))
; 1

(let [a (atom 0)]
  (transform ATOM inc a)
  @a)
; 1

;; ;;;;;;;;;;
;; 6. Strings
;; ;;;;;;;;;;

;; ;;;;;;;;;
;; BEGINNING
;; ;;;;;;;;;

;; see above

;; ;;;
;; END
;; ;;;

;; see above

;; ;;;;;
;; FIRST
;; ;;;;;

;; see above

;; ;;;;
;; LAST
;; ;;;;

;; see above

;; ;;;;;;;;;
;; regex-nav
;; ;;;;;;;;;

;; When supplied with a regex, navigates to every match in a string, and supports replacement with a new substring.

(select (regex-nav #"t") "test")                                                        ; ["t" "t"]
(select [:a (regex-nav #"t")] {:a "test"})                                              ; ["t" "t"]
(select [(regex-nav #"(\S+):\ (\d+)") (nthpath 2)] "Mary: 1st George: 2nd Arthur: 3rd") ; ["1" "2" "3"]

(setval (regex-nav #"t") "z" "test")           ; "zesz"
(setval [:a (regex-nav #"t")] "z" {:a "test"}) ; {:a "zesz"}

(transform (regex-nav #"t") str/capitalize "test")                                                        ; "TesT"
(transform [:a (regex-nav #"t")] str/capitalize {:a "test"})                                              ; {:a "TesT"}
(transform (regex-nav #"\s+\w") str/triml "Hello      World!")                                            ; "HelloWorld!"
(transform (regex-nav #"aa*") (fn [s] (-> s count str)) "aadt")                                           ; "2dt"
(transform (regex-nav #"[Aa]+") (fn [s] (apply str (take (count s) (repeat "@")))) "Amsterdam Aardvarks") ; "@msterd@m @@rdv@rks"
(transform (subselect (regex-nav #"\d\w+")) reverse "Mary: 1st George: 2nd Arthur: 3rd")                  ; "Mary: 3rd George: 2nd Arthur: 1st"

;; Specter implicitly converts regexes in paths to call regex-nav
(setval #"t" "z" "test") ; "zesz"

(comment
  ;; same as
  (setval (regex-nav #"t") "z" "test")) ; "zesz"

;; ;;;;;;
;; srange
;; ;;;;;;

;; see above

;; ;;;;;;;;;;;
;; 7. Metadata
;; ;;;;;;;;;;;

;; ;;;;;;;;;;;;;
;; ALL-WITH-META
;; ;;;;;;;;;;;;;

;; see above

;; ;;;;
;; META
;; ;;;;

(select-one META (with-meta {:a 0} {:meta :data})) ; {:meta :data}

(meta (transform META #(assoc % :meta :datum)
                 (with-meta {:a 0} {:meta :data})))
; {:meta :datum}

(comment
  (with-meta {:a 0} {:meta :data})              ; {:a 0}
  (meta (with-meta {:a 0} {:meta :data}))       ; {:meta :data}
  (transform META #(assoc % :meta :datum)
             (with-meta {:a 0} {:meta :data}))) ; {:a 0}

;; ;;;;;;;;
;; 8. Views
;; ;;;;;;;;

;; ;;;;;;;;;
;; NIL->LIST
;; ;;;;;;;;;

(select-one NIL->LIST nil)  ; ()
(select-one NIL->LIST :foo) ; :foo

;; ;;;;;;;;
;; NIL->SET
;; ;;;;;;;;

(select-one NIL->SET nil)  ; #{}
(select-one NIL->SET :foo) ; :foo

;; ;;;;;;;;;;;
;; NIL->VECTOR
;; ;;;;;;;;;;;

(select-one NIL->VECTOR nil)  ; []
(select-one NIL->VECTOR :foo) ; :foo

;; ;;;;;;;;
;; nil->val
;; ;;;;;;;;

(select-one (nil->val :a) nil) ; :a
(select-one (nil->val :a) :b)  ; :b

;; ;;;;;;
;; parser
;; ;;;;;;

(defn parse   [email] (str/split email #"@"))
(defn unparse [email] (str/join "@" email))

(select [ALL (parser parse unparse) #(= "gmail.com" (second %))]
        ["test1@example.com" "test2@gmail.com" "test3@gmail.com"])
; [["test2" "gmail.com"] ["test3" "gmail.com"]]

(setval [ALL (parser parse unparse) #(= "gmail.com" (second %)) FIRST END] "+spam" ["test@example.com" "test@gmail.com"])
; ["test@example.com" "test+spam@gmail.com"]

(comment
  (select [ALL (parser parse unparse) #(= "gmail.com" (second %)) FIRST]     ["test@example.com" "test@gmail.com"])  ; ["test"]
  (select [ALL (parser parse unparse) #(= "gmail.com" (second %)) FIRST END] ["test@example.com" "test@gmail.com"])) ; [""]

;; ;;;;;;;;;;;
;; transformed
;; ;;;;;;;;;;;

(select-one (transformed [ALL odd?] #(* % 2)) (range 10))               ; (0 2 2 6 4 10 6 14 8 18)
(transform [(transformed [ALL odd?] #(* % 2)) ALL] #(/ % 2) (range 10)) ; (0 1 1 3 2 5 3 7 4 9)

(comment
  (select [ALL odd? #(* % 2)] (range 10))) ; [1 3 5 7 9]

;; ;;;;;;;;;
;; traversed
;; ;;;;;;;;;

(select-any (traversed ALL +) [1 2 3 4]) ; 10

;; ;;;;
;; view
;; ;;;;

(select-one [FIRST (view inc)] (range 5)) ; 1

(comment
  (select-one [FIRST inc] (range 5))  ; 0
  (select-one [FIRST]     (range 5))) ; 0

;; ;;;;;;;;;;;;;;;;;;;
;; 9. Value collection
;; ;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;
;; DISPENSE
;; ;;;;;;;;

(transform [ALL VAL]          + (range 10)) ; (0 2 4 6 8 10 12 14 16 18)
(transform [ALL VAL DISPENSE] + (range 10)) ; (0 1 2 3 4 5 6 7 8 9)

(comment
  (+ 0)  ; 0
  (+ 1)  ; 1
  ; …
  (+ 9)) ; 9

;; ;;;
;; VAL
;; ;;;

;; VAL collects the current structure

;; Collected values are passed as initial arguments to the update fn.

(select [VAL ALL] (range 3)) ; [[(0 1 2) 0] [(0 1 2) 1] [(0 1 2) 2]]

(transform [VAL ALL] (fn [val-coll x] (+ x (count val-coll))) (range 5)) ; (5 6 7 8 9)

(comment
  (+ 0 (count (range 5)))  ; 5
  (+ 1 (count (range 5)))  ; 6
  (+ 2 (count (range 5)))  ; 7
  (+ 3 (count (range 5)))  ; 8
  (+ 4 (count (range 5)))) ; 9

;; ;;;;;;;
;; collect
;; ;;;;;;;

;; collect adds the result of running select with the given path on the current value to the collected vals.
;; Note that collect, like select, returns a vector containing its results.
;; If transform is called, each collected value will be passed as an argument to the transforming function with the resulting value as the last argument.

(select-one [(collect ALL) FIRST] (range 3))         ; [[0 1 2] 0]
(select [(collect ALL) ALL] (range 3))               ; [[[0 1 2] 0] [[0 1 2] 1] [[0 1 2] 2]]
(select [(collect ALL) (collect ALL) ALL] (range 3)) ; [[[0 1 2] [0 1 2] 0] [[0 1 2] [0 1 2] 1] [[0 1 2] [0 1 2] 2]]

;; add the sum of the evens to the first element of the seq
(transform [(collect ALL even?) FIRST]
           (fn [evens first] (reduce + first evens))
           (range 5))
; (6 1 2 3 4)

;; replace the first element of the seq with the entire seq
(transform [(collect ALL) FIRST] (fn [all _] all) (range 3))
; ([0 1 2] 1 2)

;; ;;;;;;;;;;;
;; collect-one
;; ;;;;;;;;;;;

;; collect-one adds the result of running select-one with the given path on the current value to the collected vals.
;; Note that collect-one, like select-one, returns a single result.
;; If there is more than one result, an exception will be thrown.
;; If transform is called, each collected value will be passed as an argument to the transforming function with the resulting value as the last argument.

(select-one [(collect-one FIRST) LAST] (range 5))                        ; [0 4]
(select     [(collect-one FIRST) ALL] (range 3))                         ; [[0 0] [0 1] [0 2]]
(transform  [(collect-one :b) :a] + {:a 2, :b 3})                        ; {:a 5, :b 3}
(transform  [(collect-one :b) (collect-one :c) :a] * {:a 3, :b 5, :c 7}) ; {:a 105, :b 5, :c 7}

;; ;;;;;;;;;;
;; collected?
;; ;;;;;;;;;;

;; Creates a filter function navigator that takes in all the collected values as input.
;; For arguments, can use (collected? [a b] ...) syntax to look at each collected value 
;; as individual arguments, or (collected? v ...) syntax to capture all the collected values as a single vector.

;; collected? operates in the same fashion as `pred`, but it takes the collected values as its arguments rather than the structure.

#_{:clj-kondo/ignore [:unresolved-symbol]}
(select [ALL (collect-one FIRST) LAST (collected? [k] (= k :a))] {:a 0 :b 1}) ; [[:a 0]]

#_{:clj-kondo/ignore [:unresolved-symbol]}
(select [ALL (collect-one FIRST) LAST (collected? [k] (< k 2))]
        (zipmap (range 5) ["a" "b" "c" "d" "e"]))
; [[0 "a"] [1 "b"]]

(comment
  (zipmap (range 5) ["a" "b" "c" "d" "e"]) ; {0 "a", 1 "b", 2 "c", 3 "d", 4 "e"}
  (select [ALL (collect-one FIRST)]
          (zipmap (range 5) ["a" "b" "c" "d" "e"])) ; [[0 [0 "a"]] [1 [1 "b"]] [2 [2 "c"]] [3 [3 "d"]] [4 [4 "e"]]]
  (select [ALL (collect-one FIRST) LAST]
          (zipmap (range 5) ["a" "b" "c" "d" "e"]))) ; [[0 "a"] [1 "b"] [2 "c"] [3 "d"] [4 "e"]]

#_{:clj-kondo/ignore [:unresolved-symbol]}
(transform [ALL (collect-one FIRST) LAST (collected? [k] (< k 2)) DISPENSE]
           str/upper-case
           (zipmap (range 5) ["a" "b" "c" "d" "e"]))
; {0 "A", 1 "B", 2 "c", 3 "d", 4 "e"}

;; ;;;;;;
;; putval
;; ;;;;;;

;; Adds an external value to the collected vals.
;; Useful when additional arguments are required to the transform function that would otherwise require partial application or a wrapper function.

;; increment val at path [:a :b] by 3
(transform [:a :b (putval 3)] + {:a {:b 0}}) ; {:a {:b 3}}

;; ;;;;;;;;;;;;;;;;;;;;
;; with-fresh-collected
;; ;;;;;;;;;;;;;;;;;;;;

;; with-fresh-collected continues navigating on the given path with the collected vals reset to [].
;; Once navigation leaves the scope of with-fresh-collected, the collected vals revert to what they were before.

#_{:clj-kondo/ignore [:unresolved-symbol]}
(select-any (with-fresh-collected
              (collect-one (keypath 0))
              (selected? (collected? [n] (even? n))))
            [4 2 3])
; [4 2 3]

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

