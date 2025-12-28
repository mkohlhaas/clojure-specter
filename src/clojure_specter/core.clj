#_{:clj-kondo/ignore [:refer-all]}
(ns clojure-specter.core
  (:require [com.rpl.specter :refer :all]))

(comment
  (declare MAP-VALS ALL END filterer))

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
