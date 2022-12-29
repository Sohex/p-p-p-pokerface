(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (def values {\T 10, \J 11, \Q 12, \K 13, \A 14})
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get values r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= (get (frequencies (vals (frequencies (map rank hand)))) 2) 2))

(defn straight? [hand]
  (let [ranks (map rank hand)]
    (if (= (range (apply min ranks) (+ (apply min ranks) 5)) (sort ranks))
      true
      (= (range (apply min (replace {14 1} ranks)) (+ (apply min (replace {14 1} ranks)) 5)) (sort (replace {14 1} ranks))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1] [two-pairs? 2]
                   [three-of-a-kind? 3] [straight? 4]
                   [flush? 5] [full-house? 6]
                   [four-of-a-kind? 7] [straight-flush? 8]}
        hand-types (fn [checker] (if ((first checker) hand)
                                   (second checker)
                                   0))]
    (apply max (map hand-types checkers)))) 
