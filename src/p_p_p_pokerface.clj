(ns p-p-p-pokerface)

(defn rank [card]
  (def values {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[fst _] card]
    (if (Character/isDigit fst) (Integer/valueOf (str fst)) (values fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (let [values (apply max ( vals(frequencies (for[card hand](rank card)))))]
    (if (< 1 values) true false)))


(defn three-of-a-kind? [hand]
  (let [values (apply max ( vals(frequencies (for[card hand](rank card)))))]
    (if (< 2 values) true false)))


(defn four-of-a-kind? [hand]
  (let [values (apply max ( vals(frequencies (for[card hand](rank card)))))]
    (if (< 3 values) true false)))

(defn flush? [hand]
  (let [values (apply max ( vals(frequencies (for[card hand](suit card)))))]
    (if (= 5 values) true false)))

(defn full-house? [hand]
  (let [values (sort( vals(frequencies (for[card hand](rank card)))))
        [fst snd] values]
    (if (and (= fst 2) (= snd 3)) true false)))

(defn two-pairs? [hand]
  (let [values (sort( vals(frequencies (for[card hand](rank card)))))
        [fst snd] values]
    (if (and (= fst 1) (= snd 2)) true false)))

(defn straight? [hand]
  (let [y (sort(for[card hand](rank card)))
        x (if (and (= (nth y 4) 14) (= (nth y 0) 2)) 1 (nth y 4))
        values (sort(replace {(nth y 4) x} y))]
      (every? true? (for [x (range 0 (- (count values) 1))] (= (- (nth values (+ x 1)) (nth values x)) 1)))))


(defn straight-flush? [hand]
    (and (flush? hand) (straight? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        x (filter #((first _) hand) checkers)
        y (map #(second %) x)]
    x))



