(ns structured-data)

(defn do-a-thing [x]
  (let [twice (+ x x)]
    (Math/pow twice twice)))

(defn spiff [v]
  (let 
    [first (if (number? (get v 0)) (get v 0) 0)
    third (if (number? (get v 2)) (get v 2) 0)] 
    (+ first third)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let 
    [[first_tmp third_tmp] [(get v 0) (get v 2)]
    first (if (number? first_tmp) first_tmp 0)
    third (if (number? third_tmp) third_tmp 0)]
      (+ first third)
    )
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
      (- x2 x1)
    )
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [w (width rectangle)
        h (height rectangle)]
        (== w h)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
    [point_x point_y] point]
    (and (<= x1 point_x x2) (<= y1 point_y y2))))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (and (contains-point? outer (point x1 y1))
         (contains-point? outer (point x2 y2)))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [present-authors (:authors book)
    updated (conj present-authors new-author)]
    (assoc book :authors updated)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (let [counter (fn [x] (if (number? x) "-" (count x)))]
    (map counter collection)))

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq)) true false))

(defn stars [n]
  (apply str (concat (repeat n "*"))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (distinct a-seq)))))

; See below: good but _imperative_ programming.
; assoc returns a new hash ...
;  (let [tally {:beancounter (set []), :duplicates (set [])}
;    duplicate-checker (fn [item] 
;        (if (contains? (:beancounter tally) item) 
;          (assoc tally :duplicates (conj (:duplicates tally) item))) 
;          ; Always add to beancounter. 
;          (assoc tally :beancounter (conj (:beancounter tally) item)))]
;    ; The body
;    (map duplicate-checker a-seq)


; Works. Less is more. Got rid of map inside set.
(defn old-book->new-book [books]
  (assoc books :authors (set (:authors books))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (set (apply concat (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

; Best 2015-07-14
(defn author->string [author]
  (let [years-str (fn [author]
      (if (or (:birth-year author) (:death-year author))  
        (str " (" (:birth-year author) " - " (:death-year author) ")")))]
    
  (str (:name author) (years-str author))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (cond 
    (== (count books) 0)  "No books."
    (== (count books) 1)  (str "1 book. " (apply str (interpose ". " (map book->string books))) ".")
    :else                 (str (count books) " books. " (apply str (interpose ". " (map book->string books))) ".")
    )
  )

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

