(ns pittard-victoria)


;;Problem 1
(defn log2 [n]
    (/ (Math/log n) (Math/log 2)))

(def vocabulary '(call me ishmael))

(def theta1 (list (/ 1 2) (/ 1 4) (/ 1 4)))
(def theta2 (list (/ 1 4) (/ 1 2) (/ 1 4)))

(def thetas (list theta1 theta2))

(def theta-prior (list (/ 1 2) (/ 1 2)))

(defn score-categorical [outcome outcomes params]
	(if (empty? params)
		(throw "no matching outcome")
		(if (= outcome (first outcomes))
			(first params)
			(score-categorical outcome (rest outcomes) (rest params)))))

(defn list-foldr [f base lst]
	(if (empty? lst)
		base
		(f (first lst) (list-foldr f base (rest lst)))))

(defn score-BOW-sentence [sen probabilities]
	(list-foldr (fn [word rest-score]
		(+ (log2 (score-categorical word vocabulary probabilities)) rest-score))
	0
	sen))

(defn score-corpus [corpus probabilities]
	(list-foldr (fn [sen rst]
		(+ (score-BOW-sentence sen probabilities) rst))
	0
	corpus))

(defn logsumexp [log-vals]
	(let [mx (apply max log-vals)]
		(+ mx (log2 (apply +
			(map (fn [z] (Math/pow 2 z))
				(map (fn [x] (- x mx)) log-vals)))))))

(def my-corpus '((call me) (call ishmael)))

(defn theta-corpus-joint [theta corpus theta-probs]
	(if (= theta (first thetas))
		(+ (score-corpus corpus theta) (log2 (first theta-probs)))
		(+ (score-corpus corpus theta) (log2 (first (rest theta-probs))))))

(def theta-corpus-joint-1 (theta-corpus-joint theta1 my-corpus theta-prior))
(def theta-corpus-joint-2 (theta-corpus-joint theta2 my-corpus theta-prior))

	;;test problem 1
;;(println theta-corpus-joint-1)
;;(println theta-corpus-joint-2)
;;prints -7.0, -8.0 respectively

;;Problem 2
(defn compute-marginal [corpus theta-probs]
	(logsumexp (list (theta-corpus-joint theta1 corpus theta-probs) 
	(theta-corpus-joint theta2 corpus theta-probs))))

(def compute-marginal-saved (compute-marginal my-corpus theta-prior))

;;test problem 2
;;(println compute-marginal-saved)
;;prints -6.415...

;;Problem 3
(defn compute-conditional-prob [theta corpus theta-probs]
	(- (theta-corpus-joint theta corpus theta-probs) (compute-marginal corpus theta-probs)))

(def compute-conditional-1 (compute-conditional-prob theta1 my-corpus theta-prior))
(def compute-conditional-2 (compute-conditional-prob theta2 my-corpus theta-prior))

;;test problem 3
;;(println compute-conditional-1)
;;(println compute-conditional-2)
;;prints -0.58496..., -1.58496... respectively

;;Problem 4
(defn compute-conditional-dist [corpus theta-probs]
	(list (compute-conditional-prob theta1 corpus theta-probs) 
	(compute-conditional-prob theta2 corpus theta-probs)))

(def conditional-dist (compute-conditional-dist my-corpus theta-prior))

;;test problem 4
;;(println conditional-dist)
;;prints conditional probabilities of theta1 and theta2 in a list

;;Problem 5
;;conditional-dist (under Problem 4) applies here
;;helper function
(defn exp-map [dist]
	(map (fn [x] (Math/pow 2 x)) dist))

(def exp-cond-dist (exp-map conditional-dist))

;;test problem 5
;;(println exp-cond-dist)
;; prints approx. (2/3 1/3)

;;Problem 6
(defn compute-posterior-predictive [observed-corpus new-corpus theta-probs]
	(let [conditional-dist (compute-conditional-dist observed-corpus theta-probs)]
		(compute-marginal new-corpus (exp-map conditional-dist))))

(def post-pred-dist (compute-posterior-predictive my-corpus my-corpus theta-prior))

;;test problem 6
;;(println post-pred-dist)
;;prints -6.26303...
;;(println (Math/pow 2 post-pred-dist))
;(println (Math/pow 2 compute-marginal-saved))

;;Problem 7
(defn normalize [params]
	(let [sum (apply + params)]
			(map (fn[x] (/ x sum)) params)))

(defn flip [weight]
	(if (< (rand 1) weight)
		true
		false))

(defn sample-categorical [outcomes params]
	(if (flip (first params))
		(first outcomes)
		(sample-categorical (rest outcomes) (normalize (rest params)))))

(defn sample-BOW-sentence [len probabilities]
	(if (= len 0)
		'()
			(cons (sample-categorical vocabulary probabilities)
				(sample-BOW-sentence (- len 1) probabilities))))

(defn sample-BOW-corpus [theta sent-len corpus-len]
	(repeatedly corpus-len (fn [] (sample-BOW-sentence sent-len theta))))

;;test problem 7
;;(def sampled-corpus-1 (sample-BOW-corpus theta1 2 3))
;;(println sampled-corpus-1)


;;Problem 8
(defn sample-theta-corpus [sent-len corpus-len theta-probs]
	(let [theta (sample-categorical thetas theta-probs)]
		(list theta (sample-BOW-corpus theta sent-len corpus-len))))

;;test problem 8
;;(def theta-corpus (sample-theta-corpus 2 3 theta-prior))
;;(println theta-corpus)

;;Problem 9
(defn get-theta [theta-corpus]
	(first theta-corpus))

(defn get-corpus [theta-corpus]
	(first (rest theta-corpus)))

(defn sample-thetas-corpora [sample-size sent-len corpus-len theta-probs]
	(repeatedly sample-size (fn [] (sample-theta-corpus sent-len corpus-len theta-probs))))

(defn estimate-corpus-marginal [corpus sample-size sent-len corpus-len theta-probs]
	(let [corpus-list (get-corpus (sample-thetas-corpora sample-size sent-len corpus-len theta-probs))]
	 (/ (reduce + (map (fn [x] (if (= (get-corpus x)) 1 0)) corpus-list)) 
	 sample-size)))

;;Problem 10
(def fifty-estimate (estimate-corpus-marginal my-corpus 50 2 2 theta-prior))
(def tenthousand-estimate (estimate-corpus-marginal my-corpus 10000 2 2 theta-prior))

;;test problem 9, 10
;;(println fifty-estimate)
;;(println tenthousand-estimate)


;;Problem 11
(defn get-count [obs observation-list count]
	(if (empty? observation-list)
		count
		(if (= obs (first observation-list))
			(get-count obs (rest observation-list) (+ 1 count))
			(get-count obs (rest observation-list) (count)))))

(defn get-counts [outcomes observation-list]
	(let [count-obs (fn [obs] (get-count obs observation-list 0))]
		(map count-obs outcomes)))

(defn rejection-sampler [theta observed-corpus sample-size sent-len corpus-len theta-probs]
	(let [theta-corpus-adjusted (remove (fn [i] (not= (get-corpus i) observed-corpus))
		(sample-thetas-corpora sample-size sent-len corpus-len theta-probs))]
		(if (= 0 (count theta-corpus-adjusted))
				0
				(/ (count (remove (fn[x] (not= theta (get-theta x))) theta-corpus-adjusted))
					(count theta-corpus-adjusted)))))

;;Problem 12
(def rejection-1 (rejection-sampler theta1 my-corpus 10000 2 2 theta-prior))
(def rejection-2 (rejection-sampler theta1 my-corpus 10000 2 2 theta-prior))
(def rejection-3 (rejection-sampler theta1 my-corpus 10000 2 2 theta-prior))
(def rejection-4 (rejection-sampler theta1 my-corpus 10000 2 2 theta-prior))
(def rejection-5 (rejection-sampler theta1 my-corpus 10000 2 2 theta-prior))

;;test problem 11, 12
;;(println rejection-1)
;;(println rejection-2)
;;(println rejection-3)
;;(println rejection-4)
;;(println rejection-5)