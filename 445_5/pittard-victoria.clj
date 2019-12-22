(ns pittard-victoria)

;;background
(def hidden-states '(Start N V))

(def vocabulary '(Call me Ishmael))

(def theta-transition-Start '(0 0.9 0.1))

(def theta-transition-N '(0 0.3 0.7))

(def theta-transition-V '(0 0.8 0.2))

(def theta-transition-dists-1 
	(list theta-transition-Start theta-transition-N theta-transition-V))

(def theta-obsesrvation-Start '(0 0 0))

(def theta-observation-N '(0.1 0.5 0.4))

(def theta-observation-V '(0.8 0.1 0.1))

(def theta-observation-dists-1
	(list theta-obsesrvation-Start theta-observation-N theta-observation-V))

(defn dist-lookup [state states dists]
	(if (= state (first states))
		(first dists)
		(dist-lookup state (rest states) (rest dists))))

;;note: defined alternative log2 in place of Math/log2 due to importing issues, as in previous assignments
(defn log2 [n]
	(/ (Math/log n) (Math/log 2)))

(defn logsumexp [log-vals]
	(let [mx (apply max log-vals)]
		(+ mx (log2
			(apply + 
				(map (fn [z] (Math/pow 2 z))
					(map (fn [x] (- x mx)) log-vals)))))))

(defn logscore-categorical [outcome outcomes params]
	(if (= outcome (first outcomes))
		(log2 (first params))
		(logscore-categorical outcome (rest outcomes) (rest params))))

;;problem 1
(defn score-next-state-word [current-hidden next-hidden next-observed theta-transition-dists theta-observation-dists]
	(+ (logscore-categorical next-hidden hidden-states (dist-lookup current-hidden hidden-states theta-transition-dists))
	(logscore-categorical next-observed vocabulary (dist-lookup next-hidden hidden-states theta-observation-dists))))

;;test 1
;;(println (score-next-state-word 'N 'V 'Call theta-transition-dists-1 theta-observation-dists-1 ))
;;prints approx. -0.8365

; ;;problem 2
(defn compute-next-observation-marginal [current-state next-observation theta-transition-dists theta-observation-dists]
	(let [func (fn [x]
		(score-next-state-word current-state x next-observation theta-transition-dists theta-observation-dists))]
	(logsumexp (map func hidden-states))))

;;test 2
;;(println (compute-next-observation-marginal 'N 'Call theta-transition-dists-1 theta-observation-dists-1))
;;prints approx. -0.7612

;;problem 3
(defn score-next-states-word [current-hidden next-hidden-states next-words theta-transition-dists theta-observation-dists]
	(if (empty? next-hidden-states)
		0
		(+ (score-next-state-word current-hidden (first next-hidden-states) (first next-words) theta-transition-dists theta-observation-dists) 
			(score-next-states-word (first next-hidden-states) (rest next-hidden-states) (rest next-words) theta-transition-dists theta-observation-dists))))

;;test 3
;;(println (score-next-states-word 'Start (list 'N 'V 'N) (list 'me 'Call 'me) theta-transition-dists-1 theta-observation-dists-1))
;;prints approx. -3.3104

;;problem 4
(defn compute-next-words-marginal [current-hidden next-words theta-transition-dists theta-observation-dists]
	;;(println 'x) - used to test number of calls.
	(if (empty? next-words)
		0
		(logsumexp (map (fn [x]
			(+ 	(score-next-state-word current-hidden x (first next-words) theta-transition-dists theta-observation-dists)
				(compute-next-words-marginal x (rest next-words) theta-transition-dists theta-observation-dists))) hidden-states))))

;;test 4
(println (compute-next-words-marginal 'Start (list 'Call 'me) theta-transition-dists-1 theta-observation-dists-1))
;;prints approx. -4.2270

;;problem 5
(def sequence1 (list 'Call 'me))
(def rev-sequence1 (list 'me 'Call))
;;(println (compute-next-words-marginal 'Start sequence1 theta-transition-dists-1 theta-observation-dists-1))
;;prints approx. -4.2270
;;(println (compute-next-words-marginal 'Start rev-sequence1 theta-transition-dists-1 theta-observation-dists-1))
;;prints approx. -1.9002

;;problem 6 - note: assumed W0 is always Start, and is thus not given as the first element of the inputted list
(defn hidden-prior-helper [remaining current dists]
	(if (empty? remaining)
		0
		(+ (hidden-prior-helper (rest remaining) (first remaining) dists)
			(logscore-categorical (first remaining) hidden-states (dist-lookup current hidden-states dists)))))

(defn compute-hidden-prior [hidden theta-transition-dists]
	(if (empty? hidden)
		0
		(hidden-prior-helper hidden 'Start theta-transition-dists)))

;;test 6
;;(println (compute-hidden-prior (list 'N 'N 'N 'N) theta-transition-dists-1))
;;prints approx. -5.3629

;;problem 7
(defn compute-likelihood-of-words [hidden words theta-observation-dists]
	(if (empty? hidden)
		0
		(+ 
			(compute-likelihood-of-words (rest hidden) (rest words) theta-observation-dists)
			(logscore-categorical (first words) vocabulary (dist-lookup (first hidden) hidden-states theta-observation-dists)))))

;;test 7
;;(println (compute-likelihood-of-words (list 'N 'N 'N) (list 'Call 'me 'Ishmael) theta-observation-dists-1))
;;prints approx. -5.6439

;;problem 8
(defn compute-hidden-posterior [hidden-states words theta-transition-dists theta-observation-dists]
	(- 
		(+
			(compute-next-words-marginal 'Start words theta-transition-dists theta-observation-dists)
			(compute-hidden-prior hidden-states theta-transition-dists))
		(compute-likelihood-of-words hidden-states words theta-observation-dists)))

;;test 8
;;(println (compute-hidden-posterior (list 'V 'N 'N) (list 'Call 'me 'Ishmael) theta-transition-dists-1 theta-observation-dists-1))
;;prints approx. -9.2005

;;problem 10
(def compute-next-words-marginal-mem 
	(memoize (fn [current-hidden next-words theta-transition-dists theta-observation-dists]
		;;(println 'y) - tested number of calls
		(if (empty? next-words)
			0
			(logsumexp (map (fn [x]
				(+ 	(score-next-state-word current-hidden x (first next-words) theta-transition-dists theta-observation-dists)
					(compute-next-words-marginal-mem x (rest next-words) theta-transition-dists theta-observation-dists)))
				hidden-states))))))

;;test 10
;;(println (compute-next-words-marginal-mem 'Start (list 'Call 'me) theta-transition-dists-1 theta-observation-dists-1))
;;prints approx. -4.2270