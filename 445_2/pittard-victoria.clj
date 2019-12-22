(ns pittard-victoria)


(def moby-word-tokens '(CALL me Ishmael . Some years ago never mind
how long precisely having little or no money in my purse , and
nothing particular to interest me on shore , I thought I would
sail about a little and see the watery part of the world . It is
a way I have of driving off the spleen , and regulating the
circulation . Whenever I find myself growing grim about the mouth
whenever it is a damp , drizzly November in my soul whenever I
find myself involuntarily pausing before coffin warehouses , and
bringing up the rear of every funeral I meet and especially
whenever my hypos get such an upper hand of me , that it requires
a strong moral principle to prevent me from deliberately stepping
into the street , and methodically knocking people's hats off
then , I account it high time to get to sea as soon as I can .
This is my substitute for pistol and ball . With a philosophical
flourish Cato throws himself upon his sword I quietly take to the
ship . There is nothing surprising in this . If they but knew it 
, almost all men in their degree , some time or other , cherish
very nearly the same feelings toward the ocean with me .))

;;1
(defn member-of-list? [w l]
	(if (empty? l)
        false
        (if (= w (first l))
        	true
          	(member-of-list? w (rest l)))))

(defn get-vocabulary [word-tokens vocab]
	(if (empty? word-tokens)
    	vocab
        (if (member-of-list? (first word-tokens) vocab)
        	(get-vocabulary (rest word-tokens) vocab)
          	(get-vocabulary (rest word-tokens) (cons (first word-tokens) vocab)))))

;;1 test
;;(println (get-vocabulary (list 'the 'man 'is 'man 'the) (list)))
;;returned: (is man the)

;;2
(defn get-count-of-word [w word-tokens count]
	(if (empty? word-tokens)
   		count
        (if (= w (first word-tokens))
        	(get-count-of-word w (rest word-tokens) (+ count 1))
          	(get-count-of-word w (rest word-tokens) count))))

(defn get-word-counts [vocab word-tokens]
	(let [count-word (fn [w]
		(get-count-of-word w word-tokens 0))]
	(map count-word vocab)))

;;2 test
;;(println (get-count-of-word 'the (list 'the 'the 'man) 0))
;;(println (get-count-of-word 'man (list 'the 'the 'man) 0))
;;returned: 2 and 1, respectively
;;(println (get-word-counts (list 'man 'the 'is) (list 'the 'man 'is 'is)))
;;reurned: (1 1 2)

;;3
(defn flip [p]
	(if (< (rand 1) p)
		true
        false))

(defn normalize [params]
  	(let [sum (apply + params)]
  	(map (fn [x] (/ x sum)) params)))

(defn sample-categorical [outcomes params]
 	(if (flip (first params))
      	(first outcomes)
        (sample-categorical (rest outcomes)
       		(normalize (rest params)))))

(defn uniform-distribution [outcomes]
	(let [num-outcomes (count outcomes)]
 	(map (fn [x] (/ 1 num-outcomes))
 		outcomes)))

(def moby-vocab (get-vocabulary moby-word-tokens '()))
(def moby-word-frequencies (get-word-counts moby-vocab moby-word-tokens))

;;3 test
;;(println (uniform-distribution (list 'the 'a 'every)))
;;returned: (1/3 1/3 1/3)
;;(println moby-word-frequencies)

;;4
(defn sample-uniform-BOW-sentence [n vocab]
	(if (= n 0)
		'()
		(cons (sample-categorical vocab (uniform-distribution vocab))
		(sample-uniform-BOW-sentence (- n 1) vocab))))

;;4 test
;(println (sample-uniform-BOW-sentence 4 (list 'the 'a 'every)))
;;returns a list of length 4, containing the words "the" "a" and "every" in different orders

;;5
(defn compute-uniform-BOW-prob [vocab sentence]
	(if (empty? sentence)
		1
		(if (member-of-list? (first sentence) vocab)
			(* (/ 1 (count vocab)) (compute-uniform-BOW-prob vocab (rest sentence)))
			0)))

;;5 test
;;(println (compute-uniform-BOW-prob (list 'the 'a 'every) (list 'every 'every)))
;;returned: 1/9

;;6
(def uni-moby-sent-1 (sample-uniform-BOW-sentence 3 moby-vocab))
(def uni-moby-sent-prob-1 (compute-uniform-BOW-prob moby-vocab uni-moby-sent-1))
(def uni-moby-sent-2 (sample-uniform-BOW-sentence 3 moby-vocab))
(def uni-moby-sent-prob-2 (compute-uniform-BOW-prob moby-vocab uni-moby-sent-2))
(def uni-moby-sent-3 (sample-uniform-BOW-sentence 3 moby-vocab))
(def uni-moby-sent-prob-3 (compute-uniform-BOW-prob moby-vocab uni-moby-sent-3))
(def uni-moby-sent-4 (sample-uniform-BOW-sentence 3 moby-vocab))
(def uni-moby-sent-prob-4 (compute-uniform-BOW-prob moby-vocab uni-moby-sent-4))
(def uni-moby-sent-5 (sample-uniform-BOW-sentence 3 moby-vocab))
(def uni-moby-sent-prob-5 (compute-uniform-BOW-prob moby-vocab uni-moby-sent-5))

;;6 test
;;(println uni-moby-sent-n), where n is a number 1-5
;;returned: for each n, a different three word sentence
;;(println uni-moby-sent-prob-n), where n is a number 1-5
;;returned: for each n, 1/2744000

;;7
(defn sample-BOW-sentence [len vocabulary probabilities]
	(if (= len 0)
		'()
		(cons (sample-categorical vocabulary probabilities)
		(sample-BOW-sentence (- len 1) vocabulary probabilities))))

(def moby-word-probabilities (normalize moby-word-frequencies))

;;7 test
;;(println moby-word-probabilities)

;;8
(def moby-sent (sample-BOW-sentence 3 moby-vocab moby-word-probabilities))

;;8 test
;;(println moby-sent)
;;returned: at each iteration, a different three word sentence

;;9
(defn lookup-probability [w outcomes probs]
	(if (= w (first outcomes))
		(first probs)
		(lookup-probability w (rest outcomes) (rest probs))))

(defn product [l] 
	(apply * l))

;;9 test
;;(println (lookup-probability 'the (list 'the 'a 'every) (list 0.2 0.5 0.3)))
;;(println (lookup-probability 'a (list 'the 'a 'every) (list 0.2 0.5 0.3)))
;;returned: 0.2 and 0.5 respectively

;;10
(defn compute-BOW-prob [sentence vocabulary probabilities]
	(if (empty? sentence)
		1
		(* (lookup-probability (first sentence) vocabulary probabilities)
		(compute-BOW-prob (rest sentence) vocabulary probabilities))))

;;11
(def new-moby-sent-prob-1 (compute-BOW-prob uni-moby-sent-1 moby-vocab moby-word-probabilities))
(def new-moby-sent-prob-2 (compute-BOW-prob uni-moby-sent-2 moby-vocab moby-word-probabilities))
(def new-moby-sent-prob-3 (compute-BOW-prob uni-moby-sent-3 moby-vocab moby-word-probabilities))
(def new-moby-sent-prob-4 (compute-BOW-prob uni-moby-sent-4 moby-vocab moby-word-probabilities))
(def new-moby-sent-prob-5 (compute-BOW-prob uni-moby-sent-5 moby-vocab moby-word-probabilities))

;;11 test
;;(println uni-moby-sent-1)
;;(println uni-moby-sent-prob-1)
;;(println new-moby-sent-prob-1)
;;(println uni-moby-sent-2)
;;(println uni-moby-sent-prob-2)
;;(println new-moby-sent-prob-2)
;;(println uni-moby-sent-3)
;;(println uni-moby-sent-prob-3)
;;(println new-moby-sent-prob-3)
;;(println uni-moby-sent-4)
;;(println uni-moby-sent-prob-4)
;;(println new-moby-sent-prob-4)
;;(println uni-moby-sent-5)
;;(println uni-moby-sent-prob-5)
;;(println new-moby-sent-prob-5)