(comment "Defines a struct for a complex number")
(defstruct complex :r :i)

(defn mult "Multiplies one complex number by another"
  [a, b]
  (struct complex (- (* (a :r) (b :r)) (* (a :i) (b :i))) (+ (* (a :r) (b :i)) (* (a :i) (b :r)))))

(defn magn "Measure the squared magnitude of a complex number"
  [cpx]
  (+ (* (cpx :r) (cpx :r)) (* (cpx :i) (cpx :i))))
  
(defn addc "Add two complex numbers"
  [a, b]
  (struct complex (+ (a :i) (b :i)) (+ (a :r) (b :r))))
  
(def a (struct complex 0.9 -0.1))
		
(defn createX [y]
  (loop [all [], x -1]
    (if (< x 1)
	  (recur (conj all (struct complex x y)), (+ x (/ 1 60)))
	  all)))
	  
(defn createY []
  (loop [all [], y (/ -3 2)]
    (if (< y (/ 3 2))
	  (recur (conj all (createX y)), (+ y (/ 1 60)))
	  all)))
  
(defn iterator "create an iterator function"
  [c]
  (fn [cpx] 
    (loop [a cpx, limit 100] 
	  (if (> (magn a) 4)
	    " "
		(if (< limit 0)
		  "O"
		  (recur (addc (mult a a) c), (dec limit)))))))
		  
(defn fractal "Draws a Fractal"
  [c]
  (map (fn [coll] (println (map (iterator c) coll))) (createY)))
  
(fractal a)