(comment "Fractal Flowers")

(comment "Image processing")

(import '(java.awt.image BufferedImage)
        '(java.io File)
		'(javax.imageio ImageIO))

(defn colour "Creates an colour as an array of 3 integers."
  [r g b]
  (int-array [r g b]))
		
(defn image "Creates an image to write to."
  [width height]
  (new BufferedImage width height (BufferedImage/TYPE_INT_RGB)))

(defn draw "Draws a colour to a pixel of an image"
  [image x y c]
  (if (and (> x -1) (> y -1) (< x (. image getWidth)) (< y (. image getHeight)))
    (. image setRGB (int x) (int y) c)))

(defn write-to-file "Writes an image to file."
  [image f]
  (. ImageIO write image "png" (new File f)))

(defn filename
  [arg]
  (str (System/getProperty "user.dir") "/deleteme" arg))
  
(def img (image 800 800))
(defn done [] (write-to-file img (filename ".png")))

(defn file-exists?
  [filename]
  (. (new File filename) exists))
  
(def html (str "<html><body><img src=\"" (filename ".png") "\"/></body></html>"))
  
(let [f (filename ".html")]
  (if (not (file-exists? f)) (spit f html)))

(comment "geometry")

(defstruct place :x :y :r :s :c)

(defn drawplace
  [p]
  (draw img (p :x) (p :y) (int (p :c))))

(defn writeplace
  [p]
  (println p))

(defn addplace 
  [p2 p1]
  (do
    (drawplace p1)
    (struct place 
      (+ (p1 :x) (* (. Math cos (p1 :r)) (p2 :x) (p1 :s)) (* (. Math sin (p1 :r)) (p2 :y) (p1 :s)))
	  (+ (p1 :y) (* (. Math sin (p1 :r)) (p2 :x) (p1 :s)) (* (. Math cos (p1 :r)) (p2 :y) (p1 :s) -1))
	  (+ (p1 :r) (p2 :r))
	  (* (p1 :s) (p2 :s))
	  (mod (* (p1 :c) (p2 :c)) (* 255 255 255)))))
	
(def centre (struct place 400 400 0 1 3))
(def step1 (struct place 0 1 0.08 1.01 5))
(def step2 (struct place 0 1 -0.04 0.99 7))
(def fn1 (partial addplace step1))
(def fn2 (partial addplace step2))

(comment 
(defn compose
  [pt1 fn1 & fns]
  (fn [p2] (let [p3 (fn1 pt1)] apply fns p3)))
)
  
(defn f-then
  [fn1 fn2]
  (fn [arg] (fn2 (fn1 arg))))
  
(defn f-both
  [fn1 fn2]
  (fn [arg] (do (fn1 arg) (fn2 arg))))
  
(defn f-dup
  [f n]
  (fn [arg] 
    (loop [arg arg n n]
	  (if (< n 1) 
	    arg
		(recur (f arg) (dec n))))))
		
(defn f-tree
  [fn1 fn2 n]
  (loop [fna fn1 fnb fn2 n n]
    (if (< n 1) 
	  fna
	  (recur (f-then fn1 (f-both fna fnb)) (f-then fn2 (f-both fnb fna)) (dec n)))))
		
		
(comment "fractal tree")
(comment "                                x y r s c")
(def fn-a (partial addplace (struct place 0 1 0.01 1 5)))
(def fn-b (partial addplace (struct place 0 1 -0.01 1 7)))
(def fn-s (partial addplace (struct place 0 0 -0.1 0.75 1)))

(def fn-c (f-then fn-s (f-dup fn-a 50)))
(def fn-d (f-then fn-s (f-dup fn-b 55)))

(def fn-e 
  (f-then 
    fn-c
    (f-both
      (f-then fn-c (f-both fn-c fn-d))
      (f-then fn-d (f-both fn-c fn-d)))))
	  
(def fn-f (f-tree fn-c fn-d 9))

(defn fun []
  (fn-f centre))


(comment "spiral"
  
(defn curve
  []
  (iterate (partial addplace step1) centre))
  
(defn play
  []
  (map drawplace (take 200 (curve))))
)
 
  
(comment "tree"

(def my-tree [println [println println]] )

(declare apply-child)

(defn apply-tree
  [inp fns]
  (apply-child ((first fns) inp) (rest fns)))
	
(defn apply-child
  [inp coll]
  (if (not (empty? coll))
	(do 
	  (apply-tree inp (first coll))
      (apply-child inp (rest coll)))))
	
  
(comment
(defn apply-tree
  [myinput funcs]
  (if (not (coll? funcs))
	(funcs myinput)
	(if (not (empty? funcs))
      (do 
	    (apply-tree myinput (first funcs))
	    (apply-tree ((first funcs) myinput) (rest funcs))
	  )
    )
  )
)
)
)

(comment "fun"

(defn link
  [leaf stalk reps]
  (loop [leaf leaf stalk stalk reps reps]
    (if (< reps 1)
	  leaf
	  (recur [stalk leaf] stalk (dec reps)))))

(def d1 (link [fn1] fn1 20))
(def d2 (link [fn2] fn2 20))
(def d3 [fn1 d1 d2])
(def d4 (link d3 fn1 20))
	  
(defn fun 
  []
  (apply-tree centre d4))
)
