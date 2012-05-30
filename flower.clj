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
  
(def img (image 400 400))
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
      (+ (p1 :x) (* (. Math sin (p1 :r)) (p2 :x) (p1 :s)) (* (. Math cos (p1 :r)) (p2 :y) (p1 :s) -1))
	  (+ (p1 :y) (* (. Math cos (p1 :r)) (p2 :x) (p1 :s)) (* (. Math sin (p1 :r)) (p2 :y) (p1 :s)))
	  (+ (p1 :r) (p2 :r))
	  (* (p1 :s) (p2 :s))
	  (p1 :c))))
	
(def centre (struct place 200 200 0 1 (* 255 255 255)))
(def step1 (struct place 0 1 0.08 1.01 (* 255 255 255)))
(def step2 (struct place 0 1 -0.04 0.99 (* 255 255)))
(def fn1 (partial addplace step1))
(def fn2 (partial addplace step2))



(comment "spiral")
  
(defn curve
  []
  (iterate (partial addplace step1) centre))
  
(defn play
  []
  (map drawplace (take 200 (curve))))


  
  
(comment "tree")

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

(comment "fun")

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

