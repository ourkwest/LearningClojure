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
  [image filename]
  (. ImageIO write image "png" (new File filename)))

(defn filename
  [arg]
  (str (System/getProperty "user.dir") "/deleteme_" arg))
  
(def img (image 400 400))
(defn done [] (write-to-file img (filename ".png")))

(defn file-exists?
  [filename]
  (. (new File filename) exists))
  
(let [filename (filename ".html")]
  (if (not (file-exists? filename)) (spit "") )
  )

(comment "geometry")

(defstruct place :x :y :r :s :c)
(defn addplace 
  [p2 p1]
  (struct place 
    (+ (p1 :x) (* (. Math sin (p1 :r)) (p2 :x) (p1 :s)) (* (. Math cos (p1 :r)) (p2 :y) (p1 :s) -1))
	(+ (p1 :y) (* (. Math cos (p1 :r)) (p2 :x) (p1 :s)) (* (. Math sin (p1 :r)) (p2 :y) (p1 :s)))
	(+ (p1 :r) (p2 :r))
	(* (p1 :s) (p2 :s))
	(* (p1 :c) (p2 :c))))
	
(def centre (struct place 200 200 0 1 1))
(def step1 (struct place 0.1 0.1 0.08 1.01 1.1))

(defn drawplace
  [p]
  (draw img (p :x) (p :y) (int (p :c))))

(defn writeplace
  [p]
  (println p))
  
(defn curve
  []
  (iterate (partial addplace step1) centre))
  
(defn play
  []
  (map drawplace (take 200 (curve))))


  
  
(comment "tree")

(def my-tree [println [println println]] )

(defn apply-tree
  [myinput funcs]
  (if (not (coll? funcs))
	(funcs myinput)
	(if (not (empty? funcs))
      (do 
	    (apply-tree myinput (first funcs))
	    (apply-tree myinput (rest funcs))
	  )
    )
  )
)

