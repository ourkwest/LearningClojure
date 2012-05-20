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
  (. (. image getRaster) setPixel (int x) (int y) c))

(defn write-to-file "Writes an image to file."
  [image filename]
  (. ImageIO write image "png" (new File filename)))

(def img (image 400 400))
(defn done [] (write-to-file img "C:/Coding/temp/writeme2.png"))


(comment "geometry")

(defstruct place :x :y :r :s)
(defn addplace 
  [p2 p1]
  (struct place 
    (+ (p1 :x) (* (. Math sin (p1 :r)) (p2 :x) (p1 :s)) (* (. Math cos (p1 :r)) (p2 :y) (p1 :s) -1))
	(+ (p1 :y) (* (. Math cos (p1 :r)) (p2 :x) (p1 :s)) (* (. Math sin (p1 :r)) (p2 :y) (p1 :s)))
	(+ (p1 :r) (p2 :r))
	(* (p1 :s) (p2 :s))))
	
(def centre (struct place 200 200 0 1))
(def step1 (struct place 0.75 0.75 0.1 0.99))

(defn drawplace
  [p]
  (draw img (p :x) (p :y) (colour 255 25 150)))

(defn writeplace
  [p]
  (println p))
  
(defn curve
  []
  (iterate (partial addplace step1) centre))
  
(defn play
  []
  (map drawplace (take 150 (curve))))

