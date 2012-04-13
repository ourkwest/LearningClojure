(comment "Complex numbers")

(defstruct complex :r :i)
(defstruct complex-space :p1 :p2)

(defn mult "Multiplies one complex number by another"
  [a, b]
  (struct complex 
    (- (* (a :r) (b :r)) (* (a :i) (b :i))) 
	(+ (* (a :r) (b :i)) (* (a :i) (b :r)))))

(defn magn "Measure the squared magnitude of a complex number"
  [cpx]
  (+ (* (cpx :r) (cpx :r)) (* (cpx :i) (cpx :i))))
  
(defn addc "Add two complex numbers"
  [a, b]
  (struct complex (+ (a :i) (b :i)) (+ (a :r) (b :r))))
  
(defn width "Find the width of a complex space"
  [{{r1 :r} :p1 {r2 :r} :p2}]
  (- r2 r1))
  
(defn height "Find the height of a complex space"
  [{{i1 :i} :p1 {i2 :i} :p2}]
  (- i2 i1))

  

(comment "Image processing")

(import '(java.awt.image BufferedImage)
        '(java.io File)
		'(javax.imageio ImageIO))

(defn image "Creates an image to write to."
  [width height]
  (new BufferedImage width height (BufferedImage/TYPE_INT_RGB)))

(defn draw "Draws a colour to a pixel of an image"
  [image [x y] c]
  (. (. image getRaster) setPixel (int x) (int y) c))

(defn write-to-file "Writes an image to file."
  [image filename]
  (. ImageIO write image "png" (new File filename)))

(def red (int (* (rand) 255))) ;; 20
(def grn (int (* (rand) 255))) ;; 3
(def blu (int (* (rand) 255))) ;; 24
(println (str red "," grn "," blu))
  
(defn loopy-colour "Turn a single value into an int-array representing a colour"
  [value]
  (int-array [
    (mod (* value red) 255) 
	(mod (* value grn) 255) 
	(mod (* value blu) 255)]))

(defn monochrome "Turn a single value into an int-array representing a colour"
  [value]
  (if (= value 0) (int-array [0 0 0]) (int-array [255 255 255])))
  
	
	
(comment "And the rest...")

(def img (image 900 900))
(def spc (struct complex-space (struct complex -0.0 -0.5) (struct complex 1.0 0.5)))
(def a (struct complex 0.9 -0.1))

(defn coords-of [image]
  (for [x (range (. image getWidth)) y (range (. image getHeight))] [x y]))

(defn make-iter "Make an iterator function."
  [c palette]
  (fn [cpx] 
    (loop [a cpx, limit 100] 
	  (if (> (magn a) (* 2 2))
	    (palette limit)
		(if (< limit 0)
		  (palette 0)
		  (recur (addc (mult a a) c), (dec limit)))))))

(defn make-mapping "Create a mapper function between an image and a complex space."
  [image space]
  (let [
    x-offset (min ((space :p1) :r) ((space :p2) :r))
	y-offset (min ((space :p1) :i) ((space :p2) :i))
    x-scale (/ (width space) (. image getWidth)) 
    y-scale (/ (height space) (. image getHeight))]
	(fn [[x y]] (struct complex (+ (* x x-scale) x-offset) (+ (* y y-scale) y-offset)))))
			  
(defn draw-fractal
  [image space complex palette]
  (let [gen-colour (make-iter complex palette) img-to-spc (make-mapping img spc)]
    (doall (for [pixel (coords-of image)] (draw image pixel (gen-colour (img-to-spc pixel)))))))

(def timing (time (draw-fractal img spc a loopy-colour)))
(write-to-file img "C:/Coding/temp/writeme.png")