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
  


(comment "Image processing")

(import '(java.awt.image BufferedImage)
        '(java.io File)
		'(javax.imageio ImageIO))

(defn image "Creates an image to write to."
  [width height]
  (new BufferedImage width height (BufferedImage/TYPE_INT_RGB)))

(defn draw "Draws a colour to a pixel of an image"
  [image x y c]
  (. (. image getRaster) setPixel (int x) (int y) c))

(defn write-to-file "Writes an image to file."
  [image filename]
  (. ImageIO write image "png" (new File filename)))

(def red (int (* (rand) 255)))
(def grn (int (* (rand) 255)))
(def blu (int (* (rand) 255)))
  
(defn make-colour "Turn a single value into an int-array representing a colour"
  [value]
  (int-array [
    (mod (* value red) 255) 
	(mod (* value grn) 255) 
	(mod (* value blu) 255)]))
  
(comment "And the rest...")

(def a (struct complex 0.9 -0.1))

(def img-radius 150)
(def cpx-radius 2)
(def step (/ cpx-radius img-radius))
(def cpx-points 
  (let [cpx-range (range (- cpx-radius) (+ cpx-radius 0) step)]
    (for [x cpx-range y cpx-range] (struct complex x y))))

(def img (image (* img-radius 2) (* img-radius 2)))

(defn make-iterator "Make an iterator function."
  [c]
  (fn [cpx] 
    (loop [a cpx, limit 100] 
	  (if (> (magn a) (* cpx-radius cpx-radius))
	    [cpx (make-colour limit)]
		(if (< limit 0)
		  [cpx (make-colour 0)]
		  (recur (addc (mult a a) c), (dec limit)))))))
  
(defn make-renderer
  [image]
  (fn [cpx-colour] 
    (let [[cpx colour] cpx-colour half-size (/ (. image getWidth) 2)]
      (draw image (+ (/ (cpx :r) step) half-size) (+ (/ (cpx :i) step) half-size) colour))
  ))
		  
(def iter (make-iterator a))
(defn write [] (write-to-file img "C:/Coding/temp/writeme.png"))

(map (make-renderer img) (map iter cpx-points))

