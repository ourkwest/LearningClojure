(comment "Complex numbers")

(defstruct complex :r :i)

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

(defn draw "Draws a pixel to an image"
  [image x y r g b]
  (. (. image getRaster) setPixel (int x) (int y) (int-array [r g b])))

(defn write-to-file "Writes an image to file."
  [image filename]
  (. ImageIO write image "png" (new File filename)))


  
(comment "And the rest...")

(def a (struct complex 0.9 -0.1))

(def img-radius 500)
(def cpx-radius 2)
(def step (/ cpx-radius img-radius))
(def cpx-points 
  (let [cpx-range (range (- cpx-radius) (+ cpx-radius 0) step)]
    (for [x cpx-range y cpx-range] (struct complex x y))))

(def img (image (* img-radius 2) (* img-radius 2)))

(defn make-iterator "Make an iterator function."
  [c image]
  (fn [cpx] 
    (loop [a cpx, limit 100] 
	  (if (> (magn a) (* cpx-radius cpx-radius))
	    (draw image (+ (/ (cpx :r) step) img-radius) (+ (/ (cpx :i) step) img-radius) limit (* limit 2) 255)
		(if (< limit 0)
		  (draw image (+ (/ (cpx :r) step) img-radius) (+ (/ (cpx :i) step) img-radius) 255 0 0)
		  (recur (addc (mult a a) c), (dec limit)))))))

(def iter (make-iterator a img))
(defn write [] (write-to-file img "C:/Coding/temp/delme.png"))

(map iter cpx-points)

