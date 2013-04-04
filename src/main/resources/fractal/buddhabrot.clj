;; Original work by Nurullah Akkaya
;; http://nakkaya.com/2009/10/04/fractals-in-clojure-buddhabrot-fractal/
;;
(ns fractal.buddhabrot
  (:import (javax.swing JFrame JLabel)
           (java.awt Graphics Dimension Color)
           (java.awt.image BufferedImage))
  (use [blancas.eisen.core :only (init-eisen eisenf host-model call)]))

;; To get the fractal computation started:
(comment
  (load "fractal/buddhabrot")
  (ns fractal.buddhabrot)
  (def fractal {:buffer (make-array Integer/TYPE 800 800) :size 800 :iteration 50})
  (start fractal)
)

;; To draw the fractal at intervals during the computation:
(comment
  (draw fractal)
)

;; Host data model for setting up the computation from user code.

(def user-code "src/main/resources/fractal/buddhabrot.esn")

(host-model
  sqrt     #(java.lang.Math/sqrt %)  ;; Package Java interop for user code.
  generate identity)                 ;; Generation function to be replaced.

;; Start the computation using user code.

(defn start [fractal]
  (init-eisen)
  (eisenf user-code)
  (future (call generate fractal)))

(defn calc-pixel-color
  [iteration max-iterations]
  (let [gray (int (/ (* iteration 255) max-iterations))
        r    gray
        g    (min (int ( / (* 5 ( * gray gray)) 255)) 255)
        b    (min (int (+ 40 ( / (* 5 (* gray gray)) 255))) 255)]
    (try
      (Color. r g b)
      (catch Exception e (new Color 0 0 0)))))

(defn paint-canvas [buffer size graphics]
  (let  [biggest  (apply max (map #(apply max %) buffer))]
    (doseq [y (range size)
            x (range size)]

      (.setColor graphics (calc-pixel-color (aget buffer y x) biggest))
      (.drawLine graphics x y x y))))

;; Draws the fractal.

(defn draw [{buffer :buffer size :size}]
  (let [image  (BufferedImage. size size BufferedImage/TYPE_INT_RGB)
        canvas (proxy [JLabel] []
                 (paint [g] (.drawImage g image 0 0 this)))]

    (paint-canvas buffer size (.createGraphics image))

    (doto (JFrame.)
      (.add canvas)
      (.setSize (Dimension. size size))
      (.show))))
