(ns fractal.buddhabrot
  (:import (javax.swing JFrame JLabel)
           (java.awt Graphics Dimension Color)
           (java.awt.image BufferedImage))
  (use [blancas.eisen.core :only (init-eisen host-model call)]))

(def user-code "src/main/resources/fractal/buddhabrot.esn")

(host-model
  sqrt     #(java.lang.Math %)  ;; Package Java interop for user code.
  generate identity)            ;; Generation function to be replaced.

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

(defn draw [{buffer :buffer size :size}]
  (let [image  (BufferedImage. size size BufferedImage/TYPE_INT_RGB)
        canvas (proxy [JLabel] []
                 (paint [g] (.drawImage g image 0 0 this)))]

    (paint-canvas buffer size (.createGraphics image))

    (doto (JFrame.)
      (.add canvas)
      (.setSize (Dimension. size size))
      (.show))))
