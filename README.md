# Eisen

Eisen is a toolkit that supports the development of programmable applications and domain-specific languages (DSL).

### The Language

Eisen is designed to be an embedded language with the semantics of Clojure and a syntax similar to Standard ML.

### The Compiler

Eisen compiles to Clojure, which compiles to Java bytecode.

### The Library

The Eisen library offers the following facilities:

* Access to the host data and functions from user code.
* Running user code from a host program.
* Functions for parsing and running Eisen code at the Clojure REPL.
* A simple Eisen REPL.
* The compiler's parsing and translation functions.
* Compiler hooks for extending Eisen or writing DSL's.

## Setup

Leiningen:

```clojure
[org.blancas/eisen "0.1.0"]
```

Maven:

```xml
<dependency>
  <groupId>org.blancas</groupId>
  <artifactId>eisen</artifactId>
  <version>0.1.0</version>
</dependency>
```

## Sample Usage

The following sections show parts of two samples to highlight some Eisen features.

### The Sname Game
 
The [Snake](https://github.com/blancas/eisen/wiki/Extending-the-Snake-Game-Interactively) sample is a modified version of the game in Programming Clojure that may be changed and extended. The host program declares a mutable model for the user code to read and change.

```clojure
(host-model
  width       75
  height      50
  win-length   2
  point-size  10
  body        (list [1 1])
  dir         [1 0]
  add-points  (fn [& pts] (vec (apply map + pts))))
```

The game reads the user code pasted on a simple text field and evaluates it.

```
(defn run-eisen []
  (let [code (JOptionPane/showInputDialog
	        nil "Paste your code here:" "Change the game"
	        JOptionPane/PLAIN_MESSAGE)]
    (when (seq code)
      (let [result (eisen code)]
	(when-not (:ok result)
  	  (JOptionPane/showMessageDialog nil (:error result)))))))
```

On the Eisen side, this code makes the snake wrap around the borders.

```sml
fun add p1 p2 =
    let p = map `+` p1 p2;
        x = first p;
        y = second p
    in
        cond x > _width  => #[1, y];
             x < 0       => #[_width, y];
             y > _height => #[x, 1];
             y < 0       => #[x, _height];
             :else       => #[x, y]
        end
    end
```

The following line installs the function `add` above, which the host calls as `add-points`.

```
setv add-points = add
```

### A Buddhabrot Fractal

The [fractal](https://github.com/blancas/eisen/wiki/Computing-a-Buddhabrot-Fractal) sample is a modified version of [Nurullah Akkaya](http://nakkaya.com/2009/10/04/fractals-in-clojure-buddhabrot-fractal/)'s program that delegates the computation to user code written in Eisen. 

The main program defines the model shared with the user code. Function `start` was modified to setup the Eisen library and evaluate the user code.

```clojure
(host-model
  sqrt     #(java.lang.Math/sqrt %)  ;; Package Java interop for user code.
  generate identity)                 ;; Generation function to be replaced.

;; Start the computation using user code.

(defn start [fractal]
  (init-eisen)
  (eisenf user-code)
  (future (call generate fractal)))
```

These are some of the functions. (Note that destructuring is not currently supported.)

```sml
(* Computes a point's path. *)

fun calcPath x y max =
  let c = #[x, y]
  in
      loop z    = c;
           path = #[];
           iter = 0
      in
           if iter > max then #[]
           else
             if abs z > 2.0 then z : path
             else recur (add c (mult z z)) (z : path) (inc iter)
      end
  end

(* Translates a point into a screen coordinate. *)

fun point2coord size c =
  let real = first c;
      imag = second c
  in
      #[ int (0.3 * size * (real + 0.5) + size / 2), 
         int (0.3 * size * imag + size / 2) ]
  end

(* Sets a point into the fractal's buffer. *)

fun bufferSet fractal point =
  let size   = get fractal :size;
      buffer = get fractal :buffer;
      p      = point2coord size point;
      x      = first p;
      y      = second p
  in
      if x > 0 && y > 0 && x < size && y < size then
         `aset-int` buffer y x (aget buffer y x  + 1)
  end

(* Generates the fractal image. *)

fun compute fractal =
  let buffer = get fractal :buffer;
      iter   = get fractal :iteration
  in
      doseq [point <- iterate inc 1]
          let x    = rand 6 - 3;
              y    = rand 6 - 3;
              path = calcPath x y iter;
          in
              if mod point 1000000 == 0 then
                 println "Point:" point;

              doseq [p <- path] bufferSet fractal p end
          end
      end
  end

(* Installs the local function. *)

setv generate = compute
```

## Documentation

Eisen is documented in the [Wiki](https://github.com/blancas/eisen/wiki).

Browse the Codox [Eisen v0.1.0 API](http://blancas.github.com/eisen).

## License

Copyright © 2013 Armando Blancas.

Licensed under the [Eclipse Public License](http://www.eclipse.org/legal/epl-v10.html).
