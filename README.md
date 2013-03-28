# Eisen

Eisen is a language toolkit that supports the development of programmable applications and domain-specific languages.

### The Language

Eisen is designed to be an embedded langauge with the semantics of Clojure and a syntax similar to Standard ML.

### The Compiler

Eisen compiles to Clojure, which compiles to Java bytecode.

### The Library

The Eisen library offers the following facilities:

* Access to the host data and functions from user code.
* Running user code from a host program.
* Parsing and running Eisen code at the Clojure REPL.
* A simple Eisen REPL.

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

## Documentation

## License

Copyright © 2013 Armando Blancas.

Licensed under the [Eclipse Public License](http://www.eclipse.org/legal/epl-v10.html).
