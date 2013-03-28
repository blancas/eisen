# Eisen

Eisen is a language toolkit that supports the development of programmable applications and domain-specific languages.

### The Language

Eisen is designed to be an embedded langauge with the semantics of Clojure and a syntax similar to Standard ML.

### The Compiler

Eisen compiles to Clojure, which compiles to Java bytecode.

### The Library

The Eisen library lets host programs run user code that overrides data and functions for customizations and extensions. The library also exposes the compiler's parsing and translation functions for extending the Eisen language itself, and for developing domain-specific languages.

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
