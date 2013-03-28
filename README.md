# Eisen

Eisen is a language toolkit that supports the development of programmable applications.

### The Language

Eisen is designed to be an embedded langauge with the semantics of Clojure and a syntax that closely follows *Standard ML*.

### The Compiler

Eisen compiles to Clojure, which compiles to Java bytecode.

### The Library

The Eisen Library makes it easy for a host program to run user code that overrides data and functions to provide customization and extensions. It also allows the host program to extend the Eisen language itself with new expressions and statements.

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
