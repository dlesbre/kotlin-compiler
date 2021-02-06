# kotlin-compiler

This is a compiler for a simplified fragment of the kotlin language written as part of a course on compiler in 2018.

This compiler translates kotlin into x86 assembly.

It supports :

- basic operatiors (`+`, `-`, `*`, `/`, `%`, `&&`, `||`, `!`)
- basic comparaisons (`===`, `!==`, `==`, `!=`, `<`, `<=`, `>` `>=`)
- basic types (`Int`, `Unit`, `Boolean`, `String`, the `?` and `->` constructors and user defined classes)
- functions and function with type templates
- simple method-less classes (i.e. structs) with type templates
