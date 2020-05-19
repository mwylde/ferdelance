# Fer-de-lance

This is a Rust compiler for Joe Politz's Fer-de-lance language targeting x86-64. Implementing this compiler is the 
final project from UCSD's [CSE-131](https://cseweb.ucsd.edu/classes/sp17/cse131-a/index.html) compilers course in
Spring of 2017. Thanks to Dr. Politz for making his lectures and assignments publicly available!

FDL is a simple ahead-of-time compiled, dynamically-typed language, with the following features:
* Integers, bools, and pairs
* Functions with tail-call elimination
* Closures
* Let bindings
* Command-line input / print output

This compiler extends the language with
* Arbitrarily-sized tuples
* Garbage collection

In addition, this implementation targets x86-64 and extends integer support to 63 bits.

Despite this simplicity, it's possible to write somewhat interesting programs.

## Examples

Sum
```
def sum(a, s) {
  if a == 0: s
  else: sum(a - 1, s + a)
}
sum(input(0), 0)
```

Map
```
def map(f, l) {
  if l == false: l
  else: (f(l[0]), map(f, l[1]))
}

let scale = input(0) in
  map((\x -> x * scale), (3, (2, (1, false))))
```

See more examples [here](https://github.com/mwylde/ferdelance/tree/master/example).

## Getting started

To install, you will first need Rust. You will also need Clang (used to build the runtime and for linking) and Nasm. 
Currently only Linux is supported.

From there:
```
$ git clone git@github.com:mwylde/ferdelance.git
$ cargo install --path ferdelance
$ ferdelance federdelance/example/map.fdl
$ federdelance/example/map.out 5
(15, (10, (5, (false)))
```


