# hasktron

## Introduction
This is an effort to explore the use of pure Haskell to model Artificial Neural Networks. This is in Haskell as a learning opportunity.

## Orientation
this is a Haskell Stack-based project, useful commands include:
```sh
$ stack test
$ stack build
```
```sh
$ hfmt -w <filename>
$ hlint <filename>
```

#### short term:
  * Feed Forward evaluation
  * Backwards Propegation learning
  * use of a zipper-style data structure for moving between layers
  * exploration of additional NN topologies
#### Medium goals:
  * solution (and performance profiling) of some trivial problems, eg
    * hand written numeral recognition
#### Longer term:
  * use of genetic programming/genetic Algorithms to explore nn-topologies
  * exploration of performant implementations
    * pure vectors
    * mutable vectors
    * actual linear algebra libraries
