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

### Goals
#### short term:
  * Feed Forward evaluation
  * Backwards Propegation learning
  * use of a zipper-style data structure for moving between layers
  * neuron typeclass
#### Medium goals:
  * solution (and performance profiling) of some trivial problems, eg
    * hand written numeral recognition
  * separation of NN code from application
  * share code publicly
  * exploration of additional NN topologies
#### Longer term:
  * use of genetic programming/genetic Algorithms to explore nn-topologies
  * exploration of performant implementations
    * pure vectors
    * mutable vectors
    * actual linear algebra libraries

# references
for those looking for an overview of ANN and Backpropagation, I like [this series of videos]( https://www.youtube.com/playlist?list=PLZHQObOWTQDNU6R1_67000Dx_ZCJB-3pi)

[Here](https://mattmazur.com/2015/03/17/a-step-by-step-backpropagation-example/) is a detailed example, using numbers.


