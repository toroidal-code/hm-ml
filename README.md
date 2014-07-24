# Hindley-Milner in OCaml

This repo contains an implementation of Algorithm W, a linear-time inference engine for the Damas-Hindley-Milner type system.

The `hm.ml` file contains a classic implementation, with added recursive let-bindings.

## HMc

`hp.ml`, tentatively called HMc or Hindley-Milner with calling semantics, is a variant I am developing for my work with [Brick](https://github.com/brick-lang/brick-lang).

It distinguishes between lambda terms (usually called anonymous functions) and functions in the sense of commonly used programming languages today. In this explanation I refer to passing all arguments to a function as a "call", perhaps better described as a full application. Passing only n values to an m-ary function where n is strictly less than m is referred to as a "curry".

HMc attempts to marry calling syntax of languages like Ruby, Python, and Java with the simplicity of currying in languages like Haskell and Ocaml.

To this end, we distinguish between "application" and "calling". There is also a distinction between "functions", and "lambdas".

A lambda is a unary mapping of expressions. An abstraction, if you will.

Functions are n-ary constructs. They are composed of multiple "fragments", which are unary lambdas that wrap the body of the bound expression. For example:

    fn(f,g) -> g(f)

Underneath, this can be represented as

    Function(["f","g"],
        Fragment("f",
            Fragment("g",
                Call(Ident("g"),
                      Ident("f")))))
