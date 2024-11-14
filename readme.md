# mili

> Minimal linear lambda calculus with linear types, de Bruijn indices
> and unbounded iteration/minimization

Linear lambda calculus is a restricted version of the lambda calculus in
which every variable occurs exactly once. This typically implies ptime
normalization and therefore Turing incompleteness.

We extend the linear lambda calculus with unbounded
iteration/minimization such that it barely becomes Turing complete,
while still benefitting from the advantages of syntactic linearity.

## core

Mili's core syntactic representation consists of only five constructs:

``` haskell
data Term = Abs Int Term                   -- | Abstraction at level
          | App Term Term                  -- | Application
          | Lvl Int                        -- | de Bruijn level
          | Num Nat                        -- | Peano numeral
          | Rec (Term, Nat) Term Term Term -- | Unbounded iteration
```

Lets and Pairs are only used in the higher-level syntax. This allows us
to use a minimal abstract reduction machine.

## roadmap

-   [x] basic syntax
-   [x] linearity check
-   [x] basic reduction
-   [ ] type syntax
-   [ ] type checking
-   [ ] reduction shortcut
-   [ ] preprocessor
    -   [ ] `:test`
    -   [ ] `:import`
-   [ ] more examples

## references

-   Alves, Sandra, et al. "Linear recursion." arXiv preprint
    arXiv:1001.3368 (2010).
-   Lincoln, Patrick, and John C. Mitchell. "Operational aspects of
    linear lambda calculus." LICS. Vol. 92. 1992.
-   Mairson, Harry G. "Linear lambda calculus and PTIME-completeness."
    Journal of Functional Programming 14.6 (2004): 623-633.
