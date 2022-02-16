# little-lambda - An UTLC REPL

## Getting Started
### Prerequisites
- GHC >= 8.10.7
- stack >= 2.7.3
### Installation
```
> git clone https://github.com/pe200012/little-lambda
> cd little-lambda
> stack run
```

## Example
```
 stack run
Lambda Calculus REPL 0.0.1
> let i = λ 0
()
> i i i
λ 0
> let k = λ λ 1
()
> k i k
λ 0
> let zero = λ λ 0
()
> let suc = λ λ λ 1 (2 1 0)
()
> suc zero
λ λ 1 (zero 1 0)
> let x = suc x
()
> x
λ λ 1 (μ suc 0 1 0)
> let x = let y = y in x
()
> x
let y = μ 0 in μ let y = μ 0 in 0
> let x = let y = y in x y
()
> x
let y = μ 0 in μ let y = μ 0 in 0 y y
>
```

## TODO
- [ ] handle nested recursive let-binding

## LICENSE
This repo is licensed under BSD-3.
