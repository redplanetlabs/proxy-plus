# proxy-plus

`proxy+` is a replacement for Clojure's `proxy` that's faster and more usable. `proxy` has a strange implementation where it overrides every possible method and uses a mutable field to store a map of string -> function for dispatching the methods. This causes it to be unable to handle methods with the same name but different arities.

`proxy+` fixes these issues with `proxy`. Usage is like `reify`, and it's up to 10x faster.

## Usage

This library provides the macro `proxy+`. The first argument is fields to provide to the superclass's constructor. Next comes `reify`-like definitions to provide overrides.  When extending a base class, the base class should come first. Example usage:

```clj
(proxy+ [super-arg1 super-arg2]
  BaseClass
  (foo [this] -1)
  (foo [this a b] (+ a b))

  SomeInterface
  (bar [this a b c] (* a b c))

  SomeInterface2
  (bar [this] 100)
  )
```

## Benchmark

The code for this benchmark is in `scripts/benchmarks.clj`.

```
proxy one override dispatch performance (10,000 iterations):
------------------------------------------------------------
Evaluation count : 405960 in 60 samples of 6766 calls.
             Execution time mean : 151.149913 µs
    Execution time std-deviation : 3.993821 µs
   Execution time lower quantile : 147.409234 µs ( 2.5%)
   Execution time upper quantile : 162.450275 µs (97.5%)
                   Overhead used : 1.683863 ns

Found 6 outliers in 60 samples (10.0000 %)
	low-severe	 3 (5.0000 %)
	low-mild	 3 (5.0000 %)
 Variance from outliers : 14.1608 % Variance is moderately inflated by outliers


proxy+ one override dispatch performance (10,000 iterations):
-------------------------------------------------------------
Evaluation count : 1126260 in 60 samples of 18771 calls.
             Execution time mean : 54.002204 µs
    Execution time std-deviation : 561.796450 ns
   Execution time lower quantile : 53.246506 µs ( 2.5%)
   Execution time upper quantile : 55.310651 µs (97.5%)
                   Overhead used : 1.683863 ns

Found 3 outliers in 60 samples (5.0000 %)
	low-severe	 3 (5.0000 %)
 Variance from outliers : 1.6389 % Variance is slightly inflated by outliers



proxy ten overrides dispatch performance (10,000 iterations):
-------------------------------------------------------------
Evaluation count : 15780 in 60 samples of 263 calls.
             Execution time mean : 3.835142 ms
    Execution time std-deviation : 53.304494 µs
   Execution time lower quantile : 3.727639 ms ( 2.5%)
   Execution time upper quantile : 3.959029 ms (97.5%)
                   Overhead used : 1.721424 ns

Found 4 outliers in 60 samples (6.6667 %)
	low-severe	 1 (1.6667 %)
	low-mild	 3 (5.0000 %)
 Variance from outliers : 1.6389 % Variance is slightly inflated by outliers


proxy+ ten overrides dispatch performance (10,000 iterations):
--------------------------------------------------------------
Evaluation count : 146280 in 60 samples of 2438 calls.
             Execution time mean : 416.130110 µs
    Execution time std-deviation : 7.529965 µs
   Execution time lower quantile : 406.496697 µs ( 2.5%)
   Execution time upper quantile : 431.908321 µs (97.5%)
                   Overhead used : 1.721424 ns

Found 7 outliers in 60 samples (11.6667 %)
	low-severe	 7 (11.6667 %)
 Variance from outliers : 7.7810 % Variance is slightly inflated by outliers
```

# License

Copyright 2020 Red Planet Labs, Inc. proxy-plus is licensed under Apache License v2.0.
