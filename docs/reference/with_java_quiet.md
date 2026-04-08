# Suppress Java stderr and evaluate an expression

Redirects the JVM's System.err to the null device, evaluates `expr`,
then restores the original stream. Useful for silencing Java HTTP retry
logs emitted by the Unidata/netCDF libraries.

## Usage

``` r
with_java_quiet(expr)
```

## Arguments

- expr:

  An expression to evaluate while Java stderr is suppressed.

## Value

The result of evaluating `expr`.
