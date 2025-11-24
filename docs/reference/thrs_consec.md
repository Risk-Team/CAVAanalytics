# Consecutive days

Calculation of consecutive days. It can be used with aggregateGrid.

## Usage

``` r
thrs_consec(col, duration, lowert, uppert, frequency)
```

## Arguments

- col:

  numeric vector

- duration:

  either "max" or "total".

- lowert:

  numeric. Lower threshold

- uppert:

  numeric. Upper threshold

- frequency:

  logical. Whether frequency or abosulte numbers should be returned.
  Only works with duration != max

## Value

numeric of length 1
