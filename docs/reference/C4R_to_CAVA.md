# Converting C4R lists into CAVAanalytics list

Converting a series of C4R lists (output of loadeR::loadeGridData) into
a CAVAanalytics list

## Usage

``` r
C4R_to_CAVA(obs = NULL, projections, historical = NULL)
```

## Arguments

- obs:

  default is NULL. List containing a C4R list. Example list(dataset)
  where dataset is a C4R list

- projections:

  named list containing a list of C4R lists or NULL. For example,
  list(rcp26=list(mod_1, mod_2), rcp85= list(mod_1, mod_2)) where mod_1
  and mod_2 are C4R lists.

- historical:

  list containing a list of C4R lists, one per model or NULL. For
  example, list(list(mod_1, mod_2)) where mod_1 and mod_2 are C4R lists.
