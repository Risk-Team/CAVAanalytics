# Check that CORDEX and observation URLs are reachable on THREDDS

Uses the same inventory and URLs as \[load_data()\] when
\`path.to.data\` is \`CORDEX-CORE\` or \`CORDEX-CORE-BC\`. Lists every
model URL for the given \`domain\` and dataset (all experiments).
Performs a light HTTP check only (does not open datasets). Does not use
years.

## Usage

``` r
check_data_thredds(path.to.data = "CORDEX-CORE", domain = NULL)
```

## Arguments

- path.to.data:

  \`CORDEX-CORE\`, \`CORDEX-CORE-BC\`, or \`NULL\` to check only the
  standard THREDDS URLs for ERA5 and W5E5. Local directory paths are not
  supported here.

- domain:

  CORDEX domain (e.g. \`AFR-22\`). Required when \`path.to.data\` is
  set.

  ERA5 and W5E5 are always included (same URLs as \[load_data()\] with
  \`path.to.obs = "ERA5"\` or \`"W5E5"\`).

## Value

Invisibly, a tibble with columns \`path\`, \`role\` (\`model\`,
\`ERA5\`, or \`W5E5\`), and \`exists\`. The CLI reports how many paths
are reachable.

## See also

\[load_data()\], \[check_data_hub()\]
