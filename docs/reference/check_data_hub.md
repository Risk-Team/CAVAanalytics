# Check that CORDEX and observation paths exist on the HUB filesystem

Uses the same inventory and path rules as \[load_data_hub()\] (including
rewriting JupyterHub prefixes to the GPFS data root). Lists every model
path in the inventory for the given \`domain\` and \`database\` (all
experiments: historical, rcp26, rcp85). Does not load grid data and does
not use years.

## Usage

``` r
check_data_hub(database = "CORDEX-CORE", domain = NULL)
```

## Arguments

- database:

  \`CORDEX-CORE\`, \`CORDEX-CORE-BC\`, or \`NULL\` to check only the
  standard reanalysis paths (ERA5 and W5E5 on the hub filesystem).

- domain:

  CORDEX domain (e.g. \`AFR-22\`). Required when \`database\` is set.

  ERA5 and W5E5 are always included (same locations as
  \[load_data_hub()\] with \`path.to.obs = "ERA5"\` or \`"W5E5"\`).

## Value

Invisibly, a tibble with columns \`path\`, \`role\` (\`model\`,
\`ERA5\`, or \`W5E5\`), and \`exists\`. The CLI reports how many paths
are reachable.

## See also

\[load_data_hub()\], \[check_data_thredds()\]
