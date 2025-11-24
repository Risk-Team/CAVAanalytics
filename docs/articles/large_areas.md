# Analyses of large regions

``` r

library(CAVAanalytics)
library(patchwork)
library(ggplot2)
library(magrittr)
```

Despite the potential challenge of working with vast datasets,
CAVAanalytics provides memory-efficient functions that facilitate
processing in spatial chunks. Although this approach requires additional
computational time, it enables the analysis of large areas within the
constraints of available memory.

For instance, to calculate biases in the CORDEX-CORE model simulations
for the number of dry days across Australia during winter, using ERA5 as
the observational reference dataset, CAVAanalytics offers the
load_data_and_model_biases function. This function allows for the
analysis to be conducted in manageable spatial chunks, controlled by the
chunk.size parameter. The following R code snippet demonstrates how to
perform this calculation:

``` r

austr_dry <-load_data_and_model_biases(country = "Australia", variable = "pr", years.hist = 1976:2005, lowert=1,
chunk.size = 10, season = list(6:8), path.to.data = "CORDEX-CORE", domain = "AUS-22", years.proj = NULL, path.to.obs = "ERA5", xlim = c(110,160), ylim = c(-55, -5))
```

Now let’s visualize the CORDEX-CORE ensemble model biases. As we can
see, the CORDEX-CORE ensemble is underestimating the number of dry days
considering ERA5 as reference.

``` r

plotting(austr_dry, plot_titles = "N° days", ensemble = T, legend_range = c(-25,25),
         palette = rev(IPCC_palette(type="pr", divergent = T)))
```

![](../reference/figures/large_fig1.png)

And each individual model bias.

``` r


plotting(austr_dry, plot_titles = "N° days", ensemble = F, palette =rev(IPCC_palette(type="pr", divergent = T))) 
```

![](../reference/figures/large_fig2.png)

For an actual paper describing the biases of the CORDEX‑Australasia
ensemble, refer to [Evans et al.,
(2020)](https://jasonevans.id.au/publications/evansetal2020.pdf).

You can also check the load_data_and_climate_change_signal and the
load_data_and_projections functions for more options
