<p align="center">
  <img src="man/figures/sticker2.png" width="250" height="250" />
</p>

<h4 align="center">An R package for easy access, processing, and visualization of gridded climate products</h4>

<p align="center">
  <img src="https://img.shields.io/github/r-package/v/Risk-Team/CAVAanalytics" alt="GitHub R package version">
  <img src="https://img.shields.io/github/release-date-pre/Risk-team/CAVAanalytics" alt="Release date">
  <a href="https://zenodo.org/doi/10.5281/zenodo.11127220"><img src="https://zenodo.org/badge/558266668.svg" alt="DOI"></a>
</p>

---

> **Check [GitHub Issues](https://github.com/Risk-Team/CAVAanalytics/issues) for known server downtimes**

> **New:** Bias-corrected CORDEX-CORE simulations (ISIMIP methodology) are now available as ready-to-use, pre-computed datasets — no local bias-correction step required.

---

## Overview

[**CAVAanalytics**](https://risk-team.github.io/CAVAanalytics/) is a comprehensive R package designed to simplify climate data analysis. It provides a unified framework for accessing datasets such as W5E5, ERA5, and CORDEX-CORE models, enabling efficient loading, analysis, indicator calculation, and visualization.

## Key Features

- **Simplified Data Access** — Retrieve CORDEX-CORE models globally without managing netCDF files. Requests go to THREDDS servers, loading only the data you need directly into memory.
- **Consistent Analytical Framework** — Work seamlessly with multiple climate models for straightforward indicator calculation and visualization.
- **Pre-computed Bias-Corrected Datasets** — Access ISIMIP-style, bias-corrected CORDEX-CORE simulations (using ERA5 as reference), currently available for AFR-22 and WAS-22 domains, with more to come.

## Quick Example

Retrieve precipitation data for Sudan (1990–2000 historical, 2020–2030 projected), compute the climate change signal, and visualize the projected change in total annual precipitation.

Detailed examples are available in the [tutorial](https://risk-team.github.io/CAVAanalytics/articles/Introduction.html).

```r
library(CAVAanalytics)

# Load data
remote.data <- load_data(
  country = "Sudan", variable = "pr",
  years.hist = 1990:2000, years.proj = 2020:2030,
  path.to.data = "CORDEX-CORE-BC", aggr.m = "sum", domain = "AFR-22"
)

# Compute climate change signal
sudan_ccs <- climate_change_signal(remote.data, season = list(1:12), bias.correction = FALSE)

# Plot results
plotting(
  sudan_ccs, ensemble = FALSE,
  plot_titles = "Precipitation change (mm)",
  palette = IPCC_palette(type = "pr", divergent = TRUE),
  legend_range = c(-400, 400)
)
```

|  <img width="3600" height="2400" alt="Sudan precipitation change" src="https://github.com/user-attachments/assets/9ae55cc8-5708-4dbb-906e-0eb137ea6ea6" /> |
|:---:|
| *Projected change in total annual precipitation compared to the 1990–2000 baseline period in Sudan* |

## Installation

CAVAanalytics depends on **rJava**. If you are new to climate4R, install rJava first:

| Platform | Instructions |
|----------|-------------|
| **Windows** | [Installing rJava on Windows](https://cimentadaj.github.io/blog/2018-05-25-installing-rjava-on-windows-10/installing-rjava-on-windows-10/) |
| **Linux / macOS** | [Installing rJava on Linux and macOS](https://github.com/SantanderMetGroup/loadeR/wiki/Installation) |

Once rJava loads successfully in RStudio, install CAVAanalytics:

```r
# Verify rJava works
if (!requireNamespace("rJava", quietly = TRUE)) install.packages("rJava")
library(rJava)

# Install pak if needed
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")

# Install CAVAanalytics
pak::pkg_install("Risk-Team/CAVAanalytics")
```

## Why R (and not Python)?

CAVAanalytics was built with R packages like `climate4R` and `tidyverse` to prioritize visualization. However, R lacks Python's out-of-memory parallel computation capabilities (via `xarray` + `dask`), which means CAVAanalytics relies on in-memory (RAM) processing. This can limit analysis of very large areas such as entire CORDEX domains. That said, CAVAanalytics was primarily designed for **country-level assessments** ([memory-efficient functions are also available](https://risk-team.github.io/CAVAanalytics/reference/index.html)).

For large-scale data retrieval, consider [**cavapy**](https://github.com/Risk-Team/cavapy), the Python companion focused on efficient data access.

## Contributing

Contributions are welcome — fork this repository and submit a PR. If you find CAVAanalytics useful, please consider [giving it a star](https://github.com/Risk-Team/CAVAanalytics/stargazers)!

## Issues

Report bugs or problems [here](https://github.com/Risk-Team/CAVAanalytics/issues).
