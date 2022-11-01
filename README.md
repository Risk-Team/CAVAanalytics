# cavaR

If you encountered difficulties working with several netCDF files and perform meaningful analysis, especially related to climate change, then this package might be what you are looking for. 

cavaR is the package version of CAVA-Analytics. It allows easy loading of cllimate models, both from local data and remotely, through [climate4RUDG](https://github.com/SantanderMetGroup/climate4R.UDG). 
cavaR can be seen as a wrapper of several packages, but the main engine for loading and processing climate models is the  [climate4R framework](https://github.com/SantanderMetGroup/climate4R), applied with a tidyverse approach. 

## Installation

The development version of cavaR can be installed directly from github with:

``` 
library(devtools)
install_github("Risk-Team/cavaR")
```
if you encounter problems with dependencies, such as loadeR, downscaleR and climate4R.indices, follow the following instructions:
[loadeR](https://github.com/SantanderMetGroup/loadeR),
[downscaleR](https://github.com/SantanderMetGroup/downscaleR),
[climate4R.indices](https://github.com/SantanderMetGroup/climate4R.indices)

A conda environment will follow

## A framework to work with climate data and other netCDF files

cavaR makes it easier to work with a large number of climate or impact model simulations (netCDF files) and perform meaningful analysis. The idea behind cavaR is to first load the data and then work with the output of the load_data with other functions. 

### Loading data: example

cavaR simplifies and standardize how to load multiple climate models or other netcdf files (e.g impact models from ISIMIP). To automatically load CORDEX-CORE simulations (RCM RegCM4-7), specify path.to.rcps="CORDEX-CORE" and the domain of interest (e.g "AFR-22"). To automatically load the W5E5 dataset, specify path.obs="W5E5". 

load_data returns a tibble with list columns, in a format that allows the user to apply the tidyverse approach for further processing the data. 

``` 
fpath <- system.file("extdata/", package="cavaR")

exmp1 <- load_data(country = "Moldova", variable="hurs", years.hist=2000, years.projections=2010
              path.to.rcps = fpath)

exmp2 <- load_data(country = "Somalia", variable="hurs", years.hist=2000, years.projections=2010
              path.to.rcps = "CORDEX-CORE", path.obs="W5E5", domain="AFR-22")

```

