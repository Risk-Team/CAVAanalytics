# cavaR

If you had hard times working with several netCDF files and perform meaningful analysis, especially related to climate change, then this package might be what you are looking for. 

cavaR is the package version of [CAVA-Analytics](https://github.com/Risk-Team/CAVA-Analytics). It allows easy loading of cllimate models, both from local data and remotely, through [climate4RUDG](https://github.com/SantanderMetGroup/climate4R.UDG). 
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

cavaR makes it easier to work with a large number of climate or impact model simulations (netCDF files) and perform meaningful analysis. The idea behind cavaR is to first load the data and then work with the output of the load_data with other **cavaR** functions. 

## 1st Step
### Loading data: load_data function

**cavaR** simplifies and standardize how to load multiple climate models/simulations or other netcdf files (e.g impact models from ISIMIP). To **load local data**, specify the path to your directories, containing, for example, several RCPs and a folder with historical runs. 

``` 
exmp1 <- load_data(country = "Somalia", variable="hurs", years.hist=2000, years.projections=2010
              path.to.rcps = "~/Databases/CORDEX-CORE/AFR-22", path.obs="~/Databases/W5E5")

```

| ![image17](https://user-images.githubusercontent.com/40058235/199230403-5d252400-e543-42ea-89bd-297d777ee6a4.png) | 
|:--:| 
| *When data is uploaded locally, path.to.rcps would contain at least one folder called historical, containing the historical simulation runs and one or more folders per RCP, SSP or other. Within these folders, there can be multiple models or simulations that will be loaded and checked for temportal consistency* |

**To load remote CORDEX-CORE data**, set path.to.rcps to "CORDEX-CORE" and specify the domain. For example:

``` 
local.data <- load_data(country = "Kenya", variable="tasmax", years.hist=1980:2000, years.projections=2010:2030
              path.to.rcps = "CORDEX-CORE", path.obs="W5E5", domain="AFR-22")

# with a small subset of the CAS-22 domain, availabel with the package 
fpath <- system.file("extdata/", package="cavaR")
exmp1 <- load_data(country = "Moldova", variable="hurs", years.hist=2000, years.projections=2010
              path.to.rcps = fpath)
```

would load the variable tasmax for 3 CORDEX-CORE simulations and 2 RCPs alongside the W5E5 dataset for the country of Kenya. To select a bounding box, run:

``` 
remote.data <- load_data(country = NULL, variable="tasmax", years.hist=1980:2000, years.projections=2010:2030
              path.to.rcps = "CORDEX-CORE", path.obs="W5E5", domain="AFR-22", xlim=c(10,20), ylim=c(-30,-20))

```

To automatically load CORDEX-CORE simulations (RCM RegCM4-7), specify path.to.rcps="CORDEX-CORE" and the domain of interest (e.g "AFR-22"). To automatically load the W5E5 dataset, specify path.obs="W5E5". 

load_data returns a tibble with list columns, in a format that allows the user to apply the tidyverse approach for further processing the data. 

``` 
fpath <- system.file("extdata/", package="cavaR")

exmp1 <- load_data(country = "Moldova", variable="hurs", years.hist=2000, years.projections=2010
              path.to.rcps = fpath)

exmp2 <- load_data(country = "Somalia", variable="hurs", years.hist=2000, years.projections=2010
              path.to.rcps = "CORDEX-CORE", path.obs="W5E5", domain="AFR-22")

```

