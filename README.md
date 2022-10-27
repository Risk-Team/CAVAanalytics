# cavaR
cavaR is the package version of CAVA-Analytics. It allows easy loading of cllimate models and the output of the load_data function is used as input for the remaining functions of the cavaR package. 
cavaR can be seen as a wrapper of several packages, but the main engine for loading and processing climate models is the  [climate4R framework](https://github.com/SantanderMetGroup/climate4R)
## Working examples

### Installation

``` 
library(devtools)
install_github("Risk-Team/cavaR")
```

if you encounter problems with dependencies, such as loadeR, downscaleR and climate4R.indices, follow the following instructions:
[loadeR](https://github.com/SantanderMetGroup/loadeR),
[downscaleR](https://github.com/SantanderMetGroup/downscaleR),
[climate4R.indices](https://github.com/SantanderMetGroup/climate4R.indices)

A docker version will follow
### Loading example data

``` 
fpath <- system.file("extdata/", package="chatR")
exmp <- load_data(country = "Moldova", variable="hurs", n.cores=6,
              path.to.rcps = fpath)

```

### Visualizing climate projections

``` 


```
