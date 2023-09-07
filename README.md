<h1 align="center">
<img src="man/figures/sticker.png" width = "300" height = "300" align="center" />
  <br>
  <h4 align="center">CAVAanalytics: Operational R package for climate services</h4>
  <br>
<div align="center">
   <img src="https://img.shields.io/github/v/release/Risk-team/CAVAanalytics?include_prereleases" alt="GitHub R package version" style="display: inline-block;">
  <img src="https://img.shields.io/github/commits-since/Risk-team/CAVAanalytics/v1.0.0-alpha?include_prereleases" alt="GitHub Commits Since" style="display: inline-block;">
   <img src="https://img.shields.io/github/release-date-pre/Risk-team/CAVAanalytics" alt="release date" style="display: inline-block;">
<a href="http://hits.dwyl.com/Risk-team/CAVAanalytics"><img src="http://hits.dwyl.com/Risk-team/CAVAanalytics.svg"/></a>
</div>
</h1>




#### **Updates

**Some updates are currently underway, in particular, related to CAVA Platform interface. Please note that only the CORDEX-CORE AFR-22 domain is available as of September 2023. Tentatively, we will be releasing one domain per month afterward**.

Dedicated **Python** support is coming soon. In the meantime, see below section.

## Quick intro on CAVA and climate models
CAVA (Climate and Agriculture Risk Visualization and Assessment) is a
framework and approach to climate services developed jointly by **The Food and Agriculture Organization of the United Nations (FAO) and the University of Cantabria**. CAVA makes use of climate models to inform users about future climate conditions (climate projections in jargon). Climate modeling is a complex subject but a very good introduction to this topic is given by Andy Pitman and colleagues and can be downloaded [here](https://climateextremes.org.au/wp-content/uploads/Climate-modelling-an-overview-The-ARC-Centre-of-Excellence-for-Climate-Extremes.pdf). You are strongly encouraged to read it if you are new to this topic.  

CAVA makes use of particular types of climate models, called Regional Climate Models. These models are used to downscale Global Climate Models at higher spatial resolution. The project in charge of providing Regional Climate Models is called CORDEX (Coordinated Regional Climate Downscaling Experiment). These models are available for specific geographical areas around the world, called domain. 


| ![CORDEX domains](https://github.com/Risk-Team/CAVAanalytics/assets/40058235/c0e559d9-0372-4d8a-a3cd-e1b58fcef2b0) |
|:-------------------------------------------------------------------------------------------------------------------:|
|                                                *Boundaries of the fourteen official CORDEX domains. [Source](https://cordex.org/data-access/regional-climate-change-simulations-for-cordex-domains/)*                                                |



## More about CAVA

CAVA (Climate and Agriculture Risk Visualization and Assessment) is a
framework and approach to climate services developed jointly by **The Food and Agriculture Organization of the United Nations (FAO) and the University of Cantabria**. [CAVA is made of a Platform
(GUI)](https://fao-cava.predictia.es/), which is freely accessible
and satisfy the needs of most standard users interested in agroclimatic indicators and an R package
([**CAVAanalytics**](https://risk-team.github.io/CAVAanalytics/)), which empowers users with direct access to high-resolution climate models and means for easily working and calculating climatic indicators on multi-model ensembles. 

CAVAanalytics can be used locally (installation through GitHub),
remotely (registered users can access the University of Cantabria
JupyterHub) and through a Docker image.


| ![framework](https://github.com/Risk-Team/CAVAanalytics/assets/40058235/ac4cfeae-d6d7-49eb-85c2-bfb6c44f39f6) |
|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|
| *CAVA framework. CAVA can be used through a graphic user interface (CAVA Platform) or through the CAVAanalytics R package. The package can be used locally (GitHub installation), remotely (University of Cantabria JupyterHub), or locally but through a Docker image to solve dependencies issues* |



## CAVAanalytics

[**CAVAanalytics**](https://risk-team.github.io/CAVAanalytics/) is an operational package for climate services. It offers a consistent framework to
load, analyze, calculate indicators, and visualize results for multi-model ensembles (this usually happens when working with climate models, but impact models can also be analysed with CAVAanalytics). [**CAVAanalytics**](https://risk-team.github.io/CAVAanalytics/)
provides an access point for CORDEX-CORE simulations at 25 Km resolution
already interpolated (EPSG:4326) plus the W5E5 and ERA5 datasets. CAVAanalytics can
be seen as a wrapper of several packages, but the main engine for
loading and processing climate models is the [climate4R
framework](https://github.com/SantanderMetGroup/climate4R), applied with
a tidyverse approach.

## Installation

Based on how you want to use CAVAanalytics, there are three options.

### 1) Locally

**If you are new to climate4R**, install its main packages first.
You can do so by
```
install.packages(c("rJava", "devtools"))

remotes::install_github(c("SantanderMetGroup/loadeR.java",
                 "SantanderMetGroup/climate4R.UDG",
                 "SantanderMetGroup/loadeR",
                 "SantanderMetGroup/transformeR",
                 "SantanderMetGroup/downscaleR"))
```
**Then**
```
install_github("Risk-Team/CAVAanalytics")
```
It is possible that the installation of loadR.java fails. If so, have a
look at the [loadR main
page](https://github.com/SantanderMetGroup/loadeR) and the [wiki
page](https://github.com/SantanderMetGroup/loadeR/wiki/Installation) to
solve the issue. loadR.java depends on rJava and this installation can
be troublesome.

### 2) JupyterHub

You can request access to the University of Cantabria JupyterHub, where
CAVAanalytics is already installed. This will give you access to
computational resources and you would be able to perform your climate
analysis using a Jupyter Notebook environment. When using JupyterHub you will be using data physically stored at the Cantabria server. If you would like to
access these resources, you are welcome to contact
<riccardo.soldan@fao.org> or <Hideki.Kanamaru@fao.org> stating your intended usage type.

### 3) Docker

The Docker image is available at Docker.io, rso9192/cava. This docker image is built on top of [rocker/rstudio](https://davetang.org/muse/2021/04/24/running-rstudio-server-with-docker/)

#### Linux

```
sudo docker pull docker.io/rso9192/cava:version1.1.3

sudo docker run --rm \
           -p 8888:8787 \
           -e PASSWORD=password \
           rso9192/cava:version1.1.3

```
Now open your favourite browser and type **http://localhost:8888/**. You should see a login page: enter the **username "rstudio"** and **password "password"** to login and that's it! You can now use CAVAanalytics through Rstudio server. 

#### Windows
If you are using Windows, you can install the Windows subsystem for Linux [WSL](https://ubuntu.com/wsl). Then you can run the above commands. Otherwise, you can install Docker Desktop. 

## Quick example

The idea behind CAVAanalytics is to divide the process of working with multiple models into 3 steps. **Firstly**  download or upload data (multiple
models), **secondly** perform the intended analysis, **thirdly** visualize the results.
One nice thing about step 1, is that CAVAanalytics will automatically bind multiple members to create the multimodel ensemble and check temporal consistency. It will also automatically convert units (e.g. Kelvin into Celsius). 


| ![Framework](https://user-images.githubusercontent.com/40058235/199256415-ed32c42b-e2f8-48e0-b4fe-558de6612038.png) |
|:-------------------------------------------------------------------------------------------------------------------:|
|                                                *CAVAanalytics steps*                                                |

Below we give an example of how you can use CAVAanalytics to easily retrieve climate information (both past and future). More examples are available from the [CAVAanalytics website](https://risk-team.github.io/CAVAanalytics/articles/Introduction.html).

**To load CORDEX-CORE data stored remotely**, set path.to.data to
“CORDEX-CORE” and specify the domain. This will load CORDEX-CORE
simulations. Similarly, when path.to.obs is set to W5E5, you are
accessing the dataset stored remotely.


``` r
library(CAVAanalytics)
# 1st step
 remote.data <- load_data(country = "Sudan", variable="tasmax",
 years.hist=1990:1999, years.proj=2050:2059,
 path.to.data = "CORDEX-CORE", aggr.m="mean", domain="AFR-22")
# 2nd step
 climate_change_signal(remote.data, season=1:12, bias.correction = F) %>% 
# 3rd step
 plotting(., ensemble=FALSE, plot_titles = "temperature change",
 palette=IPCC_palette(type = "tmp", divergent = F), bins=T)
```

| ![Rplot](https://github.com/Risk-Team/CAVAanalytics/assets/40058235/7240d4fc-a768-4a39-9848-ee0c28645424) |
|:-------------------------------------------------------------------------------------------------------------------:|
| *Average annual projected change in maximum temperature compared to the 1990-1999 baseline period in Sudan* |

## Python
**Firstly, why Python?** 

While CAVAanalytics was built on top of R packages, such as `climate4R` and `tidyverse` to allow users to focus on results rather than the code, R does not leverage the same level of "computational efficiency" as Python. This is mainly because Python offers out-of-memory computation for arrays thanks to the integration between `xarray` and `dask`. R does not have this feature for arrays, which is typically the way in which climate data is used. This means that CAVAanalytics largely works on memory (RAM), effectively limiting the geographical area in which analyses can be performed ([check out the memory-efficient functions available from CAVAanalytics though](https://risk-team.github.io/CAVAanalytics/reference/index.html)). CAVAanalytics would not allow users to perform analyses for entire CORDEX domains very efficiently because it would need a lot of RAM. However, CAVAanalytics was mainly developed for country-level assessment and never conceived to be used for large-scale climate data analyses. This is where Python comes in. The regridded CORDEX-CORE models and all observational datasets used by CAVAanalytics can also be accessed using Python. **The benefit of using our data is that we provide one URL per model for all supported variables and that we have already regridded CORDEX-CORE models, making retrieving the data extremely easy**. See below an example:

```
import xarray as xr

# URL to ERA5 data
obs_url =  "https://data.meteo.unican.es/thredds/dodsC/copernicus/cds/ERA5_0.25"
# URL to W5E5 V2 data
obs_url =    "https://data.meteo.unican.es/thredds/dodsC/mirrors/W5E5/W5E5_v2"
# Open dataset
ds = xr.open_dataset(obs_url)
```

The list of available CORDEX-CORE models can be accessed with:

```
import pandas as pd

csv_url = "https://data.meteo.unican.es/inventory.csv"
data = pd.read_csv(csv_url)

# Drop rows with missing values in the 'activity' column
data = data.dropna(subset=['activity'])

filtered_data = data[data['activity'].str.contains("FAO")]

```
A dedicated function to automatically perform these steps will be made available soon. 

## Applications

CAVAanalytics has been used by the Risk-team at FAO to lead the development of climate change impact potential assessments for the Green Climate Fund (GCF). Nonetheless, CAVAanalytics can be used by anyone interested in simple and more advanced climate change analyses or retrieving CORDEX-CORE data to be used in impact models. 
