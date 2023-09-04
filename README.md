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

**Some updates are currently underway, in particular, related to CAVA Platform interface**.

Python support is coming soon. This will allow Python users to access CORDEX-CORE models available for CAVAanalytics through xclim.

## What is CAVA?

CAVA (Climate and Agriculture Risk Visualization and Assessment) is a
framework and approach to climate services developed jointly by **The Food and Agriculture Organization of the United Nations (FAO) and the University of Cantabria**. [CAVA is made of a Platform
(GUI)](https://fao-cava.predictia.es/auth), which is freely accessible
and satisfy the needs of most standard users interested in agroclimatic indicators and an R package
(**CAVAanalytics**), which empowers users with direct access to high-resolution climate models and means for easily working and calculating climatic indicators on multi-model ensembles. 

CAVAanalytics can be used locally (installation through GitHub),
remotely (registered users can access the University of Cantabria
JupyterHub) and through a Docker image.


| ![framework](https://github.com/Risk-Team/CAVAanalytics/assets/40058235/ac4cfeae-d6d7-49eb-85c2-bfb6c44f39f6) |
|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|
| *CAVA framework. CAVA can be used through a graphic user interface (CAVA Platform) or through the CAVAanalytics R package. The package can be used locally (GitHub installation), remotely (University of Cantabria JupyterHub), or locally but through a Docker image to solve dependencies issues* |



## CAVAanalytics

**CAVAanalytics** is an operational package for climate services. It offers a consistent framework to
load, analyze, calculate indicators, and visualize results for multi-model ensembles (this usually happens when working with climate models, but impact models can also be analysed with CAVAanalytics). **CAVAanalytics**
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

**Please note that only the AFR-22 domain is available as of September 2023. Tentatively, we will be releasing one domain per month afterwards**

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

## Available data
Observational datasets:
- [x] W5E5
- [x] ERA5

CORDEX-CORE domain:
- [x] AFR-22
- [ ] SEA-22
- [ ] EAS-22
- [ ] CAS-22
- [ ] CAM-22
- [ ] SAM-22
- [ ] WAS-22
- [ ] AUS-22
- [ ] EUR-22

:white_large_square

## Applications

CAVAanalytics has been used by the Risk-team at FAO to lead the development of climate change impact potential assessments for the Green Climate Fund (GCF). Nonetheless, CAVAanalytics can be used by anyone interested in simple and more advanced climate change analyses or retrieving CORDEX-CORE data to be used in impact models. 
Additionally, the raw but interpolated CORDEX-CORE models can be accessed at [University of Cantabria server]( https://hub.climate4r.ifca.es/thredds/catalog/files/ESGF/interp025/CORDEX/output/catalog.html).

## Contributing

You are welcome to contribute to `CAVAanalytics` by forking this repository.
