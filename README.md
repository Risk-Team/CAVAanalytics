# CAVAanalytics 
<img src="man/figures/sticker.png" width = "300" height = "300" align="right" />

<div>
   <img src="https://img.shields.io/github/v/release/Risk-team/CAVAanalytics?include_prereleases" alt="GitHub R package version" style="display: inline-block;">
  <img src="https://img.shields.io/github/commits-since/Risk-team/CAVAanalytics/v1.0.0-alpha?include_prereleases" alt="GitHub Commits Since" style="display: inline-block;">
   <img src="https://img.shields.io/github/release-date-pre/Risk-team/CAVAanalytics" alt="release date" style="display: inline-block;">
</div>

## Table of Contents
- [What is CAVA?](#what-is-cava)
- [CAVAanalytics](#cavaanalytics)
- [Installation](#installation)
    - [1) Locally](#1-locally)
    - [2) JupyterHub](#2-jupyterhub)
    - [3) Docker](#3-docker)
- [Quick example](#quick-example)



## What is CAVA?

CAVA (Climate and Agriculture Risk Visualization and Assessment) is a
framework and approach to the analysis and visualization of
state-of-the-art climate data. [CAVA is made of a Platform
(GUI)](https://fao-cava.predictia.es/auth), which is freely accessible
and satisfy the needs of most standard users, and a R package
(**CAVAanalytics**) which allows remote access to climate data and
several functions for more advanced climate data analyses.

CAVAanalytics can be used locally (installation through GitHub),
remotely (registered users can access the University of Cantabria
JupyterHub with 180 Gb RAM) and through a Docker image.

|                                                                                           ![framework](https://github.com/Risk-Team/CAVAanalytics/assets/40058235/d0647a38-a128-496d-9d7b-81365c8c7f62)                                                                                            |
|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|
| *CAVA framework. CAVA can be used through a graphic user interface (CAVA Platform) or through the CAVAanalytics R package. The package can be used locally (GitHub installation), remotely (University of Cantabria JupyterHub) or locally but through a Docker image to solve dependencies issues* |



## CAVAanalytics

**CAVAanalytics** is a package that offers a consistent framework to
load, analyze, and visualize multi-model ensembles. **CAVAanalytics**
provides an access point for CORDEX-CORE simulations at 25 Km resolution
already interpolated plus the W5E5 and ERA5 datasets. CAVAanalytics can
be seen as a wrapper of several packages, but the main engine for
loading and processing climate models is the [climate4R
framework](https://github.com/SantanderMetGroup/climate4R), applied with
a tidyverse approach.

CAVAanalytics is being developed with climate models in mind, but it can
be used for any model, like impact models (ISIMIP) if data is locally
available.

## Installation

Based on how you want to use CAVAanalytics, there are three options.

### 1) Locally

**If you are new to climate4R**, install its main packages first.
You can do so by
```
install.packages("rJava")
library(devtools)
install_github(c("SantanderMetGroup/loadeR.java",
                 "SantanderMetGroup/loadeR",
                 "SantanderMetGroup/transformeR",
                 "SantanderMetGroup/downscaleR"))
```
**Then**
```
install_github("Risk-Team/CAVAanalytics")
```
it is possible that the installation of loadR.java fails. If so, have a
look at the [loadR main
page](https://github.com/SantanderMetGroup/loadeR) and the [wiki
page](https://github.com/SantanderMetGroup/loadeR/wiki/Installation) to
solve the issue. loadR.java depends on rJava and this installation can
be troublesome.

### 2) JupyterHub

You can request access to the University of Cantabria JupyterHub, where
CAVAanalytics is already installed. This will give you access to
computational resources and you would be able to perform your climate
analysis using a Jupyter Notebook environment. When using the JupyterHub you will be using data physically stored at Cantabria server. If you would like to
access these resources, you are welcome to contact
<riccardo.soldan@fao.org> or <Hideki.Kanamaru@fao.org>

### 3) Docker

A Docker image will be made available soon, containing all the required
software and libraries needed to immediately start working with CAVAanalytics.


## Quick example

CAVAanalytics makes it easier to work with a large number of climate or
impact model simulations (netCDF files) and perform meaningful analysis.
The idea behind CAVAanalytics is to first load the data (multiple
models) and then work with the output of the load_data with other
**CAVAanalytics** functions.

| ![Framework](https://user-images.githubusercontent.com/40058235/199256415-ed32c42b-e2f8-48e0-b4fe-558de6612038.png) |
|:-------------------------------------------------------------------------------------------------------------------:|
|                                                *CAVAanalytics steps*                                                |


**To load CORDEX-CORE data stored remotely**, set path.to.data to
“CORDEX-CORE” and specify the domain. This will load CORDEX-CORE
simulations. Similarly, when path.to.obs is set to W5E5, you are
accessing the dataset stored remotely.

``` r
library(CAVAanalytics)
  # 1st step
remote.data <- load_data(country = "Sudan", variable="tasmax", years.hist=1995, years.proj=2050:2055,
              path.to.data = "CORDEX-CORE", aggr.m="mean", domain="AFR-22")
  # 2nd step
  projections(remote.data, season=1:12, bias.correction = F) %>% 
  # 3rd step
  plotting(., ensemble=FALSE, plot_titles = "Average tasmax")
```

| ![Rplot](https://github.com/Risk-Team/CAVAanalytics/assets/40058235/471c6fdc-713b-44c6-b130-f912f7bd6d23) |
|:-------------------------------------------------------------------------------------------------------------------:|
|              *Average annual projected maximum temperature in Sudan according to 6 CORDEX-CORE models*                                                |
