<h1 align="center">
<img src="man/figures/sticker2.png" width = "300" height = "300" align="center" />
  <br>
  <h5 align="center">R package providing  a framework for easy access, processing, and advanced visualization of gridded climate products</h5>
  <br>
<div align="center">
   <img src="https://img.shields.io/github/r-package/v/Risk-Team/CAVAanalytics" alt="GitHub R package version" style="display: inline-block;">
   <img src="https://img.shields.io/github/release-date-pre/Risk-team/CAVAanalytics" alt="release date" style="display: inline-block;">
   <a href="https://zenodo.org/doi/10.5281/zenodo.11127220"><img src="https://zenodo.org/badge/558266668.svg" alt="DOI"></a>
<a href="http://hits.dwyl.com/Risk-team/CAVAanalytics"><img src="http://hits.dwyl.com/Risk-team/CAVAanalytics.svg"/></a>
</div>
</h1>

--------------------------------------------------------------------------------------------------
**Check GitHub issues for known servers' downtimes**

--------------------------------------------------------------------------------------------------


[**CAVAanalytics**](https://risk-team.github.io/CAVAanalytics/) is an R package offering a consistent framework to load, analyze, calculate indicators, and visualize results for climate and impact models. It also streamlines access to W5E5, ERA5 and CORDEX-CORE models.

### Read on if:

- You tried to download CORDEX models from ESGF or COPERNICUS but found the process difficult (managing netCDF files, etc)
- You tried to work with multiple climate models but found the process of computing climate indicators and then visualising the results hard

**CAVAanalytics** addresses these issues by:

- Providing streamlined access to CORDEX-CORE models for the whole world. This means that you don't need to download netCDF files, data requests are sent to THREDDS servers with an openDAP protocol and you retrieve only the information you asked, directly in memory
- Providing a framework to work with multiple models so that indicators calculation and visualization of results is extremely easy


### Quick example
**Firstly**  download or upload data, **secondly** perform the intended analysis, **thirdly** visualize the results.

| ![Framework](https://user-images.githubusercontent.com/40058235/199256415-ed32c42b-e2f8-48e0-b4fe-558de6612038.png) |
|:-------------------------------------------------------------------------------------------------------------------:|
|                                                *CAVAanalytics steps*                                                |

Below we give an example of how you can use CAVAanalytics to retrieve climate information easily (both past and future). Detailed examples are available from the [tutorial webpage](https://risk-team.github.io/CAVAanalytics/articles/Introduction.html).

``` r
library(CAVAanalytics)
# 1st step
 remote.data <- load_data(country = "Sudan", variable="pr",
 years.hist=1990:2000, years.proj=2020:2030,
 path.to.data = "CORDEX-CORE", aggr.m="sum", domain="AFR-22")
# 2nd step
sudan_ccs <- climate_change_signal(remote.data, season=list(1:12), bias.correction = F)
# 3rd step
 plotting(sudan_ccs, ensemble=FALSE, plot_titles = "Precipitation change (mm)",
 palette=IPCC_palette(type = "pr", divergent = T), legend_range = c(-550,550))
```

| ![Rplot01](https://github.com/Risk-Team/CAVAanalytics/assets/40058235/80a4a686-3192-4226-9535-6cff58192c96) |
|:-------------------------------------------------------------------------------------------------------------------:|
| *Projected change in total annual precipitation compared to the 1990-2000 baseline period in Sudan* |


### Installation

Based on how you want to use CAVAanalytics, there are three options.

#### 1) Locally

**If you are new to climate4R**, You need to first install **rJava**. This installation can be problematic. Follow the instructions below

**Windows**

[Installing rJava in Windows](https://cimentadaj.github.io/blog/2018-05-25-installing-rjava-on-windows-10/installing-rjava-on-windows-10/)

**Linux and macOS**

[Installing rJava in Linux and macOS](https://github.com/SantanderMetGroup/loadeR/wiki/Installation)

If the installation of rJava is successful, you should be able to load the library from Rstudio.

```
# This should not give any error
library(rJava)
```
You can now install CAVAanalytics and its main packages

```
install.packages("devtools")

remotes::github_install(c("SantanderMetGroup/loadeR.java",
                 "SantanderMetGroup/climate4R.UDG",
                 "SantanderMetGroup/loadeR",
                 "SantanderMetGroup/transformeR",
                 "SantanderMetGroup/downscaleR"))

remotes::github_install("Risk-Team/CAVAanalytics")
```

#### 2) Docker

The Docker image is available at Docker.io, rso9192/cava. This docker image is built on top of [rocker/rstudio](https://davetang.org/muse/2021/04/24/running-rstudio-server-with-docker/)

##### Linux

Open the terminal 

```
# if not already installed
sudo apt-get install snapd
# if not already installed
sudo snap install docker

sudo docker pull docker.io/rso9192/cava:version3.2.6

sudo docker run --rm \
           -p 8888:8787 \
           -e PASSWORD=password \
           -v /path/to/local/directory:/home \
           rso9192/cava:version3.2.6


```
Replace **/path/to/local/directory** with the local directory on your host machine where you want to save your plots or data. For example, you can create a folder on your Desktop called CAVA_results. Then, you would run the above command as:

```
sudo docker run --rm \
           -p 8888:8787 \
           -e PASSWORD=password \
           -v /home/Desktop/CAVA_results:/home \
           rso9192/cava:version3.2.6
```


Now open your favorite browser and type **http://localhost:8888/**. You should see a login page: enter the **username "rstudio"** and **password "password"** to login and that's it! You can now use CAVAanalytics through Rstudio server. 

##### Windows
If you are using Windows, you can install [Docker Desktop](https://docs.docker.com/desktop/install/windows-install/) first. Note that Docker Desktop would require the installation of WSL (Windows Subsystem for Linux). 
To do that, open a Command Prompt (as administrator) and run

```
wsl --install

```
This will instal the Windows Subsystem for Linux in your computer. 
Then **start the Docker app** and open an ubuntu terminal and run

```
sudo docker pull docker.io/rso9192/cava:version3.2.6
# ignore the messages about login
```
Now run your image as below. Remember to replace **/path/to/local/directory** with the local directory on your host machine where you want to save your plots or data. For example, you can create a folder on your Desktop called CAVA_results. Then, you could run the above command as:

```
sudo docker run --rm \
           -p 8888:8787 \
           -e PASSWORD=password \
           -v /mnt/c/Users/my_username/Desktop/CAVA_results:/home \
           rso9192/cava:version3.2.6
```

Now open your favorite browser and type **http://localhost:8888/**. You should see a login page: enter the **username "rstudio"** and **password "password"** to login and that's it! You can now use CAVAanalytics through Rstudio server. 

##### Mac

If you are using a Mac, you first need to install [Docker desktop for Mac](https://docs.docker.com/desktop/install/mac-install/). 

Then open a terminal and while Docker Desktop is open, run:

```
sudo docker pull docker.io/rso9192/cava:version3.2.6
```

At this point, you can run the Docker image with the below comand. 

```
sudo docker run --rm \
           -p 8888:8787 \
           -e PASSWORD=password \
           -v /path/to/local/directory:/home \
           rso9192/cava:version3.2.6
```
Remember to replace **/path/to/local/directory** with the local directory on your host machine where you want to save your plots or data. Now open your favorite browser and type **http://localhost:8888/**. You should see a login page: enter the **username "rstudio"** and **password "password"** to login and that's it! You can now use CAVAanalytics through Rstudio server. 



### Python
**Firstly, why Python?** 

While CAVAanalytics was developed using R packages like `climate4R` and `tidyverse` to prioritize result visualization, R lacks the computational efficiency of Python, particularly for handling large climate datasets. Python supports out-of-memory parallel computation for arrays through the integration of`xarray` and `dask`, a feature absent in R. Consequently, CAVAanalytics relies heavily on in-memory (RAM) processing, which limits its ability to efficiently analyze large-scale geographical areas, such as entire CORDEX domains. This limitation arises because such analyses demand substantial RAM resources. However, CAVAanalytics was primarily designed for country-level assessments rather than extensive climate data analyses. ([check out the memory-efficient functions available from CAVAanalytics though](https://risk-team.github.io/CAVAanalytics/reference/index.html))

For users interested in accessing gridded climate projections more efficiently, [cavapy](https://github.com/Risk-Team/cavapy) is recommended. Cavapy focuses on data retrieval in Python, whereas CAVAanalytics offers a broader suite of tools for processing and visualization.

### Contributions

If you would like to contribute to this project, you are welcome to fork this repository and submit your contribution. If you liked CAVAanalytics, please remember to [add a star](https://github.com/Risk-Team/CAVAanalytics/stargazers)! 

### Issues

You can report issues [here](https://github.com/Risk-Team/CAVAanalytics/issues)
