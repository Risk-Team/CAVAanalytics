<h1 align="center">
<img src="man/figures/sticker2.png" width = "300" height = "300" align="center" />
  <br>
  <h4 align="center">Operational R package for climate services</h4>
  <br>
<div align="center">
   <img src="https://img.shields.io/github/v/release/Risk-team/CAVAanalytics?include_prereleases" alt="GitHub R package version" style="display: inline-block;">
   <img src="https://img.shields.io/github/release-date-pre/Risk-team/CAVAanalytics" alt="release date" style="display: inline-block;">
<a href="http://hits.dwyl.com/Risk-team/CAVAanalytics"><img src="http://hits.dwyl.com/Risk-team/CAVAanalytics.svg"/></a>
</div>
</h1>


#### **Updates

**THREDDS Servers have been updated, massively increasing the speed of the load_data function**

**Current available CORDEX domains are: AFR-22, SEA-22, EAS-22.**

**Current available observational datasets: W5E5, ERA5**

--------------------------------------------------------------------------------------------------

[**CAVAanalytics**](https://risk-team.github.io/CAVAanalytics/) is an operational package for climate services. 
It offers a consistent framework to load, analyze, calculate indicators, and visualize results for multi-model ensembles.

[**CAVAanalytics**](https://risk-team.github.io/CAVAanalytics/) provides:

- Direct remote access to CORDEX-CORE simulations at 25 Km resolution already interpolated (EPSG:4326) 
- Direct remote access to W5E5 and ERA5 datasets
- Processing and visualization capabilities

CAVAanalytics can be seen as a wrapper of several packages, but the main engine for loading and processing climate model outputs is the [climate4Rframework](https://github.com/SantanderMetGroup/climate4R), applied with a tidyverse approach.

CAVAanalytics is part of a bigger framework, called [Climate and Agriculture Risk Visualization and Assessment (CAVA)](https://risk-team.github.io/CAVAanalytics/articles/CAVA.html). 


### Quick example

The idea behind CAVAanalytics is to divide the process of working with multiple models into 3 steps. **Firstly**  download or upload data (multiple
models), **secondly** perform the intended analysis, **thirdly** visualize the results.
One nice thing about step 1, is that CAVAanalytics will automatically bind multiple members to create the multimodel ensemble and check temporal consistency. It will also automatically convert units (e.g. Kelvin into Celsius). 


| ![Framework](https://user-images.githubusercontent.com/40058235/199256415-ed32c42b-e2f8-48e0-b4fe-558de6612038.png) |
|:-------------------------------------------------------------------------------------------------------------------:|
|                                                *CAVAanalytics steps*                                                |

Below we give an example of how you can use CAVAanalytics to easily retrieve climate information (both past and future). More examples are available from the [tutorial webpage](https://risk-team.github.io/CAVAanalytics/articles/Introduction.html).

**To load CORDEX-CORE data stored remotely**, set path.to.data to
“CORDEX-CORE” and specify the domain. This will load CORDEX-CORE
simulations. Similarly, when path.to.obs is set to W5E5, you are
accessing the dataset stored remotely.


``` r
library(CAVAanalytics)
# 1st step
 remote.data <- load_data(country = "Sudan", variable="tasmax",
 years.hist=1990:2000, years.proj=2050:2060,
 path.to.data = "CORDEX-CORE", aggr.m="mean", domain="AFR-22")
# 2nd step
 climate_change_signal(remote.data, season=list(1:12), bias.correction = F) %>% 
# 3rd step
 plotting(., ensemble=TRUE, plot_titles = "Temperature change (°C)",
 palette=IPCC_palette(type = "tmp", divergent = F), bins=T)
```

| ![Rplot01](https://github.com/Risk-Team/CAVAanalytics/assets/40058235/3dce772c-51e9-4cd1-9ad4-5ddfcdf75698) |
|:-------------------------------------------------------------------------------------------------------------------:|
| *Average annual projected change in maximum temperature compared to the 1990-2000 baseline period in Sudan* |


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

remotes::install_github(c("SantanderMetGroup/loadeR.java",
                 "SantanderMetGroup/climate4R.UDG",
                 "SantanderMetGroup/loadeR",
                 "SantanderMetGroup/transformeR",
                 "SantanderMetGroup/downscaleR"))

remotes::install_github("Risk-Team/CAVAanalytics")
```

#### 2) JupyterHub

You can request access to the University of Cantabria JupyterHub, where
CAVAanalytics is already installed. This will give you access to
computational resources and you would be able to perform your climate
analysis using a Jupyter Notebook environment. When using JupyterHub you will be using data physically stored at the Cantabria server. If you would like to
access these resources, you are welcome to contact
<riccardo.soldan@fao.org> or <Hideki.Kanamaru@fao.org> stating your intended usage type.

#### 3) Docker

The Docker image is available at Docker.io, rso9192/cava. This docker image is built on top of [rocker/rstudio](https://davetang.org/muse/2021/04/24/running-rstudio-server-with-docker/)

##### Linux

Open the terminal 

```
# if not already installed
sudo apt-get install snapd
# if not already installed
sudo snap install docker

sudo docker pull docker.io/rso9192/cava:version2.0.3

sudo docker run --rm \
           -p 8888:8787 \
           -e PASSWORD=password \
           -v /path/to/local/directory:/home \
           rso9192/cava:version2.0.3


```
Replace **/path/to/local/directory** with the local directory on your host machine where you want to save your plots or data. For example, you can create a folder on your Desktop called CAVA_results. Then, you would run the above command as:

```
sudo docker run --rm \
           -p 8888:8787 \
           -e PASSWORD=password \
           -v /home/Desktop/CAVA_results:/home \
           rso9192/cava:version2.0.3
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
sudo docker pull docker.io/rso9192/cava:version2.0.2
# ignore the messages about login
```
Now run your image as below. Remember to replace **/path/to/local/directory** with the local directory on your host machine where you want to save your plots or data. For example, you can create a folder on your Desktop called CAVA_results. Then, you could run the above command as:

```
sudo docker run --rm \
           -p 8888:8787 \
           -e PASSWORD=password \
           -v /mnt/c/Users/my_username/Desktop/CAVA_results:/home \
           rso9192/cava:version2.0.3
```

Now open your favorite browser and type **http://localhost:8888/**. You should see a login page: enter the **username "rstudio"** and **password "password"** to login and that's it! You can now use CAVAanalytics through Rstudio server. 

##### Mac

If you are using a Mac, you first need to install [Docker desktop for Mac](https://docs.docker.com/desktop/install/mac-install/). 

Then open a terminal and while Docker Desktop is open, run:

```
sudo docker pull docker.io/rso9192/cava:version2.0.3
```

At this point, you can run the Docker image with the below comand. 

```
sudo docker run --rm \
           -p 8888:8787 \
           -e PASSWORD=password \
           -v /path/to/local/directory:/home \
           rso9192/cava:version2.0.3
```
Remember to replace **/path/to/local/directory** with the local directory on your host machine where you want to save your plots or data. Now open your favorite browser and type **http://localhost:8888/**. You should see a login page: enter the **username "rstudio"** and **password "password"** to login and that's it! You can now use CAVAanalytics through Rstudio server. 



### Python
**Firstly, why Python?** 

While CAVAanalytics was built on top of R packages, such as `climate4R` and `tidyverse` to allow users to focus on visualizing results, R does not leverage the same level of "computational efficiency" as Python. This is mainly because Python offers out-of-memory computation for arrays thanks to the integration between `xarray` and `dask`. R does not have this feature for arrays, which is typically the way in which climate data is used. This means that CAVAanalytics largely works on memory (RAM), effectively limiting the geographical area in which analyses can be performed ([check out the memory-efficient functions available from CAVAanalytics though](https://risk-team.github.io/CAVAanalytics/reference/index.html)). CAVAanalytics would not allow users to perform analyses for entire CORDEX domains very efficiently because it would need a lot of RAM. However, CAVAanalytics was mainly developed for country-level assessment and never conceived to be used for large-scale climate data analyses. This is where Python comes in. The regridded CORDEX-CORE models and all observational datasets used by CAVAanalytics can also be accessed using Python. **The benefit of using our data is that we provide one URL per model for all supported variables and that we have already regridded CORDEX-CORE models, making retrieving the data extremely easy**. See below an example:

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

### Issues

Youc can report issues [here](https://github.com/Risk-Team/CAVAanalytics/issues)
