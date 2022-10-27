#' Climate models upload
#'
#' Automatically load climate models (netCDF/NcML) in a tidy format.
#' @export
#' @import stringr
#' @import purrr
#' @import furrr
#' @import dplyr


#'
#' @param path.to.rcps Path to the directory containing the RCPs/SSPs folders and historical simulations. For example,
#' home/user/data/. data would contain subfolders with the climate models. Historical simulations have to be contained in a folder called historical. If path.to.rcps is set as CORDEX-CORE, CORDEX-CORE simulations from RCM RegCM4 will be loaded
#' @param country A character string, in english, indicating the country of interest. To select a bounding box,
#' set country to NULL and define arguments xlim and ylim
#' @param variable  A character string indicating the variable
#' @param xlim Vector of length = 2, with minimum and maximum longitude coordinates, in decimal degrees, of the bounding box selected.
#' @param ylim Same as xlim, but for the selection of the latitudinal range.
#' @param path.to.obs Default to NULL, if not, indicate the absolute path to the directory containing a reanalysis dataset, for example ERA5. To automatically load W5E5. specify W5E5
#' @param years.proj Numerical range, years to select for projections
#' @param years.hist Numerical range, years to select for historical simulations and observations
#' @param n.cores Integer, number of cores to use in parallel processing, default is 9
#' @param domain Specify the CORDEX-CORE domain (e.g AFR-22, EAS-22). Used with path.to.rcps = CORDEX-CORE
#' @param buffer Numeric. Default is zero.
#' @return Tibble with column list
#' @examples
#' fpath <- system.file("extdata/", package="cavaR")
#' exmp1 <- load_data(country = "Moldova", variable="hurs", years.hist=2000. years.proj=2010,
#'               path.to.rcps = fpath)
#' exmp2 <- load_data(country = "Somalia", variable="tas", years.hist=2000. years.proj=2010,
#'               path.to.rcps = "CORDEX-CORE", domain="AFR-22")


load_data <- function(
    path.to.rcps,
    country,
    variable,
    xlim=NULL,
    ylim=NULL,
    years.proj,
    years.hist,
    path.to.obs=NULL,
    n.cores=NULL,
    buffer=0,
    domain=NULL) {

  # stop and warnings ####

  if(path.to.rcps!="CORDEX-CORE" & str_detect(path.to.rcps, "/")) {

    if (!any(str_detect(list.files(path.to.rcps), "stor"))) {

      stop("Please add the historical simulations rounds of your model. The folder name needs to contain at least the letters stor")
    }

    if (length(list.files(path.to.rcps)) >= 2) message("Your directory contains the following folders: \n", paste(list.dirs(path.to.rcps)[-1], "\n"), "all files within the listed folders will be uploaded \n")

    files <- list.dirs(path.to.rcps, full.names = TRUE)[-1] %>%
      map(., ~ list.files(.x, full.names = TRUE))

    RCP =list.dirs(path.to.rcps, full.names = F)[-1]

  } else if (path.to.rcps=="CORDEX-CORE") {

    start <-  "CORDEX-"

    domain <-  domain

    GCM= c("_MOHC-HadGEM2-ES_", "_MPI-M-MPI-ESM-MR_", "_NCC-NorESM1-M_")

    RCP= c("historical", "rcp26", "rcp85")

    RCM= c("_r1i1p1_ICTP-RegCM4-7_v0" )

    files= map(RCP, ~ paste0(GCM, .x)) %>%
      map(., ~ paste0(start, domain, .x, RCM))

  } else {

    stop(" either specify a valid path or CORDEX-CORE for remote upload")

  }


  if (!is.null(path.to.obs)) {

    obs.file <- ifelse(path.to.obs=="W5E5", "W5E5", list.files(path.to.obs, full.names = TRUE) )

  }  else {

    warning("if you do not specify a reanalysis/observational gridded dataset, bias-correction cannot be performed. To load W5E5 set path.to.obs as W5E5 \n")

  }

  # geolocalization ####

  options(warn=-1)

  if (!is.null(country) & !is.null(xlim)) {
    stop("Either select a country or a region of interest, not both")
  } else {
    country_shp = if (!is.null(country))
      raster::getData("GADM", country = country, level = 1)
    else
      as(extent(min(xlim), max(xlim), min(ylim), max(ylim)), "SpatialPolygons")
    raster::crs(country_shp) = sp::CRS("+init=epsg:4326")
    xlim <-
      c(round(country_shp@bbox[1, 1] - buffer),
        round(country_shp@bbox[1, 2] + buffer))  # longitude boundaries for the region of seasonerest
    ylim <-
      c(round(country_shp@bbox[2, 1] - buffer),
        # latitude boundaries for the region of seasonerest
        round(country_shp@bbox[2, 2] + buffer))
  }

  range.x <- max(xlim) - min(xlim)
  range.y <-  max(ylim) - min(ylim)

  options(warn=1)



  # number of cores ####

  if (is.null(n.cores)) {

    future::plan(
      list(
        future::tweak(
          future::multisession,
          workers = 3),
        future::tweak(
          future::multisession,
          workers = 3),
        future::tweak(
          future::multisession,
          workers = 3)
      )
    )

    answer <- readline("The process is currently parallelized using 9 cores. Type TRUE to continue or set the argument n.cores \n")

    ifelse(answer, " ", stop("set number of cores"))

  }  else {


    future::plan(multisession, workers = n.cores)


  }
  # making the dataset ####


  models.df = tibble(path= files, RCP=RCP) %>%
    mutate(
      models = future_map(path,  ~ future_map(.x, function(x)  {
        if (str_detect(x, "historical")) {
          message(Sys.time(), " Loading ", x)
          data <- suppressMessages(loadGridData(
            dataset = x,
            var = variable,
            years = years.hist,
            lonLim = xlim,
            latLim = ylim,
            season = 1:12
          ))
          message(Sys.time(), " Done")
          return(data)
        } else {
          message(Sys.time(), " Loading ", x)
          data <- suppressMessages(loadGridData(
            dataset = x,
            var = variable,
            years = years.proj,
            lonLim = xlim,
            latLim = ylim,
            season = 1:12
          ))
          message(Sys.time(), " Done")
          return(data)
        }

      })))

  message(paste("\n", Sys.time(), "Aggregating members \n"))

  models.df2 <- models.df %>%
    mutate(models_mbrs = lapply(models, function(x)
      common_dates(x))) %>%
    {if (!is.null(path.to.obs)) {
      mutate(., obs = list(suppressMessages(
        loadGridData(
          obs.file,
          var = variable,
          years = years.hist,
          lonLim = xlim,
          latLim = ylim,
          season = 1:12
        )
      )))

    } else {.}} %>%
    dplyr::select(-models, -path)

  models <- models.df %>%
    dplyr::select(path)

  message(paste(Sys.time(), "Done"))
  rm(models.df)
  gc()

  return(list(models.df2, country_shp, models, "C4R.dataframe"))

} # end of function
