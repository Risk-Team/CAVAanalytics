#' Climate models upload
#'
#' Automatically load climate models (netCDF/NcML) in a tidy format.
#' @export
#' @import stringr
#' @import purrr
#' @import furrr
#' @import future
#' @import dplyr
#' @import climate4R.UDG


#'
#' @param path.to.data Path to the directory containing the RCP/SSPs folders and historical simulations (optional). For example,
#' home/user/data/. data would contain subfolders with the climate/impact models. Historical simulations have to be contained in a folder called historical. If path.to.data is set as CORDEX-CORE, CORDEX-CORE simulations from RCM RegCM4 will be loaded
#' @param country A character string, in english, indicating the country of interest. To select a bounding box,
#' set country to NULL and define arguments xlim and ylim
#' @param variable  A character string indicating the variable
#' @param xlim Vector of length = 2, with minimum and maximum longitude coordinates, in decimal degrees, of the bounding box selected.
#' @param ylim Same as xlim, but for the selection of the latitudinal range.
#' @param path.to.obs Default to NULL, if not, indicate the absolute path to the directory containing a reanalysis dataset, for example ERA5. To automatically load W5E5. specify W5E5
#' @param years.proj Numerical range, years to select for projections
#' @param years.hist Numerical range, years to select for historical simulations and observations
#' @param n.cores Integer, number of cores to use in parallel processing, default is 3
#' @param domain Specify the CORDEX-CORE domain (e.g AFR-22, EAS-22). Used with path.to.data = CORDEX-CORE. Default is NULL
#' @param buffer Numeric. Default is zero.
#' @return Tibble with column list
#' @examples
#' fpath <- system.file("extdata/", package="cavaR")
#' exmp1 <- load_data(country = "Moldova", variable="hurs", years.hist=2000, years.proj=2010,
#'               path.to.data = fpath)
#' exmp2 <- load_data(country = "Somalia", variable="tas", years.hist=2000, years.proj=2010,
#'               path.to.data = "CORDEX-CORE", domain="AFR-22")


load_data <- function(
    path.to.data,
    country,
    variable,
    xlim=NULL,
    ylim=NULL,
    years.proj,
    years.hist=NULL,
    path.to.obs=NULL,
    n.cores=3,
    buffer=0,
    domain=NULL) {

  # stop and warnings ####

  if(path.to.data!="CORDEX-CORE" & stringr::str_detect(path.to.data, "/")) {

    if (!any(stringr::str_detect(list.files(path.to.data), "historical")) & is.null(years.hist)) {

      warning("Historical simulation rounds not found. If present, the folder needs to be named historical")
    } else if (!any(stringr::str_detect(list.files(path.to.data), "historical")) & !is.null(years.hist)) {

      stop("Historical simulation rounds not found. The folder needs to be named historical")

    } else {

      message("Your directory contains the following folders: \n", paste(list.dirs(path.to.data)[-1], "\n"), "all files within the listed folders will be uploaded \n")

      files <- list.dirs(path.to.data, full.names = TRUE)[-1] %>%
        map(., ~ list.files(.x, full.names = TRUE))

      forcing =list.dirs(path.to.data, full.names = F)[-1]

    }

  } else if (path.to.data=="CORDEX-CORE") {

    start <-  "CORDEX-"

    domain <-  domain

    GCM= c("_MOHC-HadGEM2-ES_", "_MPI-M-MPI-ESM-MR_", "_NCC-NorESM1-M_")

    forcing= c("historical", "rcp26", "rcp85")

    RCM= c("_r1i1p1_ICTP-RegCM4-7_v0" )

    files= purrr::map(forcing, ~ paste0(GCM, .x)) %>%
      purrr::map(., ~ paste0(start, domain, .x, RCM))

  } else {

    stop(" either specify a valid path or CORDEX-CORE for remote upload")

  }


  if (!is.null(path.to.obs)) {

    obs.file <- ifelse(path.to.obs=="W5E5", "W5E5", list.files(path.to.obs, full.names = TRUE) )

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

 future::plan(multisession, workers = n.cores)

  # making the dataset ####


  models.df = tibble(path= files, forcing=forcing) %>%
    dplyr::mutate(
      models = purrr::map(path,  ~ furrr::future_map(.x, function(x)  {
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
    dplyr::mutate(models_mbrs = lapply(models, function(x)
      common_dates(x))) %>%
    {if (!is.null(path.to.obs)) {
      dplyr::mutate(., obs = list(suppressMessages(
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

  return(list(models.df2, country_shp, as.character(models), "C4R.dataframe"))

} # end of function
