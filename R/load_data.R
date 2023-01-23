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
#' @param domain Specify the CORDEX-CORE domain (e.g AFR-22, EAS-22). Used with path.to.data = CORDEX-CORE. Default is NULL
#' @param buffer Numeric. Default is zero.
#' @return Tibble with column list
#' @examples
#' fpath <- system.file("extdata/", package="cavaR")
#' exmp1 <- load_data(country = "Moldova", variable="hurs", years.hist=2000, years.proj=2010,
#'               path.to.data = fpath)
#' exmp2 <- load_data(country = "Somalia", variable="tas", years.hist=2000, years.proj=2010,
#'               path.to.data = "CORDEX-CORE", domain="AFR-22")





load_data <-
  function(path.to.data,
           country,
           variable,
           xlim = NULL,
           ylim = NULL,
           years.proj,
           years.hist = NULL,
           path.to.obs = NULL,
           buffer = 0,
           domain = NULL) {

    # intermediate functions --------------------------------------------------

    # check that the arguments have been correctly specified and return an error when not

    check_path <- function(path.to.data, years.hist, domain, years.proj, variable) {
      if(missing(variable)) stop("Error: missing variable")

      if(path.to.data == "CORDEX-CORE") {
        if(is.null(domain)) stop("Error: domain is not specified when uploading CORDEX-CORE data remotely")
        if(is.null(years.proj) | is.null(years.hist)) stop("Error: specify both years.hist and years.proj when uploading CORDEX-CORE data remotely")
      } else {
        if(!stringr::str_detect(path.to.data, "/")) stop("Error: please specify a valid path or CORDEX-CORE for remote upload")
        if(!any(stringr::str_detect(list.files(path.to.data), "historical")) & is.null(years.hist)) {
          warning("Warning: Historical simulation rounds not found. If present, the folder needs to be named historical")
        } else if(!any(stringr::str_detect(list.files(path.to.data), "historical")) & !is.null(years.hist)) {
          stop("Error: Historical simulation rounds not found. The folder needs to be named historical")
        } else if(any(stringr::str_detect(list.files(path.to.data), "historical")) & is.null(years.hist)) {
          stop("Error: Historical simulation rounds found but years.hist is not specified")
        } else {
          message("\nYour directory contains the following folders:\n", paste(list.dirs(path.to.data)[-1], "\n"), "all files within the listed folders will be uploaded\n")
        }
      }
    }

    # create the file names used later for the loadGridData function for remote upload

    load_cordex_data <- function(domain) {
      start <-  "CORDEX-"
      domain <-  domain
      GCM = c("_MOHC-HadGEM2-ES_",
              "_MPI-M-MPI-ESM-MR_",
              "_NCC-NorESM1-M_")
      forcing = c("historical", "rcp26", "rcp85")
      RCM = c("_r1i1p1_ICTP-RegCM4-7_v0")
      purrr::map(forcing, ~ paste0(GCM, .x)) %>%
        purrr::map(., ~ paste0(start, domain, .x, RCM))
    }

    # create the file names used later for the loadGridData function for local upload
    load_local_data <- function(path.to.data) {
      files <- list.dirs(path.to.data, full.names = TRUE)[-1] %>%
        purrr::map(., ~ list.files(.x, full.names = TRUE))

    }

    # create the file name used for loading the obs data. This can be the W5E5 dataset or satellite data
    load_obs_data <- function(path.to.obs) {
      ifelse(path.to.obs == "W5E5",
             "W5E5",
             list.files(path.to.obs, full.names = TRUE))

    }

    # return the xlim and ylim of a country of interest or BBox

    geo_localize <- function(country, xlim, ylim, buffer) {
      if (!is.null(country) & !is.null(xlim)) {
        stop("Either select a country or a region of interest, not both")
      } else {
        country_shp = if (!is.null(country)) {
          raster::getData("GADM", country = country, level = 1)
        } else {
          as(extent(min(xlim), max(xlim), min(ylim), max(ylim)), "SpatialPolygons")
        }
        raster::crs(country_shp) = sp::CRS("+init=epsg:4326")
        xlim <-
          c(round(country_shp@bbox[1, 1] - buffer),
            round(country_shp@bbox[1, 2] + buffer))
        ylim <-
          c(round(country_shp@bbox[2, 1] - buffer),
            round(country_shp@bbox[2, 2] + buffer))
        return(list(
          xlim = xlim,
          ylim = ylim,
          country_shp = country_shp
        ))
      }
    }


# start -------------------------------------------------------------------

    # check for valid path
    check_path(path.to.data, years.hist, domain, years.proj, variable)

    # load data
    if (path.to.data == "CORDEX-CORE") {
      files <- load_cordex_data(domain)
      forcing <- c("historical", "rcp26", "rcp85")
    } else {
      files <- load_local_data(path.to.data)
      forcing <- list.dirs(path.to.data, full.names = F)[-1]
    }

    # load observation data
    obs.file <- if (!is.null(path.to.obs))
      load_obs_data(path.to.obs)

    # geolocalization
    result <- geo_localize(country, xlim, ylim, buffer)
    xlim <- result$xlim
    ylim <- result$ylim

    # making the dataset
    future::plan(multisession, workers = 3)

    if (path.to.data == "CORDEX-CORE")
      message(Sys.time(),
              " Retrieving CORDEX-CORE data, RCM RegCM4. This may take a while... ")
    else
      message(Sys.time())

    models.df = tibble(path = files, forcing = forcing) %>%
      dplyr::mutate(models = purrr::map(path,  ~ furrr::future_map(.x, function(x)  {
        if (str_detect(x, "historical")) {
          message("\n", Sys.time(), " Loading ", x)
          data <- suppressMessages(
            loadGridData(
              dataset = x,
              var = variable,
              years = years.hist,
              lonLim = xlim,
              latLim = ylim,
              season = 1:12
            )
          )
          message(Sys.time(), " Done")
          return(data)
        } else {
          message("\n", Sys.time(), " Loading ", x)
          data <- suppressMessages(
            loadGridData(
              dataset = x,
              var = variable,
              years = years.proj,
              lonLim = xlim,
              latLim = ylim,
              season = 1:12
            )
          )
          message(Sys.time(), " Done")
          return(data)
        }

      }, .progress = T)))

    message("\n", Sys.time(), " Aggregating members \n")

    # aggregating members and adding obs data if specified
    models.df2 <- models.df %>%
      dplyr::mutate(models_mbrs = purrr::map(models, common_dates)) %>%
      dplyr::mutate(obs = if (!is.null(path.to.obs)) {
        message("\n", Sys.time(), " Uploading obs data")
        list(suppressMessages(
          loadGridData(
            obs.file,
            var = variable,
            years = years.hist,
            lonLim = xlim,
            latLim = ylim,
            season = 1:12
          )
        ))
      }

      else {
        NULL
      }) %>%
      dplyr::select(-models, -path)

    models <- models.df %>%
      dplyr::select(path)

    message("\n", Sys.time(), " Done \n")

    invisible(structure(
      list(models.df2, result$country_shp, as.character(models)),
      class = "cavaR_list",
      components = list("data.frame with list columns", "bbox", "vector of model names")
    ))

  }


