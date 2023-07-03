#' Models upload
#'
#' Automatically load models (netCDF/NcML) in a tidy format with acess to state-of-the-art climate models and reanalysis datasets


#' @param path.to.data path to the directory containing the RCP/SSPs folders and historical simulations (optional). For example,
#' home/user/data/. data would contain subfolders with the climate/impact models. Historical simulations have to be contained in a folder called historical. If path.to.data is set as CORDEX-CORE, CORDEX-CORE simulations will be loaded
#' @param country character, in English, indicating the country of interest. To select a bounding box,
#' set country to NULL and define arguments xlim and ylim
#' @param variable  character indicating the variable name
#' @param xlim numeric of length = 2, with minimum and maximum longitude coordinates, in decimal degrees, of the bounding box of interest
#' @param ylim same as xlim, but for the selection of the latitudinal range
#' @param path.to.obs character, default to NULL. To automatically load W5E5 or ERA5, specify W5E5 or ERA5. Otherwise, indicate the absolute path to the directory containing another observational dataset
#' @param years.proj numeric, specify year range for projections
#' @param years.hist numeric, specify year range for the historical experiment
#' @param domain charachter, specify the CORDEX-CORE domain (e.g AFR-22, EAS-22). Used with path.to.data = CORDEX-CORE. Default is NULL
#' @param buffer numeric, default is zero.
#' @param aggr.m character, monthly aggregation. One of none, mean or sum
#' @param n.sessions numeric, number of sessions to use in parallel processing. Default to 6. Increasing the number of sessions will not necessarily results in better performances. Leave as default unless necessary
#' @return list of length 3. List[[1]] contains a tibble with list columns. List[[2]] the bbox and list[[3]] the temporal structure of the models
#' @importFrom loadeR loadGridData
#' @importFrom magrittr %>%
#' @examples
#' exmp <- load_data(country = "Togo", variable="tas", years.hist=2000, years.proj=2010,
#'               path.to.data = "CORDEX-CORE", domain="AFR-22")
#'
#' @export


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
           domain = NULL,
           aggr.m = "none",
           n.sessions = 6) {
    # intermediate functions --------------------------------------------------

    # check that the arguments have been correctly specified and return an error when not

    check_args <-
      function(path.to.data,
               years.hist,
               domain,
               years.proj,
               variable,
               aggr.m,
               n.sessions) {
        stopifnot(is.numeric(n.sessions))
        match.arg(aggr.m, choices = c("none", "sum", "mean"))
        if (missing(variable))
          cli::cli_abort(c("x" = "argument variable as no default"))
        if (path.to.data == "CORDEX-CORE") {
          if (is.null(domain))
            cli::cli_abort(c("x" = "domain has no default when uploading CORDEX-CORE data remotely"))
          if (is.null(years.proj) |
              is.null(years.hist))
            cli::cli_abort(
              c("x" = "specify both years.hist and years.proj when downloading CORDEX-CORE data")
            )

          if (any(!(years.hist %in% 1980:2005)))
            cli::cli_abort(c("x" = "Available years for the historical period are 1980:2005"))

          if (any(!(years.proj %in% 2006:2099)))
            cli::cli_abort(c("x" = "Available years for projections are 2006:2099"))

          variable <-
            match.arg(variable,
                      choices = c("tas", "tasmax", "tasmin", "hurs", "rsds", "sfcWind", "pr"))
        } else {
          if (!is.null(domain))
            cli::cli_alert_warning(c("!" = "Argument domain is ignored"))
          if (!stringr::str_detect(path.to.data, "/"))
            cli::cli_abort(c("x" = "please specify a valid path or CORDEX-CORE for remote upload"))
          if (!any(stringr::str_detect(list.files(path.to.data), "historical")) &
              is.null(years.hist)) {
            cli::cli_alert_warning(
              c("!" = "Historical experiment not found. If present, the folder needs to be named historical")
            )
          } else if (!any(stringr::str_detect(list.files(path.to.data), "historical")) &
                     !is.null(years.hist)) {
            cli::cli_abort(
              c("x" = "Historical experiment not found. The folder needs to be named historical")
            )
          } else if (any(stringr::str_detect(list.files(path.to.data), "historical")) &
                     is.null(years.hist)) {
            cli::cli_abort(c("x" = "Historical experiment found but years.hist is not specified"))
          } else {
            cli::cli_alert_info(c(
              "Your directory contains the following folders:\n",
              paste0(list.dirs(path.to.data)[-1], "\n")
            ))
          }
        }
      }

    # create the file names used later for the loadGridData function for remote upload

    load_cordex_data <- function(domain) {
      cli::cli_progress_step("Accessing inventory")
      csv_url <- "https://data.meteo.unican.es/inventory.csv"
      data <- read.csv(url(csv_url)) %>%
        dplyr::filter(stringr::str_detect(activity, "FAO"), domain ==  domain) %>%
        dplyr::group_by(experiment) %>%
        dplyr::summarise(path = list(as.character(location))) %>%
        dplyr::select(path)

      return(data[[1]])

    }

    # create the file names used later for the loadGridData function for local upload
    load_local_data <- function(path.to.data) {
      files <- list.dirs(path.to.data, full.names = TRUE)[-1] %>%
        purrr::map(., ~ list.files(.x, full.names = TRUE))

    }

    # create the file name used for loading the obs data. This can be the W5E5 dataset or satellite data
    load_obs_data <- function(path.to.obs) {
      if (path.to.obs == "ERA5") {
        "https://data.meteo.unican.es/thredds/dodsC/copernicus/cds/ERA5_0.25"
      } else if (path.to.obs == "W5E5") {
        "https://data.meteo.unican.es/thredds/dodsC/mirrors/W5E5/W5E5_v2"

      } else  {
        list.files(path.to.obs, full.names = TRUE)
      }

    }

    # return the xlim and ylim of a country of interest or BBox

    geo_localize <- function(country, xlim, ylim, buffer) {
      if (!is.null(country) & !is.null(xlim)) {
        cli::cli_abort(c("x" = "Either select a country or a region of interest, not both"))
      } else {
        country_shp = if (!is.null(country)) {
          rnaturalearth::ne_states(country = country, returnclass = "sf") %>%
          sf::st_set_crs(., NA)
        } else {
          sf::st_bbox(c(
            xmin = min(xlim),
            xmax = max(xlim),
            ymax = max(ylim),
            ymin = min(ylim)
          )) %>%
            sf::st_as_sfc() %>%
            data.frame(geometry = .) %>%
            sf::st_as_sf()
        }

        xlim <-
          c(sf::st_bbox(country_shp)[1] - buffer,
            sf::st_bbox(country_shp)[3] + buffer)
        ylim <-
          c(sf::st_bbox(country_shp)[2] - buffer,
            sf::st_bbox(country_shp)[4] + buffer)
        return(list(
          xlim = xlim,
          ylim = ylim,
          country_shp = country_shp
        ))
      }
    }

    # start -------------------------------------------------------------------

    # check for valid path
    check_args(path.to.data,
               years.hist,
               domain,
               years.proj,
               variable,
               aggr.m,
               n.sessions)

    # load data
    if (path.to.data == "CORDEX-CORE") {
      files <- load_cordex_data(domain)
      experiment <- c("historical", "rcp26", "rcp85")
    } else {
      files <- load_local_data(path.to.data)
      experiment <- list.dirs(path.to.data, full.names = F)[-1]
    }

    # load observation data
    obs.file <- if (!is.null(path.to.obs))
      load_obs_data(path.to.obs)

    # geolocalization
    result <- geo_localize(country, xlim, ylim, buffer)
    xlim <- result$xlim
    ylim <- result$ylim

    # making the dataset
    future::plan(future::multisession, workers = n.sessions)

    if (path.to.data == "CORDEX-CORE")
      cli::cli_progress_step(
        paste0(
          "Downloading CORDEX-CORE data (18 simulations). This might take a while. Using ",
          n.sessions,
          " sessions",
          ifelse(n.sessions == 6, " by default...", "...")
        )
      )
    else
      cli::cli_progress_step("Uploading local data...")


    models_df <-
      dplyr::tibble(path = files, experiment = experiment) %>%
      tidyr::unnest(cols = path) %>%
      dplyr::mutate(models = furrr::future_map(unlist(files), function(x)  {
        if (stringr::str_detect(x, "historical")) {
          data <- suppressMessages(
            loadGridData(
              dataset = x,
              var = variable,
              years = years.hist,
              lonLim = xlim,
              latLim = ylim,
              season = 1:12,
              aggr.m =  aggr.m
            )
          ) %>%
            {
              if (path.to.data == "CORDEX-CORE") {
                if (stringr::str_detect(variable, "tas")) {
                  transformeR::gridArithmetics(., 273.15, operator = "-")
                } else if (stringr::str_detect(variable, "pr")) {
                  transformeR::gridArithmetics(., 86400, operator = "*")

                }

              } else {
                .

              }

            }

          return(data)
        } else {
          data <- suppressMessages(
            loadGridData(
              dataset = x,
              var = variable,
              years = years.proj,
              lonLim = xlim,
              latLim = ylim,
              season = 1:12,
              aggr.m =  aggr.m
            )
          ) %>%
            {
              if (path.to.data == "CORDEX-CORE") {
                if (stringr::str_detect(variable, "tas")) {
                  transformeR::gridArithmetics(., 273.15, operator = "-")
                } else if (stringr::str_detect(variable, "pr")) {
                  transformeR::gridArithmetics(., 86400, operator = "*")

                }

              } else {
                .

              }

            }

          return(data)
        }

      })) %>%
      dplyr::group_by(experiment) %>%
      dplyr::summarise(models_mbrs = list(models))

    cli::cli_progress_done()

    size <- as.numeric(object.size(models_df))

    cli::cli_alert_info(
      if (path.to.data == "CORDEX-CORE")
        "Downloaded {prettyunits::pretty_bytes(size)}"
      else
        "Uploaded {prettyunits::pretty_bytes(size)}"
    )

    cli::cli_progress_step("Binding members and checking temporal consistency")


    models_df <- models_df %>%
      dplyr::mutate(models_mbrs = purrr::map(models_mbrs, common_dates)) %>%
      dplyr::mutate(obs = if (!is.null(path.to.obs)) {
        cli::cli_progress_done()
        cli::cli_progress_step(paste0(
          ifelse(
            path.to.obs %in% c("ERA5", "W5E5"),
            "Downloading ",
            "Uploading "
          ),
          path.to.obs
        ))
        out_obs <- list(suppressMessages(
          loadGridData(
            obs.file,
            var = if (path.to.obs == "ERA5")
              c(
                "pr" = "tp",
                "tasmax" = "t2mx",
                "tasmin" = "t2mn",
                "hurs" = "hurs",
                "sfcWind" = "sfcwind",
                "tas" = "t2m"
              )[variable]
            else
              variable,
            years = years.hist,
            lonLim = xlim,
            latLim = ylim,
            season = 1:12,
            aggr.m =  aggr.m
          )
        ) %>%
          {
            if (path.to.obs == "ERA5" | path.to.obs == "W5E5") {
              if (stringr::str_detect(variable, "tas")) {
                transformeR::gridArithmetics(., 273.15, operator = "-")
              } else if (stringr::str_detect(variable, "pr")) {
                transformeR::gridArithmetics(.,
                                             ifelse(path.to.obs == "ERA5", 1000, 86400),
                                             operator = "*")

              }

            } else {
              .

            }

          })
        cli::cli_progress_done()
        out_obs

      } else {
        NULL
      })

    cli::cli_progress_done()

    # conversion of units

    if (path.to.data == "CORDEX-CORE") {
      if (variable == "pr")  {
        cli::cli_alert_info("Precipitation has been converted into mm/day")
      }  else if (stringr::str_detect(variable, "tas")) {
        cli::cli_alert_info("Temperature has been converted into Celsius")
      }

    } else if (!is.null(path.to.obs)) {
      if (path.to.obs %in% c("W5E5", "ERA5")) {
        if (variable == "pr")  {
          cli::cli_alert_info(
            paste0(
              "Precipitation data from ",
              path.to.obs,
              " has been converted into mm/day"
            )
          )
        }  else if (stringr::str_detect(variable, "tas")) {
          cli::cli_alert_info(
            paste0(
              "Temperature data from ",
              path.to.obs,
              " has been converted into Celsius"
            )
          )
        }
      }
    }


    # Defining temporal resolution
    temp_res <- purrr::map(1:nrow(models_df), function(i) {
      dates <- models_df$models_mbrs[[i]]$Dates$start
      dates <- as.Date(dates)
      table(lubridate::year(dates), lubridate::month(dates))
    })

    names(temp_res) <- models_df$experiment

    # Checking temporal resolution
    differ_check <- lapply(1:nrow(models_df), function(n) {
      sapply(temp_res[[n]], function(x) {
        any(x != x[1])
      })
    }) %>% unlist()

    if (any(differ_check)) {
      cli::cli_bullets(c("!" = "There might be temporal inconsistency in your data, check .[[3]]"))
    }

    # returning object
    invisible(structure(
      list(models_df, result$country_shp, temp_res),
      class = "CAVAanalytics_list",
      components = list("data.frame with list columns", "bbox", "temporal resolution")
    ))

  }
