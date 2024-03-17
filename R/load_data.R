#' Models download/upload
#'
#' Automatically load models (netCDF/NcML) in a tidy format with access to state-of-the-art climate models and reanalyses datasets


#' @param path.to.data character (CORDEX-CORE or path to local data) or NULL. If path to local data, specify path to the directory containing the RCP/SSPs folders and historical simulations (optional). For example,
#' home/user/data/. data would contain subfolders with the climate/impact models. Historical simulations have to be contained in a folder called historical. If path.to.data is set as CORDEX-CORE, CORDEX-CORE simulations will be downloaded
#' @param country character, in English, indicating the country of interest or an object of class sf. To select a bounding box,
#' set country to NULL and define arguments xlim and ylim
#' @param variable  character indicating the variable name
#' @param xlim numeric of length = 2, with minimum and maximum longitude coordinates, in decimal degrees, of the bounding box of interest
#' @param ylim same as xlim, but for the selection of the latitudinal range
#' @param path.to.obs character, default to NULL. To automatically load W5E5 or ERA5, specify W5E5 or ERA5. Otherwise, indicate the absolute path to the directory containing another observational dataset
#' @param years.proj NULL or numeric, specify year range for projections
#' @param years.hist NULL or numeric, specify year range for the historical experiment
#' @param years.obs NULL or numeric, specify year range for observation. Specifying years.obs will overwrite years.hist for observations
#' @param domain charachter, specify the CORDEX-CORE domain (e.g AFR-22, EAS-22). Used with path.to.data = CORDEX-CORE. Default is NULL. List of domain names can be found at https://cordex.org/domains/
#' @param buffer numeric, default is zero.
#' @param aggr.m character, monthly aggregation. One of none, mean or sum
#' @param n.sessions numeric, number of sessions to use in parallel processing. Default to 6. Increasing the number of sessions will not necessarily results in better performances. Leave as default unless necessary
#' @return list of length 3. List[[1]] contains a tibble with list columns. List[[2]] the bbox and list[[3]] the temporal structure of the models
#' @importFrom magrittr %>%
#'
#' @export


load_data <-
  function(path.to.data,
           country,
           variable,
           xlim = NULL,
           ylim = NULL,
           years.hist = NULL,
           years.proj,
           path.to.obs = NULL,
           buffer = 0,
           domain = NULL,
           aggr.m = "none",
           n.sessions = 6,
           years.obs = NULL) {
    # intermediate functions --------------------------------------------------

    # check that the arguments have been correctly specified and return an error when not

    check_args <-
      function(path.to.data,
               years.hist,
               domain,
               years.proj,
               variable,
               aggr.m,
               n.sessions,
               path.to.obs,
               years.obs) {
        stopifnot(is.numeric(n.sessions))
        match.arg(aggr.m, choices = c("none", "sum", "mean"))
        if (!is.null(domain)) {
          match.arg(
            domain,
            choices = c(
              "AFR-22",
              "SEA-22",
              "AUS-22",
              "EAS-22",
              "CAM-22",
              "SAM-22",
              "WAS-22",
              "CAM-22",
              "SAM-22",
              "NAM-22",
              "EUR-22",
              "CAS-22"
            )
          )

        }
        if (is.null(years.proj) &
            is.null(years.hist) & is.null(years.obs))
          cli::cli_abort(c("x" = "select at least one of years.hist, years.proj or years.obs"))

        if (missing(variable))
          cli::cli_abort(c("x" = "argument variable as no default"))

        if (!is.null(path.to.obs)) {
          if (!is.null(years.obs) & path.to.obs == "ERA5" &
              any(!(years.obs %in% 1976:2021)))
            cli::cli_abort(c("x" = "Available years for ERA5 observations are 1976:2021"))

          if (!is.null(years.obs) & path.to.obs == "W5E5" &
              any(!(years.obs %in% 1980:2021)))
            cli::cli_abort(c("x" = "Available years for W5E5 observations are 1980:2019"))

          if (is.null(years.obs) & is.null(years.hist))
            cli::cli_abort(c("x" = "Specify years.hist or years.obs since path.obs is not NULL"))

        }

        if (!is.null(path.to.data) &&
            path.to.data == "CORDEX-CORE") {
          if (is.null(domain))
            cli::cli_abort(c("x" = "domain has no default when uploading CORDEX-CORE data remotely"))
          if (is.null(years.proj) &
              is.null(years.hist) & !is.null(years.obs))
            cli::cli_abort(c("x" = "set path.to.data as NULL to only upload observations"))

          if (!is.null(years.hist) & !is.null(years.obs))
            cli::cli_alert_warning(c("!" = "years.obs overwrite years.hist for the observational dataset"))

          if (is.null(years.hist) &
              !is.null(years.obs) & !is.null(years.proj))
            cli::cli_abort(
              c("x" = "If you are loading observations and projections, also specify an historical period")
            )

          if (any(!(years.hist %in% 1976:2005)))
            cli::cli_abort(c("x" = "Available years for the CORDEX historical simulation are 1976:2005"))


          if (any(!(years.proj %in% 2006:2099)))
            cli::cli_abort(c("x" = "Available years for projections are 2006:2099"))

          variable <-
            match.arg(variable,
                      choices = c("tas", "tasmax", "tasmin", "hurs", "rsds", "sfcWind", "pr"))

        } else if (is.null(path.to.data)) {
          cli::cli_alert_warning(c("!" = "Only observations will be uploaded"))
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

    load_cordex_data <- function(domains) {
      cli::cli_progress_step("Accessing inventory")
      csv_url <- "https://data.meteo.unican.es/inventory.csv"
      data <- read.csv(url(csv_url)) %>%
        dplyr::filter(stringr::str_detect(activity, "FAO"), domain ==  domains) %>%
        dplyr::group_by(experiment) %>%
        dplyr::summarise(path = list(as.character(location))) %>%
        {
          if (is.null(years.hist) & !is.null(years.proj)) {
            dplyr::filter(., experiment != "historical")

          } else if (!is.null(years.hist) & is.null(years.proj)) {
            dplyr::filter(., experiment == "historical")
          } else {
            .
          }
        } %>%
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
        country_shp = if (!is.null(country) & !inherits(country, "sf")) {
          suppressMessages(
            rnaturalearth::ne_countries(
              country = country,
              scale = "medium",
              returnclass = "sf"
            ) %>%
              sf::st_set_crs(., NA)
          )
        } else if (!is.null(country) & inherits(country, "sf")) {
          country %>%
            sf::st_transform("EPSG:4326")

        } else {
          sf::st_bbox(c(
            xmin = min(xlim),
            xmax = max(xlim),
            ymax = max(ylim),
            ymin = min(ylim)
          )) %>%
            sf::st_as_sfc() %>%
            data.frame(geometry = .) %>%
            sf::st_as_sf() %>%
            sf::st_set_crs(., NA)
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
    check_args(
      path.to.data,
      years.hist,
      domain,
      years.proj,
      variable,
      aggr.m,
      n.sessions,
      path.to.obs,
      years.obs
    )

    # load data
    if (is.null(path.to.data)) {

    } else if (path.to.data == "CORDEX-CORE") {
      files <- load_cordex_data(domains = domain)
      experiment <-
        if (!is.null(years.hist) & !is.null(years.proj))
          c("historical", "rcp26", "rcp85")
      else if (is.null(years.hist) & !is.null(years.proj))
        c("rcp26", "rcp85")
      else
        "historical"
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
    if (is.null(path.to.data)) {

    } else if (path.to.data == "CORDEX-CORE") {
      cli::cli_progress_step(
        paste0(
          "Downloading CORDEX-CORE data (" ,
          ifelse(is.null(years.hist), 12, ifelse(is.null(years.proj), 6, 18)),
          " simulations)",
          " using ",
          n.sessions,
          " sessions",
          ifelse(n.sessions == 6, " by default", "")
        )
      )
    }  else {
      cli::cli_progress_step("Uploading local data...")
    }

    if (!is.null(path.to.data)) {
      # when observations only are to be loaded or downloaded

      models_df <-
        dplyr::tibble(path = files, experiment = experiment) %>%
        tidyr::unnest(cols = path) %>%
        dplyr::mutate(models = suppressWarnings(furrr::future_map(unlist(files), function(x)  {
          if (stringr::str_detect(x, "historical")) {
            data <-
              suppressMessages(
                loadeR::loadGridData(
                  dataset = x,
                  var = variable,
                  years = years.hist,
                  lonLim = xlim,
                  latLim = ylim,
                  aggr.m =  aggr.m
                )
              ) %>%
              {
                if (path.to.data == "CORDEX-CORE") {
                  if (stringr::str_detect(variable, "tas")) {
                    suppressMessages(transformeR::gridArithmetics(., 273.15, operator = "-"))
                  } else if (stringr::str_detect(variable, "pr")) {
                    suppressMessages(transformeR::gridArithmetics(., 86400, operator = "*"))

                  } else {
                    .

                  }

                } else {
                  .

                }

              }

            return(data)
          } else {
            data <-
              suppressMessages(
                loadeR::loadGridData(
                  dataset = x,
                  var = variable,
                  years = years.proj,
                  lonLim = xlim,
                  latLim = ylim,
                  aggr.m =  aggr.m
                )
              ) %>%
              {
                if (path.to.data == "CORDEX-CORE") {
                  if (stringr::str_detect(variable, "tas")) {
                    suppressMessages(transformeR::gridArithmetics(., 273.15, operator = "-"))
                  } else if (stringr::str_detect(variable, "pr")) {
                    suppressMessages(transformeR::gridArithmetics(., 86400, operator = "*"))

                  } else {
                    .

                  }

                } else {
                  .

                }

              }

            return(data)
          }

        }))) %>%
        dplyr::group_by(experiment) %>%
        dplyr::summarise(models_mbrs = list(models))

      cli::cli_progress_done()
      size <- as.numeric(object.size(models_df))
      cli::cli_text(
        if (path.to.data == "CORDEX-CORE")
          "{cli::symbol$arrow_right} Downloaded {prettyunits::pretty_bytes(size)}"
        else
          "{cli::symbol$arrow_right} Uploaded {prettyunits::pretty_bytes(size)}"
      )

      cli::cli_progress_step("Making multi-model ensemble and checking temporal consistency")

      models_df <- models_df %>%
        dplyr::mutate(models_mbrs = purrr::map(models_mbrs, ~ common_dates(.x)))

      cli::cli_progress_done()

    } else {
      # when path.to.data is NULL and only observations are needed

      models_df <-
        dplyr::tibble(obs = NA) # empty tibble to add obs later

    }

    # Load obs data outside of mutate
    if (!is.null(path.to.obs)) {
      cli::cli_progress_step(paste0(
        ifelse(
          path.to.obs %in% c("ERA5", "W5E5"),
          "Downloading ",
          "Uploading "
        ),
        path.to.obs
      ))

      out_obs <- suppressMessages(list(
        loadeR::loadGridData(
          obs.file,
          var = if (path.to.obs == "ERA5")
            c(
              "pr" = "tp",
              "tasmax" = "t2mx",
              "tasmin" = "t2mn",
              "hurs" = "hurs",
              "sfcWind" = "sfcwind",
              "tas" = "t2m",
              "rsds" = "ssrd"
            )[variable]
          else
            variable,
          years = if (!is.null(years.obs))
            years.obs
          else
            years.hist,
          lonLim = xlim,
          latLim = ylim,
          aggr.m = aggr.m
        )
        %>%
          {
            if (path.to.obs == "ERA5" | path.to.obs == "W5E5") {
              if (stringr::str_detect(variable, "tas")) {
                obs_tr <- transformeR::gridArithmetics(., 273.15, operator = "-")
                obs_tr$Variable$varName = variable
                obs_tr
              } else if (stringr::str_detect(variable, "pr")) {
                obs_tr <- transformeR::gridArithmetics(.,
                                                       ifelse(path.to.obs == "ERA5", 1000, 86400),
                                                       operator = "*")
                obs_tr$Variable$varName = variable
                obs_tr
              } else if (stringr::str_detect(variable, "rsds")) {
                obs_tr <- transformeR::gridArithmetics(.,
                                                       ifelse(path.to.obs == "ERA5", 86400, 1),
                                                       operator = "/")
                obs_tr$Variable$varName = variable
                obs_tr
              } else {
                obs_tr <- transformeR::gridArithmetics(., 1, operator = "*")
                obs_tr$Variable$varName = variable
                obs_tr
              }
            }
          }
      ))

      cli::cli_progress_done()

    }

    # Add obs to models_df if loaded

    if (!is.null(path.to.obs)) {
      models_df$obs <- out_obs

    } else {
      models_df$obs <- NULL
    }


    # Conversion of units messages
    if (!is.null(path.to.data)) {
      if (path.to.data == "CORDEX-CORE") {
        if (variable == "pr")  {
          cli::cli_text(
            "{cli::symbol$arrow_right} Precipitation data from CORDEX-CORE has been converted into mm/day"
          )
        }  else if (stringr::str_detect(variable, "tas")) {
          cli::cli_text(
            "{cli::symbol$arrow_right} Temperature data from CORDEX-CORE has been converted into Celsius"
          )
        }

      }
    }

    if (!is.null(path.to.obs)) {
      if (path.to.obs %in% c("W5E5", "ERA5")) {
        if (variable == "pr")  {
          cli::cli_text(
            paste0(
              "{cli::symbol$arrow_right}",
              " Precipitation data from ",
              path.to.obs,
              " has been converted into mm/day"
            )
          )
        }  else if (stringr::str_detect(variable, "tas")) {
          cli::cli_text(
            paste0(
              "{cli::symbol$arrow_right}",
              " Temperature data from ",
              path.to.obs,
              " has been converted into Celsius"
            )
          )
        }
      }
    }

    if (ncol(models_df) > 1) {
      # when models are loaded
      # check temporal resolutions only when models are uploaded/downloded.
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
    } else {
      # no need to check temporal consistency as only observations are loaded
      temp_res <- NULL
    }
    # returning object
    invisible(structure(
      list(models_df, result$country_shp, temp_res),
      class = "CAVAanalytics_list",
      components = list("data.frame with list columns", "bbox", "temporal resolution")
    ))

  }
