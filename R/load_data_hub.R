#' Models upload (only works from the University of Cantabria Jupyter HUB environment)
#'
#' Automatically upload CORDEX-CORE models available at UC servers

#' @param path.to.data character, indicating the shared path to the data. Leave as default unless necessary
#' @param country character, in English, indicating the country of interest or an object of class sf. To select a bounding box,
#' set country to NULL and define arguments xlim and ylim
#' @param variable  character indicating the variable name
#' @param xlim numeric of length = 2, with minimum and maximum longitude coordinates, in decimal degrees, of the bounding box of interest
#' @param ylim same as xlim, but for the selection of the latitudinal range
#' @param path.to.obs character, default to NULL. To automatically load W5E5 or ERA5, specify W5E5 or ERA5.
#' @param years.proj numeric, specify year range for projections
#' @param years.hist numeric, specify year range for the historical experiment
#' @param years.obs NULL or numeric, specify year range for observation. Specifying years.obs will overwrite years.hist for observations
#' @param domain charachter, specify the CORDEX-CORE domain (e.g AFR-22, EAS-22).
#' @param buffer numeric, default is zero.
#' @param res_folder character, either interp05 or interp025. Default is interp05
#' @param aggr.m character, monthly aggregation. One of none, mean or sum
#' @param n.sessions numeric, number of sessions to use in parallel processing. Default to 6. Increasing the number of sessions will not necessarily results in better performances. Leave as default unless necessary
#' @return list of length 2. List[[1]] contains a tibble with list columns and List[[2]] the bbox
#' @importFrom magrittr %>%
#' @importFrom loadeR loadGridData
#'
#' @export


load_data_hub <-
  function(path.to.data = "/home/jovyan/shared/data",
           country,
           variable,
           xlim = NULL,
           ylim = NULL,
           years.hist,
           years.proj,
           buffer = 0,
           domain,
           aggr.m = "none",
           path.to.obs = NULL,
           years.obs = NULL,
           res_folder = "interp05",
           n.sessions = 6) {
    # intermediate functions --------------------------------------------------

    # check that the arguments have been correctly specified and return an error when not

    check_args <-
      function(years.hist,
               domain,
               years.proj,
               variable,
               aggr.m,
               path.to.obs,
               res_folder,
               years.obs) {
        match.arg(aggr.m, choices = c("none", "sum", "mean"))
        match.arg(res_folder, choices = c("interp05", "interp025"))
        if (!is.null(path.to.obs))
          match.arg(path.to.obs, choices = c("W5E5", "ERA5"))
        if (!is.null(domain)) {
          match.arg(
            domain,
            choices = c(
              "AFR-22",
              "CAS-22",
              "WAS-22",
              "SEA-22",
              "AUS-22",
              "EAS-22",
              "CAM-22",
              "SAM-22"
            )
          )
        }
         if (is.null(years.proj) |
            is.null(years.hist))
          cli::cli_abort(c("x" = "years.hist and years.proj needs to be specified"))

        if (missing(variable))
          cli::cli_abort(c("x" = "argument variable as no default"))

        if (!is.null(path.to.obs)) {
          if (!is.null(years.obs) & path.to.obs == "ERA5" &
              any(!(years.obs %in% 1976:2021)))
            cli::cli_abort(c("x" = "Available years for ERA5 observations are 1976:2021"))

          if (!is.null(years.obs) & path.to.obs == "W5E5" &
              any(!(years.obs %in% 1980:2021)))
            cli::cli_abort(c("x" = "Available years for W5E5 observations are 1980:2019"))

   
        }

        variable <-
          match.arg(variable,
                    choices = c("tas", "tasmax", "tasmin", "hurs", "rsds", "sfcWind", "pr"))
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
      years.hist,
      domain,
      years.proj,
      variable,
      aggr.m,
      path.to.obs,
      res_folder,
      years.obs
    )

    # geolocalization
    result <- geo_localize(country, xlim, ylim, buffer)
    xlim <- result$xlim
    ylim <- result$ylim

    # making the dataset
    start <-
      paste0(path.to.data,
             "/ncml/ESGF/",
             res_folder,
             "/CORDEX/output/",
             domain,
             "//")
    run <-
      c("GERICS/", ifelse(domain == "WAS-22", "ORNL/", "ICTP/"))
    GCMs <-
      c("MOHC-HadGEM2-ES/", "MPI-M-MPI-ESM-LR/", "NCC-NorESM1-M/")
    time_frame <- c("historical/", "rcp26/", "rcp85/")
    file_end <-
      c("r1i1p1//REMO2015//v1//day//",
        "r1i1p1//RegCM4-7//v0/day//")

    # building the dataset
    cli::cli_text(paste(Sys.time(), "Uploading data, this might take several minutes.."))

    files <- paste0(start, run) %>%
      purrr::map(.,
                 ~ paste0(.x, GCMs) %>%
                   purrr::map(., ~ paste0(.x, time_frame) %>%
                                purrr::map(., ~ paste0(.x, file_end)))) %>%
      unlist() %>%
      stringr::str_replace(
        .,
        "MPI-M-MPI-ESM-LR/",
        ifelse(
          stringr::str_detect(., "(ICTP)|(ORNL)"),
          "MPI-M-MPI-ESM-MR/",
          "MPI-M-MPI-ESM-LR/"
        )
      ) %>%
      stringr::str_replace(
        .,
        "MOHC-HadGEM2-ES/",
        ifelse(
          stringr::str_detect(., "ORNL"),
          "MIROC-MIROC5/",
          "MOHC-HadGEM2-ES/"
        )
      )  %>%
      list.files(., full.names = TRUE)

    future::plan(future::multisession, workers = n.sessions)
    models_df <-
      dplyr::tibble(
        path = unlist(files),
        experiment = stringr::str_extract(path, "rcp\\d+"),
        simulation = stringr::str_extract(path, "GERICS")
      ) %>%
      dplyr::mutate(
        experiment = ifelse(is.na(experiment), "historical", experiment),
        simulation = ifelse(is.na(simulation), "ICTP", simulation)
      ) %>%
      dplyr::mutate(climate_data = furrr::future_map(path,   function(x)  {
        if (stringr::str_detect(x, "historical"))
          suppressMessages(
            loadeR::loadGridData(
              dataset = x,
              var = variable,
              years = years.hist,
              lonLim = xlim,
              latLim = ylim,
              season = 1:12
            )    %>%
              {
                if (stringr::str_detect(variable, "tas")) {
                  suppressMessages(transformeR::gridArithmetics(., 273.15, operator = "-"))
                } else if (stringr::str_detect(variable, "pr")) {
                  suppressMessages(transformeR::gridArithmetics(., 86400, operator = "*"))

                } else {
                  .

                }


              }
          )

        else
          suppressMessages(
            loadeR::loadGridData(
              dataset = x,
              var = variable,
              years = years.proj,
              lonLim = xlim,
              latLim = ylim,
              season = 1:12
            )  %>%
              {
                if (stringr::str_detect(variable, "tas")) {
                  suppressMessages(transformeR::gridArithmetics(., 273.15, operator = "-"))
                } else if (stringr::str_detect(variable, "pr")) {
                  suppressMessages(transformeR::gridArithmetics(., 86400, operator = "*"))

                } else {
                  .

                }


              }
          )

      }))

    cli::cli_text(paste(Sys.time(), "Done"))

    cli::cli_text(
      paste0(
        Sys.time(),
        " Making multi-model ensemble with ",
        length(files) / 3,
        " members and loading ",
        path.to.obs,
        " dataset"
      )
    )

    options(warn = -1)

    models_df <- models_df %>%
      dplyr::group_by(experiment) %>%
      dplyr::summarise(models = list(climate_data)) %>%
      dplyr::mutate(models_mbrs = lapply(models, function(x)  {
        CAVAanalytics::common_dates(x)
      })) %>%
      dplyr::select(-models)

    if (!is.null(path.to.obs)) {
      path <-
        if (path.to.obs == "W5E5")
          paste0(path.to.data, "/observations/W5E5/v2.0/w5e5_v2.0.ncml")
      else
        paste0(path.to.data, "/observations/ERA5/0.25/ERA5_025.ncml")

      obs = list(suppressMessages(
        loadGridData(
          path,
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
          years = if (is.null(years.obs))
            years.hist
          else
            years.obs,
          lonLim = xlim,
          latLim = ylim,
          season = 1:12
        )    %>%
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
            } else {
              .

            }
          }
      ))

      models_df$obs <- obs

    }

    cli::cli_text(paste(Sys.time(), "Done"))

    # returning object
    invisible(structure(
      list(models_df, result$country_shp),
      class = "CAVAanalytics_list",
      components = list("data.frame with list columns", "bbox")
    ))

  }
