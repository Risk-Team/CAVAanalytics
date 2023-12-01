#' Visualization  of model biases
#'
#' Automatically compute the difference between observations and the historical experiment

#' @param data output of load_data
#' @param bias.correction logical
#' @param uppert  numeric of length=1, upper threshold
#' @param lowert numeric of length=1, lower threshold
#' @param season list, containing seasons to select. For example, list(1:6, 7:12)
#' @param consecutive logical, to use in conjunction with lowert or uppert
#' @param duration either "max" or specify a number. Used only when consecutive is TRUE. For example, to know the number of consecutive days with tmax above 35, lasting more than 3 days, specify uppert=35, consecutive =T and duration=3
#' @param n.sessions numeric, number of sessions to use, default is one. Parallelization can be useful when multiple scenarios are used (RCPS, SSPs). However, note that parallelizing will increase RAM usage
#' @importFrom magrittr %>%
#' @return list with SpatRaster. To explore the output run attributes(output)
#'
#' @export


model_biases <-
  function(data,
           bias.correction = F,
           uppert = NULL,
           lowert = NULL,
           season,
           consecutive = F,
           n.sessions = 1,
           duration = "max") {
    # Intermediate functions --------------------------------------------------

    # check inputs requirement
    check_inputs <-
      function(data,
               bias.correction,
               uppert,
               lowert,
               consecutive,
               duration,
               season) {
        if (!is.list(season))
          cli::cli_abort("season needs to be a list, for example, list(1:3)")
        stopifnot(is.logical(consecutive), is.logical(bias.correction))
        if (!(duration == "max" || is.numeric(duration))) {
          cli::cli_abort("duration must be 'max' or a number")
        }
        if (!is.null(lowert) &
            !is.null(uppert))
          cli::cli_abort(c("x" = "select only one threshold"))
        if (!is.null(lowert) |
            !is.null(uppert)) {
          dates <- data[[1]]$models_mbrs[[1]]$Dates$start
          dates <- as.Date(dates)
          # calculate the differences between consecutive dates
          diffs <- diff(dates)
          # check if the differences are equal to 1
          if (any(diffs == 1)) {

          } else {
            cli::cli_abort(c("x" = "Data is monthly or greater, thresholds cannot be calculated. Set as NULL"))
          }
        }

        if (consecutive &
            is.null(uppert) &
            is.null(lowert))
          cli::cli_abort("Specify a threshold for which you want to calculate consecutive days")
        if (!any(stringr::str_detect(colnames(data[[1]]), "obs")) |
            !any(stringr::str_detect(data[[1]]$experiment, "historical"))) {
          cli::cli_abort(
            c("x" = "This function requires an observational dataset and the historical experiment to calculate model biases")
          )

        }
        if (length(data[[1]]$obs[[1]]$xy$x) != length(data[[1]]$models_mbrs[[1]]$xy$x)) {
          cli::cli_alert_warning(
            "Observation and historical experiment do not have the same spatial resolution. Models will be interpolated to match the observational dataset"
          )
        }
      }

    # generate messages on the type of operations being performed
    create_message <-
      function(var,
               bias.correction,
               uppert,
               lowert,
               consecutive,
               duration) {
        if (is.null(uppert) & is.null(lowert)) {
          paste0(
            "Calculation of model biases for ",
            ifelse(var == "pr", "total ", "mean "),
            ifelse(bias.correction, "bias-corrected ", " "),
            var
          )
        }
        else if ((!is.null(uppert) |
                  !is.null(lowert)) & !consecutive) {
          paste0(
            "Calculation of model biases for number of days with ",
            var,
            ifelse(
              !is.null(lowert),
              paste0(" below threshold of ", lowert),
              paste0(" above threshold of ", uppert)
            ),
            ifelse(bias.correction, " after bias-correction", "")
          )
        }
        else if ((!is.null(uppert) |
                  !is.null(lowert)) &
                 (consecutive & duration == "max")) {
          paste0(
            "Calculation of model biases for maximum length of consecutive number of days ",
            ifelse(
              !is.null(lowert),
              paste0("below ", lowert),
              paste0("above ", uppert)
            ),
            ifelse(bias.correction, " after bias-correction", "")
          )
        }
        else if ((!is.null(uppert) |
                  !is.null(lowert)) &
                 (consecutive & is.numeric(duration))) {
          paste0(
            var,
            ". Calculation of model biases for total total number of consecutive days with duration longer than ", duration, " days, ",
            ifelse(
              !is.null(lowert),
              paste0("below threshold of ", lowert),
              paste0("above threshold of ", uppert)
            ),
            ifelse(bias.correction, " after bias-correction", "")
          )
        }
      }

    # subset based on a season of interest
    filter_data_by_season <- function(datasets, season) {
      if (all(season == sort(season))) {

      } else {
        cli::cli_alert_warning(
          "Some data will be lost on year-crossing season subset (see the 'Time slicing' section of subsetGrid documentation for more details)"
        )
      }

      lon <- datasets$models_mbrs[[1]]$xyCoords$x
      lon_obs <- datasets$obs[[1]]$xyCoords$x

      if (length(lon) != length(lon_obs)) {
        datasets <- datasets %>%
          dplyr::filter(experiment == "historical") %>%
          dplyr::mutate(models_mbrs = map(
            models_mbrs,
            ~ suppressMessages(
              transformeR::interpGrid(.x, new.coordinates = transformeR::getGrid(obs[[1]]))
            )
          ))
      } else {
        cli::cli_alert_warning(
          "Observations and model simulations have the same spatial resolution, proceeding with calculations"
        )
      }
      datasets %>%
        dplyr::filter(experiment == "historical") %>%
        dplyr::mutate_at(c("models_mbrs", "obs"),
                         ~ purrr::map(., ~ suppressMessages(
                           transformeR::subsetGrid(., season = season)
                         )))

    }


    # function used to perform the calculations

    perform_calculations <-
      function(datasets,
               mod.numb,
               var,
               bias.correction,
               uppert,
               lowert,
               consecutive,
               duration,
               country_shp,
               season) {
        season_name <-
          paste0(lubridate::month(season[[1]], label = T),
                 "-",
                 lubridate::month(season[[length(season)]], label = T))
        data_list <- datasets %>%
          {
            if (bias.correction) {
              cli::cli_text(
                paste(
                  "{cli::symbol$arrow_right}",
                  " Performing monthly bias correction with the empirical quantile mapping",
                  " method, for each model and month separately. This can take a while. Season",
                  glue::glue_collapse(season, "-")
                )
              )
              dplyr::mutate(.,
                            models_mbrs = purrr::map(models_mbrs, function(mod) {
                              bc <-
                                suppressMessages(
                                  downscaleR::biasCorrection(
                                    y = obs[[1]],
                                    x = mod,
                                    precipitation = ifelse(var == "pr", TRUE, FALSE),
                                    method = "eqm",
                                    window = if (any(diffs == 1))
                                      c(30, 30)
                                    else
                                      c(1, 1),
                                    cross.val = "kfold",
                                    folds = 2,
                                    extrapolation = "constant"
                                  )
                                )

                              mod_temp <-
                                transformeR::intersectGrid.time(mod, bc, which.return = 2)
                              mod_temp$Dates$start <-
                                mod$Dates$start
                              mod_temp$Dates$end <-  mod$Dates$end

                              return(mod_temp)
                            }, .progress = T))
            } else
              .
          }  %>%   # computing annual aggregation. if threshold is specified, first apply threshold
          dplyr::mutate_at(c("models_mbrs", "obs"),
                           ~ furrr::future_map(., ~
                                                 suppressMessages(
                                                   transformeR::aggregateGrid(# perform aggregation based on seasonended output
                                                     ., aggr.y =
                                                       if (var == "pr" &
                                                           !consecutive &
                                                           (is.null(uppert) &
                                                            is.null(lowert))) {
                                                         list(FUN = "sum", na.rm = TRUE)
                                                       } else if (var != "pr" &
                                                                  !consecutive &
                                                                  (is.null(lowert) &
                                                                   is.null(uppert))) {
                                                         list(FUN = "mean", na.rm = TRUE)
                                                       } else if (consecutive) {
                                                         list(
                                                           FUN = thrs_consec,
                                                           duration = duration,
                                                           lowert = lowert,
                                                           uppert = uppert
                                                         )
                                                       } else if (!consecutive) {
                                                         list(FUN = thrs,
                                                              uppert = uppert,
                                                              lowert = lowert)
                                                       })
                                                 ))) %>%
          dplyr::mutate(rst_obs = purrr::map(obs, function(data_obs) {
            rs <-
              make_raster(data_obs, if (length(data_obs$Dates$start) == 1)
                c(1, 2)
                else
                  c(2, 3), country_shp)
          })) %>%
          # individual models rasters
          dplyr::mutate(rst_models = purrr::map(models_mbrs, function(y) {
            rs_list <- purrr::map(1:dim(y$Data)[[1]], function(ens) {
              array_mean <-
                if (length(y$Dates$start) == 1)
                  apply(y$Data[ens, , ,], c(1, 2), mean, na.rm = TRUE)
              else
                apply(y$Data[ens, , ,], c(2, 3), mean, na.rm = TRUE) # climatology per member adjusting by array dimension

              y$Data <- array_mean

              rs <- make_raster(y, c(1, 2), country_shp)

              names(rs) <-
                paste0("Member ",
                       ens,
                       "_historical_",
                       names(rs),
                       "_",
                       season_name)
              return(rs)
            })
          })) %>%
          # biases
          dplyr::mutate(
            rst_mod_biases = purrr::map(rst_models, function(y) {
              biases <- terra::rast(y) - rst_obs[[1]]
              return(biases)
            }),
            rst_ens_biases = purrr::map(rst_mod_biases, function(y) {
              mean_biases <- terra::mean(y)
              names(mean_biases) <-
                stringr::str_remove(names(terra::rast(y))[[1]], "Member \\d+_")
              return(mean_biases)
            }),
            models_temp = purrr::map(models_mbrs, function(x) {
              dimnames(x$Data)[[1]] <- x$Members
              dimnames(x$Data)[[2]] <- x$Dates$start
              dimnames(x$Data)[[3]] <- x$xyCoords$y
              dimnames(x$Data)[[4]] <- x$xyCoords$x

              obs_data <- apply(obs[[1]]$Data, 1, mean, na.rm = T)
              obs_date <- as.Date(obs[[1]]$Dates$start)

              df_obs <-
                data.frame(obs_value = obs_data, date = obs_date)

              df <- suppressMessages(
                reshape2::melt(x$Data) %>%
                  dplyr::mutate(date = as.Date(Var2)) %>%
                  dplyr::mutate(experiment = "historical") %>%
                  dplyr::group_by(Var1, date) %>%
                  dplyr::summarise(value = mean(value)) %>%
                  dplyr::left_join(., df_obs) %>%
                  dplyr::mutate(season = season_name)
              )

              return(df)

            })
          )
        invisible(structure(
          list(
            data_list$rst_ens_biases[[1]],
            data_list$rst_mod_biases[[1]],
            data_list$models_temp[[1]]
          ),
          class = "CAVAanalytics_model_biases",
          components = list(
            "SpatRaster for ensemble biases",
            "SpatRaster for model biases",
            "data frame for temporal biases"
          )
        ))
      }

    # beginning of code -------------------------------------------------------
    if (class(data) != "CAVAanalytics_list")
      cli::cli_abort(c("x" = "The input data is not the output of CAVAanalytics load_data"))
    # check input requirements
    check_inputs(data,
                 bias.correction,
                 uppert,
                 lowert,
                 consecutive,
                 duration,
                 season)
    # retrieve information
    mod.numb <- dim(data[[1]]$models_mbrs[[1]]$Data) [1]
    datasets <- data[[1]]
    country_shp <- data[[2]]
    var <- datasets$models_mbrs[[1]]$Variable$varName
    dates <- datasets$models_mbrs[[1]]$Dates$start
    dates <- as.Date(dates)
    # calculate the differences between consecutive dates to understand temporal resolution and adjust the window argument in bias correction
    diffs <- diff(dates)


    #create plots by season
    data_list <- purrr::map(season, function(sns) {
      mes <-
        create_message(var, bias.correction, uppert, lowert, consecutive, duration)

      # set parallel processing
      future::plan(future::multisession, workers = n.sessions)

      # filter data by season
      datasets <- filter_data_by_season(datasets, season = sns)
      cli::cli_text(
        paste0(
          "{cli::symbol$arrow_right}",
          " Model biases, season ",
          glue::glue_collapse(sns, "-"),
          ". ",
          mes
        )
      )

      cli::cli_progress_step("Performing calculations")

      # perform calculations
      data_list <-
        perform_calculations(
          datasets,
          mod.numb,
          var,
          bias.correction,
          uppert,
          lowert,
          consecutive,
          duration,
          country_shp,
          season = sns
        )

      cli::cli_progress_done()
      # return results
      return(data_list)

    })

    invisible(structure(
      list(
        terra::rast(lapply(data_list, `[[`, 1)),
        terra::rast(lapply(data_list, `[[`, 2)),
        do.call(rbind, lapply(data_list, `[[`, 3))
      ),
      class = "CAVAanalytics_model_biases",
      components = list(
        "SpatRaster for ensemble biases",
        "SpatRaster for model biases",
        "data frame for temporal biases"
      )
    ))

  }
