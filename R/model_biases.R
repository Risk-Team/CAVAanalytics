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
#' @param frequency logical. Used only when consecutive is TRUE and duration is not "max". For example, to know the number of heatwaves defined as the number of days with Tmax higher than 35 for at least 3 consecutive days, specify uppert=35, consecutive =T and duration=3, frequency=T
#' @param n.sessions numeric, number of sessions to use, default is one. Parallelization can be useful when multiple scenarios are used (RCPS, SSPs). However, note that parallelizing will increase RAM usage
#' @param method character, bias-correction method to use. One of eqm (Empirical Quantile Mapping), qdm (Quantile Delta Mapping) or scaling. Default to eqm. When using the scaling method, the multiplicative approach is automatically applied only when the variable is precipitation.
#' @param cross_validation character, one of none or 3fold. Whether 3-fold cross validation should be used to avoid overfitting during bias-correction. Default to "none"
#' @param window character, one of none or monthly. Whether bias correction should be applied on a monthly or annual basis. Monthly is the preferred option when performing bias-correction using daily data
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
           frequency = F,
           n.sessions = 1,
           duration = "max",
           method = "eqm",
           cross_validation = "none",
           window = "monthly") {
    # Intermediate functions --------------------------------------------------

    # check inputs requirement
    check_inputs <-
      function(data,
               bias.correction,
               uppert,
               lowert,
               consecutive,
               duration,
               season,
               method,
               cross_validation,
               window) {
        if (!is.list(season))
          cli::cli_abort("season needs to be a list, for example, list(1:3)")
        stopifnot(is.logical(consecutive), is.logical(bias.correction))
        if (!(duration == "max" || is.numeric(duration))) {
          cli::cli_abort("duration must be 'max' or a number")
        }
        if (!(method == "eqm" ||
              method == "qdm" || method == "scaling")) {
          cli::cli_abort("method must be one of 'eqm', 'qdm' or 'scaling'")
        }
        if (!(cross_validation == "none" ||
              cross_validation == "3fold")) {
          cli::cli_abort("cross_validation must be one of 'none', or '3fold'")
        }
        if (!(window == "none" || window == "monthly")) {
          cli::cli_abort("window must be one of 'none', or 'monthly'")
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

        if (bias.correction & window == "monthly") {
          dates <- data[[1]]$models_mbrs[[1]]$Dates$start
          dates <- as.Date(dates)
          # calculate the differences between consecutive dates
          diffs <- diff(dates)
          # check if the differences are equal to 1
          if (any(diffs == 1)) {

          } else {
            cli::cli_abort(c("x" = "Data is monthly or greater, set window to none"))
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
        if ((length(data[[1]]$obs[[1]]$xy$x) != length(data[[1]]$models_mbrs[[1]]$xy$x)) |
            (length(data[[1]]$obs[[1]]$xy$y) != length(data[[1]]$models_mbrs[[1]]$xy$y))) {
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
               duration,
               frequency) {
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
            ". Calculation of model biases for ",
            ifelse(frequency, "frequency " , "total number "),
            "of days with duration longer than ",
            duration,
            " consecutive days, ",
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
      lat <- datasets$models_mbrs[[1]]$xyCoords$y
      lat_obs <- datasets$obs[[1]]$xyCoords$y

      if ((length(lon) != length(lon_obs)) |
          (length(lat) != length(lat_obs))) {
        datasets <- datasets %>%
          dplyr::filter(experiment == "historical") %>%
          dplyr::mutate(models_mbrs = purrr::map(models_mbrs,
                                                 ~ suppressMessages(
                                                   suppressWarnings(
                                                     transformeR::interpGrid(.x, new.coordinates = transformeR::getGrid(obs[[1]]))
                                                   )
                                                 )))
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
               season,
               frequency,
               method,
               cross_validation,
               window) {
        season_name <-
          convert_vector_to_month_initials(season)
        data_list <- datasets %>%
          {
            if (bias.correction) {
              cli::cli_text(
                paste(
                  "{cli::symbol$arrow_right}",
                  " Performing ",
                  ifelse(window == "monthly", "monthly", ""),
                  " bias correction with the ",
                  method,
                  " method, for each model separately. This can take a while. Season",
                  glue::glue_collapse(season, "-"),
                  ifelse(
                    cross_validation == "3fold",
                    ". kfold cross-validation is applied to avoid overfitting",
                    ""
                  )
                )
              )
              dplyr::mutate(.,
                            models_mbrs = purrr::map(models_mbrs, function(mod) {
                              bc <-
                                suppressMessages(
                                  downscaleR::biasCorrection(
                                    y = obs[[1]],
                                    x = mod,
                                    scaling.type = if (var == "pr")
                                      "multiplicative"
                                    else
                                      "additive",
                                    precipitation = if (var == "pr") TRUE else FALSE,
                                    method = method,
                                    cross.val = if (cross_validation == "3fold")
                                      "kfold"
                                    else
                                      "none",
                                    folds = 3,
                                    window = if (window == "monthly")
                                      c(30, 30)
                                    else
                                      NULL,
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
                                                           uppert = uppert,
                                                           frequency = frequency
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
    check_inputs(
      data,
      bias.correction,
      uppert,
      lowert,
      consecutive,
      duration,
      season,
      method,
      cross_validation,
      window
    )
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
        create_message(var,
                       bias.correction,
                       uppert,
                       lowert,
                       consecutive,
                       duration,
                       frequency)

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
          season = sns,
          frequency,
          method,
          cross_validation,
          window
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
