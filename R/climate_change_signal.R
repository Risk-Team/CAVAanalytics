#' Calculation of climate change signal
#'
#' Automatically computes climate change signal and agreement in the sign of change

#' @param data output of load_data
#' @param uppert  numeric of length=1, upper threshold
#' @param lowert numeric of length=1, lower threshold
#' @param season list, containing seasons to select. For example, list(1:6, 7:12)
#' @param consecutive logical, to use in conjunction with lowert or uppert
#' @param duration parameter that can be set to either "max" or a specific number. It is relevant only when 'consecutive' is set to TRUE. For instance, to calculate the count of consecutive days with Tmax (maximum temperature) above 35°C, lasting for more than 3 days, you can set 'uppert' to 35, 'consecutive' to TRUE, and 'duration' to 3.
#' @param frequency logical value. This parameter is relevant only when 'consecutive' is set to TRUE and 'duration' is not set to "max". For instance, if you want to determine the count of heatwaves, defined as the number of days with Tmax (maximum temperature) exceeding 35°C for a minimum of 3 consecutive days, set 'uppert' to 35, 'consecutive' to TRUE, 'duration' to 3, and 'frequency' to TRUE.
#' @param bias.correction logical
#' @param threshold numerical value with range 0-1. It indicates the threshold for assigning model agreement. For example, 0.6 indicates that model agreement is assigned when 60 percent of the models agree in the sign of the change
#' @param n.sessions numeric, number of sessions to use, default is one. Parallelisation can be useful when multiple scenarios are used (RCPS, SSPs). However, note that parallelising will increase RAM usage
#' @importFrom magrittr %>%
#' @return list with SpatRaster. To explore the output run attributes(output)
#'
#' @export

climate_change_signal <- function(data,
                                  uppert = NULL,
                                  lowert = NULL,
                                  season,
                                  consecutive = F,
                                  duration = "max",
                                  frequency = F,
                                  bias.correction = F,
                                  threshold = 0.6,
                                  n.sessions = 1) {
  # Intermediate functions --------------------------------------------------

  # check inputs requirement
  check_inputs <-
    function(data,
             uppert,
             lowert,
             consecutive,
             duration,
             bias.correction,
             season,
             agreement) {
      stopifnot(is.logical(consecutive))
      stopifnot(is.numeric(threshold), threshold >= 0, threshold <= 1)
      if (!is.list(season))
        cli::cli_abort("season needs to be a list, for example, list(1:3)")
      if (!(duration == "max" || is.numeric(duration))) {
        cli::cli_abort("duration must be 'max' or a number")
      }
      if (!any(stringr::str_detect(colnames(data[[1]]), "obs")) &
          isTRUE(bias.correction)) {
        cli::cli_abort(
          c("x" = "Bias correction cannot be performed, no observational dataset found. Set as F")
        )
      }
      if (!is.null(lowert) &
          !is.null(uppert))
        cli::cli_abort(c("x" = "Specify only one threshold argument"))
      if ((is.null(lowert) &
           is.null(uppert)) & bias.correction)
        cli::cli_abort(
          c("x" = "Bias correction can change the results of the climate change signal only for the calculation of indicators. Specify lowert or uppert aguments to use this option")
        )
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
        cli::cli_abort(c("x" = "Specify a threshold for which you want to calculate consecutive days"))
      if (!any(stringr::str_detect(data[[1]]$experiment, "hist")))
        cli::cli_abort(c("x" = "Please load historical data to use the climate_change_signal function"))

      if (bias.correction) {
        if ((length(data[[1]]$obs[[1]]$xy$x) != length(data[[1]]$models_mbrs[[1]]$xy$x)) |
            (length(data[[1]]$obs[[1]]$xy$y) != length(data[[1]]$models_mbrs[[1]]$xy$y)))  {
          cli::cli_alert_warning(
            "Observation and historical experiment do not have the same spatial resolution. Models will be interpolated to match the observational dataset"
          )
        }

      }
    }

  # generate messages on the type of operations being performed
  create_message <-
    function(var,
             uppert,
             lowert,
             consecutive,
             duration,
             bias.correction,
             frequency) {
      if (is.null(uppert) & is.null(lowert)) {
        paste0("Climate change signal for ",
               ifelse(var == "pr", "total ", "mean "),
               var)
      }
      else if ((!is.null(uppert) |
                !is.null(lowert)) & !consecutive) {
        paste0(
          "Climate change signal for number of days with ",
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
          "Climate change signal for maximum consecutive number of days ",
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
          ". Climate change signal for",
          ifelse(frequency, " frequency " , " total number "),
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
    if (any(stringr::str_detect(colnames(datasets), "obs"))) {
      datasets %>%  dplyr::mutate_at(c("models_mbrs", "obs"),
                                     ~ purrr::map(., ~ suppressMessages(
                                       transformeR::subsetGrid(., season = season)
                                     )))
    } else {
      datasets %>%  dplyr::mutate_at(c("models_mbrs"),
                                     ~ purrr::map(., ~ suppressMessages(
                                       transformeR::subsetGrid(., season = season)
                                     )))
    }
  }

  # function used to perform the calculations

  perform_calculations <-
    function(datasets,
             mod.numb,
             var,
             uppert,
             lowert,
             consecutive,
             duration,
             country_shp,
             bias.correction,
             season,
             frequency,
             threshold) {
      season_name <-
        convert_vector_to_month_initials(season)
      data_list <- datasets  %>%
        {
          if (bias.correction) {
            cli::cli_text(
              paste(
                "{cli::symbol$arrow_right}",
                " Performing bias correction with the empirical quantile mapping",
                " method, for each model and month separately. This can take a while. Season",
                glue::glue_collapse(season, "-")
              )
            )
            dplyr::mutate(.,
                          models_mbrs = purrr::map2(models_mbrs, experiment, function(mod, forc) {
                            if (forc == "historical") {
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
                                    extrapolation = "constant"
                                  )
                                )
                            } else {
                              bc <-
                                suppressMessages(
                                  downscaleR::biasCorrection(
                                    y = obs[[1]],
                                    x = dplyr::filter(datasets, experiment == "historical")$models_mbrs[[1]],
                                    newdata = mod,
                                    precipitation = ifelse(var == "pr", TRUE, FALSE),
                                    method = "eqm",
                                    window = if (any(diffs == 1))
                                      c(30, 30)
                                    else
                                      c(1, 1),
                                    extrapolation = "constant"
                                  )
                                )

                            }

                            mod_temp <-
                              transformeR::intersectGrid.time(mod, bc, which.return = 2)
                            mod_temp$Dates$start <-
                              mod$Dates$start
                            mod_temp$Dates$end <-  mod$Dates$end

                            return(mod_temp)
                          }, .progress = T))
          } else
            .
        }  %>%
        # computing annual aggregation. if threshold is specified, first apply threshold
        dplyr::mutate(models_agg_y = furrr::future_map(models_mbrs, function(x)
          suppressMessages(
            transformeR::aggregateGrid(# perform aggregation based on season output
              x, aggr.y =
                if (var == "pr" &
                    !consecutive &
                    (is.null(uppert) & is.null(lowert))) {
                  list(FUN = "sum", na.rm = TRUE)
                } else if (var != "pr" &
                           !consecutive &
                           (is.null(lowert) & is.null(uppert))) {
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
          )))  %>%
        dplyr::mutate(models_agg_tot = purrr::map(models_agg_y, function(x)  {
          suppressMessages(x %>%
                             transformeR::climatology(.))
        })) %>%
        dplyr::select(-models_mbrs) %>% # calculate climate change signal by subtracting historical data
        dplyr::mutate(
          ccs_mbrs = purrr::map(models_agg_tot, function(y) {
            h <-
              dplyr::filter(., stringr::str_detect(experiment, "hist"))$models_agg_tot[[1]]
            transformeR::gridArithmetics(y, h, operator = "-")
          }),
          rst_ccs_sign = purrr::map2(experiment, ccs_mbrs, function(x, y) {
            y$Data <- apply(y$Data, c(1, 3, 4), mean)
            arry_sign <-
              agreement(y$Data,  threshold)
            y$Data <- arry_sign
            rast_sign <- make_raster(y, c(1, 2), country_shp)
            names(rast_sign) <-
              paste0(x, "_", names(rast_sign), "_", season_name)
            return(rast_sign)
          }),
          rst_ens_mean_ccs = purrr::map2(experiment, ccs_mbrs, function(x, y) {
            ccs_mean <- make_raster(y, c(3, 4), country_shp)
            names(ccs_mean) <-
              paste0(x, "_", names(ccs_mean), "_", season_name)
            return(ccs_mean)
          }),
          rst_ens_sd_ccs = purrr::map2(experiment, ccs_mbrs, function(x, y) {
            ccs_sd <- make_raster(y, c(3, 4), country_shp, stat = "sd")
            names(ccs_sd) <-
              paste0(x, "_", names(ccs_sd), "_", season_name)
            return(ccs_sd)
          }),
          rst_models_ccs = purrr::map2(experiment, ccs_mbrs, function(x, y) {
            rs_list <- purrr::map(1:dim(y$Data)[[1]], function(ens) {
              array_mean <-
                apply(y$Data[ens, , , ], c(1, 2), mean, na.rm = TRUE) # climatology per member adjusting by array dimension

              y$Data <- array_mean

              rs <- make_raster(y, c(1, 2), country_shp)

              names(rs) <-
                paste0("Member ", ens, "_", x, "_", names(rs), "_", season_name)
              return(rs)
            }),
          models_temp_ccs = purrr::map2(experiment, models_agg_y, function(x, y) {
            h <-
              dplyr::filter(., stringr::str_detect(experiment, "hist"))$models_agg_tot[[1]]

            if (stringr::str_detect(x, "hist")) {
              NULL

            } else {
              mbrs = dim(y$Data)[1]
              yrs = dim(y$Data)[2]
              lt = dim(y$Data)[3]
              ln = dim(y$Data)[4]

              h.expanded = array(rep(h$Data, each = yrs), dim = c(mbrs, yrs, lt, ln))

              delta <-
                transformeR::gridArithmetics(y, h.expanded, operator = "-")
              dimnames(delta$Data)[[1]] <- delta$Members
              dimnames(delta$Data)[[2]] <- delta$Dates$start
              dimnames(delta$Data)[[3]] <- delta$xyCoords$y
              dimnames(delta$Data)[[4]] <- delta$xyCoords$x

              reshape2::melt(delta$Data) %>%
                dplyr::mutate(date = as.Date(Var2)) %>%
                dplyr::mutate(experiment = x) %>%
                dplyr::mutate(season = season_name) %>%
                dplyr::group_by(date, experiment, Var1, season) %>%
                dplyr::summarise(value = median(value, na.rm = T)) # spatial aggregation because ccs do not support spatiotemporal

            }
          })
        ) %>%
        dplyr::filter(!stringr::str_detect(experiment, "hist"))

      gc()
      invisible(structure(
        list(
          terra::rast(data_list$rst_ens_mean_ccs),
          terra::rast(data_list$rst_ens_sd_ccs),
          terra::rast(unlist(data_list$rst_models_ccs)),
          terra::rast(data_list$rst_ccs_sign),
          do.call(rbind, purrr::map(
            1:nrow(data_list), ~ data_list$models_temp_ccs[[.]]
          ))
        ),
        class = "CAVAanalytics_ccs",
        components = list(
          "SpatRaster for ccs mean",
          "SparRaster stack for ccs sd",
          "SpatRaster stack for individual members",
          "SpatRaster stack for ccs agreement",
          "dataframe for spatially aggregated data"

        )
      ))
    }


  # beginning of code -------------------------------------------------------
  if (class(data) != "CAVAanalytics_list")
    cli::cli_abort(c("x" = "The input data is not the output of CAVAanalytics load_data"))
  # check input requirements
  check_inputs(data,
               uppert,
               lowert,
               consecutive,
               duration,
               bias.correction,
               season,
               threshold)

  # retrieve information
  mod.numb <- dim(data[[1]]$models_mbrs[[1]]$Data) [1]
  datasets <- data[[1]]
  country_shp <- data[[2]]
  var <- datasets$models_mbrs[[1]]$Variable$varName
  dates <- datasets$models_mbrs[[1]]$Dates$start
  dates <- as.Date(dates)
  # calculate the differences between consecutive dates to understand temporal resolution
  diffs <- diff(dates)

  # set parallel processing
  future::plan(future::multisession, workers = n.sessions)
  #create plots by season
  data_list <- purrr::map(season, function(sns) {
    mes <-
      create_message(var,
                     uppert,
                     lowert,
                     consecutive,
                     duration,
                     bias.correction,
                     frequency)

    # filter data by season
    datasets <- filter_data_by_season(datasets, sns)
    cli::cli_text(
      paste0(
        "{cli::symbol$arrow_right}",
        " climate change signal, season ",
        glue::glue_collapse(sns, "-"),
        ". Model agreement calculated based on ",
        threshold * 100,
        "% threshold. ",
        mes
      )
    )

    # perform calculations
    cli::cli_progress_step("Performing calculations")
    data_list <-
      perform_calculations(
        datasets,
        mod.numb,
        var,
        uppert,
        lowert,
        consecutive,
        duration,
        country_shp,
        bias.correction,
        season = sns,
        frequency,
        threshold
      )
    cli::cli_progress_done()
    # return results
    return(data_list)
  })

  invisible(structure(
    list(
      terra::rast(lapply(data_list, `[[`, 1)),
      terra::rast(lapply(data_list, `[[`, 2)),
      terra::rast(lapply(data_list, `[[`, 3)),
      terra::rast(lapply(data_list, `[[`, 4)),
      do.call(rbind, lapply(data_list, `[[`, 5))
    ),
    class = "CAVAanalytics_ccs",
    components = list(
      "SpatRaster for ccs mean",
      "SparRaster stack for ccs sd",
      "SpatRaster stack for individual members",
      "SpatRaster stack for ccs agreement",
      "dataframe for spatially aggregated data"
    )
  ))

}
