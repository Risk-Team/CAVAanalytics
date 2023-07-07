#' Calculation of climate change signal
#'
#' Automatically computes climate change signal

#' @param data output of load_data
#' @param uppert  numeric of length=1, upper threshold
#' @param lowert numeric of length=1, lower threshold
#' @param season Numerical, seasons to select. For example, 1:12
#' @param consecutive logical, to use in conjunction with lowert or uppert
#' @param duration character, either "max" or "total"
#' @param bias.correction logical
#' @param n.sessions numeric, number of sessions to use, default is one. Parallelisation can be useful when multiple scenarios are used (RCPS, SSPs). However, note that parallelising will increase RAM usage
#' @importFrom magrittr %>%
#' @return list with SpatRasters. .[[1]] contains the SpatRasters for the ensemble mean. .[[2]] contains the SpatRasters for the ensemble sd and .[[3]] conins the SpatRasters for individual models
#'
#' @export
#' @examples
#' exmp <- load_data(country = "Togo", variable="tasmax", years.hist=2000:2005, years.proj=2030:2035,
#'               path.to.data = "CORDEX-CORE", domain="AFR-22")
#' ccs_exmp <- climate_change_signal(exmp, season = 1:12)
#' plotting(ccs_exmp, ensemble=T, plot_titles="mean delta tasmax", legend_range= c(-1.5, 1.5), palette=c("blue", "cyan", "white", "yellow", "red"), alpha=0.7)


climate_change_signal <- function(data,
                                  uppert = NULL,
                                  lowert = NULL,
                                  season,
                                  consecutive = F,
                                  duration = "max",
                                  bias.correction = F,
                                  n.sessions = 1) {
  # Intermediate functions --------------------------------------------------

  # check inputs requirement
  check_inputs <-
    function(data,
             uppert,
             lowert,
             consecutive,
             duration,
             bias.correction) {
      if (class(data) != "CAVAanalytics_list")
        cli::cli_abort(c("x" = "The input data is not the output of CAVAanalytics load_data"))
      stopifnot(is.logical(consecutive))
      match.arg(duration, c("max", "total"))
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
      if (any(stringr::str_detect(data$experiment, "hist")))
        cli::cli_abort(c("x" = "Please load historical data to use the climate_change_signal function"))
    }

  # generate messages on the type of operations being performed
  create_message <-
    function(var,
             uppert,
             lowert,
             consecutive,
             duration,
             bias.correction) {
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
               (consecutive & duration == "total")) {
        paste0(
          var,
          ". Climate change signal for total number of consecutive days with duration longer than 6 days, ",
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
    datasets %>% dplyr::mutate_at(c("models_mbrs"),
                                  ~ purrr::map(., ~ transformeR::subsetGrid(., season = season)))
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
             bias.correction) {
      gc()
      data_list <- datasets  %>%
        {
          if (bias.correction) {
            cli::cli_alert_info(
              paste(
                " Performing bias correction with the empirical quantile mapping",
                " method, for each model and month separately. This can take a while. Season",
                glue::glue_collapse(season, "-")
              )
            )
            dplyr::mutate(.,
                          models_mbrs = furrr::future_map2(models_mbrs, experiment, function(mod, forc) {
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
                          }))
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
                  list(FUN = "sum")
                } else if (var != "pr" &
                           !consecutive &
                           (is.null(lowert) & is.null(uppert))) {
                  list(FUN = "mean")
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
          )))  %>%
        dplyr::select(-models_mbrs)  %>%
        # individual models
        dplyr::mutate(rst_models = purrr::map2(experiment, models_agg_y, function(x, y) {
          rs_list <- purrr::map(1:dim(y$Data)[[1]], function(ens) {
            array_mean <-
              if (length(y$Dates$start) == 1)
                apply(y$Data[ens, , ,], c(1, 2), mean, na.rm = TRUE)
            else
              apply(y$Data[ens, , ,], c(2, 3), mean, na.rm = TRUE) # climatology per member adjusting by array dimension

            y$Data <- array_mean

            rs <- make_raster(y)  %>%
              terra::crop(., country_shp, snap = "out") %>%
              terra::mask(., country_shp)

            names(rs) <-
              paste0("Member ", ens, "_", x, "_", names(rs))
            return(rs)
          })

        }))  %>%
        dplyr::mutate(
          rst_ens_mean_ccs = purrr::map(rst_models, function(y) {
            h <-
              dplyr::filter(., stringr::str_detect(experiment, "hist"))$rst_models[[1]]
            ccs <- terra::rast(y) - terra::rast(h)
            ccs_mean <- terra::mean(ccs)
            names(ccs_mean) <- names(terra::rast(y))[[1]]
            return(ccs_mean)
          }),
          rst_ens_sd_ccs = purrr::map(rst_models, function(y) {
            h <-
              dplyr::filter(., stringr::str_detect(experiment, "hist"))$rst_models[[1]]
            ccs <- terra::rast(y) - terra::rast(h)
            ccs_sd <- terra::stdev(ccs)
            names(ccs_sd) <- names(terra::rast(y))[[1]]
            return(ccs_sd)
          }),
          rst_models_ccs = purrr::map(rst_models, function(y) {
            h <-
              dplyr::filter(., stringr::str_detect(experiment, "hist"))$rst_models[[1]]
            ccs <- terra::rast(y) - terra::rast(h)
            return(ccs)
          })
        )  %>%
        dplyr::filter(!stringr::str_detect(experiment, "hist"))
      gc()
      invisible(structure(
        list(
          terra::rast(data_list$rst_ens_mean_ccs),
          terra::rast(data_list$rst_ens_sd_ccs),
          terra::rast(data_list$rst_models_ccs)
        ),
        class = "CAVAanalytics_ccs",
        components = list(
          "SpatRaster for ccs mean",
          "SparRaster stack for ccs sd",
          "SpatRaster stack for individual members"
        )
      ))
    }


  # beginning of code -------------------------------------------------------

  # check input requirements
  check_inputs(data, uppert, lowert, consecutive, duration, bias.correction)

  # retrieve information
  mod.numb <- dim(data[[1]]$models_mbrs[[1]]$Data) [1]
  datasets <- data[[1]]
  country_shp <- data[[2]]
  var <- datasets$models_mbrs[[1]]$Variable$varName
  dates <- datasets$models_mbrs[[1]]$Dates$start
  dates <- as.Date(dates)
  # calculate the differences between consecutive dates to understand temporal resolution
  diffs <- diff(dates)

  # create message
  mes <-
    create_message(var, uppert, lowert, consecutive, duration, bias.correction)

  # set parallel processing
  future::plan(future::multisession, workers = n.sessions)

  # filter data by season
  datasets <- filter_data_by_season(datasets, season)
  cli::cli_alert_info(paste0(
    " climate change signal, season ",
    glue::glue_collapse(season, "-"),
    ". ",
    mes
  ))

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
      bias.correction
    )
  cli::cli_progress_done()
  # return results
  return(data_list)
}
