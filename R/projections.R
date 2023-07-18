#' Analysis of future projections
#'
#' Automatically process climate model projections and compute useful statistics

#' @param data output of load_data
#' @param bias.correction logical
#' @param uppert  numeric of length=1, upper threshold
#' @param lowert numeric of length=1, lower threshold
#' @param season numeric, seasons to select. For example, 1:12
#' @param consecutive logical, to use in conjunction with lowert or uppert
#' @param duration character, either "max" or "total". Used only when consecutive is TRUE
#' @param n.sessions numeric, number of sessions to use, default is one. Parallelization can be useful when multiple scenarios are used (RCPS, SSPs). However, note that parallelizing will increase RAM usage
#' @importFrom magrittr %>%
#' @return list with SpatRaster. .[[1]] contains SpatRaster for the ensemble mean. .[[2]] contains SpatRaster for the ensemble sd and .[[3]] conins SpatRaster for individual models
#'
#' @export



projections <-
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
               duration) {
        if (class(data) != "CAVAanalytics_list")
          cli::cli_abort(c("x" = "The input data is not the output of CAVAanalytics load_data"))
        stopifnot(is.logical(consecutive), is.logical(bias.correction))
        match.arg(duration, c("max", "total"))
        if (length(data[[1]]$experiment) == 1 &
            data[[1]]$experiment[[1]] == "historical")
          cli::cli_abort(c("x" = "Projections are not part of CAVAanalytics list"))
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
          stop("Specify a threshold for which you want to calculate consecutive days")
        if (!any(stringr::str_detect(colnames(data[[1]]), "obs")) &
            isTRUE(bias.correction)) {
          cli::cli_abort(
            c("x" = "Bias correction cannot be performed, no observational dataset found. Set as F")
          )

        }
        if (bias.correction) {
          if (length(data[[1]]$obs[[1]]$xy$x) != length(data[[1]]$models_mbrs[[1]]$xy$x)) {
            cli::cli_alert_warning(
              "Observation and historical experiment do not have the same spatial resolution. Models will be interpolated to match the observational dataset"
            )
          }

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
            "Calculation of ",
            ifelse(var == "pr", "total ", "mean "),
            ifelse(bias.correction, "bias-corrected ", " "),
            var
          )
        }
        else if ((!is.null(uppert) |
                  !is.null(lowert)) & !consecutive) {
          paste0(
            "Calculation of number of days with ",
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
            "Calculation of maximum length of consecutive number of days ",
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
            ". Calculation of total total number of consecutive days with duration longer than 6 days, ",
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
      if (any(stringr::str_detect(colnames(datasets), "obs"))) {
        datasets %>%  dplyr::mutate_at(c("models_mbrs", "obs"),
                                       ~ purrr::map(., ~ transformeR::subsetGrid(., season = season)))
      } else {
        datasets %>%  dplyr::mutate_at(c("models_mbrs"),
                                       ~ purrr::map(., ~ transformeR::subsetGrid(., season = season)))
      }
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
               country_shp) {
        gc()

        data_list <- datasets %>%
          dplyr::filter(experiment != "historical") %>%
          {
            if (bias.correction) {
              cli::cli_alert_info(
                paste(
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
          dplyr::mutate(models_agg_y = furrr::future_map(models_mbrs, function(x)
            suppressMessages(
              transformeR::aggregateGrid(# perform aggregation based on seasonended output
                x, aggr.y =
                  if (var == "pr" &
                      !consecutive &
                      (is.null(uppert) & is.null(lowert))) {
                    list(FUN = "sum")
                  } else if (var != "pr" &
                             !consecutive &
                             (is.null(lowert) &
                              is.null(uppert))) {
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
            ))) %>%
          dplyr::select(-models_mbrs) %>%
          # ensemble mean
          dplyr::mutate(
            rst_ens_mean = purrr::map2(experiment, models_agg_y, function(x, y) {
              ens <-
                suppressMessages(transformeR::aggregateGrid(y, aggr.mem = list(FUN = "mean", na.rm = TRUE)))
              array_mean <-
                apply(ens$Data, c(2, 3), mean, na.rm = TRUE) # climatology
              ens$Data <- array_mean
              rs <- make_raster(ens) %>%
                terra::crop(., country_shp, snap = "out") %>%
                terra::mask(., country_shp)
              names(rs) <-
                paste0(x, "_", names(rs))
              return(rs)
            }),
            # ensemble SD
            rst_ens_sd = purrr::map2(experiment, models_agg_y, function(x, y) {
              ens <-
                suppressMessages(transformeR::aggregateGrid(y, aggr.mem = list(FUN = "sd", na.rm = TRUE)))
              array_mean <-
                apply(ens$Data, c(2, 3), mean, na.rm = TRUE) # climatology
              ens$Data <- array_mean
              rs <- make_raster(ens) %>%
                terra::crop(., country_shp, snap = "out") %>%
                terra::mask(., country_shp)
              names(rs) <-
                paste0(x, "_", names(rs))
              return(rs)
            }),
            # individual models
            rst_models = purrr::map2(experiment, models_agg_y, function(x, y) {
              rs_list <- purrr::map(1:dim(y$Data)[[1]], function(ens) {
                array_mean <-
                  if (length(y$Dates$start) == 1)
                    apply(y$Data[ens, , , ], c(1, 2), mean, na.rm = TRUE)
                else
                  apply(y$Data[ens, , , ], c(2, 3), mean, na.rm = TRUE) # climatology per member adjusting by array dimension
                y$Data <- array_mean
                rs <- make_raster(y) %>%
                  terra::crop(., country_shp, snap = "out") %>%
                  terra::mask(., country_shp)

                names(rs) <-
                  paste0("Member ", ens, "_", x, "_", names(rs))
                return(rs)
              })

            })
          )
        gc()
        invisible(structure(
          list(
            terra::rast(data_list$rst_ens_mean),
            terra::rast(data_list$rst_ens_sd),
            terra::rast(purrr::map(
              data_list$rst_models, ~ terra::rast(.x)
            ))
          ),
          class = "CAVAanalytics_projections",
          components = list(
            "SpatRaster for ensemble mean",
            "SpatRaster for ensemble sd",
            "SpatRaster for individual members"
          )
        ))

      }

    # beginning of code -------------------------------------------------------

    # check input requirements
    check_inputs(data, bias.correction, uppert, lowert, consecutive, duration)

    # retrieve information
    mod.numb <- dim(data[[1]]$models_mbrs[[1]]$Data) [1]
    datasets <- data[[1]]
    country_shp <- data[[2]]
    var <- datasets$models_mbrs[[1]]$Variable$varName
    dates <- datasets$models_mbrs[[1]]$Dates$start
    dates <- as.Date(dates)
    # calculate the differences between consecutive dates to understand temporal resolution and adjust the window argument in bias correction
    diffs <- diff(dates)

    # create message
    mes <-
      create_message(var, bias.correction, uppert, lowert, consecutive, duration)

    # set parallel processing
    future::plan(future::multisession, workers = n.sessions)

    # filter data by season
    datasets <- filter_data_by_season(datasets, season)
    cli::cli_alert_info(paste0(
      " projections, season ",
      glue::glue_collapse(season, "-"),
      ". ",
      mes
    ))

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
        country_shp
      )

    cli::cli_progress_done()
    # return results
    return(data_list)
  }
