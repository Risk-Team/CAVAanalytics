#' Analysis of observations-historical period
#'
#' Automatically process the observational period and compute user-defined indicators

#' @param data output of load_data
#' @param uppert  numeric of length=1, upper threshold
#' @param lowert numeric of length=1, lower threshold
#' @param season list, containing seasons to select. For example, list(1:6, 7:12)
#' @param consecutive logical, to use in conjunction with lowert or uppert
#' @param duration A parameter that can be set to either "max" or a specific number. It is relevant only when 'consecutive' is set to TRUE. For instance, to calculate the count of consecutive days with Tmax (maximum temperature) above 35°C, lasting for more than 3 days, you can set 'uppert' to 35, 'consecutive' to TRUE, and 'duration' to 3.
#' @param frequency logical value. This parameter is relevant only when 'consecutive' is set to TRUE and 'duration' is not set to "max". For instance, if you want to determine the count of heatwaves, defined as the number of days with Tmax (maximum temperature) exceeding 35°C for a minimum of 3 consecutive days, set 'uppert' to 35, 'consecutive' to TRUE, 'duration' to 3, and 'frequency' to TRUE.
#' @param trends logical value. Whether linear regression should be applied to assess yearly increase
#' @importFrom magrittr %>%
#' @return list with SpatRaster. To explore the output run attributes(output)
#'
#' @export



observations <-
  function(data,
           uppert = NULL,
           lowert = NULL,
           season,
           consecutive = F,
           frequency = F,
           trends = F,
           duration = "max") {
    # Intermediate functions --------------------------------------------------

    # check inputs requirement
    check_inputs <-
      function(data,
               uppert,
               lowert,
               consecutive,
               duration,
               season,
               trends) {
        if (!is.list(season))
          cli::cli_abort("season needs to be a list, for example, list(1:3)")
        if (!any(stringr::str_detect(colnames(data[[1]]), "obs")))
          cli::cli_abort(
            c("x" = "Observational dataset not detected. To use this function you need to specify path.to.obs in load_data")
          )
        stopifnot(is.logical(consecutive))
        stopifnot(is.logical(trends))
        if (!(duration == "max" || is.numeric(duration))) {
          cli::cli_abort("duration must be 'max' or a number")
        }
        if (!is.null(lowert) &
            !is.null(uppert))
          cli::cli_abort(c("x" = "select only one threshold"))
        if (!is.null(lowert) |
            !is.null(uppert)) {
          dates <- data[[1]]$obs[[1]]$Dates$start
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

      }

    # generate messages on the type of operations being performed
    create_message <-
      function(var,
               uppert,
               lowert,
               consecutive,
               duration,
               frequency,
               trends) {
        if (is.null(uppert) & is.null(lowert)) {
          paste0(
            "Calculation of ",
            ifelse(trends, "yearly increase in ", " "),
            ifelse(var == "pr", "total ", "mean "),
            var
          )
        }
        else if ((!is.null(uppert) |
                  !is.null(lowert)) & !consecutive) {
          paste0(
            "Calculation of ",
            ifelse(trends, "yearly increase in ", " "),
            "number of days with ",
            var,
            ifelse(
              !is.null(lowert),
              paste0(" below threshold of ", lowert),
              paste0(" above threshold of ", uppert)
            )
          )
        }
        else if ((!is.null(uppert) |
                  !is.null(lowert)) &
                 (consecutive & duration == "max")) {
          paste0(
            "Calculation of ",
            ifelse(trends, "yearly increase in ", " "),
            "maximum length of consecutive number of days ",
            ifelse(
              !is.null(lowert),
              paste0("below ", lowert),
              paste0("above ", uppert)
            )
          )
        }
        else if ((!is.null(uppert) |
                  !is.null(lowert)) &
                 (consecutive & is.numeric(duration))) {
          paste0(
            var,
            ". Calculation of ",
            ifelse(trends, "yearly increase in ", " "),
            ifelse(frequency, "frequency", "total number"),
            " of days with duration longer than ",
            duration,
            " consecutive days, ",
            ifelse(
              !is.null(lowert),
              paste0("below threshold of ", lowert),
              paste0("above threshold of ", uppert)
            )
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
      datasets %>%  dplyr::mutate_at(c("obs"),
                                     ~ purrr::map(., ~ suppressMessages(
                                       transformeR::subsetGrid(., season = season)
                                     )))
    }

    # function used to perform the calculations

    perform_calculations <-
      function(datasets,
               var,
               uppert,
               lowert,
               consecutive,
               duration,
               country_shp,
               season,
               frequency,
               trends) {
        season_name <-
          convert_vector_to_month_initials(season)

        data_list <- datasets %>%
          dplyr::slice(1) %>%    # computing annual aggregation. if threshold is specified, first apply threshold
          dplyr::mutate(obs_agg_y = purrr::map(obs, function(x)
            suppressMessages(
              transformeR::aggregateGrid(# perform aggregation based on season and output
                x, aggr.y =
                  if (var == "pr" &
                      !consecutive &
                      (is.null(uppert) & is.null(lowert))) {
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
          dplyr::select(obs_agg_y) %>%
          {
            if (!trends) {
              # Obs mean
              dplyr::mutate(
                .,
                rst_mean = purrr::map(obs_agg_y, function(obs_agg) {
                  rs <-
                    make_raster(obs_agg, if (length(obs_agg$Dates$start) == 1)
                      c(1, 2)
                      else
                        c(2, 3), country_shp) # adjust by array dimension
                  names(rs) <-
                    paste0("obs", "_", names(rs), "_", season_name)
                  return(rs)
                }),
                obs_temp = purrr::map(obs_agg_y, function(x) {
                  dimnames(x$Data)[[1]] <- x$Dates$start
                  dimnames(x$Data)[[2]] <- x$xyCoords$y
                  dimnames(x$Data)[[3]] <- x$xyCoords$x
                  df <- reshape2::melt(x$Data) %>%
                    dplyr::mutate(date = as.Date(Var1)) %>%
                    dplyr::mutate(experiment = "obs") %>%
                    dplyr::mutate(season = season_name)
                  return(df)


                })
              )


            } else {
              dplyr::mutate(
                .,
                obs_spat = purrr::map(obs_agg_y, function(x) {
                  c4R <- x
                  results <- models_trends(x, observation = T)
                  c4R$Data <- results[1, , ]# coef
                  x$Data <- results[2, , ] # p.value

                  coef <- make_raster(c4R, c(1, 2), country_shp)
                  names(coef) <-
                    paste0("obs", "_coef_", names(coef), "_", season_name)
                  p.value <- make_raster(x, c(1, 2), country_shp)

                  names(p.value) <-
                    paste0("obs", "_p_", names(p.value), "_", season_name)
                  return(list(coef, p.value))

                }),
                obs_temp = purrr::map(obs_agg_y, function(x) {
                  dimnames(x$Data)[[1]] <- x$Dates$start
                  dimnames(x$Data)[[2]] <- x$xyCoords$y
                  dimnames(x$Data)[[3]] <- x$xyCoords$x
                  df <- reshape2::melt(x$Data) %>%
                    dplyr::mutate(date = as.Date(Var1)) %>%
                    dplyr::mutate(experiment = "obs") %>%
                    dplyr::mutate(season = season_name)
                  return(df)


                })
              )



            }
          }

        if (trends) {
          invisible(structure(
            list(
              data_list$obs_spat[[1]][[1]],
              data_list$obs_spat[[1]][[2]],
              data_list$obs_temp[[1]]
            ),
            class = "CAVAanalytics_observations",
            components = list(
              "SpatRaster for trends coefficients",
              "SpatRaster for trends p.values",
              "dataframe for annually aggregated data"
            )
          ))

        }  else {
          invisible(structure(
            list(data_list$rst_mean[[1]],
                 data_list$obs_temp[[1]]),
            class = "CAVAanalytics_observations",
            components = list(
              "SpatRaster for observation mean",
              "dataframe for annually aggregated data"
            )
          ))

        }
      }

    # beginning of code -------------------------------------------------------
    if (class(data) != "CAVAanalytics_list")
      cli::cli_abort(c("x" = "The input data is not the output of CAVAanalytics load_data"))
    # check input requirements
    check_inputs(data, uppert, lowert, consecutive, duration, season, trends)

    # retrieve information
    datasets <- data[[1]]
    country_shp <- data[[2]]
    var <- datasets$obs[[1]]$Variable$varName
    dates <- datasets$obs[[1]]$Dates$start
    dates <- as.Date(dates)
    # calculate the differences between consecutive dates to understand temporal resolution and adjust the window argument in bias correction
    diffs <- diff(dates)

    # create message
    data_list <- purrr::map(season, function(sns) {
      mes <-
        create_message(var,
                       uppert,
                       lowert,
                       consecutive,
                       duration,
                       frequency,
                       trends)

      # filter data by season
      datasets <- filter_data_by_season(datasets, season = sns)
      cli::cli_text(
        paste0(
          "{cli::symbol$arrow_right}",
          " observations, season ",
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
          var,
          uppert,
          lowert,
          consecutive,
          duration,
          country_shp,
          season = sns,
          frequency,
          trends
        )

      cli::cli_progress_done()
      # return results
      return(data_list)
    })

    if (!trends) {
      invisible(structure(
        list(terra::rast(lapply(
          data_list, `[[`, 1
        )),
        do.call(rbind, lapply(
          data_list, `[[`, 2
        ))),
        class = "CAVAanalytics_observations",
        components = list(
          "SpatRaster for observation mean",
          "dataframe for annually aggregated data"
        )
      ))

    } else {
      # when linear regression is not applied
      invisible(structure(
        list(
          terra::rast(lapply(data_list, `[[`, 1)),
          terra::rast(lapply(data_list, `[[`, 2)),
          do.call(rbind, lapply(data_list, `[[`, 3))
        ),
        class = "CAVAanalytics_observations",
        components = list(
          "SpatRaster for trends coefficients",
          "SpatRaster for trends p.values",
          "dataframe for annually aggregated data"
        )
      ))

    }

  }
