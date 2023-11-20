#' Analysis of long-term trends
#'
#' Compute multivariate linear regression and linear regression through design-based inference

#' @param data output of load_data
#' @param bias.correction logical, whether to perform bias.correction or not
#' @param uppert  numeric of length=1, upper threshold
#' @param lowert numeric of length=1, lower threshold
#' @param season list, containing seasons to select. For example, list(1:6, 7:12)
#' @param consecutive logical, to use in conjunction with lowert or uppert
#' @param duration either "max" or specify a number. Used only when consecutive is TRUE. For example, to know the number of consecutive days with tmax above 35, lasting more than 3 days, specify uppert=35, consecutive =T and duration=3
#' @param observation logical, whether to visualize trends for the observational dataset or projections
#' @param intraannual_var logical, whether linear regression is applied to annual variability, measured as standard deviation
#' @param n.sessions numeric, number of sessions to use, default is one. Parallelisation can be useful when multiple scenarios are used (RCPS, SSPs). However, note that parallelising will increase RAM usage
#' @importFrom magrittr %>%
#' @return list with SpatRaster. To explore the output run attributes(output)
#'
#' @export

trends = function(data,
                  bias.correction = FALSE,
                  uppert = NULL,
                  lowert = NULL,
                  season,
                  consecutive = FALSE,
                  duration = "max",
                  intraannual_var = FALSE,
                  observation = FALSE,
                  n.sessions = 1) {
  # intermediate functions --------------------------------------------------

  # checking inputs requirement, including correct specification of function arguments

  check_inputs <-
    function(data,
             var,
             consecutive,
             observation,
             bias.correction,
             uppert,
             lowert,
             duration,
             intraannual_var,
             season) {
      if (!is.list(season))
        cli::cli_abort("season needs to be a list, for example, list(1:3)")
      stopifnot(
        is.logical(consecutive),
        is.logical(bias.correction),
        is.logical(observation),
        is.logical(intraannual_var)
      )
      if (is.null(data[[1]]$models_mbrs))
        cli::cli_abort(c("x" = "trends require projections even when observation is TRUE"))
      if (var != "pr" &
          intraannual_var)
        cli::cli_abort(c("x" = "intraannual variability is only meaningful for precipitation"))
      if (intraannual_var &
          (!is.null(lowert) |
           !is.null(uppert)))
        cli::cli_abort(
          c("x" = "Thresholds are not meaningful when calculating intraannual variaiblity, set as NULL")
        )
      if (intraannual_var) {
        dates <- data[[1]]$models_mbrs[[1]]$Dates$start
        dates <- as.Date(dates)
        # calculate the differences between consecutive dates
        diffs <- diff(dates)
        # check if the differences are equal to 1
        if (any(diffs == 1)) {
          cli::cli_abort(
            c("x" = "Intraannual variability requires monthly data. Reload your data specifying sum in aggr.m")
          )
        }
      }
      if (!is.null(lowert) &
          !is.null(uppert))
        cli::cli_abort(c("x" = "select only one threshold"))
      if (!is.null(lowert) |
          !is.null(uppert)) {
        dates <-  data[[1]]$models_mbrs[[1]]$Dates$start
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
          (is.null(uppert)) &
          is.null(lowert))
        cli::cli_abort(c("x" = "Specify a threshold for which you want to calculate consecutive days"))
      if (!(duration == "max" || is.numeric(duration))) {
        cli::cli_abort("duration must be 'max' or a number")
      }
      if (observation) {
        if (!any(stringr::str_detect(colnames(data[[1]]), "obs"))) {
          cli::cli_abort(c("x" = "An observational dataset is needed for this option, check your data"))
        }
        if (bias.correction) {
          cli::cli_abort(
            c("x" = "Applying bias correction to look at the historical period does not make sense. Observations are used as default")
          )
        }
      }
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
             duration,
             observation,
             intraannual_var) {
      if (is.null(uppert) & is.null(lowert)) {
        mes = paste0(
          "Calculation of yearly increase in ",
          if (var == "pr")
            ifelse(intraannual_var, "intraannual variability ", "total ")
          else
            "mean ",
          ifelse(bias.correction, "bias-corrected ", " "),
          var
        )
      }
      if ((!is.null(uppert) | !is.null(lowert)) & !consecutive) {
        mes = paste0(
          var,
          ". Calculation of yearly increase in number of days ",
          ifelse(
            !is.null(lowert),
            paste0("below ", lowert),
            paste0("above ", uppert)
          ),
          ifelse(bias.correction, "after bias-correction ", " ")
        )
      }
      if ((!is.null(uppert) |
           !is.null(lowert)) &
          (consecutive & duration == "total")) {
        mes = paste0(
          var,
          ". Calculation of yearly increase in total total number of consecutive days with duration longer than ", duration ," days, ",
          ifelse(
            !is.null(lowert),
            paste0("below threshold of ", lowert),
            paste0("above threshold of ", uppert)
          ),
          ifelse(bias.correction, " after bias-correction", "")
        )
      }
      if ((!is.null(uppert) |
           !is.null(lowert)) & (consecutive & duration == "max")) {
        mes = paste0(
          var,
          ". Calculation of yearly increase in maxium number of consecutive days ",
          ifelse(
            !is.null(lowert),
            paste0("below threshold of ", lowert),
            paste0("above threshold of ", uppert)
          ),
          ifelse(bias.correction, " after bias-correction", "")
        )
      }

      return(mes)
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

  # perform calculation

  perform_calculations <-
    function(datasets,
             var,
             uppert,
             lowert,
             consecutive,
             duration,
             country_shp,
             bias.correction,
             observation,
             intraannual_var,
             season) {
      season_name <-
        paste0(lubridate::month(season[[1]], label = T),
               "-",
               lubridate::month(season[[length(season)]], label = T))

      data_list <- datasets %>%
        dplyr::filter(experiment != "historical") %>%
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
                          models_mbrs = purrr::map(models_mbrs, function(x) {
                            bc <-
                              suppressMessages(
                                downscaleR::biasCorrection(
                                  y = obs[[1]],
                                  x = dplyr::filter(datasets, experiment == "historical")$models_mbrs[[1]],
                                  newdata = x,
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
                              transformeR::intersectGrid.time(x, bc, which.return = 2)
                            mod_temp$Dates$start <- x$Dates$start
                            mod_temp$Dates$end <-  x$Dates$end

                            return(mod_temp)
                          }, .progress = T))
          } else
            .
        }  %>%   # computing annual aggregation. if threshold is specified, first apply threshold
        dplyr::mutate(
          models_agg_y = furrr::future_map(if (!observation)
            models_mbrs
            else
              obs, function(x)
                suppressMessages(
                  transformeR::aggregateGrid(# perform aggregation based on season and output
                    x, aggr.y =
                      if (var == "pr" &
                          !consecutive &
                          (is.null(uppert) & is.null(lowert))) {
                        if (intraannual_var)
                          list(FUN = "sd", na.rm = TRUE)
                        else
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
                ))
        ) %>%
        dplyr::select(-models_mbrs) %>%
        {
          if (!observation) {
            dplyr::mutate(
              .,
              ens_spat =  purrr::map2(models_agg_y, experiment, function(x, y) {
                cli::cli_text(paste0("{cli::symbol$arrow_right}", " Processing ", y))
                c4R <- x
                results <- ens_trends(x)
                c4R$Data <- results[1, ,] # coef
                x$Data <- results[2, ,] # p.value

                coef <-  make_raster(c4R, c(1, 2), country_shp)

                names(coef) <-
                  paste0(y, "_coef", "_", names(coef), "_", season_name)

                p.value <- make_raster(x, c(1, 2), country_shp)
                names(p.value) <-
                  paste0(y, "_p", "_", names(p.value), "_", season_name)

                return(list(coef, p.value))

              }),
              models_spat = purrr::map2(models_agg_y, experiment, function(x, y) {
                cli::cli_text(paste0("{cli::symbol$arrow_right}", " Processing ", y))
                c4R <- x
                results <- models_trends(x)
                c4R$Data <- results[1, , ,]# coef
                x$Data <- results[2, , ,] # p.value
                rst_stack_coef <-
                  lapply(1:dim(c4R$Data)[1], function(i_mod) {
                    c4R$Data <- c4R$Data[i_mod, ,]
                    rst <- make_raster(c4R, c(1, 2), country_shp)
                    names(rst) <-
                      paste0("Member ", i_mod, "_", y, "_coef_" , names(rst),"_", season_name)
                    return(rst)
                  }) %>%  c()

                rst_stack_p <-
                  lapply(1:dim(x$Data)[1], function(i_mod) {
                    x$Data <- x$Data[i_mod, ,]
                    rst <- make_raster(x, c(1, 2), country_shp)
                    names(rst) <-
                      paste0("Member ", i_mod, "_", y, "_p_" , names(rst), "_", season_name)
                    return(rst)
                  }) %>% c()

                return(list(rst_stack_coef , rst_stack_p))

              }),
              models_temp = purrr::map2(models_agg_y, experiment, function(x, y) {
                dimnames(x$Data)[[1]] <- x$Members
                dimnames(x$Data)[[2]] <- x$Dates$start
                dimnames(x$Data)[[3]] <- x$xyCoords$y
                dimnames(x$Data)[[4]] <- x$xyCoords$x

                df <- reshape2::melt(x$Data) %>%
                  dplyr::mutate(date = as.Date(Var2)) %>%
                  dplyr::mutate(experiment = y) %>%
                  dplyr::mutate(season=season_name)
                return(df)

              })
            )


          } else {
            # if observation is TRUE
            dplyr::slice(., 1) %>%
              dplyr::mutate(
                models_spat = purrr::map(models_agg_y, function(x) {
                  cli::cli_text("{cli::symbol$arrow_right} Processing observation")
                  c4R <- x
                  results <- models_trends(x, observation = T)
                  c4R$Data <- results[1, ,]# coef
                  x$Data <- results[2, ,] # p.value

                  coef <- make_raster(c4R, c(1, 2), country_shp)
                  names(coef) <-
                    paste0("obs", "_coef_", names(coef), "_", season_name)
                  p.value <- make_raster(x, c(1, 2), country_shp)

                  names(p.value) <-
                    paste0("obs", "_p_", names(p.value),"_", season_name)
                  return(list(coef, p.value))

                }),
                models_temp = purrr::map(models_agg_y, function(x) {
                  dimnames(x$Data)[[1]] <- x$Dates$start
                  dimnames(x$Data)[[2]] <- x$xyCoords$y
                  dimnames(x$Data)[[3]] <- x$xyCoords$x
                  df <- reshape2::melt(x$Data) %>%
                    dplyr::mutate(date = as.Date(Var1)) %>%
                    dplyr::mutate(experiment = "obs") %>%
                    dplyr::mutate(season=season_name)
                  return(df)


                })
              )



          }


        }

      if (!observation) {
        invisible(structure(
          list(
            do.call(c, purrr::map(
              1:nrow(data_list), ~ data_list$ens_spat[[.]][[1]]
            )),
            do.call(c, purrr::map(
              1:nrow(data_list), ~ data_list$ens_spat[[.]][[2]]
            )),
            do.call(c, purrr::map(
              1:nrow(data_list),
              ~ terra::rast(data_list$models_spat[[.]][[1]])
            )),
            do.call(c, purrr::map(
              1:nrow(data_list),
              ~ terra::rast(data_list$models_spat[[.]][[2]])
            )),
            do.call(rbind, purrr::map(
              1:nrow(data_list), ~ data_list$models_temp[[.]]
            ))
          ),
          class = "CAVAanalytics_trends",
          components = list(
            "SpatRaster for trends coefficients (ensemble)",
            "SpatRaster for trends p.values (ensemble)",
            "SpatRaster for trends coefficients (models)",
            "SpatRaster for trends p.values (models)",
            "dataframe for trends (models)"
          )
        ))

      } else {
        # for observation
        invisible(structure(
          list(
            data_list$models_spat[[1]][[1]],
            data_list$models_spat[[1]][[2]],
            data_list$models_temp
          ),
          class = "CAVAanalytics_trends",
          components = list(
            "SpatRaster for trends coefficients (obs)",
            "SpatRaster for trends p.values (obs)",
            "dataframe for trends"
          )
        ))


      }
    } # end of function

  # beginning of code -------------------------------------------------------
  if (class(data) != "CAVAanalytics_list")
    cli::cli_abort(c("x" = "The input data is not the output of CAVAanalytics load_data"))
  # retrieve information
  datasets <- data[[1]]
  country_shp <- data[[2]]
  var <-  datasets$models_mbrs[[1]]$Variable$varName
  dates <- datasets$models_mbrs[[1]]$Dates$start
  dates <- as.Date(dates)
  # calculate the differences between consecutive dates to understand temporal resolution and adjust the window argument in bias correction
  diffs <- diff(dates)

  # check input requirements
  check_inputs(
    data,
    var,
    consecutive,
    observation,
    bias.correction,
    uppert,
    lowert,
    duration,
    intraannual_var,
    season
  )
  # set parallel processing
  future::plan(future::multisession, workers = n.sessions)


  data_list <- purrr::map(season, function(sns) {

    # create message
    mes <-
      create_message(
        var,
        bias.correction,
        uppert,
        lowert,
        consecutive,
        duration,
        observation,
        intraannual_var
      )

    # filter data by season
    datasets <- filter_data_by_season(datasets, season=sns)

    cli::cli_text(
      paste0(
        "{cli::symbol$arrow_right}",
        " trends,",
        ifelse(observation, " observations,", " projections,"),
        " season ",
        glue::glue_collapse(sns, "-"),
        ". ",
        mes
      )
    )

    # perform operations

    data_list <-
      perform_calculations(
        datasets,
        var,
        uppert,
        lowert,
        consecutive,
        duration,
        country_shp,
        bias.correction,
        observation,
        intraannual_var,
        season=sns
      )

    # return results
    return(data_list)

  })

  if (!observation) {
    invisible(structure(
      list(
        terra::rast(lapply(data_list, `[[`, 1)),
        terra::rast(lapply(data_list, `[[`, 2)),
        terra::rast(lapply(data_list, `[[`, 3)),
        terra::rast(lapply(data_list, `[[`, 4)),
        do.call(rbind, lapply(data_list, `[[`, 5))
      ),
      class = "CAVAanalytics_trends",
      components = list(
        "SpatRaster for trends coefficients (ensemble)",
        "SpatRaster for trends p.values (ensemble)",
        "SpatRaster for trends coefficients (models)",
        "SpatRaster for trends p.values (models)",
        "dataframe for trends (models)"
      )
    ))

  } else {
    # for observation
    invisible(structure(
      list(
        terra::rast(lapply(data_list, `[[`, 1)),
        terra::rast(lapply(data_list, `[[`, 2)),
        do.call(rbind, lapply(data_list, `[[`, 3))
      ),
      class = "CAVAanalytics_trends",
      components = list(
        "SpatRaster for trends coefficients (obs)",
        "SpatRaster for trends p.values (obs)",
        "dataframe for trends"
      )
    ))

  }

}
