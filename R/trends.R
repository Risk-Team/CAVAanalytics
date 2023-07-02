#' Analysis of long-term trends
#'
#' Compute multivariate linear regression and linear regression through design-based inference

#' @param data output of load_data
#' @param bias.correction logical, whether to perform bias.correction or not
#' @param uppert  numeric of length=1, upper threshold
#' @param lowert numeric of length=1, lower threshold
#' @param season numeric, seasons to select. For example, 1:12
#' @param consecutive logical, to use in conjunction with lowert or uppert
#' @param duration character, either "max" or "total"
#' @param historical logical, whether to visualize trends for the historical period or projections
#' @param intraannual_var logical, whether linear regression is applied to annual variability, measured as standard deviation
#' @param n.sessions numeric, number of sessions to use, default is one. Parallelisation can be useful when multiple scenarios are used (RCPS, SSPs). However, note that parallelising will increase RAM usage
#' @importFrom magrittr %>%
#' @return list with SpatRaster. To explore the output run attributes(output)
#'
#' @export
#' @examples
#' exmp <- load_data(country = "Togo", variable="pr", years.hist=1995, years.proj=2030:2060,
#' path.to.data = "CORDEX-CORE", path.to.obs="W5E5", domain="AFR-22", aggr.m="sum") %>%
#' trends(., season = 1:12, historical=F, intraannual_var=F)
#'

trends = function(data,
                  bias.correction = FALSE,
                  uppert = NULL,
                  lowert = NULL,
                  season,
                  consecutive = FALSE,
                  duration = "max",
                  intraannual_var = FALSE,
                  historical = FALSE,
                  n.sessions = 1) {
  # intermediate functions --------------------------------------------------

  # checking inputs requirement, including correct specification of function arguments

  check_inputs <-
    function(data,
             var,
             consecutive,
             historical,
             bias.correction,
             uppert,
             lowert,
             duration,
             intraannual_var) {
      if (class(data) != "CAVAanalytics_list")
        cli::cli_abort(c("x" = "The input data is not the output of CAVAanalytics load_data"))
      stopifnot(
        is.logical(consecutive),
        is.logical(bias.correction),
        is.logical(historical),
        is.logical(intraannual_var)
      )

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
          (is.null(uppert)) &
          is.null(lowert))
        cli::cli_abort(c("x" = "Specify a threshold for which you want to calculate consecutive days"))
      stopifnot(duration == "max" | duration == "total")
      if (historical) {
        if (!any(stringr::str_detect(colnames(data[[1]]), "obs"))) {
          cli::cli_abort(c("x" = "An observational dataset is needed for this option, check your data"))
        } else {
          if (length(unique(
            stringr::str_extract(data[[1]]$obs[[1]]$Dates$start, "\\d{4}")
          )) < 25)
            cli::cli_abort(c("x" = "To analyse trends, consider having at least 25 years of observations"))
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

      if (length(unique(
        stringr::str_extract(data[[1]]$models_mbrs[[2]]$Dates$start, "\\d{4}")
      )) < 25) {
        if (!historical) {
          cli::cli_abort(c("x" = "To analyse trends, consider having at least 25 years of data"))
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
             historical,
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
          ". Calculation of yearly increase in total total number of consecutive days with duration longer than 6 days, ",
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
    if (any(stringr::str_detect(colnames(datasets), "obs"))) {
      datasets %>%  dplyr::mutate_at(c("models_mbrs", "obs"),
                                     ~ purrr::map(., ~ transformeR::subsetGrid(., season = season)))
    } else {
      datasets %>%  dplyr::mutate_at(c("models_mbrs"),
                                     ~ purrr::map(., ~ transformeR::subsetGrid(., season = season)))
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
             historical,
             intraannual_var) {
      gc()

      data_list <- datasets %>%
        dplyr::filter(experiment != "historical") %>%
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
                          models_mbrs = furrr::future_map(models_mbrs, function(x) {
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

                            out <-
                              transformeR::intersectGrid.time(x, bc, which.return = 2)
                            out$Dates$start <- x$Dates$start
                            out$Dates$end <-  x$Dates$end
                            return(out)
                          }))
          } else
            .
        }  %>%   # computing annual aggregation. if threshold is specified, first apply threshold
        dplyr::mutate(
          models_agg_y = furrr::future_map(if (!historical)
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
                          list(FUN = "sd")
                        else
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
                ))
        ) %>%
        dplyr::select(-models_mbrs) %>%
        {
          if (!historical) {
            dplyr::mutate(
              .,
              ens_spat =  purrr::map2(models_agg_y, experiment, function(x, y) {
                cli::cli_alert_info(paste0("Processing ", y))
                c4R <- x
                results <- ens_trends(x)
                c4R$Data <- results[1, , ] # coef
                x$Data <- results[2, , ] # p.value

                coef <-  make_raster(c4R) %>%
                  terra::crop(., country_shp, snap = "near") %>%
                  terra::mask(., country_shp)

                names(coef) <-
                  paste0(y, "_coef", "_", names(coef))

                p.value <- make_raster(x) %>%
                  terra::crop(., country_shp, snap = "near") %>%
                  terra::mask(., country_shp)

                names(p.value) <-
                  paste0(y, "_p", "_", names(p.value))

                return(list(coef, p.value))

              }),
              models_spat = purrr::map2(models_agg_y, experiment, function(x, y) {
                cli::cli_alert_info(paste0(" Processing ", y))
                c4R <- x
                results <- models_trends(x)
                c4R$Data <- results[1, , , ]# coef
                x$Data <- results[2, , , ] # p.value
                rst_stack_coef <-
                  lapply(1:dim(c4R$Data)[1], function(i_mod) {
                    c4R$Data <- c4R$Data[i_mod, , ]
                    rst <- make_raster(c4R) %>%
                      terra::crop(., country_shp, snap = "near") %>%
                      terra::mask(., country_shp)
                    names(rst) <-
                      paste0("Member ", i_mod, "_", y, "_coef_" , names(rst))
                    return(rst)
                  }) %>%  c()

                rst_stack_p <-
                  lapply(1:dim(x$Data)[1], function(i_mod) {
                    x$Data <- x$Data[i_mod, , ]
                    rst <- make_raster(x) %>%
                      terra::crop(., country_shp, snap = "near") %>%
                      terra::mask(., country_shp)
                    names(rst) <-
                      paste0("Member ", i_mod, "_", y, "_p_" , names(rst))
                    return(rst)
                  }) %>% c()

                return(list(rst_stack_coef , rst_stack_p))

              }),
              ens_temp = purrr::map2(models_agg_y, experiment, function(x, y) {
                ens <-
                  suppressMessages(transformeR::aggregateGrid(x, aggr.mem = list(FUN = "mean")))
                dimnames(ens$Data)[[1]] <-
                  ens$Dates$start
                dimnames(ens$Data)[[2]] <-
                  ens$xyCoords$y
                dimnames(ens$Data)[[3]] <-
                  ens$xyCoords$x
                df <- reshape2::melt(ens$Data) %>%
                  dplyr::mutate(date = as.Date(Var1)) %>%
                  dplyr::mutate(experiment = y)
                return(df)

              }),
              models_temp = purrr::map2(models_agg_y, experiment, function(x, y) {
                dimnames(x$Data)[[1]] <- x$Members
                dimnames(x$Data)[[2]] <- x$Dates$start
                dimnames(x$Data)[[3]] <- x$xyCoords$y
                dimnames(x$Data)[[4]] <- x$xyCoords$x

                df <- reshape2::melt(x$Data) %>%
                  dplyr::mutate(date = as.Date(Var2)) %>%
                  dplyr::mutate(experiment = y)
                return(df)

              })
            )


          } else {
            # if historical is TRUE
            dplyr::slice(., 1) %>%
              dplyr::mutate(
                models_spat = purrr::map(models_agg_y, function(x) {
                  cli::cli_alert_info(" Processing observation")
                  c4R <- x
                  results <- models_trends(x, historical = T)
                  c4R$Data <- results[1, , ]# coef
                  x$Data <- results[2, , ] # p.value

                  coef <- make_raster(c4R) %>%
                    terra::crop(., country_shp, snap = "near") %>%
                    terra::mask(., country_shp)
                  names(coef) <-
                    paste0("obs", "_coef_", names(coef))
                  p.value <- make_raster(x) %>%
                    terra::crop(., country_shp, snap = "near") %>%
                    terra::mask(., country_shp)

                  names(p.value) <-
                    paste0("obs", "_p_", names(p.value))
                  return(list(coef, p.value))

                }),
                models_temp = purrr::map(models_agg_y, function(x) {
                  dimnames(x$Data)[[1]] <- x$Dates$start
                  dimnames(x$Data)[[2]] <- x$xyCoords$y
                  dimnames(x$Data)[[3]] <- x$xyCoords$x
                  df <- reshape2::melt(x$Data) %>%
                    dplyr::mutate(date = as.Date(Var1)) %>%
                    dplyr::mutate(experiment = "obs")
                  return(df)


                })
              )



          }


        }
      gc()
      if (!historical) {
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
              1:nrow(data_list), ~ data_list$ens_temp[[.]]
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
            "dataframe for trends (ensemble)",
            "dataframe for trends (models)"
          )
        ))

      } else { # for historical
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
            "dataframe"
          )
        ))


      }



    } # end of function

  # beginning of code -------------------------------------------------------
  # retrieve information
  datasets <- data[[1]]
  country_shp <- data[[2]]
  var <- datasets$models_mbrs[[1]]$Variable$varName
  dates <-  datasets$models_mbrs[[1]]$Dates$start
  dates <- as.Date(dates)
  # calculate the differences between consecutive dates to understand temporal resolution and adjust the window argument in bias correction
  diffs <- diff(dates)

  # check input requirements
  check_inputs(
    data,
    var,
    consecutive,
    historical,
    bias.correction,
    uppert,
    lowert,
    duration,
    intraannual_var
  )



  # create message
  mes <-
    create_message(
      var,
      bias.correction,
      uppert,
      lowert,
      consecutive,
      duration,
      historical,
      intraannual_var
    )
  # set parallel processing
  future::plan(future::multisession, workers = n.sessions)

  # filter data by season
  datasets <- filter_data_by_season(datasets, season)

  cli::cli_alert_info(paste0(
    " trends,",
    ifelse(historical, " observations,", " projections,"),
    " season ",
    glue::glue_collapse(season, "-"),
    ". ",
    mes
  ))

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
      historical,
      intraannual_var
    )

  # return results
  return(data_list)

}
