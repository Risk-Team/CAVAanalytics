#' Projections analysis
#'
#' Automatically process climate model projections and compute useful statistics.

#' @param data output of load_data
#' @param bias.correction logical
#' @param uppert  numeric of length=1, upper threshold
#' @param lowert numeric of length=1, lower threshold
#' @param season numeric, seasons to select. For example, 1:12
#' @param scaling.type character, default to "additive". Indicates whether to use multiplicative or additive approach for bias correction
#' @param consecutive logical, to use in conjunction with lowert or uppert
#' @param duration character, either "max" or "total".
#' @return list with raster stacks
#'
#' @export
#' @examples
#' load_data(country = "Somalia", variable="tas", years.hist=2000, years.proj=2010,
#'               path.to.data = "CORDEX-CORE", domain="AFR-22") %>%
#' projections(., season = 1:12)


projections <-
  function(data,
           bias.correction = F,
           uppert = NULL,
           lowert = NULL,
           season,
           scaling.type = "additive",
           consecutive = F,
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
          stop("The input data is not the output of CAVAanalytics load_data")
        stopifnot(is.logical(consecutive), is.logical(bias.correction))
        match.arg(duration, c("max", "total"))
        match.arg(scaling.type, c("additive", "multiplicative"))
        if (!is.null(lowert) &
            !is.null(uppert))
          stop("select only one threshold")
        if (!is.null(lowert) |
            !is.null(uppert)) {
          dates <- data[[1]]$models_mbrs[[1]]$Dates$start
          dates <- as.Date(dates)
          # calculate the differences between consecutive dates
          diffs <- diff(dates)
          # check if the differences are equal to 1
          if (any(diffs == 1)) {
          } else {
            stop("Data is monthly or greater, thresholds cannot be calculated. Set as NULL")
          }
        }

        if (consecutive &
            is.null(uppert) &
            is.null(lowert))
          stop("Specify a threshold for which you want to calculate consecutive days")
        if (!any(stringr::str_detect(colnames(data[[1]]), "obs")) &
            isTRUE(bias.correction)) {
          warning("Bias correction cannot be performed, no observational dataset found. Set as F")
          bias.correction = F
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
        else if ((!is.null(uppert) | !is.null(lowert)) & !consecutive) {
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
                  !is.null(lowert)) & (consecutive & duration == "max")) {
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
                  !is.null(lowert)) & (consecutive & duration == "total")) {
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
        datasets %>% dplyr::mutate_at(c("models_mbrs", "obs"),
                                      ~ purrr::map(., ~ transformeR::subsetGrid(., season = season)))
      } else {
        datasets %>% dplyr::mutate_at(c("models_mbrs"),
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
               country_shp,
               scaling.type) {
        gc()
        scaling.type= if (var=="pr") "multiplicative" else scaling.type
        data_list <- datasets %>%
          dplyr::filter(forcing != "historical") %>%
          {
            if (bias.correction) {
              message(
                paste(
                  Sys.time(),
                  " Performing monthly bias correction with the scaling",
                  " method, scaling type ",
                  scaling.type,
                  ", for each model and month separately. This can take a while. Season",
                  glue::glue_collapse(season, "-")
                )
              )
              dplyr::mutate(.,
                            models_mbrs = furrr::future_map(models_mbrs, function(x) {
                              bc <-
                                suppressMessages(
                                  downscaleR::biasCorrection(
                                    y = obs[[1]],
                                    x = dplyr::filter(datasets, forcing == "historical")$models_mbrs[[1]],
                                    newdata = x,
                                    precipitation = ifelse(var=="pr",TRUE,FALSE),
                                    method = "scaling",
                                    scaling.type = scaling.type,
                                    window=c(30,30)
                                  )
                                )

                              mod_temp <-
                                transformeR::intersectGrid.time(x, bc, which.return = 2)
                              mod_temp$Dates$start <- x$Dates$start
                              mod_temp$Dates$end <-  x$Dates$end
                              return(mod_temp)
                            }))
            } else
              .
          }  %>%  # computing annual aggregation. if threshold is specified, first apply threshold
          dplyr::mutate(
            models_agg_y = furrr::future_map(models_mbrs, function(x)
              suppressMessages(
                transformeR::aggregateGrid(# perform aggregation based on seasonended output
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
              ))) %>%
          dplyr::select(-models_mbrs) %>%
          # ensemble mean
          dplyr::mutate(rst_ens_mean = purrr::map2(forcing, models_agg_y, function(x, y) {
            ens <-
              suppressMessages(transformeR::aggregateGrid(y, aggr.mem = list(FUN = "mean", na.rm = TRUE)))
            array_mean <-
              apply(ens$Data, c(2, 3), mean, na.rm = TRUE) # climatology
            ens$Data <- array_mean
            rs <- make_raster(ens) %>%
              raster::crop(., country_shp) %>%
              raster::mask(., country_shp)
            names(rs) <-
              paste0(x, "_", names(rs)) %>%  stringr::str_remove(., "X")
            return(rs)
          }),
          # ensemble SD
          rst_ens_sd = purrr::map2(forcing, models_agg_y, function(x, y) {
            ens <-
              suppressMessages(transformeR::aggregateGrid(y, aggr.mem = list(FUN = "sd", na.rm = TRUE)))
            array_mean <-
              apply(ens$Data, c(2, 3), mean, na.rm = TRUE) # climatology
            ens$Data <- array_mean
            rs <- make_raster(ens) %>%
              raster::crop(., country_shp) %>%
              raster::mask(., country_shp)
            names(rs) <-
              paste0(x, "_", names(rs)) %>%  stringr::str_remove(., "X")
            return(rs)
          }),
          # individual models
          rst_models = purrr::map2(forcing, models_agg_y, function(x, y) {
            rs_list <- purrr::map(1:dim(y$Data)[[1]], function(ens) {
              array_mean <-
                if (length(y$Dates$start) == 1)
                  apply(y$Data[ens, , , ], c(1, 2), mean, na.rm = TRUE)
              else
                apply(y$Data[ens, , , ], c(2, 3), mean, na.rm = TRUE) # climatology per member adjusting by array dimension
              y$Data <- array_mean
              rs <- make_raster(y) %>%
                raster::crop(., country_shp) %>%
                raster::mask(., country_shp)

              names(rs) <-
                paste0("Member ", ens, "_", x, "_", names(rs)) %>%  stringr::str_remove(., "X")
              return(rs)
            })

          })
          )
        gc()
        invisible(structure(
          list(
            raster::stack(data_list$rst_ens_mean),
            raster::stack(data_list$rst_ens_sd),
            raster::stack(purrr::map(
              data_list$rst_models, ~ raster::stack(.x)
            ))
          ),
          class = "CAVAanalytics_projections",
          components = list(
            "raster stack for ensemble mean",
            "raster stack for ensemble sd",
            "raster stack for individual members"
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

    # create message
    mes <-
      create_message(var, bias.correction, uppert, lowert, consecutive, duration)

    # set parallel processing
    future::plan(future::multisession, workers = 2)

    # filter data by season
    datasets <- filter_data_by_season(datasets, season)
    message(Sys.time(),
            " projections, season ",
            glue::glue_collapse(season, "-"),
            ". ",
            mes)

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
        scaling.type
      )
    message(Sys.time(), " Done")
    # return results
    return(data_list)
  }