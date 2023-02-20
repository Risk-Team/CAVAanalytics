#' Step two: projection analysis
#'
#' Automatically process climate model projections and compute useful statistics.

#' @param data output of load_data
#' @param bias.correction logical
#' @param uppert  numeric of length=1, upper threshold
#' @param lowert numeric of length=1, lower threshold
#' @param season Numerical, seasons to select. For example, 1:12
#' @param scaling.type character, default to "additive". Indicates whether to use multiplicative or additive approach for bias correction
#' @param consecutive logical, to use in conjunction with lowert or uppert
#' @param duration character, either "max" or "total".
#' @return list with raster stacks
#' @examples
#' load_data(country = "Somalia", variable="tas", years.hist=2000, years.proj=2010,
#'               path.to.data = "CORDEX-CORE", domain="AFR-22") %>%
#' projections(., season = 1:12)
#' @export

projections <- function(data, bias.correction=F, uppert=NULL, lowert=NULL,
                        season, scaling.type="additive", consecutive=F, duration="max") {


# Intermediate functions --------------------------------------------------

# check inputs requirement
  check_inputs <- function(data, bias.correction, uppert, lowert, consecutive, duration) {
    if (class(data)!="cavaR_list") stop("The input data is not the output of cavaR load_data")
    stopifnot(is.logical(consecutive), is.logical(bias.correction))
    match.arg(duration, c("max", "total"))
    match.arg(scaling.type, c("additive", "multiplicative"))
    if (!is.null(lowert) & !is.null(uppert)) stop("select only one threshold")
    if (consecutive & is.null(uppert) & is.null(lowert)) stop("Specify a threshold for which you want to calculate consecutive days")
    if (!any(stringr::str_detect(colnames(data[[1]]),"obs"))& isTRUE(bias.correction)) {
      warning("Bias correction cannot be performed, no observational dataset found. Set as F")
      bias.correction=F
    }
  }

# generate messages on the type of operations being performed
  create_message <- function(var, bias.correction, uppert, lowert, consecutive, duration) {
    if (is.null(uppert) & is.null(lowert)) {
      paste0("Calculation of ", ifelse(var == "pr", "total ", "mean "), ifelse(bias.correction, "bias-corrected ", " "), var)
    }
    else if ((!is.null(uppert) | !is.null(lowert)) & !consecutive) {
      paste0("Calculation of number of days with ", var, ifelse(!is.null(lowert), paste0(" below threshold of ", lowert), paste0(" above threshold of ", uppert)), ifelse(bias.correction, " after bias-correction", ""))
    }
    else if ((!is.null(uppert) | !is.null(lowert)) & (consecutive & duration == "max")) {
      paste0("Calculation of maximum length of consecutive number of days ", ifelse(!is.null(lowert), paste0("below ", lowert), paste0("above ", uppert)), ifelse(bias.correction, " after bias-correction", ""))
    }
    else if ((!is.null(uppert) | !is.null(lowert)) & (consecutive & duration == "total")) {
      paste0(var, ". Calculation of total total number of consecutive days with duration longer than 6 days, ", ifelse(!is.null(lowert), paste0("below threshold of ", lowert), paste0("above threshold of ", uppert)), ifelse(bias.correction, " after bias-correction", ""))
    }
  }

# subset based on a season of interest
  filter_data_by_season <- function(datasets, season) {
    if (any(stringr::str_detect(colnames(datasets), "obs"))) {
      datasets %>% dplyr::mutate_at(c("models_mbrs", "obs"), ~ purrr::map(., ~ transformeR::subsetGrid(., season = season)))
    } else {
      datasets %>% dplyr::mutate_at(c("models_mbrs"), ~ purrr::map(., ~ transformeR::subsetGrid(., season = season)))
    }
  }

# function used to perform the calculations

perform_calculations <- function(datasets, mod.numb, var, bias.correction, uppert, lowert, consecutive, duration, country_shp, scaling.type) {


    data_list <- datasets %>%
      dplyr::filter(forcing != "historical") %>%
      {
        if (bias.correction) {
          message(
            paste(
              Sys.time(),
              " Performing bias correction with the scaling",
              " method, scaling type ", scaling.type, " for each model separately and then calculating the ensemble mean. Season",
              glue::glue_collapse(season, "-")
            )
          )
          dplyr::mutate(.,
                        models_mbrs = furrr::future_map(models_mbrs, function(x) {
                          if (var == "pr") {
                            bc <-
                              suppressMessages(downscaleR::biasCorrection(
                                y = obs[[1]],
                                x = dplyr::filter(datasets, forcing == "historical")$models_mbrs[[1]],
                                newdata = x,
                                precipitation = TRUE,
                                method = "scaling",
                                scaling.type = "multiplicative"
                              ))
                          } else {
                            bc <-
                              suppressMessages(downscaleR::biasCorrection(
                                y = obs[[1]],
                                x = dplyr::filter(datasets, forcing == "historical")$models_mbrs[[1]],
                                newdata = x,
                                precipitation = FALSE,
                                method ="scaling",
                                scaling.type = scaling.type
                              ))
                          }
                          out <-
                            transformeR::intersectGrid.time(x, bc, which.return = 2)
                          out$Dates$start <- x$Dates$start
                          out$Dates$end <-  x$Dates$end
                          return(out)
                        }))
        } else
          .
      }  %>%  # computing annual aggregation. if threshold is specified, first apply threshold
      dplyr::mutate(
        models_agg_y = furrr::future_map(models_mbrs, function(x)
          suppressMessages(transformeR::aggregateGrid(# perform aggregation based on seasonended output
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
              }))
        ),
        # ensemble mean
        rst_ens_mean = purrr::map2(forcing, models_agg_y, function(x, y) {
          y <- suppressMessages(transformeR::aggregateGrid(y, aggr.mem = list(FUN = "mean", na.rm = TRUE)))
          array_mean <-
            apply(y$Data, c(2, 3), mean, na.rm = TRUE) # climatology
          y$Data <- array_mean
          rs <- make_raster(y) %>%
            raster::crop(., country_shp, snap = "out") %>%
            raster::mask(., country_shp)
          names(rs) <- paste0(x, "_", names(rs)) %>%  stringr::str_remove(., "X")
          return(rs)
        }),
        # individual models
        rst_models = purrr::map2(forcing, models_agg_y, function(x, y) {
          rs_list <- purrr::map(1:dim(y$Data)[[1]], function(ens) {
            array_mean <-     array_mean <- if (length(y$Dates$start)==1) apply(y$Data[ens,,,], c(1, 2), mean, na.rm = TRUE) else apply(y$Data[ens,,,], c(2, 3), mean, na.rm = TRUE) # climatology per member adjusting by array dimension
            y$Data <- array_mean
            rs <- make_raster(y)%>%
              raster::crop(., country_shp, snap = "out") %>%
              raster::mask(., country_shp)

            names(rs) <- paste0("Member ", ens, "_", x, "_", names(rs)) %>%  stringr::str_remove(., "X")
            return(rs)
          })

        })
      )

    invisible(structure(
      list(raster::stack(data_list$rst_ens_mean), raster::stack(purrr::map(data_list$rst_models, ~ raster::stack(.x)))),
      class = "cavaR_projections",
      components = list("raster stack for ensemble mean", "raster stack for individual members")
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
  mes <- create_message(var, bias.correction, uppert, lowert, consecutive, duration)

  # set parallel processing
  future::plan(future::multisession, workers = 2)

  # filter data by season
  datasets <- filter_data_by_season(datasets, season)
  message(Sys.time(), " projections, season ", glue::glue_collapse(season, "-"), ". ", mes)

  # perform calculations
  data_list <- perform_calculations(datasets, mod.numb, var, bias.correction, uppert, lowert, consecutive, duration, country_shp, scaling.type)

  # return results
  return(data_list)
}




#' Step two: trends analysis
#'
#' Perform linear trends.

#' @param data output of load_data
#' @param bias.correction logical, whether to perform bias.correction or not
#' @param uppert  numeric of length=1, upper threshold
#' @param lowert numeric of length=1, lower threshold
#' @param season Numerical, seasons to select. For example, 1:12
#' @param scaling.type character, default to "additive". Indicates whether to use multiplicative or additive approach for bias correction.
#' @param consecutive logical, to use in conjunction with lowert or uppert
#' @param duration character, either "max" or "total".
#' @param spatial logical, whether to visualize trends for spatially aggregated data or not
#' @param historical logical, whether to visualize trends for the historical period or projections
#' @return list with raster stacks
#' @examples
#' fpath <- system.file("extdata/", package="cavaR")
#' exmp <- load_data(country = "Moldova", variable="hurs", years.hist=2000, years.proj=2010, path.to.data = fpath) %>%
#' projections(., season = 1:12)
#'
#'

trends = function(data,
                  bias.correction=FALSE,
                  uppert = NULL,
                  lowert = NULL,
                  method = "scaling",
                  scaling.type="additive",
                  season,
                  legend_range = NULL,
                  consecutive=FALSE,
                  duration="max",
                  spatial,
                  historical=FALSE) {


# intermediate functions --------------------------------------------------

  # checking inputs requirement, including correct specification of function arguments

  check_inputs <- function(data,consecutive, bias.correction, uppert, lowert, duration, scaling.type) {
  if (class(data)!="cavaR_list") stop("The input data is not the output of cavaR load_data")
  stopifnot(is.logical(consecutive), is.logical(bias.correction), is.logical(spatial), is.logical(historical))
  match.arg(scaling.type, c("additive", "multiplicative"))
  if (!is.null(lowert) &
      !is.null(uppert))
    stop("select only one threshold")
  if (consecutive &
      (is.null(uppert)) &
      is.null(lowert))
    stop("Specify a threshold for which you want to calculate consecutive days")
  stopifnot(duration == "max" | duration == "total")
  if (!any(stringr::str_detect(colnames(data[[1]]),"obs"))& isTRUE(bias.correction)) {
    warning("Bias correction cannot be performed, no observational dataset found. Set as F")
    bias.correction=F
  }
  }

  # generate messages on the type of operations being performed

 create_message <- function(var, bias.correction, uppert, lowert, consecutive, duration, historical) {

    if (is.null(uppert) & is.null(lowert)) {
      mes=paste0(
        "Calculation of yearly increase in ",
        ifelse(var == "pr", "total ", "mean "),
        ifelse(bias.correction, "bias-corrected ", " "),
        var
      )
    }
    if ((!is.null(uppert) | !is.null(lowert)) & !consecutive) {
      mes=paste0(
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
         !is.null(lowert)) & (consecutive & duration == "total")) {
      mes=paste0(
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
      mes=paste0(
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
     datasets %>% dplyr::mutate_at(c("models_mbrs", "obs"), ~ purrr::map(., ~ subsetGrid(., season = season)))
   } else {
     datasets %>% dplyr::mutate_at(c("models_mbrs"), ~ purrr::map(., ~ subsetGrid(., season = season)))
   }
 }

 # beginning of code -------------------------------------------------------

 # check input requirements
 check_inputs(data, consecutive, bias.correction, uppert, lowert, duration)

 # retrieve information
 datasets <- data[[1]]
 country_shp <- data[[2]]
 var <- datasets$models_mbrs[[1]]$Variable$varName

 # create message
 mes <- create_message(var, bias.correction, uppert, lowert, consecutive, duration)

 # filter data by season
 datasets <- filter_data_by_season(datasets, season)

 message(Sys.time(), " trends,", ifelse(historical, " historical period,", "")," season ", glue::glue_collapse(season, "-"), ". ", mes, ifelse(spatial, ". Spatial set as TRUE, trends are calculated by pixel", ". Spatial set as FALSE, spatial average is performed before calculation of trends") )



}


