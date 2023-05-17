#' Analysis of long-term trends
#'
#' Compute multivariate linear regression and linear regression through design-based inference

#' @param data output of load_data
#' @param bias.correction logical, whether to perform bias.correction or not
#' @param uppert  numeric of length=1, upper threshold
#' @param lowert numeric of length=1, lower threshold
#' @param season numeric, seasons to select. For example, 1:12
#' @param scaling.type character, default to "additive". Indicates whether to use multiplicative or additive approach for bias correction.
#' @param consecutive logical, to use in conjunction with lowert or uppert
#' @param duration character, either "max" or "total"
#' @param historical logical, whether to visualize trends for the historical period or projections
#' @param interannual_var logical, whether linear regression is applied to annual variability, measured as standard deviation
#' @return list with raster stacks. To explore the output run attributes(output)
#'
#' @export
#' @examples
#' exmp <- load_data(country = "Sudan", variable="pr", years.hist=1995, years.proj=2030:2060,
#' path.to.data = "CORDEX-CORE", path.to.obs="W5E5", domain="AFR-22", aggr.m="sum") %>%
#' trends(., season = 1:12, historical=F, interannual_var=T)
#'

trends = function(data,
                  bias.correction = FALSE,
                  uppert = NULL,
                  lowert = NULL,
                  scaling.type = "additive",
                  season,
                  consecutive = FALSE,
                  duration = "max",
                  interannual_var=FALSE,
                  historical = FALSE) {
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
             scaling.type,
             interannual_var) {
      if (class(data) != "CAVAanalytics_list")
        stop("The input data is not the output of CAVAanalytics load_data")
      stopifnot(
        is.logical(consecutive),
        is.logical(bias.correction),
        is.logical(historical),
        is.logical(interannual_var)
      )
      match.arg(scaling.type, c("additive", "multiplicative"))
      if (var!="pr" & interannual_var) stop("Interannual variability is only meaningful for precipitation")
     if (interannual_var & (!is.null(lowert) | !is.null(uppert))) stop("Thresholds are not meaningful when calculating interannual variaiblity, set as NULL")
     if (interannual_var) {
       dates <- data[[1]]$models_mbrs[[1]]$Dates$start
       dates <- as.Date(dates)
       # calculate the differences between consecutive dates
       diffs <- diff(dates)
       # check if the differences are equal to 1
       if (any(diffs == 1)) {
        stop("Interannual variability requires monthly data. Reload your data specifying sum in aggr.m")
       }
     }
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
          (is.null(uppert)) &
          is.null(lowert))
        stop("Specify a threshold for which you want to calculate consecutive days")
      stopifnot(duration == "max" | duration == "total")
      if (historical) {
        if (!any(stringr::str_detect(colnames(data[[1]]), "obs"))){
          stop("An observational dataset is needed for this option, check your data")} else {
            if(length(unique(stringr::str_extract(data[[1]]$obs[[1]]$Dates$start, "\\d{4}")))< 25)
              stop("To analyse trends, consider having at least 25 years of observations")
          }
        if (bias.correction) {

          stop("Applying bias correction to look at the historical period does not make sense. Set bias.correction as FALSE")
        }
      }
      if (!any(stringr::str_detect(colnames(data[[1]]), "obs")) &
          isTRUE(bias.correction)) {
        warning("Bias correction cannot be performed, no observational dataset found. Set as F")
        bias.correction = F
      }

      if(length(unique(stringr::str_extract(data[[1]]$models_mbrs[[2]]$Dates$start, "\\d{4}")))< 25) {
        if (!historical) {
          stop("To analyse trends, consider having at least 25 years of data")
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
             interannual_var) {
      if (is.null(uppert) & is.null(lowert)) {
        mes = paste0(
          "Calculation of yearly increase in ",
          if(var == "pr") ifelse(interannual_var,"interannual variability ", "total ") else "mean ",
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
           !is.null(lowert)) & (consecutive & duration == "total")) {
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
      datasets %>% dplyr::mutate_at(c("models_mbrs", "obs"), ~ purrr::map(., ~ transformeR::subsetGrid(., season = season)))
    } else {
      datasets %>% dplyr::mutate_at(c("models_mbrs"), ~ purrr::map(., ~ transformeR::subsetGrid(., season = season)))
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
             scaling.type,
             historical,
             interannual_var) {
      gc()
      scaling.type= if (var=="pr") "multiplicative" else scaling.type
      data_list <- datasets %>%
        dplyr::filter(forcing != "historical") %>%
        {
          if (bias.correction) {
            message(
              paste(
                Sys.time(),
                " Performing bias correction with the scaling",
                " method, scaling type ", scaling.type, ", for each model and month separately. This can take a while. Season",
                glue::glue_collapse(season, "-")
              )
            )
            dplyr::mutate(.,
                          models_mbrs = furrr::future_map(models_mbrs, function(x) {
                            bc <-
                              suppressMessages(downscaleR::biasCorrection(
                                y = obs[[1]],
                                x = dplyr::filter(datasets, forcing == "historical")$models_mbrs[[1]],
                                newdata = x,
                                precipitation = ifelse(var=="pr",TRUE, FALSE),
                                method = "scaling",
                                scaling.type = scaling.type,
                                window = c(30,30)
                              ))

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
          models_agg_y = furrr::future_map(if (!historical) models_mbrs else obs, function(x)
            suppressMessages(transformeR::aggregateGrid(# perform aggregation based on season and output
              x, aggr.y =
                if (var == "pr" &
                    !consecutive &
                    (is.null(uppert) & is.null(lowert))) {
                  if (interannual_var) list(FUN = "sd") else list(FUN = "sum")
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
          )) %>%
        dplyr::select(-models_mbrs) %>%
        {
          if (!historical) {
            dplyr::mutate(.,
                          ens_spat =  purrr::map2(models_agg_y,forcing, function(x,y) {
                            message(Sys.time(), " Processing ", y)
                            c4R <- x
                            results <- ens_trends(x)
                            c4R$Data <- results[1,,]# coef
                            x$Data <- results[2,,] # p.value

                            coef <-  make_raster(c4R) %>%
                              raster::crop(., country_shp) %>%
                              raster::mask(., country_shp)

                            names(coef) <- paste0(y, "_coef", "_", names(coef)) %>%  stringr::str_remove(., "X")

                            p.value <- make_raster(x) %>%
                              raster::crop(., country_shp) %>%
                              raster::mask(., country_shp)

                            names(p.value) <- paste0(y, "_p", "_", names(p.value)) %>%  stringr::str_remove(., "X")

                            return(list(coef, p.value))

                          }),
                          models_spat= purrr::map2(models_agg_y,forcing, function(x,y) {
                            message(Sys.time(), " Processing ", y)
                            c4R <- x
                            results <- models_trends(x)
                            c4R$Data <- results[1,,,]# coef
                            x$Data <- results[2,,,] # p.value
                            rst_stack_coef <- lapply(1:dim(c4R$Data)[1], function(i_mod) {
                              c4R$Data <- c4R$Data[i_mod,,]
                              rst <- make_raster(c4R) %>%
                                raster::crop(., country_shp) %>%
                                raster::mask(., country_shp)
                              names(rst) <-   paste0("Member ", i_mod, "_", y, "_coef_" , names(rst)) %>%  stringr::str_remove(., "X")
                              return(rst)
                            }) %>% raster::stack()

                            rst_stack_p <- lapply(1:dim(x$Data)[1], function(i_mod) {
                              x$Data <- x$Data[i_mod,,]
                              rst <- make_raster(x) %>%
                                raster::crop(., country_shp) %>%
                                raster::mask(., country_shp)
                              names(rst) <-   paste0("Member ", i_mod, "_", y, "_p_" , names(rst)) %>%  stringr::str_remove(., "X")
                              return(rst)
                            }) %>% raster::stack()

                            return(list( rst_stack_coef , rst_stack_p))

                          }),
                          ens_temp=purrr::map2(models_agg_y, forcing, function(x,y) {
                            ens <- suppressMessages(transformeR::aggregateGrid(x, aggr.mem = list(FUN="mean")))
                            dimnames(ens$Data)[[1]] <- ens$Dates$start
                            dimnames(ens$Data)[[2]] <- ens$xyCoords$y
                            dimnames(ens$Data)[[3]] <- ens$xyCoords$x
                            df <- reshape2::melt(ens$Data) %>%
                              dplyr::mutate(date=as.Date(Var1)) %>%
                              dplyr::mutate(forcing=y)
                            return(df)

                          }),
                          models_temp=purrr::map2(models_agg_y, forcing, function(x,y) {
                            dimnames(x$Data)[[1]] <- x$Members
                            dimnames(x$Data)[[2]] <- x$Dates$start
                            dimnames(x$Data)[[3]] <- x$xyCoords$y
                            dimnames(x$Data)[[4]] <- x$xyCoords$x

                            df <- reshape2::melt(x$Data) %>%
                              dplyr::mutate(date=as.Date(Var2)) %>%
                              dplyr::mutate(forcing=y)
                            return(df)

                          }))


          } else { # if historical is TRUE
            dplyr::slice(., 1) %>%
              dplyr::mutate(
                models_spat= purrr::map(models_agg_y, function(x) {
                  message(Sys.time(), " Processing observation")
                  c4R <- x
                  results <- models_trends(x, historical = T)
                  c4R$Data <- results[1,,]# coef
                  x$Data <- results[2,,] # p.value

                  coef <- make_raster(c4R) %>%
                    raster::crop(., country_shp) %>%
                    raster::mask(., country_shp)
                  names(coef) <- paste0("obs", "_coef_", names(coef)) %>%  stringr::str_remove(., "X")
                  p.value <- make_raster(x)%>%
                    raster::crop(., country_shp) %>%
                    raster::mask(., country_shp)

                  names(p.value) <- paste0("obs", "_p_", names(p.value)) %>%  stringr::str_remove(., "X")
                  return(list(coef, p.value))

                }),
                models_temp=purrr::map(models_agg_y, function(x) {
                  dimnames(x$Data)[[1]] <- x$Dates$start
                  dimnames(x$Data)[[2]] <- x$xyCoords$y
                  dimnames(x$Data)[[3]] <- x$xyCoords$x
                  df <- reshape2::melt(x$Data) %>%
                    dplyr::mutate(date=as.Date(Var1)) %>%
                    dplyr::mutate(forcing="obs")
                  return(df)


                }))



          }


        }
      gc()
      if (!historical) {

        invisible(structure(
          list(
            do.call(raster::stack, purrr::map(1:nrow(data_list), ~data_list$ens_spat[[.]][[1]])),
            do.call(raster::stack,purrr::map(1:nrow(data_list), ~data_list$ens_spat[[.]][[2]])),
            do.call(raster::stack,purrr::map(1:nrow(data_list), ~data_list$models_spat[[.]][[1]])),
            do.call(raster::stack,purrr::map(1:nrow(data_list), ~data_list$models_spat[[.]][[2]])),
            do.call(rbind,purrr::map(1:nrow(data_list), ~data_list$ens_temp[[.]])),
            do.call(rbind,purrr::map(1:nrow(data_list), ~data_list$models_temp[[.]]))),
          class = "CAVAanalytics_trends",
          components = list(
            "raster stack for trends coefficients (ensemble)",
            "raster stack for trends p.values (ensemble)",
            "raster stack for trends coefficients (models)",
            "raster stack for trends p.values (models)",
            "dataframe for trends (ensemble)",
            "dataframe for trends (models)"
          )
        ))

      } else {

        invisible(structure(
          list(
            data_list$models_spat[[1]][[1]],
            data_list$models_spat[[1]][[2]],
            data_list$models_temp),
          class = "CAVAanalytics_trends",
          components = list(
            "raster stack for trends coefficients (obs)",
            "raster stack for trends p.values (obs)",
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

  # check input requirements
  check_inputs(data,
               var,
               consecutive,
               historical,
               bias.correction,
               uppert,
               lowert,
               duration,
               scaling.type,
               interannual_var)



  # create message
  mes <-
    create_message(var,
                   bias.correction,
                   uppert,
                   lowert,
                   consecutive,
                   duration,
                   historical,
                   interannual_var)
  # set parallel processing
  future::plan(future::multisession, workers = 1)

  # filter data by season
  datasets <- filter_data_by_season(datasets, season)

  message(
    Sys.time(),
    " trends,",
    ifelse(historical, " observations,", " projections,"),
    " season ",
    glue::glue_collapse(season, "-"),
    ". ",
    mes)

  # perform operations

  data_list <-
    perform_calculations(datasets,
                         var,
                         uppert,
                         lowert,
                         consecutive,
                         duration,
                         country_shp,
                         bias.correction,
                         scaling.type,
                         historical,
                         interannual_var)

  # return results
  return(data_list)

}
