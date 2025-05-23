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

model_biases <- function(data,
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
  if (class(data) != "CAVAanalytics_list")
    cli::cli_abort(c("x" = "The input data is not the output of CAVAanalytics load_data"))

  # Check inputs using S3 method
  check_inputs.model_biases(
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
  mod.numb <- dim(data[[1]]$models_mbrs[[1]]$Data)[1]
  datasets <- data[[1]]
  country_shp <- data[[2]]
  var <- datasets$models_mbrs[[1]]$Variable$varName
  dates <- datasets$models_mbrs[[1]]$Dates$start
  dates <- as.Date(dates)
  # calculate the differences between consecutive dates to understand temporal resolution and adjust the window argument in bias correction
  diffs <- diff(dates)

  #create plots by season
  data_list <- purrr::map(season, function(sns) {
    mes <- create_message.model_biases(var,
                                       bias.correction,
                                       uppert,
                                       lowert,
                                       consecutive,
                                       duration,
                                       frequency)

    # set parallel processing
    future::plan(future::multisession, workers = n.sessions)

    # filter data by season
    datasets <- filter_data_by_season.model_biases(datasets, season = sns)
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
      perform_calculations.model_biases(
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

  invisible(new_CAVAanalytics_model_biases(
    terra::rast(lapply(data_list, `[[`, 1)),
    terra::rast(lapply(data_list, `[[`, 2)),
    do.call(rbind, lapply(data_list, `[[`, 3))
  ))
}

