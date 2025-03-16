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
#' @param method character, bias-correction method to use. One of eqm (Empirical Quantile Mapping), qdm (Quantile Delta Mapping) or scaling. Default to eqm. When using the scaling method, the multiplicative approach is automatically applied only when the variable is precipitation.
#' @param window character, one of none or monthly. Whether bias correction should be applied on a monthly or annual basis. Monthly is the preferred option when performing bias-correction using daily data
#' @param percentage logical, whether the climate change signal is to be calculated as relative changes (in percentage). Default to FALSE
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
                                  n.sessions = 1,
                                  method = "eqm",
                                  percentage = F,
                                  window = "monthly") {

  # check input requirements
  check_inputs.climate_change_signal(data, uppert, lowert, consecutive, duration, bias.correction, season, threshold, method, percentage, window)

  # retrieve information
  mod.numb <- dim(data[[1]]$models_mbrs[[1]]$Data)[1]
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
    mes <- create_message.climate_change_signal(var, uppert, lowert, consecutive, duration, bias.correction, frequency, percentage)

    # filter data by season
    datasets <- filter_data_by_season.default(datasets, season = sns)
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
    data_list <- perform_calculations.ccs(
        datasets, mod.numb, var, uppert, lowert, consecutive,
        duration, country_shp, bias.correction, season = sns,
        frequency, threshold, method, percentage, window
    )
    cli::cli_progress_done()

    return(data_list)
  })

  invisible(new_CAVAanalytics_ccs(
    ccs_mean = terra::rast(lapply(data_list, `[[`, 1)),
    ccs_sd = terra::rast(lapply(data_list, `[[`, 2)),
    members_ccs = terra::rast(lapply(data_list, `[[`, 3)),
    agreement = terra::rast(lapply(data_list, `[[`, 4)),
    temporal_data = do.call(rbind, lapply(data_list, `[[`, 5))
  ))
}

