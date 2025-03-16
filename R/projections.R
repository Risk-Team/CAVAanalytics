#' Analysis of future projections
#'
#' Automatically process climate model projections and compute useful statistics

#' @param data output of load_data
#' @param bias.correction logical
#' @param uppert  numeric of length=1, upper threshold
#' @param lowert numeric of length=1, lower threshold
#' @param season list, containing seasons to select. For example, list(1:6, 7:12)
#' @param consecutive logical, to use in conjunction with lowert or uppert
#' @param duration A parameter that can be set to either "max" or a specific number. It is relevant only when 'consecutive' is set to TRUE. For instance, to calculate the count of consecutive days with Tmax (maximum temperature) above 35°C, lasting for more than 3 days, you can set 'uppert' to 35, 'consecutive' to TRUE, and 'duration' to 3.
#' @param frequency logical value. This parameter is relevant only when 'consecutive' is set to TRUE and 'duration' is not set to "max". For instance, if you want to determine the count of heatwaves, defined as the number of days with Tmax (maximum temperature) exceeding 35°C for a minimum of 3 consecutive days, set 'uppert' to 35, 'consecutive' to TRUE, 'duration' to 3, and 'frequency' to TRUE.
#' @param n.sessions numeric, number of sessions to use, default is one. Parallelization can be useful when multiple scenarios are used (RCPS, SSPs). However, note that parallelizing will increase RAM usage
#' @param method character, bias-correction method to use. One of eqm (Empirical Quantile Mapping), qdm (Quantile Delta Mapping) or scaling. Default to eqm. When using the scaling method, the multiplicative approach is automatically applied only when the variable is precipitation.
#' @param window character, one of none or monthly. Whether bias correction should be applied on a monthly or annual basis. Monthly is the preferred option when performing bias-correction using daily data
#' @importFrom magrittr %>%
#' @return list with SpatRaster. To explore the output run attributes(output)
#'
#' @export

projections <- function(data,
                        bias.correction = F,
                        uppert = NULL,
                        lowert = NULL,
                        season,
                        consecutive = F,
                        frequency = F,
                        n.sessions = 1,
                        duration = "max",
                        method = "eqm",
                        window = "monthly") {
  # Check inputs using S3 method
  check_inputs.projections(data, bias.correction, uppert, lowert, consecutive, duration, season, method, window)

  # retrieve information
  mod.numb <- dim(data[[1]]$models_mbrs[[1]]$Data)[1]
  datasets <- data[[1]]
  country_shp <- data[[2]]
  var <- datasets$models_mbrs[[1]]$Variable$varName
  dates <- datasets$models_mbrs[[1]]$Dates$start
  dates <- as.Date(dates)
  # calculate the differences between consecutive dates
  diffs <- diff(dates)

  # set parallel processing
  future::plan(future::multisession, workers = n.sessions)

  # create plots by season
  data_list <- purrr::map(season, function(sns) {
    mes <- create_message.projections(var, bias.correction, uppert, lowert, consecutive, duration, frequency)

    # filter data by season
    datasets <- filter_data_by_season.default(datasets, season = sns)
    cli::cli_text(
      paste0(
        "{cli::symbol$arrow_right}",
        " projections, season ",
        glue::glue_collapse(sns, "-"),
        ". ",
        mes
      )
    )

    cli::cli_progress_step("Performing calculations")

    # perform calculations
    data_list <- perform_calculations.prj(
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
      window
    )

    cli::cli_progress_done()
    return(data_list)
  })

  invisible(structure(
    list(
      terra::rast(lapply(data_list, `[[`, 1)),
      terra::rast(lapply(data_list, `[[`, 2)),
      terra::rast(lapply(data_list, `[[`, 3)),
      do.call(rbind, lapply(data_list, `[[`, 4))
    ),
    class = "CAVAanalytics_projections",
    components = list(
      "SpatRaster for ensemble mean",
      "SpatRaster for ensemble sd",
      "SpatRaster for individual members",
      "dataframe for annually aggregated data"
    )
  ))
}


