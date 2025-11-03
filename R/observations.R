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

observations <- function(data,
                         uppert = NULL,
                         lowert = NULL,
                         season,
                         consecutive = F,
                         frequency = F,
                         trends = F,
                         duration = "max") {
  if (class(data) != "CAVAanalytics_list")
    cli::cli_abort(c("x" = "The input data is not the output of CAVAanalytics load_data"))

  # Check inputs using S3 method
  check_inputs.observations(data, uppert, lowert, consecutive, duration, season, trends)

  # retrieve information
  datasets <- data[[1]]
  country_shp <- data[[2]]
  var <- datasets$obs[[1]]$Variable$varName
  dates <- datasets$obs[[1]]$Dates$start
  dates <- as.Date(dates)
  # calculate the differences between consecutive dates to understand temporal resolution
  diffs <- diff(dates)

  # create message
  data_list <- purrr::map(season, function(sns) {
    mes <- create_message.observations(var, uppert, lowert, consecutive, duration, frequency, trends)

    # filter data by season
    datasets <- filter_data_by_season.observations(datasets, season = sns)
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
    data_list <- perform_calculations.obs(
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
    invisible(new_CAVAanalytics_observations(
      spatraster_data = terra::rast(lapply(data_list, `[[`, 1)),
      annual_data = do.call(rbind, lapply(data_list, `[[`, 2)),
      trends = FALSE
    ))
  } else {
    # when linear regression is applied
    invisible(new_CAVAanalytics_observations(
      spatraster_data = terra::rast(lapply(data_list, `[[`, 1)),
      pvalues = terra::rast(lapply(data_list, `[[`, 2)),
      annual_data = do.call(rbind, lapply(data_list, `[[`, 3)),
      trends = TRUE
    ))
  }
}


