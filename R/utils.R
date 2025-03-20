# Loading model paths ------------------------------------------------------
#' Load model paths from thredds
#' @noRd
load_model_paths.thredds <- function(domain, years.hist, years.proj) {
  cli::cli_progress_step("Accessing inventory")
  csv_url <- "https://hub.ipcc.ifca.es/thredds/fileServer/inventories/cava.csv"
  data <- tryCatch({
    read.csv(url(csv_url)) %>%
      dplyr::filter(stringr::str_detect(activity, "CORDEX"),
                    domain %in% !!domain) %>%
      dplyr::group_by(experiment) %>%
      dplyr::summarise(path = list(as.character(location))) %>%
      {
        if (is.null(years.hist) & !is.null(years.proj)) {
          dplyr::filter(., experiment != "historical")
        } else if (!is.null(years.hist) & is.null(years.proj)) {
          dplyr::filter(., experiment == "historical")
        } else {
          .
        }
      } %>%
      dplyr::select(path)
  }, error = function(e) {
    cli::cli_abort(
      c("x" = "Error establishing connection with servers. If the issue persist, flag it on our GitHub repo at https://github.com/Risk-Team/CAVAanalytics/issues")
    )
  })
  return(data[[1]])
}

#' Load model paths from hub
#' @noRd
load_model_paths.hub <- function(domain, years.hist, years.proj) {
  cli::cli_progress_step("Accessing inventory")
  csv_url <- "/home/jovyan/shared/inventories/cava/inventory.csv"
  data <- read.csv(csv_url) %>%
    dplyr::filter(stringr::str_detect(activity, "CORDEX"), domain == domain) %>%
    dplyr::group_by(experiment) %>%
    dplyr::summarise(path = list(as.character(hub))) %>%
    {
      if (is.null(years.hist) & !is.null(years.proj)) {
        dplyr::filter(., experiment != "historical")
      } else if (!is.null(years.hist) & is.null(years.proj)) {
        dplyr::filter(., experiment == "historical")
      } else {
        .
      }
    } %>%
    dplyr::select(path)
  return(data[[1]])
}

#' Load model paths from local directory
#' @noRd
load_model_paths.local <- function(path.to.data) {
  list.dirs(path.to.data, full.names = TRUE)[-1] %>%
    purrr::map(., ~ list.files(.x, full.names = TRUE))
}


#' Load observation paths from thredds
#' @noRd
load_obs_paths.thredds <- function(path.to.obs) {
  if (path.to.obs == "ERA5") {
    "https://hub.ipcc.ifca.es/thredds/dodsC/fao/observations/ERA5/0.25/ERA5_025.ncml"
  } else if (path.to.obs == "W5E5") {
    "https://hub.ipcc.ifca.es/thredds/dodsC/fao/observations/aggregations/W5E5/v2.0/w5e5_v2.0.ncml"
  } else {
    path.to.obs
  }
}

#' Load observation paths from hub
#' @noRd
load_obs_paths.hub <- function(path.to.obs) {
  if (path.to.obs == "ERA5") {
    "/home/jovyan/shared/data/observations/ERA5/0.25/ERA5_025.ncml"
  } else if (path.to.obs == "W5E5") {
    "/home/jovyan/shared/data/observations/W5E5/v2.0/w5e5_v2.0.ncml"
  } else {
    path.to.obs
  }
}


# Checking inputs ---------------------------------------------------------

#' Check inputs for thredds data loading
#' @noRd
check_inputs.load_data <-  function(path.to.data,
                                    years.hist,
                                    domain,
                                    years.proj,
                                    variable,
                                    aggr.m,
                                    n.sessions,
                                    path.to.obs,
                                    years.obs) {
  stopifnot(is.numeric(n.sessions))
  match.arg(aggr.m, choices = c("none", "sum", "mean"))
  if (!is.null(domain)) {
    match.arg(
      domain,
      choices = c(
        "AFR-22",
        "SEA-22",
        "AUS-22",
        "EAS-22",
        "CAM-22",
        "SAM-22",
        "WAS-22",
        "CAM-22",
        "SAM-22",
        "NAM-22",
        "EUR-22",
        "CAS-22"
      )
    )

  }
  if (is.null(years.proj) &
      is.null(years.hist) & is.null(years.obs))
    cli::cli_abort(c("x" = "select at least one of years.hist, years.proj or years.obs"))

  if (missing(variable))
    cli::cli_abort(c("x" = "argument variable as no default"))

  if (!is.null(path.to.obs)) {
    if (!is.null(years.obs) & path.to.obs == "ERA5" &
        any(!(years.obs %in% 1976:2021)))
      cli::cli_abort(c("x" = "Available years for ERA5 observations are 1976:2021"))

    if (!is.null(years.obs) & path.to.obs == "W5E5" &
        any(!(years.obs %in% 1980:2019)))
      cli::cli_abort(c("x" = "Available years for W5E5 observations are 1980:2019"))

    if (is.null(years.obs) & is.null(years.hist))
      cli::cli_abort(c("x" = "Specify years.hist or years.obs since path.obs is not NULL"))

  }

  if (!is.null(path.to.data) &&
      path.to.data == "CORDEX-CORE") {
    if (is.null(domain))
      cli::cli_abort(c("x" = "domain has no default when uploading CORDEX-CORE data remotely"))
    if (is.null(years.proj) &
        is.null(years.hist) & !is.null(years.obs))
      cli::cli_abort(c("x" = "set path.to.data as NULL to only upload observations"))

    if (!is.null(years.hist) & !is.null(years.obs))
      cli::cli_alert_warning(c("!" = "years.obs overwrite years.hist for the observational dataset"))

    if (is.null(years.hist) &
        !is.null(years.obs) & !is.null(years.proj))
      cli::cli_abort(
        c("x" = "If you are loading observations and projections, also specify an historical period")
      )

    if (any(!(years.hist %in% 1976:2005)))
      cli::cli_abort(c("x" = "Available years for the CORDEX historical simulation are 1976:2005"))


    if (any(!(years.proj %in% 2006:2099)))
      cli::cli_abort(c("x" = "Available years for projections are 2006:2099"))

    variable <-
      match.arg(variable,
                choices = c("tas", "tasmax", "tasmin", "hurs", "rsds", "sfcWind", "pr"))

  } else if (is.null(path.to.data)) {
    cli::cli_alert_warning(c("!" = "Only observations will be uploaded"))
  } else {
    if (!is.null(domain))
      cli::cli_alert_warning(c("!" = "Argument domain is ignored"))
    if (!stringr::str_detect(path.to.data, "/"))
      cli::cli_abort(c("x" = "please specify a valid path or CORDEX-CORE for remote upload"))
    if (!any(stringr::str_detect(list.files(path.to.data), "historical")) &
        is.null(years.hist)) {
      cli::cli_alert_warning(
        c("!" = "Historical experiment not found. If present, the folder needs to be named historical")
      )
    } else if (!any(stringr::str_detect(list.files(path.to.data), "historical")) &
               !is.null(years.hist)) {
      cli::cli_abort(c("x" = "Historical experiment not found. The folder needs to be named historical"))
    } else if (any(stringr::str_detect(list.files(path.to.data), "historical")) &
               is.null(years.hist)) {
      cli::cli_abort(c("x" = "Historical experiment found but years.hist is not specified"))
    } else {
      cli::cli_alert_info(c(
        "Your directory contains the following folders:\n",
        paste0(list.dirs(path.to.data)[-1], "\n")
      ))
    }
  }


}

#' Check inputs for hub data loading
#' @noRd
check_inputs.load_data_hub <- function(database,
                                       years.hist,
                                       domain,
                                       years.proj,
                                       variable,
                                       aggr.m,
                                       n.sessions,
                                       path.to.obs,
                                       years.obs) {
  stopifnot(is.numeric(n.sessions))
  match.arg(aggr.m, choices = c("none", "sum", "mean"))
  if (!is.null(domain)) {
    match.arg(
      domain,
      choices = c(
        "AFR-22",
        "SEA-22",
        "AUS-22",
        "EAS-22",
        "CAM-22",
        "SAM-22",
        "WAS-22",
        "CAM-22",
        "SAM-22",
        "NAM-22",
        "EUR-22",
        "CAS-22"
      )
    )

  }
  if (is.null(years.proj) &
      is.null(years.hist) & is.null(years.obs))
    cli::cli_abort(c("x" = "select at least one of years.hist, years.proj or years.obs"))

  if (missing(variable))
    cli::cli_abort(c("x" = "argument variable as no default"))

  if (!is.null(path.to.obs)) {
    if (!is.null(years.obs) & path.to.obs == "ERA5" &
        any(!(years.obs %in% 1976:2021)))
      cli::cli_abort(c("x" = "Available years for ERA5 observations are 1976:2021"))

    if (!is.null(years.obs) & path.to.obs == "W5E5" &
        any(!(years.obs %in% 1980:2021)))
      cli::cli_abort(c("x" = "Available years for W5E5 observations are 1980:2019"))

    if (is.null(years.obs) & is.null(years.hist))
      cli::cli_abort(c("x" = "Specify years.hist or years.obs since path.obs is not NULL"))

  }

  if (!is.null(database) &&
      database == "CORDEX-CORE") {
    if (is.null(domain))
      cli::cli_abort(c("x" = "domain has no default when uploading CORDEX-CORE data remotely"))
    if (is.null(years.proj) &
        is.null(years.hist) & !is.null(years.obs))
      cli::cli_abort(c("x" = "set database as NULL to only upload observations"))

    if (!is.null(years.hist) & !is.null(years.obs))
      cli::cli_alert_warning(c("!" = "years.obs overwrite years.hist for the observational dataset"))

    if (is.null(years.hist) &
        !is.null(years.obs) & !is.null(years.proj))
      cli::cli_abort(
        c("x" = "If you are loading observations and projections, also specify an historical period")
      )

    if (any(!(years.hist %in% 1976:2005)))
      cli::cli_abort(c("x" = "Available years for the CORDEX historical simulation are 1976:2005"))


    if (any(!(years.proj %in% 2006:2099)))
      cli::cli_abort(c("x" = "Available years for projections are 2006:2099"))

    variable <-
      match.arg(variable,
                choices = c("tas", "tasmax", "tasmin", "hurs", "rsds", "sfcWind", "pr"))

  } else if (is.null(database)) {
    cli::cli_alert_warning(c("!" = "Only observations will be uploaded"))
  } else {
    if (!is.null(domain))
      cli::cli_alert_warning(c("!" = "Argument domain is ignored"))
    if (!stringr::str_detect(database, "/"))
      cli::cli_abort(c("x" = "please specify a valid path or CORDEX-CORE for remote upload"))
    if (!any(stringr::str_detect(list.files(database), "historical")) &
        is.null(years.hist)) {
      cli::cli_alert_warning(
        c("!" = "Historical experiment not found. If present, the folder needs to be named historical")
      )
    } else if (!any(stringr::str_detect(list.files(database), "historical")) &
               !is.null(years.hist)) {
      cli::cli_abort(c("x" = "Historical experiment not found. The folder needs to be named historical"))
    } else if (any(stringr::str_detect(list.files(database), "historical")) &
               is.null(years.hist)) {
      cli::cli_abort(c("x" = "Historical experiment found but years.hist is not specified"))
    } else {
      cli::cli_alert_info(c(
        "Your directory contains the following folders:\n",
        paste0(list.dirs(database)[-1], "\n")
      ))
    }
  }


}


#' Check inputs for climate change signal analysis
#' @noRd
check_inputs.climate_change_signal <- function(data,
                                               uppert,
                                               lowert,
                                               consecutive,
                                               duration,
                                               bias.correction,
                                               season,
                                               threshold,
                                               method,
                                               percentage,
                                               window) {
  stopifnot(is.logical(consecutive))
  stopifnot(is.logical(percentage))
  stopifnot(is.numeric(threshold), threshold >= 0, threshold <= 1)
  if (!is.list(season))
    cli::cli_abort("season needs to be a list, for example, list(1:3)")
  if (!(method == "eqm" || method == "qdm")) {
    cli::cli_abort("method must be 'eqm' or qdm")
  }
  if (!(window == "none" || window == "monthly")) {
    cli::cli_abort("window must be one of 'none', or 'monthly'")
  }
  if (!(duration == "max" || is.numeric(duration))) {
    cli::cli_abort("duration must be 'max' or a number")
  }
  if (bias.correction & window == "monthly") {
    dates <- data[[1]]$models_mbrs[[1]]$Dates$start
    dates <- as.Date(dates)
    # calculate the differences between consecutive dates
    diffs <- diff(dates)
    # check if the differences are equal to 1
    if (any(diffs == 1)) {

    } else {
      cli::cli_abort(c("x" = "Data is monthly or greater, set window to none"))
    }
  }
  if (!any(stringr::str_detect(colnames(data[[1]]), "obs")) &
      isTRUE(bias.correction)) {
    cli::cli_abort(
      c("x" = "Bias correction cannot be performed, no observational dataset found. Set as F")
    )
  }
  if (!is.null(lowert) &
      !is.null(uppert))
    cli::cli_abort(c("x" = "Specify only one threshold argument"))
  if ((is.null(lowert) &
       is.null(uppert)) & bias.correction & method == "scaling")
    cli::cli_abort(
      c("x" = "Bias correction with the scaling method can change the results of the climate change signal only for the calculation of indicators. Specify lowert or uppert aguments to use this option")
    )
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

  if (bias.correction & window == "monthly") {
    dates <- data[[1]]$models_mbrs[[1]]$Dates$start
    dates <- as.Date(dates)
    # calculate the differences between consecutive dates
    diffs <- diff(dates)
    # check if the differences are equal to 1
    if (any(diffs == 1)) {

    } else {
      cli::cli_abort(c("x" = "Data is monthly or greater, set window to none"))
    }
  }

  if (consecutive &
      is.null(uppert) &
      is.null(lowert))
    cli::cli_abort(c("x" = "Specify a threshold for which you want to calculate consecutive days"))
  if (!any(stringr::str_detect(data[[1]]$experiment, "hist")))
    cli::cli_abort(c("x" = "Please load historical data to use the climate_change_signal function"))

  if (bias.correction) {
    if ((length(data[[1]]$obs[[1]]$xy$x) != length(data[[1]]$models_mbrs[[1]]$xy$x)) |
        (length(data[[1]]$obs[[1]]$xy$y) != length(data[[1]]$models_mbrs[[1]]$xy$y)))  {
      cli::cli_alert_warning(
        "Observation and historical experiment do not have the same spatial resolution. Models will be interpolated to match the observational dataset"
      )
    }

  }
}

#' Check inputs for projections analysis
#' @noRd
check_inputs.projections <-  function(data,
                                      bias.correction,
                                      uppert,
                                      lowert,
                                      consecutive,
                                      duration,
                                      season,
                                      method,
                                      window) {
  if (!is.list(season))
    cli::cli_abort("season needs to be a list, for example, list(1:3)")
  stopifnot(is.logical(consecutive), is.logical(bias.correction))
  if (!(duration == "max" || is.numeric(duration))) {
    cli::cli_abort("duration must be 'max' or a number")
  }
  if (!(window == "none" || window == "monthly")) {
    cli::cli_abort("window must be one of 'none', or 'monthly'")
  }
  if (!(method == "eqm" ||
        method == "qdm" || method == "scaling")) {
    cli::cli_abort("method must be 'eqm', 'qdm' or 'scaling' ")
  }
  if (bias.correction & window == "monthly") {
    dates <- data[[1]]$models_mbrs[[1]]$Dates$start
    dates <- as.Date(dates)
    # calculate the differences between consecutive dates
    diffs <- diff(dates)
    # check if the differences are equal to 1
    if (any(diffs == 1)) {

    } else {
      cli::cli_abort(c("x" = "Data is monthly or greater, set window to none"))
    }
  }
  if (length(data[[1]]$experiment) == 1 &
      data[[1]]$experiment[[1]] == "historical")
    cli::cli_abort(c("x" = "Projections are not part of CAVAanalytics list"))
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
      is.null(uppert) &
      is.null(lowert))
    stop("Specify a threshold for which you want to calculate consecutive days")
  if (!any(stringr::str_detect(colnames(data[[1]]), "obs")) &
      isTRUE(bias.correction)) {
    cli::cli_abort(
      c("x" = "Bias correction cannot be performed, no observational dataset found. Set as F")
    )

  }
  if (bias.correction) {
    if ((length(data[[1]]$obs[[1]]$xy$x) != length(data[[1]]$models_mbrs[[1]]$xy$x)) |
        (length(data[[1]]$obs[[1]]$xy$y) != length(data[[1]]$models_mbrs[[1]]$xy$y))) {
      cli::cli_alert_warning(
        "Observation and historical experiment do not have the same spatial resolution. Models will be interpolated to match the observational dataset"
      )
    }

  }

}


#' Check inputs for model_biases
#' @noRd
check_inputs.model_biases <- function(data,
                                      bias.correction,
                                      uppert,
                                      lowert,
                                      consecutive,
                                      duration,
                                      season,
                                      method,
                                      cross_validation,
                                      window) {
  if (!is.list(season))
    cli::cli_abort("season needs to be a list, for example, list(1:3)")
  stopifnot(is.logical(consecutive), is.logical(bias.correction))
  if (!(duration == "max" || is.numeric(duration))) {
    cli::cli_abort("duration must be 'max' or a number")
  }
  if (!(method == "eqm" ||
        method == "qdm" || method == "scaling")) {
    cli::cli_abort("method must be one of 'eqm', 'qdm' or 'scaling'")
  }
  if (!(cross_validation == "none" ||
        cross_validation == "3fold")) {
    cli::cli_abort("cross_validation must be one of 'none', or '3fold'")
  }
  if (!(window == "none" || window == "monthly")) {
    cli::cli_abort("window must be one of 'none', or 'monthly'")
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

  if (bias.correction & window == "monthly") {
    dates <- data[[1]]$models_mbrs[[1]]$Dates$start
    dates <- as.Date(dates)
    # calculate the differences between consecutive dates
    diffs <- diff(dates)
    # check if the differences are equal to 1
    if (any(diffs == 1)) {

    } else {
      cli::cli_abort(c("x" = "Data is monthly or greater, set window to none"))
    }
  }

  if (consecutive &
      is.null(uppert) &
      is.null(lowert))
    cli::cli_abort("Specify a threshold for which you want to calculate consecutive days")
  if (!any(stringr::str_detect(colnames(data[[1]]), "obs")) |
      !any(stringr::str_detect(data[[1]]$experiment, "historical"))) {
    cli::cli_abort(
      c("x" = "This function requires an observational dataset and the historical experiment to calculate model biases")
    )

  }
  if ((length(data[[1]]$obs[[1]]$xy$x) != length(data[[1]]$models_mbrs[[1]]$xy$x)) |
      (length(data[[1]]$obs[[1]]$xy$y) != length(data[[1]]$models_mbrs[[1]]$xy$y))) {
    cli::cli_alert_warning(
      "Observation and historical experiment do not have the same spatial resolution. Models will be interpolated to match the observational dataset"
    )
  }
}

#' Check inputs for observation
#' @noRd
check_inputs.observations <-  function(data,
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



# Messages ----------------------------------------------------------------

#' Check inputs for different analysis types
#' @noRd
create_message <- function(data, ...) {
  UseMethod("create_message")
}

#' @noRd
create_message.climate_change_signal <-
  function(var,
           uppert,
           lowert,
           consecutive,
           duration,
           bias.correction,
           frequency,
           percentage) {
    if (is.null(uppert) & is.null(lowert)) {
      paste0(
        "Climate change signal for ",
        ifelse(var == "pr", "total ", "mean "),
        var,
        ifelse(percentage, " in %", "")
      )
    }
    else if ((!is.null(uppert) |
              !is.null(lowert)) & !consecutive) {
      paste0(
        "Climate change signal for number of days with ",
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
              !is.null(lowert)) &
             (consecutive & duration == "max")) {
      paste0(
        "Climate change signal for maximum consecutive number of days ",
        ifelse(
          !is.null(lowert),
          paste0("below ", lowert),
          paste0("above ", uppert)
        ),
        ifelse(bias.correction, " after bias-correction", "")
      )
    }
    else if ((!is.null(uppert) |
              !is.null(lowert)) &
             (consecutive & is.numeric(duration))) {
      paste0(
        var,
        ". Climate change signal for",
        ifelse(frequency, " frequency " , " total number "),
        "of days with duration longer than ",
        duration,
        " consecutive days, ",
        ifelse(
          !is.null(lowert),
          paste0("below threshold of ", lowert),
          paste0("above threshold of ", uppert)
        ),
        ifelse(bias.correction, " after bias-correction", "")
      )
    }
  }

#' @noRd
create_message.projections <-
  function(var,
           bias.correction,
           uppert,
           lowert,
           consecutive,
           duration,
           frequency) {
    if (is.null(uppert) & is.null(lowert)) {
      paste0(
        "Calculation of ",
        ifelse(var == "pr", "total ", "mean "),
        ifelse(bias.correction, "bias-corrected ", " "),
        var
      )
    }
    else if ((!is.null(uppert) |
              !is.null(lowert)) & !consecutive) {
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
              !is.null(lowert)) &
             (consecutive & duration == "max")) {
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
              !is.null(lowert)) &
             (consecutive & is.numeric(duration))) {
      paste0(
        var,
        ". Calculation of ",
        ifelse(frequency, "frequency ", "total number "),
        " of days with duration longer than ",
        duration,
        " consecutive days, ",
        ifelse(
          !is.null(lowert),
          paste0("below threshold of ", lowert),
          paste0("above threshold of ", uppert)
        ),
        ifelse(bias.correction, " after bias-correction", "")
      )
    }
  }

#' @noRd
create_message.model_biases <-
  function(var,
           bias.correction,
           uppert,
           lowert,
           consecutive,
           duration,
           frequency) {
    if (is.null(uppert) & is.null(lowert)) {
      paste0(
        "Calculation of model biases for ",
        ifelse(var == "pr", "total ", "mean "),
        ifelse(bias.correction, "bias-corrected ", " "),
        var
      )
    }
    else if ((!is.null(uppert) |
              !is.null(lowert)) & !consecutive) {
      paste0(
        "Calculation of model biases for number of days with ",
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
              !is.null(lowert)) &
             (consecutive & duration == "max")) {
      paste0(
        "Calculation of model biases for maximum length of consecutive number of days ",
        ifelse(
          !is.null(lowert),
          paste0("below ", lowert),
          paste0("above ", uppert)
        ),
        ifelse(bias.correction, " after bias-correction", "")
      )
    }
    else if ((!is.null(uppert) |
              !is.null(lowert)) &
             (consecutive & is.numeric(duration))) {
      paste0(
        var,
        ". Calculation of model biases for ",
        ifelse(frequency, "frequency " , "total number "),
        "of days with duration longer than ",
        duration,
        " consecutive days, ",
        ifelse(
          !is.null(lowert),
          paste0("below threshold of ", lowert),
          paste0("above threshold of ", uppert)
        ),
        ifelse(bias.correction, " after bias-correction", "")
      )
    }
  }

#' @noRd
create_message.observations <-
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



# filtering, localisation and unit conversion---------------------------------------------------------------

#' Get common dates
#' @noRd
common_dates <- function(data) {
  all_dates <- lapply(data, function(x)
    substr(x$Dates$start, 1, 10))
  common_dates <- Reduce(intersect, all_dates)

  data.filt <- lapply(data, function(x) {
    ind <- which(substr(x$Dates$start, 1, 10) %in% common_dates)
    mod <-
      transformeR::subsetDimension(x, dimension = "time", indices = ind)
  })
  return(transformeR::bindGrid(data.filt, dimension = "member"))
}


#' Conversion of Units
#' @noRd
transform_climate_data <- function(data,
                                   variable,
                                   source,
                                   conversion_factor = CONSTANTS$CONVERSION_FACTOR) {
  if (stringr::str_detect(variable, "tas")) {
    data <- suppressMessages(transformeR::gridArithmetics(data, 273.15, operator = "-"))
  } else if (stringr::str_detect(variable, "pr")) {
    factor <- if (source == "ERA5")
      1000
    else
      86400
    data <- suppressMessages(transformeR::gridArithmetics(data, factor, operator = "*"))
  } else if (stringr::str_detect(variable, "rsds")) {
    factor <- if (source == "ERA5")
      86400
    else
      1
    data <- suppressMessages(transformeR::gridArithmetics(data, factor, operator = "/"))
  } else if (stringr::str_detect(variable, "sfc")) {
    data <- suppressMessages(transformeR::gridArithmetics(data, conversion_factor, operator = "*"))
  }

  if (source %in% c("ERA5", "W5E5")) {
    data$Variable$varName <- variable
  }

  data
}

#' Print unit conversion messages
#' @noRd
print_conversion_message <- function(variable, source) {
  if (variable == "pr") {
    cli::cli_text(
      "{cli::symbol$arrow_right} Precipitation data from {source} has been converted into mm/day"
    )
  } else if (stringr::str_detect(variable, "tas")) {
    cli::cli_text(
      "{cli::symbol$arrow_right} Temperature data from {source} has been converted into Celsius"
    )
  } else if (stringr::str_detect(variable, "sfc")) {
    cli::cli_text(
      "{cli::symbol$arrow_right} Wind speed data from {source} has been converted to 2 m level"
    )
  }
}

#' Subset years in CAVAlist
#'
#' Select specific years for projections or observations in a CAVAlist
#'
#' @param CAVAlist output of the load_data or load_data_hub function
#' @param years numeric, years to select
#' @param projections logical, whether years selection should be applied to projections or observations
#' @export

years_selection = function(CAVAlist, years, projections = T) {
  cli::cli_progress_step("Performing calculations")

  if (projections) {
    new_data = CAVAlist[[1]] %>%
      dplyr::mutate(models_mbrs = purrr::map2(models_mbrs, experiment, function(x, y) {
        if (y == "historical") {
          x

        } else {
          transformeR::subsetGrid(x, years = years)

        }


      }))
  } else {
    cli::cli_alert_warning(c("!" = "Years selection is applied to observations"))
    new_data = CAVAlist[[1]] %>%
      dplyr::mutate(obs = map(obs, function(x) {
        transformeR::subsetGrid(x, years = years)

      }))

  }

  CAVAlist[[1]] = new_data
  cli::cli_progress_done()
  return(CAVAlist)

}

#' subset based on a season of interest for future data
#' @noRd
filter_data_by_season.default <- function(datasets, season) {
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
    datasets %>%  dplyr::mutate_at(c("models_mbrs"), ~ purrr::map(., ~ suppressMessages(
      transformeR::subsetGrid(., season = season)
    )))
  }
}

#' subset based on a season of interest for observations
#' @noRd
filter_data_by_season.observations <- function(datasets, season) {
  if (all(season == sort(season))) {

  } else {
    cli::cli_alert_warning(
      "Some data will be lost on year-crossing season subset (see the 'Time slicing' section of subsetGrid documentation for more details)"
    )
  }
  datasets %>%  dplyr::mutate_at(c("obs"), ~ purrr::map(., ~ suppressMessages(
    transformeR::subsetGrid(., season = season)
  )))
}

#' subset based on a season of interest and experiment for model_biases
#' @noRd
filter_data_by_season.model_biases <- function(datasets, season) {
  if (all(season == sort(season))) {

  } else {
    cli::cli_alert_warning(
      "Some data will be lost on year-crossing season subset (see the 'Time slicing' section of subsetGrid documentation for more details)"
    )
  }

  lon <- datasets$models_mbrs[[1]]$xyCoords$x
  lon_obs <- datasets$obs[[1]]$xyCoords$x
  lat <- datasets$models_mbrs[[1]]$xyCoords$y
  lat_obs <- datasets$obs[[1]]$xyCoords$y

  if ((length(lon) != length(lon_obs)) |
      (length(lat) != length(lat_obs))) {
    datasets <- datasets %>%
      dplyr::filter(experiment == "historical") %>%
      dplyr::mutate(models_mbrs = purrr::map(models_mbrs,
                                             ~ suppressMessages(
                                               suppressWarnings(
                                                 transformeR::interpGrid(.x, new.coordinates = transformeR::getGrid(obs[[1]]))
                                               )
                                             )))
  } else {
    cli::cli_alert_warning(
      "Observations and model simulations have the same spatial resolution, proceeding with calculations"
    )
  }
  datasets %>%
    dplyr::filter(experiment == "historical") %>%
    dplyr::mutate_at(c("models_mbrs", "obs"),
                     ~ purrr::map(., ~ suppressMessages(
                       transformeR::subsetGrid(., season = season)
                     )))

}


#' Return xlim and ylim of a country or BBox
#' @noRd
geo_localize <- function(country, xlim, ylim, buffer) {
  if (!is.null(country) & !is.null(xlim)) {
    cli::cli_abort(c("x" = "Either select a country or a region of interest, not both"))
  } else {
    country_shp = if (!is.null(country) & !inherits(country, "sf")) {
      suppressMessages(
        rnaturalearth::ne_countries(
          country = country,
          scale = "medium",
          returnclass = "sf"
        ) %>%
          sf::st_set_crs(., NA)
      )
    } else if (!is.null(country) & inherits(country, "sf")) {
      country %>%
        sf::st_transform("EPSG:4326")

    } else {
      sf::st_bbox(c(
        xmin = min(xlim),
        xmax = max(xlim),
        ymax = max(ylim),
        ymin = min(ylim)
      )) %>%
        sf::st_as_sfc() %>%
        data.frame(geometry = .) %>%
        sf::st_as_sf() %>%
        sf::st_set_crs(., NA)
    }

    xlim <-
      c(sf::st_bbox(country_shp)[1] - buffer,
        sf::st_bbox(country_shp)[3] + buffer)
    ylim <-
      c(sf::st_bbox(country_shp)[2] - buffer,
        sf::st_bbox(country_shp)[4] + buffer)
    return(list(
      xlim = xlim,
      ylim = ylim,
      country_shp = country_shp
    ))
  }
}



# Helper functions for calculating indices --------------------------------------------------------------

#' Consecutive days
#'
#' Calculation of consecutive days. It can be used with aggregateGrid.
#'
#' @param col numeric vector
#' @param duration either "max" or "total".
#' @param lowert numeric. Lower threshold
#' @param uppert numeric. Upper threshold
#' @param frequency logical. Whether frequency or abosulte numbers should be returned. Only works with duration != max
#' @return numeric of length 1
#' @export
# functions for consecutive days

thrs_consec = function(col, duration, lowert, uppert, frequency) {
  if (!is.numeric(col))
    stop("input has to be a numeric vector")

  if (!(duration == "max" || is.numeric(duration))) {
    stop("duration must be 'max' or a number")
  }
  #analyse consecutive days

  if (!is.null(lowert)) {
    consec = rle(col < lowert)

  } else{
    consec = rle(col > uppert)

  }

  if (duration == "max" &
      frequency)
    stop(
      "Not meaningful. By definition, the maximum duration of an event, let's say a dry spell, has frequency of 1"
    )

  #get only connsecutive days matching the threshold

  consec_days = consec$lengths[consec$values == TRUE]

  #return values out

  if (duration == "max") {
    val <- max(consec_days, na.rm = T)
    return(if (val == "-Inf")
      0
      else
        val)

  } else{
    if (!frequency)
      return(sum(consec_days[consec_days > duration], na.rm = T))
    else
      return(length(na.omit(consec_days[consec_days > duration])))

  }

}


#' Calculation of thresholds
#'
#' Calculation of number of days with certain condition. It can be used with aggregateGrid.

#' @param col numeric vector
#' @param lowert numeric. lower threshold
#' @param uppert numeric. upper threshold
#' @return numeric of length 1
#'
#' @export


thrs = function(col, lowert, uppert) {
  if (!is.numeric(col))

    stop("input has to be a numeric vector")

  if (!is.null(lowert)) {
    sum(col < lowert, na.rm = T)

  } else{
    sum(col > uppert, na.rm = T)

  }

}


#' @noRd

agreement = function(array3d, threshold) {
  # Define the inner function find.agreement within the agreement function
  find.agreement = function(x, threshold) {
    # Calculate proportion of models predicting each sign of change (negative(-1), no change(0), positive(+1))
    sign.proportion = c(length(x[x < 0]) / length(x),
                        length(x[x == 0]) / length(x),
                        length(x[x > 0]) / length(x))
    names(sign.proportion) = c("-1", "0", "1")
    # Compare the set threshold to the maximum proportion of models agreeing on any one sign of change
    # If the max proportion is higher than threshold, return 1 (meaning there is agreement in signs among model)
    # Otherwise return 0 (no agreement meeting the set threshold)
    if (max(sign.proportion) > threshold) {
      return(1)
    } else {
      return(0)
    }
  }

  # Apply find.agreement to each element of the array3d over the second and third dimensions
  array1_agreement = apply(array3d, c(2, 3), find.agreement, threshold)
  return(array1_agreement)
}


#' @noRd
models_trends <- function(c4R, observation = F) {
  # Add trend package to imports if not already present
  if (length(dim(c4R$Data)) > 2) {
    # in cases in which there is a spatial dimension
    cli::cli_progress_step(" Calculating Sen's slope and Mann-Kendall test")

    if (dim(c4R$Data)[ifelse(observation, 1, 2)] > 100)
      cli::cli_alert_warning("Check that your performed annual aggregation before using this function")

    ind.trends <-
      apply(c4R$Data, if (observation)
        c(2, 3)
        else
          c(1, 3, 4), function(y) {
            df <- reshape2::melt(y)
            # Use trend package functions
            mk <- trend::mk.test(df$value)
            sen <- trend::sens.slope(df$value)
            return(c(sen$estimates, mk$p.value))
          })

    cli::cli_process_done()
    return(ind.trends)
  } else {
    # when spatial averages are performed
    if (dim(c4R$Data)[ifelse(observation, 1, 2)] > 100)
      cli::cli_alert_warning("Check that your performed annual aggregation before using this function")

    if (!observation) {
      df_tm_series <- reshape2::melt(c4R$Data) %>%
        dplyr::group_by(Var1) %>%
        dplyr::summarise(
          coef = trend::sens.slope(value)$estimates,
          p.value = trend::mk.test(value)$p.value
        ) %>%
        dplyr::mutate(date = seq(
          as.Date(c4R$Dates$start[[1]]),
          as.Date(c4R$Dates$start[[length(c4R$Dates$start)]]),
          by = "year"
        )) %>%
        dplyr::select(Var1, value = coef, date, coef, p.value)

      return(df_tm_series)
    } else {
      values <- c4R$Data
      sen <- trend::sens.slope(values)
      mk <- trend::mk.test(values)

      df_tm_series <- data.frame(
        value = values,
        date = seq(
          as.Date(c4R$Dates$start[[1]]),
          as.Date(c4R$Dates$start[[length(c4R$Dates$start)]]),
          by = "year"
        ),
        coef = sen$estimates,
        p.value = mk$p.value
      )

      return(df_tm_series)
    }
  }
}



# Make raster -----------------------------------------------------------

#' Make a spatRaster from a C4R list
#' @noRd
make_raster <-
  function(cl4.object, dimensions, shape.file, stat = "mean") {
    xmin <-
      if (is.null(cl4.object$xyCoords$lon))
        min(cl4.object$xyCoords$x)
    else
      min(cl4.object$xyCoords$lon[1, ])
    xmax <-
      if (is.null(cl4.object$xyCoords$lon))
        max(cl4.object$xyCoords$x)
    else
      max(cl4.object$xyCoords$lon[1, ])
    ymin <-
      if (is.null(cl4.object$xyCoords$lat))
        min(cl4.object$xyCoords$y)
    else
      min(cl4.object$xyCoords$lat[, 1])
    ymax <-
      if (is.null(cl4.object$xyCoords$lat))
        max(cl4.object$xyCoords$y)
    else
      max(cl4.object$xyCoords$lat[, 1])

    array_mean <-
      apply(cl4.object$Data, dimensions, stat, na.rm = TRUE)


    cl4.object$Data <- array_mean

    rasters <- terra::rast(cl4.object$Data, extent = terra::ext(xmin, xmax, ymin, ymax)) %>%
      terra::flip(., direction = 'vertical') %>%
      terra::crop(., shape.file, snap = "out") %>%
      terra::mask(., shape.file)

    nms <-
      paste0(
        stringr::str_extract(cl4.object$Dates$start[1], "\\d{4}"),
        ".",
        stringr::str_extract(cl4.object$Dates$end[length(cl4.object$Dates$start)], "\\d{4}")
      )
    names(rasters) <-  nms

    return(rasters)

  }
# performing calculations -------------------------------------------------
#' function used to perform the calculations
#' @noRd
perform_calculations.ccs <-
  function(datasets,
           mod.numb,
           var,
           uppert,
           lowert,
           consecutive,
           duration,
           country_shp,
           bias.correction,
           season,
           frequency,
           threshold,
           method,
           percentage,
           window) {
    season_name <-
      convert_vector_to_month_initials(season)
    data_list <- datasets  %>%
      {
        if (bias.correction) {
          cli::cli_text(
            paste(
              "{cli::symbol$arrow_right}",
              " Performing ",
              ifelse(window == "monthly", "monthly", ""),
              " bias correction with the ",
              method,
              " method, for each model separately. This can take a while. Season",
              glue::glue_collapse(season, "-")
            )
          )
          dplyr::mutate(.,
                        models_mbrs = purrr::map2(models_mbrs, experiment, function(mod, forc) {
                          if (forc == "historical") {
                            bc <-
                              suppressMessages(
                                downscaleR::biasCorrection(
                                  y = obs[[1]],
                                  x = mod,
                                  precipitation =  if (var == "pr")
                                    TRUE
                                  else
                                    FALSE,
                                  method = method,
                                  scaling.type = if (var == "pr")
                                    "multiplicative"
                                  else
                                    "additive",
                                  window = if (window == "monthly")
                                    c(30, 30)
                                  else
                                    NULL,
                                  extrapolation = "constant"
                                )
                              )
                          } else {
                            bc <-
                              suppressMessages(
                                downscaleR::biasCorrection(
                                  y = obs[[1]],
                                  x = dplyr::filter(datasets, experiment == "historical")$models_mbrs[[1]],
                                  newdata = mod,
                                  precipitation = ifelse(var == "pr", TRUE, FALSE),
                                  method = method,
                                  window = if (any(diffs == 1))
                                    c(30, 30)
                                  else
                                    c(1, 1),
                                  extrapolation = "constant"
                                )
                              )

                          }

                          mod_temp <-
                            transformeR::intersectGrid.time(mod, bc, which.return = 2)
                          mod_temp$Dates$start <-
                            mod$Dates$start
                          mod_temp$Dates$end <-  mod$Dates$end

                          return(mod_temp)
                        }, .progress = T))
        } else
          .
      }  %>%
      # computing annual aggregation. if threshold is specified, first apply threshold
      dplyr::mutate(models_agg_y = furrr::future_map(models_mbrs, function(x)
        suppressMessages(
          transformeR::aggregateGrid(# perform aggregation based on season output
            x, aggr.y =
              if (var == "pr" &
                  !consecutive &
                  (is.null(uppert) & is.null(lowert))) {
                list(FUN = "sum", na.rm = TRUE)
              } else if (var != "pr" &
                         !consecutive &
                         (is.null(lowert) & is.null(uppert))) {
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
        )))  %>%
      dplyr::mutate(models_agg_tot = purrr::map(models_agg_y, function(x)  {
        suppressMessages(x %>%
                           transformeR::climatology(.))
      })) %>%
      dplyr::select(-models_mbrs) %>% # calculate climate change signal by subtracting historical data
      dplyr::mutate(
        ccs_mbrs = purrr::map(models_agg_tot, function(y) {
          h <-
            dplyr::filter(., stringr::str_detect(experiment, "hist"))$models_agg_tot[[1]]
          delta <-
            transformeR::gridArithmetics(y, h, operator = "-")

          if (percentage) {
            delta <-
              transformeR::gridArithmetics(delta, h, operator = "/") %>%
              transformeR::gridArithmetics(., 100, operator = "*")
          }

          delta
        }),
        rst_ccs_sign = purrr::map2(experiment, ccs_mbrs, function(x, y) {
          y$Data <- apply(y$Data, c(1, 3, 4), mean)
          arry_sign <-
            agreement(y$Data, threshold)
          y$Data <- arry_sign
          rast_sign <- make_raster(y, c(1, 2), country_shp)
          names(rast_sign) <-
            paste0(x, "_", names(rast_sign), "_", season_name)
          return(rast_sign)
        }),
        rst_ens_mean_ccs = purrr::map2(experiment, ccs_mbrs, function(x, y) {
          ccs_mean <- make_raster(y, c(3, 4), country_shp)
          names(ccs_mean) <-
            paste0(x, "_", names(ccs_mean), "_", season_name)
          return(ccs_mean)
        }),
        rst_ens_sd_ccs = purrr::map2(experiment, ccs_mbrs, function(x, y) {
          ccs_sd <- make_raster(y, c(3, 4), country_shp, stat = "sd")
          names(ccs_sd) <-
            paste0(x, "_", names(ccs_sd), "_", season_name)
          return(ccs_sd)
        }),
        rst_models_ccs = purrr::map2(experiment, ccs_mbrs, function(x, y) {
          rs_list <- purrr::map(1:dim(y$Data)[[1]], function(ens) {
            array_mean <-
              apply(y$Data[ens, , , ], c(1, 2), mean, na.rm = TRUE) # climatology per member adjusting by array dimension

            y$Data <- array_mean

            rs <- make_raster(y, c(1, 2), country_shp)

            names(rs) <-
              paste0("Member ", ens, "_", x, "_", names(rs), "_", season_name)
            return(rs)
          })
        }),
        models_temp_ccs = purrr::map2(experiment, models_agg_y, function(x, y) {
          h <-
            dplyr::filter(., stringr::str_detect(experiment, "hist"))$models_agg_tot[[1]]

          if (stringr::str_detect(x, "hist")) {
            NULL

          } else {
            mbrs = dim(y$Data)[1]
            yrs = dim(y$Data)[2]
            lt = dim(y$Data)[3]
            ln = dim(y$Data)[4]

            h.expanded = array(rep(h$Data, each = yrs), dim = c(mbrs, yrs, lt, ln))

            delta <-
              transformeR::gridArithmetics(y, h.expanded, operator = "-")
            if (percentage) {
              delta <-
                transformeR::gridArithmetics(delta, h.expanded, operator = "/") %>%
                transformeR::gridArithmetics(., 100, operator = "*")
            }

            dimnames(delta$Data)[[1]] <- delta$Members
            dimnames(delta$Data)[[2]] <- delta$Dates$start
            dimnames(delta$Data)[[3]] <- delta$xyCoords$y
            dimnames(delta$Data)[[4]] <- delta$xyCoords$x

            reshape2::melt(delta$Data) %>%
              dplyr::mutate(date = as.Date(Var2)) %>%
              dplyr::mutate(experiment = x) %>%
              dplyr::mutate(season = season_name) %>%
              dplyr::group_by(date, experiment, Var1, season) %>%
              dplyr::summarise(value = median(value, na.rm = T)) # spatial aggregation because ccs do not support spatiotemporal

          }
        })
      ) %>%
      dplyr::filter(!stringr::str_detect(experiment, "hist"))

    gc()
    invisible(new_CAVAanalytics_ccs(
      ccs_mean = terra::rast(data_list$rst_ens_mean_ccs),
      ccs_sd = terra::rast(data_list$rst_ens_sd_ccs),
      members_ccs = terra::rast(unlist(data_list$rst_models_ccs)),
      agreement = terra::rast(data_list$rst_ccs_sign),
      temporal_data = do.call(rbind, purrr::map(
        1:nrow(data_list), ~ data_list$models_temp_ccs[[.]]
      ))
    ))
  }



#' function used to perform the calculations
#' @noRd
perform_calculations.obs <-
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
      # Replace direct structure() call with constructor
      invisible(new_CAVAanalytics_observations(
        spatraster_data = data_list$obs_spat[[1]][[1]],
        pvalues = data_list$obs_spat[[1]][[2]],
        annual_data = data_list$obs_temp[[1]],
        trends = TRUE
      ))
    } else {
      # Replace direct structure() call with constructor
      invisible(new_CAVAanalytics_observations(
        spatraster_data = data_list$rst_mean[[1]],
        annual_data = data_list$obs_temp[[1]],
        trends = FALSE
      ))
    }
  }



#' function used to perform the calculations
#' @noRd
perform_calculations.model_biases <-
  function(datasets,
           mod.numb,
           var,
           bias.correction,
           uppert,
           lowert,
           consecutive,
           duration,
           country_shp,
           season,
           frequency,
           method,
           cross_validation,
           window) {
    season_name <-
      convert_vector_to_month_initials(season)
    data_list <- datasets %>%
      {
        if (bias.correction) {
          cli::cli_text(
            paste(
              "{cli::symbol$arrow_right}",
              " Performing ",
              ifelse(window == "monthly", "monthly", ""),
              " bias correction with the ",
              method,
              " method, for each model separately. This can take a while. Season",
              glue::glue_collapse(season, "-"),
              ifelse(
                cross_validation == "3fold",
                ". kfold cross-validation is applied to avoid overfitting",
                ""
              )
            )
          )
          dplyr::mutate(.,
                        models_mbrs = purrr::map(models_mbrs, function(mod) {
                          bc <-
                            suppressMessages(
                              downscaleR::biasCorrection(
                                y = obs[[1]],
                                x = mod,
                                scaling.type = if (var == "pr")
                                  "multiplicative"
                                else
                                  "additive",
                                precipitation = if (var == "pr")
                                  TRUE
                                else
                                  FALSE,
                                method = method,
                                cross.val = if (cross_validation == "3fold")
                                  "kfold"
                                else
                                  "none",
                                folds = 3,
                                window = if (window == "monthly")
                                  c(30, 30)
                                else
                                  NULL,
                                extrapolation = "constant"
                              )
                            )

                          mod_temp <-
                            transformeR::intersectGrid.time(mod, bc, which.return = 2)
                          mod_temp$Dates$start <-
                            mod$Dates$start
                          mod_temp$Dates$end <-  mod$Dates$end

                          return(mod_temp)
                        }, .progress = T))
        } else
          .
      }  %>%   # computing annual aggregation. if threshold is specified, first apply threshold
      dplyr::mutate_at(c("models_mbrs", "obs"),
                       ~ furrr::future_map(., ~
                                             suppressMessages(
                                               transformeR::aggregateGrid(# perform aggregation based on seasonended output
                                                 ., aggr.y =
                                                   if (var == "pr" &
                                                       !consecutive &
                                                       (is.null(uppert) &
                                                        is.null(lowert))) {
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
      dplyr::mutate(rst_obs = purrr::map(obs, function(data_obs) {
        rs <-
          make_raster(data_obs, if (length(data_obs$Dates$start) == 1)
            c(1, 2)
            else
              c(2, 3), country_shp)
      })) %>%
      # individual models rasters
      dplyr::mutate(rst_models = purrr::map(models_mbrs, function(y) {
        rs_list <- purrr::map(1:dim(y$Data)[[1]], function(ens) {
          array_mean <-
            if (length(y$Dates$start) == 1)
              apply(y$Data[ens, , , ], c(1, 2), mean, na.rm = TRUE)
          else
            apply(y$Data[ens, , , ], c(2, 3), mean, na.rm = TRUE) # climatology per member adjusting by array dimension

          y$Data <- array_mean

          rs <- make_raster(y, c(1, 2), country_shp)

          names(rs) <-
            paste0("Member ",
                   ens,
                   "_historical_",
                   names(rs),
                   "_",
                   season_name)
          return(rs)
        })
      })) %>%
      # biases
      dplyr::mutate(
        rst_mod_biases = purrr::map(rst_models, function(y) {
          biases <- terra::rast(y) - rst_obs[[1]]
          return(biases)
        }),
        rst_ens_biases = purrr::map(rst_mod_biases, function(y) {
          mean_biases <- terra::mean(y)
          names(mean_biases) <-
            stringr::str_remove(names(terra::rast(y))[[1]], "Member \\d+_")
          return(mean_biases)
        }),
        models_temp = purrr::map(models_mbrs, function(x) {
          dimnames(x$Data)[[1]] <- x$Members
          dimnames(x$Data)[[2]] <- x$Dates$start
          dimnames(x$Data)[[3]] <- x$xyCoords$y
          dimnames(x$Data)[[4]] <- x$xyCoords$x

          obs_data <- apply(obs[[1]]$Data, 1, mean, na.rm = T)
          obs_date <- as.Date(obs[[1]]$Dates$start)

          df_obs <-
            data.frame(obs_value = obs_data, date = obs_date)

          df <- suppressMessages(
            reshape2::melt(x$Data) %>%
              dplyr::mutate(date = as.Date(Var2)) %>%
              dplyr::mutate(experiment = "historical") %>%
              dplyr::group_by(Var1, date) %>%
              dplyr::summarise(value = mean(value)) %>%
              dplyr::left_join(., df_obs) %>%
              dplyr::mutate(season = season_name)
          )

          return(df)

        })
      )
    invisible(new_CAVAanalytics_model_biases(
      ensemble_biases = data_list$rst_ens_biases[[1]],
      model_biases = data_list$rst_mod_biases[[1]],
      temporal_biases = data_list$models_temp[[1]]
    ))
  }



#' function used to perform the calculations on prjections
#' @noRd
perform_calculations.projections <- function(datasets,
                                             mod.numb,
                                             var,
                                             bias.correction,
                                             uppert,
                                             lowert,
                                             consecutive,
                                             duration,
                                             country_shp,
                                             season,
                                             frequency,
                                             method,
                                             window) {
  season_name <-
    convert_vector_to_month_initials(season)
  data_list <- datasets %>%
    dplyr::filter(experiment != "historical") %>%
    {
      if (bias.correction) {
        cli::cli_text(
          paste(
            "{cli::symbol$arrow_right}",
            " Performing ",
            ifelse(window == "monthly", "monthly", ""),
            " bias correction with the ",
            method,
            " method, for each model separately. This can take a while. Season",
            glue::glue_collapse(season, "-")
          )
        )
        dplyr::mutate(.,
                      models_mbrs = purrr::map(models_mbrs, function(mod) {
                        bc <-
                          suppressMessages(
                            downscaleR::biasCorrection(
                              y = obs[[1]],
                              x = dplyr::filter(datasets, experiment == "historical")$models_mbrs[[1]],
                              newdata = mod,
                              scaling.type = if (var == "pr")
                                "multiplicative"
                              else
                                "additive",
                              precipitation =  if (var == "pr")
                                TRUE
                              else
                                FALSE,
                              method = method,
                              window =  if (window == "monthly")
                                c(30, 30)
                              else
                                NULL,
                              extrapolation = "constant"
                            )
                          )

                        mod_temp <-
                          transformeR::intersectGrid.time(mod, bc, which.return = 2)
                        mod_temp$Dates$start <-
                          mod$Dates$start
                        mod_temp$Dates$end <-  mod$Dates$end

                        return(mod_temp)
                      }, .progress = T))
      } else
        .
    }  %>%   # computing annual aggregation. if threshold is specified, first apply threshold
    dplyr::mutate(models_agg_y = furrr::future_map(models_mbrs, function(x)
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
    dplyr::select(-models_mbrs) %>%
    # ensemble mean
    dplyr::mutate(
      rst_ens_mean = purrr::map2(experiment, models_agg_y, function(x, y) {
        ens <-
          suppressMessages(transformeR::aggregateGrid(y, aggr.mem = list(FUN = "mean", na.rm = TRUE)))
        rs <-
          make_raster(ens, c(2, 3), country_shp) # adjust by array dimension
        names(rs) <-
          paste0(x, "_", names(rs), "_", season_name)
        return(rs)
      }),
      # ensemble SD
      rst_ens_sd = purrr::map2(experiment, models_agg_y, function(x, y) {
        ens <-
          suppressMessages(transformeR::aggregateGrid(y, aggr.mem = list(FUN = "sd", na.rm = TRUE)))
        rs <-
          make_raster(ens, c(2, 3), country_shp)
        names(rs) <-
          paste0(x, "_", names(rs), "_", season_name)
        return(rs)
      }),
      # individual models
      rst_models = purrr::map2(experiment, models_agg_y, function(x, y) {
        rs_list <- purrr::map(1:dim(y$Data)[[1]], function(ens) {
          array_mean <-
            if (length(y$Dates$start) == 1)
              apply(y$Data[ens, , , ], c(1, 2), mean, na.rm = TRUE)
          else
            apply(y$Data[ens, , , ], c(2, 3), mean, na.rm = TRUE) # climatology per member adjusting by array dimension
          y$Data <- array_mean
          rs <- make_raster(y, c(1, 2), country_shp)
          names(rs) <-
            paste0("Member ", ens, "_", x, "_", names(rs), "_", season_name)
          return(rs)
        })

      }),
      models_temp = purrr::map2(models_agg_y, experiment, function(x, y) {
        dimnames(x$Data)[[1]] <- x$Members
        dimnames(x$Data)[[2]] <- x$Dates$start
        dimnames(x$Data)[[3]] <- x$xyCoords$y
        dimnames(x$Data)[[4]] <- x$xyCoords$x

        df <- reshape2::melt(x$Data) %>%
          dplyr::mutate(date = as.Date(Var2)) %>%
          dplyr::mutate(experiment = y) %>%
          dplyr::mutate(season = season_name)
        return(df)

      })
    )

  invisible(new_CAVAanalytics_projections(
    ensemble_mean = terra::rast(data_list$rst_ens_mean),
    ensemble_sd = terra::rast(data_list$rst_ens_sd),
    individual_members = terra::rast(purrr::map(
      data_list$rst_models, ~ terra::rast(.x)
    )),
    annual_data = do.call(rbind, purrr::map(
      1:nrow(data_list), ~ data_list$models_temp[[.]]
    ))
  ))
}


# S3 Classes constuctors  -------------------------------------------

#' Create a new CAVAanalytics_list class
#' @param models_df data.frame containing model data
#' @param country_shp spatial object containing country shape
#' @return A CAVAanalytics_list object
#' @noRd
new_CAVAanalytics_list <- function(models_df, country_shp) {
  # Validate inputs
  stopifnot(is.data.frame(models_df))
  stopifnot(!is.null(country_shp))

  # Create and return object
  structure(
    list(models_df, country_shp),
    class = "CAVAanalytics_list",
    components = list("data.frame with list columns", "bbox")
  )
}

#' Create a new CAVAanalytics_ccs class
#' @param ccs_mean SpatRaster for climate change signal mean
#' @param ccs_sd SpatRaster for climate change signal standard deviation
#' @param members_ccs SpatRaster for individual members
#' @param agreement SpatRaster for model agreement
#' @param temporal_data data.frame for spatially aggregated data
#' @return A CAVAanalytics_ccs object
#' @noRd
new_CAVAanalytics_ccs <- function(ccs_mean,
                                  ccs_sd,
                                  members_ccs,
                                  agreement,
                                  temporal_data) {
  # Validate inputs
  stopifnot(inherits(ccs_mean, "SpatRaster"))
  stopifnot(inherits(ccs_sd, "SpatRaster"))
  stopifnot(inherits(members_ccs, "SpatRaster"))
  stopifnot(inherits(agreement, "SpatRaster"))
  stopifnot(is.data.frame(temporal_data))

  # Create and return object
  structure(
    list(ccs_mean, ccs_sd, members_ccs, agreement, temporal_data),
    class = "CAVAanalytics_ccs",
    components = list(
      "SpatRaster for ccs mean",
      "SpatRaster stack for ccs sd",
      "SpatRaster stack for individual members",
      "SpatRaster stack for ccs agreement",
      "dataframe for spatially aggregated data"
    )
  )
}

#' Constructor for CAVAanalytics_observations class
#'
#' Creates a properly structured CAVAanalytics_observations object
#'
#' @param spatraster_data A SpatRaster or list of SpatRasters containing the observation data
#' @param pvalues A SpatRaster of p-values (only used when trends=TRUE)
#' @param annual_data A dataframe containing annually aggregated data
#' @param trends Logical indicating whether this is a trends analysis object
#' @return A CAVAanalytics_observations object
#' @noRd
#'
new_CAVAanalytics_observations <- function(spatraster_data,
                                           pvalues = NULL,
                                           annual_data,
                                           trends = FALSE) {
  if (!trends) {
    structure(
      list(spatraster_data, annual_data),
      class = "CAVAanalytics_observations",
      components = list(
        "SpatRaster for observation mean",
        "dataframe for annually aggregated data"
      )
    )
  } else {
    if (is.null(pvalues)) {
      stop("pvalues must be provided when trends=TRUE")
    }
    structure(
      list(spatraster_data, pvalues, annual_data),
      class = "CAVAanalytics_observations",
      components = list(
        "SpatRaster for trends coefficients",
        "SpatRaster for trends p.values",
        "dataframe for annually aggregated data"
      )
    )
  }
}

#' Constructor for CAVAanalytics_projections class
#'
#' @param ensemble_mean SpatRaster object for ensemble mean
#' @param ensemble_sd SpatRaster object for ensemble standard deviation
#' @param individual_members SpatRaster object for individual members
#' @param annual_data data.frame for annually aggregated data
#' @return An object of class "CAVAanalytics_projections"
#' @noRd
new_CAVAanalytics_projections <- function(ensemble_mean,
                                          ensemble_sd,
                                          individual_members,
                                          annual_data) {
  # Input validation
  if (!inherits(ensemble_mean, "SpatRaster")) {
    stop("ensemble_mean must be a SpatRaster object")
  }
  if (!inherits(ensemble_sd, "SpatRaster")) {
    stop("ensemble_sd must be a SpatRaster object")
  }
  if (!inherits(individual_members, "SpatRaster")) {
    stop("individual_members must be a SpatRaster object")
  }
  if (!is.data.frame(annual_data)) {
    stop("annual_data must be a data.frame")
  }

  # Create the object with proper structure
  structure(
    list(ensemble_mean, ensemble_sd, individual_members, annual_data),
    class = "CAVAanalytics_projections",
    components = list(
      "SpatRaster for ensemble mean",
      "SpatRaster for ensemble sd",
      "SpatRaster for individual members",
      "dataframe for annually aggregated data"
    )
  )
}

#' Constructor for CAVAanalytics_model_biases class
#'
#' @param ensemble_biases SpatRaster object containing ensemble biases
#' @param model_biases SpatRaster object containing model biases
#' @param temporal_biases data.frame containing temporal biases
#' @return An object of class CAVAanalytics_model_biases
#' @noRd
new_CAVAanalytics_model_biases <- function(ensemble_biases,
                                           model_biases,
                                           temporal_biases) {
  # Input validation
  if (!inherits(ensemble_biases, "SpatRaster")) {
    cli::cli_abort("ensemble_biases must be a SpatRaster object")
  }
  if (!inherits(model_biases, "SpatRaster")) {
    cli::cli_abort("model_biases must be a SpatRaster object")
  }
  if (!is.data.frame(temporal_biases)) {
    cli::cli_abort("temporal_biases must be a data.frame")
  }

  # Create the object
  structure(
    list(
      ensemble_biases = ensemble_biases,
      model_biases = model_biases,
      temporal_biases = temporal_biases
    ),
    class = "CAVAanalytics_model_biases",
    components = list(
      "SpatRaster for ensemble biases",
      "SpatRaster for model biases",
      "data frame for temporal biases"
    )
  )
}
