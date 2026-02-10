# Load required packages when running directly
if (sys.nframe() == 0L) {
  library(magrittr)
}

# Constants ----------------------------------------------------------
CONSTANTS <- list(
  CONVERSION_FACTOR = 4.87 / log((67.8 * 10) - 5.42),
  ERA5_VAR_MAPPING = c(
    "pr" = "tp",
    "tasmax" = "t2mx",
    "tasmin" = "t2mn",
    "hurs" = "hurs",
    "sfcWind" = "sfcwind",
    "tas" = "t2m",
    "rsds" = "ssrd"
  )
)

# Helper functions --------------------------------------------------
#' Load model data from specified source
#' @keywords internal
load_model_data.hub <- function(
  x,
  variable,
  years,
  xlim,
  ylim,
  aggr.m,
  database,
  temporal_chunking,
  temporal_chunk_size
) {
  var_name <- if (
    database == "CORDEX-CORE-BC" &&
      variable == "sfcWind"
  ) {
    "sfcwind"
  } else {
    variable
  }

  if (isTRUE(temporal_chunking) && length(years) > temporal_chunk_size) {
    # Split years into chunks
    year_chunks <- split(
      years,
      ceiling(seq_along(years) / temporal_chunk_size)
    )

    cli::cli_alert_info(
      "Requesting {length(years)} years. Splitting download into {length(year_chunks)} chunks of {temporal_chunk_size} years..."
    )

    # Load first chunk as initial value
    init_data <- suppressMessages(
      loadeR::loadGridData(
        dataset = x,
        var = var_name,
        years = year_chunks[[1]],
        lonLim = xlim,
        latLim = ylim,
        aggr.m = aggr.m
      )
    )

    # Stream remaining chunks using Reduce to avoid 2x memory overhead
    if (length(year_chunks) > 1) {
      data <- Reduce(
        f = function(accumulated_data, year_chunk) {
          chunk <- suppressMessages(
            loadeR::loadGridData(
              dataset = x,
              var = var_name,
              years = year_chunk,
              lonLim = xlim,
              latLim = ylim,
              aggr.m = aggr.m
            )
          )
          suppressMessages(
            transformeR::bindGrid(
              list(accumulated_data, chunk),
              dimension = "time"
            )
          )
        },
        x = year_chunks[-1], # All chunks except first
        init = init_data
      )
    } else {
      data <- init_data
    }
  } else {
    data <- suppressMessages(
      loadeR::loadGridData(
        dataset = x,
        var = var_name,
        years = years,
        lonLim = xlim,
        latLim = ylim,
        aggr.m = aggr.m
      )
    )
  }

  if (database %in% c("CORDEX-CORE", "CORDEX-CORE-BC")) {
    data <- transform_climate_data(data, variable, database)
  }

  data
}

#' Load observational data
#' @keywords internal
load_observation_data.hub <- function(
  obs.file,
  variable,
  years,
  xlim,
  ylim,
  aggr.m,
  path.to.obs,
  temporal_chunking,
  temporal_chunk_size
) {
  cli::cli_progress_step(paste0("Uploading ", path.to.obs))

  var_name <- if (path.to.obs == "ERA5") {
    CONSTANTS$ERA5_VAR_MAPPING[variable]
  } else {
    variable
  }

  if (isTRUE(temporal_chunking) && length(years) > temporal_chunk_size) {
    year_chunks <- split(
      years,
      ceiling(seq_along(years) / temporal_chunk_size)
    )

    cli::cli_alert_info(
      "Requesting {length(years)} years. Splitting observations into {length(year_chunks)} chunks of {temporal_chunk_size} years..."
    )

    # Load first chunk as initial value
    init_data <- suppressMessages(
      loadeR::loadGridData(
        obs.file,
        var = var_name,
        years = year_chunks[[1]],
        lonLim = xlim,
        latLim = ylim,
        aggr.m = aggr.m
      )
    )

    # Stream remaining chunks using Reduce to avoid 2x memory overhead
    if (length(year_chunks) > 1) {
      data <- Reduce(
        f = function(accumulated_data, year_chunk) {
          chunk <- suppressMessages(
            loadeR::loadGridData(
              obs.file,
              var = var_name,
              years = year_chunk,
              lonLim = xlim,
              latLim = ylim,
              aggr.m = aggr.m
            )
          )
          suppressMessages(
            transformeR::bindGrid(
              list(accumulated_data, chunk),
              dimension = "time"
            )
          )
        },
        x = year_chunks[-1], # All chunks except first
        init = init_data
      )
    } else {
      data <- init_data
    }
  } else {
    data <- suppressMessages(
      loadeR::loadGridData(
        obs.file,
        var = var_name,
        years = years,
        lonLim = xlim,
        latLim = ylim,
        aggr.m = aggr.m
      )
    )
  }

  if (path.to.obs %in% c("ERA5", "W5E5")) {
    data <- transform_climate_data(data, variable, path.to.obs)
  }

  cli::cli_progress_done()
  list(data)
}


#' Models upload (only works from the University of Cantabria Jupyter HUB environment)
#'
#' Automatically upload databases available at UC servers
#'
#' @param database character, indicating the database of interest (default to CORDEX-CORE). Can be CORDEX-CORE or CORDEX-CORE-BC.
#' @param country character, in English, indicating the country of interest or an object of class sf.
#' @param variable character indicating the variable name
#' @param xlim numeric of length = 2, with minimum and maximum longitude coordinates
#' @param ylim same as xlim, but for the selection of the latitudinal range
#' @param years.hist numeric, specify year range for the historical experiment
#' @param years.proj numeric, specify year range for projections
#' @param path.to.obs character, default to NULL. To automatically load W5E5 or ERA5
#' @param buffer numeric, default is zero
#' @param domain character, specify the CORDEX-CORE or CORDEX-CORE-BC domain
#' @param aggr.m character, monthly aggregation. One of none, mean or sum
#' @param n.sessions numeric, number of sessions for parallel processing
#' @param years.obs NULL or numeric, specify year range for observation
#' @param temporal_chunking logical, default to FALSE. If TRUE, loads data in temporal chunks
#' @param temporal_chunk_size numeric, default to 10. Number of years per chunk when temporal_chunking is TRUE
#' @param res_folder character, specify the resolution of the CORDEX data. Default to "interp025". Meaningful only when working with CORDEX-CORE. In the future this option will be removed
#' @return list of length 2. List[[1]] contains a tibble with list columns and List[[2]] the bbox
#' @importFrom magrittr %>%
#' @export
load_data_hub <- function(
  database = "CORDEX-CORE",
  country,
  variable,
  xlim = NULL,
  ylim = NULL,
  years.hist = NULL,
  years.proj,
  path.to.obs = NULL,
  buffer = 0,
  domain = NULL,
  aggr.m = "none",
  n.sessions = 6,
  years.obs = NULL,
  res_folder = "interp025",
  temporal_chunking = FALSE,
  temporal_chunk_size = 10
) {
  # Validation
  check_inputs.load_data_hub(
    database,
    years.hist,
    domain,
    years.proj,
    variable,
    aggr.m,
    n.sessions,
    path.to.obs,
    years.obs,
    res_folder,
    temporal_chunking,
    temporal_chunk_size
  )

  # Data loading setup
  if (!is.null(database)) {
    if (database %in% c("CORDEX-CORE", "CORDEX-CORE-BC")) {
      files <- load_model_paths.hub(
        domain,
        years.hist,
        years.proj,
        res_folder,
        database
      )
      experiment <- if (!is.null(years.hist) & !is.null(years.proj)) {
        c("historical", "rcp26", "rcp85")
      } else if (is.null(years.hist) & !is.null(years.proj)) {
        c("rcp26", "rcp85")
      } else {
        "historical"
      }
    } else {
      cli::cli_abort(
        "Only CORDEX-CORE and CORDEX-CORE-BC are supported when using load_data_hub"
      )
    }
  }

  # Load observation data paths
  obs.file <- if (!is.null(path.to.obs)) load_obs_paths.hub(path.to.obs)

  # Geolocalization
  result <- geo_localize(country, xlim, ylim, buffer)
  xlim <- result$xlim
  ylim <- result$ylim

  # Set up parallel processing
  future::plan(future::multisession, workers = n.sessions)

  # Process model data
  models_df <- if (!is.null(database)) {
    if (database %in% c("CORDEX-CORE", "CORDEX-CORE-BC")) {
      cli::cli_progress_step(sprintf(
        "Uploading %s data (%d simulations) using %d sessions%s",
        database,
        ifelse(is.null(years.hist), 12, ifelse(is.null(years.proj), 6, 18)),
        n.sessions,
        ifelse(n.sessions == 6, " by default", "")
      ))
    }

    df <- dplyr::tibble(path = files, experiment = experiment) %>%
      tidyr::unnest(cols = path) %>%
      dplyr::mutate(
        models = suppressWarnings(
          furrr::future_map(unlist(files), function(x) {
            years <- if (stringr::str_detect(x, "historical")) {
              years.hist
            } else {
              years.proj
            }
            load_model_data.hub(
              x,
              variable,
              years,
              xlim,
              ylim,
              aggr.m,
              database,
              temporal_chunking,
              temporal_chunk_size
            )
          })
        )
      ) %>%
      dplyr::group_by(experiment) %>%
      dplyr::summarise(models_mbrs = list(models))

    cli::cli_progress_done()
    size <- as.numeric(object.size(df))
    cli::cli_text(
      "{cli::symbol$arrow_right} Uploaded {prettyunits::pretty_bytes(size)}"
    )

    cli::cli_progress_step(
      "Making multi-model ensemble and checking temporal consistency"
    )
    df %>%
      dplyr::mutate(models_mbrs = purrr::map(models_mbrs, ~ common_dates(.x)))
  } else {
    dplyr::tibble(obs = NA)
  }

  # Process observation data
  if (!is.null(path.to.obs)) {
    obs_years <- if (!is.null(years.obs)) years.obs else years.hist
    models_df$obs <- load_observation_data.hub(
      obs.file,
      variable,
      obs_years,
      xlim,
      ylim,
      aggr.m,
      path.to.obs,
      temporal_chunking,
      temporal_chunk_size
    )
  } else {
    models_df$obs <- NULL
  }

  # Print conversion messages
  if (!is.null(database) && database %in% c("CORDEX-CORE", "CORDEX-CORE-BC")) {
    print_conversion_message(variable, database)
  }
  if (!is.null(path.to.obs) && path.to.obs %in% c("W5E5", "ERA5")) {
    print_conversion_message(variable, path.to.obs)
  }

  # Return result
  invisible(new_CAVAanalytics_list(models_df, result$country_shp))
}
