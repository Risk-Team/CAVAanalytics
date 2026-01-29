# Load required packages when running directly
if (sys.nframe() == 0L) {
  library(magrittr)
}

# constants ---------------------------------------------------------------

#' Constants and configurations
#' @keywords internal
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

# loading data ------------------------------------------------------------

#' Load model data from specified source
#' @keywords internal
load_model_data <- function(
  x,
  variable,
  years,
  xlim,
  ylim,
  aggr.m,
  path.to.data,
  temporal_chunking,
  temporal_chunk_size
) {
  if (path.to.data %in% c("CORDEX-CORE", "CORDEX-CORE-BC")) {
    # Remote data download
    tryCatch(
      {
        if (isTRUE(temporal_chunking) && length(years) > temporal_chunk_size) {
          # Split years into chunks
          year_chunks <- split(
            years,
            ceiling(seq_along(years) / temporal_chunk_size)
          )

          cli::cli_alert_info(
            "Requesting {length(years)} years. Splitting download into {length(year_chunks)} chunks of {temporal_chunk_size} years..."
          )

          # Load each chunk
          data_list <- lapply(year_chunks, function(chk) {
            suppressMessages(
              loadeR::loadGridData(
                dataset = x,
                var = variable,
                years = chk,
                lonLim = xlim,
                latLim = ylim,
                aggr.m = aggr.m
              )
            )
          })

          # Bind the chunks along the time dimension
          data <- suppressMessages(
            do.call(
              transformeR::bindGrid,
              c(data_list, list(dimension = "time"))
            )
          )
        } else {
          data <- suppressMessages(
            loadeR::loadGridData(
              dataset = x,
              var = variable,
              years = years,
              lonLim = xlim,
              latLim = ylim,
              aggr.m = aggr.m
            )
          )
        }

        data <- transform_climate_data(data, variable, path.to.data)
        return(data)
      },
      error = function(e) {
        cli::cli_alert_warning(
          paste(
            "Error downloading",
            path.to.data,
            "data. If the issue persists, flag it on our GitHub repo at https://github.com/Risk-Team/CAVAanalytics/issues\n",
            e$message
          )
        )
        return(NULL)
      }
    )
  } else {
    # Local data upload
    tryCatch(
      {
        data <- suppressMessages(
          loadeR::loadGridData(
            dataset = x,
            var = variable,
            years = years,
            lonLim = xlim,
            latLim = ylim,
            aggr.m = aggr.m
          )
        )
        return(data)
      },
      error = function(e) {
        cli::cli_alert_warning(paste("Error loading local data:", e$message))
        return(NULL)
      }
    )
  }
}

#' Load observational data
#' @keywords internal
load_observation_data <- function(
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
  cli::cli_progress_step(paste0(
    ifelse(
      path.to.obs %in% c("ERA5", "W5E5"),
      "Downloading ",
      "Uploading "
    ),
    path.to.obs
  ))

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

    data_list <- lapply(year_chunks, function(chk) {
      suppressMessages(
        loadeR::loadGridData(
          obs.file,
          var = var_name,
          years = chk,
          lonLim = xlim,
          latLim = ylim,
          aggr.m = aggr.m
        )
      )
    })

    data <- suppressMessages(
      do.call(
        transformeR::bindGrid,
        c(data_list, list(dimension = "time"))
      )
    )
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

# Start of function -------------------------------------------------------

#' Models download/upload
#'
#' Automatically load models (netCDF/NcML) in a tidy format with access to state-of-the-art climate models and reanalyses datasets

#' @param path.to.data character (CORDEX-CORE, CORDEX-CORE-BC, or path to local data) or NULL. If path to local data, specify path to the directory containing the RCP/SSPs folders and historical simulations (optional). For example,
#' home/user/data/. data would contain subfolders with the climate/impact models. Historical simulations have to be contained in a folder called historical. If path.to.data is set as CORDEX-CORE or CORDEX-CORE-BC, the respective simulations will be downloaded
#' @param country character, in English, indicating the country of interest or an object of class sf. To select a bounding box,
#' set country to NULL and define arguments xlim and ylim
#' @param variable  character indicating the variable name
#' @param xlim numeric of length = 2, with minimum and maximum longitude coordinates, in decimal degrees, of the bounding box of interest
#' @param ylim same as xlim, but for the selection of the latitudinal range
#' @param path.to.obs character, default to NULL. To automatically load W5E5 or ERA5, specify W5E5 or ERA5. Otherwise, indicate the observational dataset path to be used
#' @param years.proj NULL or numeric, specify year range for projections
#' @param years.hist NULL or numeric, specify year range for the historical experiment
#' @param years.obs NULL or numeric, specify year range for observation. Specifying years.obs will overwrite years.hist for observations
#' @param temporal_chunking logical, default to FALSE. If TRUE, loads data in temporal chunks
#' @param temporal_chunk_size numeric, default to 10. Number of years per chunk when temporal_chunking is TRUE
#' @param domain charachter, specify the CORDEX-CORE domain (e.g AFR-22, EAS-22). Used with path.to.data = CORDEX-CORE or CORDEX-CORE-BC. Default is NULL. List of domain names can be found at https://cordex.org/domains/
#' @param buffer numeric, default is zero.
#' @param aggr.m character, monthly aggregation. One of none, mean or sum
#' @param n.sessions numeric, number of sessions to use in parallel processing. Default to 6. Increasing the number of sessions will not necessarily results in better performances. Leave as default unless necessary
#' @return list of length 3. List[[1]] contains a tibble with list columns. List[[2]] the bbox and list[[3]] the temporal structure of the models
#' @importFrom magrittr %>%
#'
#' @export

load_data <- function(
  path.to.data,
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
  temporal_chunking = FALSE,
  temporal_chunk_size = 10
) {
  # Input validation
  check_inputs.load_data(
    path.to.data,
    years.hist,
    domain,
    years.proj,
    variable,
    aggr.m,
    n.sessions,
    path.to.obs,
    years.obs,
    temporal_chunking,
    temporal_chunk_size
  )

  # Geolocalization
  result <- geo_localize(country, xlim, ylim, buffer)
  xlim <- result$xlim
  ylim <- result$ylim

  # Set up parallel processing
  future::plan(future::multisession, workers = n.sessions)

  # Process model data
  models_df <- if (!is.null(path.to.data)) {
    if (path.to.data %in% c("CORDEX-CORE", "CORDEX-CORE-BC")) {
      files <- load_model_paths.thredds(
        domain,
        years.hist,
        years.proj,
        path.to.data
      )
      experiment <- if (
        !is.null(years.hist) &
          !is.null(years.proj)
      ) {
        c("historical", "rcp26", "rcp85")
      } else if (is.null(years.hist) & !is.null(years.proj)) {
        c("rcp26", "rcp85")
      } else {
        "historical"
      }

      cli::cli_alert_info(sprintf(
        "Downloading %s data (%d simulations) using %d sessions%s",
        path.to.data,
        length(unlist(files)),
        n.sessions,
        ifelse(n.sessions == 6, " by default", "")
      ))
    } else {
      files <- load_model_paths.local(path.to.data)
      experiment <- list.dirs(path.to.data, full.names = FALSE)[-1]

      cli::cli_alert_info(sprintf(
        "Uploading local data (%d files) using %d sessions%s",
        length(unlist(files)),
        n.sessions,
        ifelse(n.sessions == 6, " by default", "")
      ))
    }

    df <- dplyr::tibble(path = files, experiment = experiment) %>%
      tidyr::unnest(cols = path) %>%
      dplyr::mutate(
        models = suppressWarnings(furrr::future_map(
          unlist(files),
          function(x) {
            years <- if (stringr::str_detect(x, "historical")) {
              years.hist
            } else {
              years.proj
            }
            load_model_data(
              x,
              variable,
              years,
              xlim,
              ylim,
              aggr.m,
              path.to.data,
              temporal_chunking,
              temporal_chunk_size
            )
          },
          .progress = TRUE
        ))
      ) %>%
      dplyr::group_by(experiment) %>%
      dplyr::summarise(models_mbrs = list(models))

    cli::cli_text("")
    size <- as.numeric(object.size(df))
    cli::cli_alert_success(
      sprintf(
        "%s {prettyunits::pretty_bytes(size)}",
        if (path.to.data %in% c("CORDEX-CORE", "CORDEX-CORE-BC")) {
          "Downloaded"
        } else {
          "Uploaded"
        }
      )
    )

    cli::cli_progress_step("Making multi-model ensemble")
    df_aggr <- df %>%
      dplyr::mutate(models_mbrs = purrr::map(models_mbrs, ~ common_dates(.x)))
    cli::cli_progress_done()
    df_aggr
  } else {
    dplyr::tibble(obs = NA)
  }

  # Process observation data
  if (!is.null(path.to.obs)) {
    obs.file <- if (path.to.obs %in% c("ERA5", "W5E5")) {
      load_obs_paths.thredds(path.to.obs)
    } else {
      path.to.obs
    }
    obs_years <- if (!is.null(years.obs)) {
      years.obs
    } else {
      years.hist
    }
    models_df$obs <- load_observation_data(
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
  if (
    !is.null(path.to.data) &&
      path.to.data %in% c("CORDEX-CORE", "CORDEX-CORE-BC")
  ) {
    print_conversion_message(variable, path.to.data)
  }
  if (!is.null(path.to.obs) && path.to.obs %in% c("W5E5", "ERA5")) {
    print_conversion_message(variable, path.to.obs)
  }

  # Return result
  invisible(new_CAVAanalytics_list(models_df, result$country_shp))
}


# Testing -----------------------------------------------------------------

# Test function call when script is run directly
if (sys.nframe() == 0L) {
  # Example usage of load_data function
  result <- load_data(
    path.to.data = "CORDEX-CORE",
    country = "Kenya",
    variable = "tasmax",
    years.hist = 1976:1980,
    years.proj = 2010:2020,
    domain = "AFR-22"
  )

  # Print basic info about the result
  print("Done")
}
