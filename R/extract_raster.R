#' Write rasters
#'
#' Automatically write rasters from CAVAanalytics step two
#' @export
#' @param step2 output of one of CAVAanalytics functions, such as projections.
#' @param file.extension charachter, specifying the file extension (needs to end in .tif). For example, tasmax.tif
#' @param path charachter, specify where to save the files. Default to current working directory.
#' @param stat charachter. One of mean or sd. Only valid when ensemble is TRUE
#' @param ensemble logical. Whether to save the raster for the ensemble or for individual models

extract_raster <-
  function(step2,
           file.extension,
           ensemble,
           path) {
    UseMethod("extract_raster")
  }

#' @export
extract_raster.CAVAanalytics_observations <-
  function(step2, file.extension, ensemble = F, path=NULL) {
    if (!stringr::str_ends(file.extension, ".tif"))
      cli::cli_abort("Your file.extension needs to have .tif extension")

    cli::cli_alert_warning("Argument ensemble is ignored")
    cli::cli_progress_step("Writing raster")
    if (length(step2) == 2) {
      terra::writeRaster(step2[[1]], filename = paste0(path, "mean_", file.extension))
      cli::cli_process_done()

    } else {
      terra::writeRaster(step2[[1]], filename = paste0(path, "slope_", file.extension))
      terra::writeRaster(step2[[2]], filename = paste0(path, "pvalue_", file.extension))
      cli::cli_process_done()

    }


  }
#' @export
extract_raster.CAVAanalytics_projections <-
  function(step2, file.extension, ensemble = T, path=NULL) {
    if (!stringr::str_ends(file.extension, ".tif"))
      cli::cli_abort("Your file.extension needs to have .tif extension")

    if (ensemble)  {
      cli::cli_progress_step("Writing rasters for the ensemble mean and sd")
      for (i in 1:2) {
        prefix <- ifelse(i == 1, "ens_mean_", "ens_sd_")
        for (layer_index in seq_len(terra::nlyr(step2[[i]]))) {
          layer <- step2[[i]][[layer_index]]
          layer_name <- terra::names(step2[[i]])[layer_index]
          terra::writeRaster(layer, filename = paste0(path, prefix, layer_name, "_", file.extension))
        }
      }
      cli::cli_process_done()

    } else {
      # indivisual models

      cli::cli_progress_step("Writing raster for the individual models")
      for (layer_index in seq_len(terra::nlyr(step2[[3]]))) {
        layer <- step2[[3]][[layer_index]]
        layer_name <- terra::names(step2[[3]])[layer_index]
        terra::writeRaster(layer, filename = paste0(path, layer_name, "_", file.extension))
      }
      cli::cli_process_done()

    }

  }

#' @export
extract_raster.CAVAanalytics_ccs <-
  function(step2, file.extension, ensemble = T, path=NULL) {
    if (!stringr::str_ends(file.extension, ".tif"))
      cli::cli_abort("Your file.extension needs to have .tif extension")
    if (ensemble)  {
      cli::cli_progress_step("Writing rasters for the ensemble mean and sd")
      for (i in 1:2) {
        prefix <- ifelse(i == 1, "ens_mean_", "ens_sd_")
        for (layer_index in seq_len(terra::nlyr(step2[[i]]))) {
          layer <- step2[[i]][[layer_index]]
          layer_name <- terra::names(step2[[i]])[layer_index]
          terra::writeRaster(layer, filename = paste0(path, prefix, layer_name, "_", file.extension))
        }
      }
      cli::cli_process_done()

    } else {
      # indivisual models

      cli::cli_progress_step("Writing raster for the individual models")
      for (layer_index in seq_len(terra::nlyr(step2[[3]]))) {
        layer <- step2[[3]][[layer_index]]
        layer_name <- terra::names(step2[[3]])[layer_index]
        terra::writeRaster(layer, filename = paste0(path, layer_name, "_", file.extension))
      }
      cli::cli_process_done()

    }

  }
