#' Write rasters
#'
#' Automatically write rasters from CAVAanalytics step two
#' @export
#' @param step2 output of one of CAVAanalytics functions, such as projections.
#' @param filename charachter, specifying the file name (needs to end in .tif)
#' @param stat charachter. One of mean or sd. Only valid when ensemble is TRUE
#' @param ensemble logical. Whether to save the raster for the ensemble or for individual models

extract_raster <-
  function(step2,
           filename,
           stat,
           ensemble) {
    UseMethod("extract_raster")
  }

extract_raster.CAVAanalytics_observations <- function(step2, filename, stat="mean", ensemble=F) {

if (!stringr::str_ends(filename, ".tif")) cli::cli_abort("Your filename needs to have .tif extension")

cli::cli_alert_warning("Arguments stat and ensemble are ignored")
cli::cli_progress_step("Writing raster")
terra::writeRaster(step2[[1]], filename = filename)
cli::cli_process_done()

}

extract_raster.CAVAanalytics_projections <- function(step2, filename, stat="mean", ensemble=T) {
if (!stringr::str_ends(filename, ".tif")) cli::cli_abort("Your filename needs to have .tif extension")
match.arg(stat, choices = c("mean", "sd"))

if (ensemble)  {
if (stat=="mean") {
cli::cli_progress_step("Writing raster for the ensemble mean")
terra::writeRaster(step2[[1]], filename = filename)
cli::cli_process_done()
} else {
cli::cli_progress_step("Writing raster for the ensemble sd")
terra::writeRaster(step2[[2]], filename = filename)
cli::cli_process_done()
}

} else { # indivisual models

cli::cli_progress_step("Writing raster for the individual models")
terra::writeRaster(step2[[3]], filename = filename)
cli::cli_process_done()

}

}


extract_raster.CAVAanalytics_ccs <- function(step2, filename, stat="mean", ensemble=T) {
  if (!stringr::str_ends(filename, ".tif")) cli::cli_abort("Your filename needs to have .tif extension")
  match.arg(stat, choices = c("mean", "sd"))

  if (ensemble)  {
    if (stat=="mean") {
      cli::cli_progress_step("Writing raster for the ensemble mean")
      terra::writeRaster(step2[[1]], filename = filename)
      cli::cli_process_done()
    } else {
      cli::cli_progress_step("Writing raster for the ensemble sd")
      terra::writeRaster(step2[[2]], filename = filename)
      cli::cli_process_done()
    }

  } else { # indivisual models

    cli::cli_progress_step("Writing raster for the individual models")
    terra::writeRaster(step2[[3]], filename = filename)
    cli::cli_process_done()

  }

}


extract_raster.CAVAanalytics_trends <- function(step2, filename, stat="mean", ensemble=T) {

  cli::cli_alert_warning("Argument stat is ignored")
  if (!stringr::str_ends(filename, ".tif")) cli::cli_abort("Your filename needs to have .tif extension")

  if (length(step2)>3) {
    if (ensemble) {
      cli::cli_progress_step("Writing raster for the ensemble mean (slope values of the linear regression)")
      terra::writeRaster(step2[[1]], filename = filename)
      cli::cli_process_done()} else {

      cli::cli_progress_step("Writing raster for individual models (slope values of the linear regression)")
      terra::writeRaster(step2[[3]], filename = filename)
      cli::cli_process_done()
      }

    } else {
      cli::cli_alert_warning("Argument ensemble is ignored")
      cli::cli_progress_step("Writing raster for the slope values")
      terra::writeRaster(step2[[1]], filename = filename)
      cli::cli_process_done()
    }

}



