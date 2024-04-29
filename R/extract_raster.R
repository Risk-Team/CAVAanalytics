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
           ensemble) {
    UseMethod("extract_raster")
  }

#' @export
extract_raster.CAVAanalytics_observations <- function(step2, filename, ensemble=F) {

if (!stringr::str_ends(filename, ".tif")) cli::cli_abort("Your filename needs to have .tif extension")

cli::cli_alert_warning("Argument ensemble is ignored")
cli::cli_progress_step("Writing raster")
if (length(step2)==2) {
terra::writeRaster(step2[[1]], filename = paste0("mean_",filename))
cli::cli_process_done()

} else {

terra::writeRaster(step2[[1]], filename = paste0("slope_",filename))
terra::writeRaster(step2[[2]], filename = paste0("pvalue_",filename))
cli::cli_process_done()

}


}
#' @export
extract_raster.CAVAanalytics_projections <- function(step2, filename, ensemble=T) {
if (!stringr::str_ends(filename, ".tif")) cli::cli_abort("Your filename needs to have .tif extension")

if (ensemble)  {

cli::cli_progress_step("Writing rasters for the ensemble mean and sd")
terra::writeRaster(step2[[1]], filename =  paste0("ens_mean_",filename))
terra::writeRaster(step2[[2]], filename =  paste0("ens_sd_",filename))
cli::cli_process_done()

} else { # indivisual models

cli::cli_progress_step("Writing raster for the individual models")
terra::writeRaster(step2[[3]], filename = filename)
cli::cli_process_done()

}

}

#' @export
extract_raster.CAVAanalytics_ccs <- function(step2, filename, ensemble=T) {
  if (!stringr::str_ends(filename, ".tif")) cli::cli_abort("Your filename needs to have .tif extension")
  if (ensemble)  {
      cli::cli_progress_step("Writing rasters for the ensemble mean and sd")
      terra::writeRaster(step2[[1]], filename = paste0("mean_",filename))
      terra::writeRaster(step2[[2]], filename = paste0("sd_",filename))
      cli::cli_process_done()


  } else { # indivisual models

    cli::cli_progress_step("Writing raster for the individual models")
    terra::writeRaster(step2[[3]], filename = filename)
    cli::cli_process_done()

  }

}



