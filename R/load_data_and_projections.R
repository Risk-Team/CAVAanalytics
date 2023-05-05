#' Load data and apply projections in spatial chunks
#'
#' Automatically load and process climate model projections in a memory efficient way. Useful for analysing large areas
#' @param path.to.data path to the directory containing the RCP/SSPs folders and historical simulations (optional). For example,
#' home/user/data/. data would contain subfolders with the climate/impact models. Historical simulations have to be contained in a folder called historical. If path.to.data is set as CORDEX-CORE, CORDEX-CORE simulations from RCM RegCM4 will be loaded
#' @param country character, in English, indicating the country of interest. To select a bounding box,
#' set country to NULL and define arguments xlim and ylim
#' @param variable  A character string indicating the variable
#' @param xlim numeric of length = 2, with minimum and maximum longitude coordinates, in decimal degrees, of the bounding box selected.
#' @param ylim same as xlim, but for the selection of the latitudinal range.
#' @param path.to.obs Default to NULL, if not, indicate the absolute path to the directory containing a reanalysis dataset, for example ERA5. To automatically load W5E5. specify W5E5
#' @param years.proj Numerical range, years to select for projections
#' @param years.hist Numerical range, years to select for historical simulations and observations
#' @param domain specify the CORDEX-CORE domain (e.g AFR-22, EAS-22). Used with path.to.data = CORDEX-CORE. Default is NULL
#' @param aggr.m character. Monthly aggregation. One of none, mean or sum
#' @param data output of load_data
#' @param bias.correction logical
#' @param uppert  numeric of length=1, upper threshold
#' @param lowert numeric of length=1, lower threshold
#' @param season numeric. seasons to select. For example, 1:12
#' @param scaling.type character, default to "additive". Indicates whether to use multiplicative or additive approach for bias correction
#' @param consecutive logical, to use in conjunction with lowert or uppert
#' @param duration character, either "max" or "total".
#' @param chunk.size numeric.
#' @return list with raster stacks
#'
#' @export


load_data_and_projections <- function(country, variable, years.hist=NULL,
                                      years.proj, path.to.data,
                                      path.to.obs=NULL, xlim=NULL, ylim=NULL,aggr.m="none",
                                      chunk.size, season, lowert=NULL, uppert=NULL,consecutive=F,scaling.type="additive", duration="max", bias.correction=F  ) {

  # calculate number of chunks based on xlim and ylim
  x_chunks <- seq(from = xlim[1], to = xlim[2], by = chunk.size)
  y_chunks <- seq(from = ylim[1], to = ylim[2], by = chunk.size)

  # create empty list to store output
  out_list <- list()

  # loop over chunks
  for (i in 1:(length(x_chunks)-1)) {
    for (j in 1:(length(y_chunks)-1)) {

      # set xlim and ylim for current chunk
      xlim_chunk <- c(x_chunks[i]-1.5, x_chunks[i+1])
      ylim_chunk <- c(y_chunks[j]-1.5, y_chunks[j+1])

      # load data for current chunk
      proj_chunk <- load_data(country = country, variable = variable, years.hist = years.hist, years.proj = years.proj,
                              path.to.data = path.to.data, path.to.obs = path.to.obs, xlim = xlim_chunk, ylim = ylim_chunk, aggr.m = aggr.m, buffer=0) %>%

        # do projections for current chunk
        projections(., season = season, bias.correction = bias.correction,
                    uppert = uppert, lowert = lowert, consecutive = consecutive,
                    scaling.type =   scaling.type,
                    duration =  duration)

      # add chunk to output list
      out_list[[paste0("chunk_", i, "_", j)]] <- proj_chunk

    }
  }
  message(Sys.time(), " Merging rasters")
  # Extract the first, second, and third elements of each list in `out_list`
  rst_mean <- lapply(out_list, `[[`, 1)
  rst_sd <- lapply(out_list, `[[`, 2)
  rst_mbrs <- lapply(out_list, `[[`, 3)
  # Merge the extracted rasters using `Reduce` and set their names
  merge_rasters <- function(rst_list) {
    names <- names(rst_list[[1]])
    Reduce(function(...) raster::merge(..., tolerance = 0.5), rst_list) %>% setNames(names)
  }

  rasters_mean <- merge_rasters(rst_mean)
  rasters_sd <- merge_rasters(rst_sd)
  rasters_mbrs <- merge_rasters(rst_mbrs)

  message(Sys.time(), " Done")
  invisible(structure(
    list(
      rasters_mean,
      rasters_sd,
      rasters_mbrs
    ),
    class = "CAVAanalytics_projections",
    components = list(
      "raster stack for ensemble mean",
      "raster stack for ensemble sd",
      "raster stack for individual members"
    )
  ))

}
