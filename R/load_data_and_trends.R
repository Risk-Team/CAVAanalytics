#' Load data and apply trends in spatial chunks
#'
#' Automatically load and process climate model projections in a memory efficient way. Useful for analysing large areas. Arguments historical and country are not available for this function.
#' @param path.to.data path to the directory containing the RCP/SSPs folders and historical simulations (optional). For example,
#' home/user/data/. data would contain subfolders with the climate/impact models. Historical simulations have to be contained in a folder called historical. If path.to.data is set as CORDEX-CORE, CORDEX-CORE simulations from RCM RegCM4 will be loaded
#' @param variable  character, indicating variable name
#' @param xlim numeric of length = 2, with minimum and maximum longitude coordinates, in decimal degrees, of the bounding box selected.
#' @param ylim same as xlim, but for the selection of the latitudinal range.
#' @param path.to.obs Default to NULL, if not, indicate the absolute path to the directory containing a reanalysis dataset, for example ERA5. To automatically load W5E5. specify W5E5
#' @param years.proj Numerical range, years to select for projections
#' @param years.hist Numerical range, years to select for historical simulations and observations. Only used for bias-correction, load_data_and_trends does not provide results for the historical period. Use load_data and trends instead
#' @param domain specify the CORDEX-CORE domain (e.g AFR-22, EAS-22). Used with path.to.data = CORDEX-CORE. Default is NULL
#' @param aggr.m character, monthly aggregation. One of none, mean or sum
#' @param data output of load_data
#' @param bias.correction logical
#' @param uppert  numeric of length=1, upper threshold
#' @param lowert numeric of length=1, lower threshold
#' @param season numeric, seasons to select. For example, 1:12
#' @param scaling.type character, default to "additive". Indicates whether to use multiplicative or additive approach for bias correction
#' @param consecutive logical, to use in conjunction with lowert or uppert
#' @param duration character, either "max" or "total"
#' @param interannual_var logical, whether linear regression is applied to annual variability, measured as standard deviation
#' @param chunk.size numeric, indicating the number of chunks to. The smaller the better when working with limited RAM.
#' @param overlap numeric, amount of overlap needed to create the composite. This would depend on the resolution of your data. For example, if your data is at 50 Km resolution, overlap could be 1.5. If your data is at 1 Km resolution, overlap can be 0.5.
#' @return list with merged raster stacks. To explore the output run attributes(output)
#'
#' @export


load_data_and_trends <- function(variable, years.hist=NULL,
                                                years.proj, path.to.data,
                                                path.to.obs=NULL, xlim, ylim,aggr.m="none",
                                                chunk.size, overlap=1.5, season, lowert=NULL, uppert=NULL,consecutive=F,scaling.type="additive", duration="max", bias.correction=F , interannual_var) {

  # calculate number of chunks based on xlim and ylim

    x_chunks <- seq(from = xlim[1], to = xlim[2], by = chunk.size)
    y_chunks <- seq(from = ylim[1], to = ylim[2], by = chunk.size)
  # create empty list to store output
  out_list <- list()

  # loop over chunks
  for (i in 1:(length(x_chunks)-1)) {
    for (j in 1:(length(y_chunks)-1)) {
      message("\n", Sys.time(), paste0(" CHUNK_", i, "_", j), "\n")
      # set xlim and ylim for current chunk
      xlim_chunk <- c(x_chunks[i]-overlap, x_chunks[i+1])
      ylim_chunk <- c(y_chunks[j]-overlap, y_chunks[j+1])

      # load data for current chunk
      proj_chunk <- load_data(country = NULL, variable = variable, years.hist = years.hist, years.proj = years.proj,
                              path.to.data = path.to.data, path.to.obs = path.to.obs, xlim = xlim_chunk, ylim = ylim_chunk, aggr.m = aggr.m, buffer=0) %>%

        # do projections for current chunk
        trends(., season = season, bias.correction = bias.correction,
                              uppert = uppert, lowert = lowert, consecutive = consecutive,
                              scaling.type =   scaling.type,
                              duration =  duration, interannual_var=interannual_var, historical=F)

      # add chunk to output list
      out_list[[paste0("chunk_", i, "_", j)]] <- proj_chunk

    }
  }
  message(Sys.time(), " Merging rasters")
  # Extract the first, second, and third elements of each list in `out_list`
  rst_coef <- lapply(out_list, `[[`, 1)
  rst_pval <- lapply(out_list, `[[`, 2)
  rst_mbrs_coef <- lapply(out_list, `[[`, 3)
  rst_mbrs_pval <- lapply(out_list, `[[`, 4)
  # Merge the extracted rasters using `Reduce` and set their names
  merge_rasters <- function(rst_list) {
    names <- names(rst_list[[1]])
    Reduce(function(...) raster::merge(..., tolerance = 0.5), rst_list) %>% setNames(names)
  }

  rasters_coef <-merge_rasters(rst_coef)
  rasters_pval <- merge_rasters(rst_pval)
  rasters_mbrs_coef <- merge_rasters(rst_mbrs_coef)
  rasters_mbrs_pval <- merge_rasters(rst_mbrs_pval)

  message(Sys.time(), " Done")
  invisible(structure(
    list(
      rasters_coef,
      rasters_pval,
      rasters_mbrs_coef,
      rasters_mbrs_pval,
      NULL,
      NULL
    ),
    class = "CAVAanalytics_trends",
    components = list(
      "raster stack for trends coefficients (ensemble)",
      "raster stack for trends p.values (ensemble)",
      "raster stack for trends coefficients (models)",
      "raster stack for trends p.values (models)",
      "dataframe for trends (ensemble)",
      "dataframe for trends (models)"
    )
  ))

}
