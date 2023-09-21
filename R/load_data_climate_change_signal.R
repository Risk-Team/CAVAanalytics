#' Load data and apply climate change signal in spatial chunks
#'
#' Automatically load and process climate models in a memory efficient way. Useful for analysing large areas
#' @param path.to.data path to the directory containing the RCP/SSPs folders and historical simulations (optional). For example,
#' home/user/data/. data would contain subfolders with the climate/impact models. Historical simulations have to be contained in a folder called historical. If path.to.data is set as CORDEX-CORE, CORDEX-CORE simulations will be loaded
#' @param variable  character indicating the variable name
#' @param xlim numeric of length = 2, with minimum and maximum longitude coordinates, in decimal degrees, of the bounding box of interest
#' @param ylim same as xlim, but for the selection of the latitudinal range
#' @param path.to.obs character, default to NULL. To automatically load W5E5 or ERA5, specify W5E5 or ERA5. Otherwise, indicate the absolute path to the directory containing another observational dataset
#' @param years.proj numeric, specify year range for projections
#' @param years.hist numeric, specify year range for the historical experiment
#' @param domain charachter, specify the CORDEX-CORE domain (e.g AFR-22, EAS-22). Used with path.to.data = CORDEX-CORE. Default is NULL
#' @param aggr.m character, monthly aggregation. One of none, mean or sum
#' @param bias.correction logical
#' @param uppert  numeric of length=1, upper threshold
#' @param lowert numeric of length=1, lower threshold
#' @param season numeric, seasons to select. For example, 1:12
#' @param consecutive logical, to use in conjunction with lowert or uppert
#' @param n.sessions numeric, number of sessions to use in parallel processing. Default to 6. Increasing the number of sessions will not necessarily results in better performances. Leave as default unless necessary
#' @param duration character, either "max" or "total"
#' @param chunk.size numeric, indicating the number of chunks to. The smaller the better when working with limited RAM.
#' @param overlap numeric, amount of overlap needed to create the composite. Default 0.5
#' @importFrom magrittr %>%
#' @return list with SpatRaster. To explore the output run attributes(output)
#' @export



load_data_and_climate_change_signal <-
  function(variable,
           years.hist = NULL,
           years.proj,
           path.to.data,
           path.to.obs = NULL,
           xlim,
           ylim,
           aggr.m = "none",
           domain,
           chunk.size,
           overlap = 0.5,
           season,
           lowert = NULL,
           uppert = NULL,
           consecutive = F,
           scaling.type = "additive",
           duration = "max",
           bias.correction = F,
           n.sessions = 1) {
    # calculate number of chunks based on xlim and ylim
    if (missing(chunk.size) | missing(season)) {
      cli::cli_abort("chunk.size and season must be specified")
    }
    x_chunks <- seq(from = xlim[1], to = xlim[2], by = chunk.size)
    y_chunks <- seq(from = ylim[1], to = ylim[2], by = chunk.size)
    x_chunks <- if (length(x_chunks)<2)  xlim else x_chunks
    y_chunks <- if (length(y_chunks)<2)  ylim else y_chunks
    # create empty list to store output
    out_list <- list()
    # set parallel processing
    future::plan(future::multisession, workers = n.sessions)

    # loop over chunks
    for (i in 1:(length(x_chunks) - 1)) {
      for (j in 1:(length(y_chunks) - 1)) {
        # set xlim and ylim for current chunk
        xlim_chunk <- c(x_chunks[i] - overlap, x_chunks[i + 1])
        ylim_chunk <- c(y_chunks[j] - overlap, y_chunks[j + 1])
        cli::cli_progress_step(paste(
          paste0("Loading and processing spatial CHUNK_", i, "_", j),
          ". Coordinates ",
          "xlim=",
          paste(xlim_chunk, collapse = ","),
          " ylim=",
          paste(ylim_chunk, collapse = ",")
        ))
        # load data for current chunk
        proj_chunk <-
          suppressMessages(
            load_data(
              country = NULL,
              variable = variable,
              years.hist = years.hist,
              years.proj = years.proj,
              path.to.data = path.to.data,
              path.to.obs = path.to.obs,
              xlim = xlim_chunk,
              ylim = ylim_chunk,
              domain,
              aggr.m = aggr.m,
              buffer = 0
            )  %>%

              # do ccs for current chunk
              climate_change_signal(
                .,
                season = season,
                bias.correction = bias.correction,
                uppert = uppert,
                lowert = lowert,
                consecutive = consecutive,
                duration =  duration,
                n.sessions = n.sessions
              )
          )

        # add chunk to output list
        out_list[[paste0("chunk_", i, "_", j)]] <- proj_chunk
        cli::cli_progress_done()

      }
    }
    cli::cli_progress_step("Merging rasters")
    # Extract the first, second, and third elements of each list in `out_list`
    rst_mean <- lapply(out_list, `[[`, 1)
    rst_sd <- lapply(out_list, `[[`, 2)
    rst_mbrs <- lapply(out_list, `[[`, 3)
    # Merge the extracted rasters using `Reduce` and set their names
    merge_rasters <- function(rst_list) {
      names <- names(rst_list[[1]])
      Reduce(function(...)
        terra::merge(...), rst_list) %>% setNames(names)
    }

    cli::cli_process_done()

    rasters_mean <- tryCatch(
      expr = merge_rasters(rst_mean),
      warning = function(w) {
        # Translate the warning into something more understandable
        translated_warning <-
          "Terra had to interpolate your SpatRasters to merge them. You can ignore this warning if you used sensible values for overalp and chunk_size arguments"
        # ... handle the warning ...
        # You can print the translated warning, log it, or perform any other action
        cli::cli_alert_warning(translated_warning)
      }
    )
    rasters_sd <- merge_rasters(rst_sd)
    rasters_mbrs <- merge_rasters(rst_mbrs)



    invisible(structure(
      list(rasters_mean,
           rasters_sd,
           rasters_mbrs),
      class = "CAVAanalytics_ccs",
      components = list(
        "SpatRaster for ensemble mean",
        "SpatRaster for ensemble sd",
        "SpatRaster for individual members"
      )
    ))

  }
