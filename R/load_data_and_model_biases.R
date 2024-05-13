#' Load data and apply function model_biases in spatial chunks
#'
#' Automatically load and process climate models in a memory efficient way. Useful for analysing large areas
#' @param path.to.data character (CORDEX-CORE or path to local data) or NULL. If path to local data, specify path to the directory containing the RCP/SSPs folders and historical simulations (optional). For example,
#' home/user/data/. data would contain subfolders with the climate/impact models. Historical simulations have to be contained in a folder called historical. If path.to.data is set as CORDEX-CORE, CORDEX-CORE simulations will be downloaded
#' @param country character, in English, indicating the country of interest or an object of class sf. Country will be used to crop and mask the data but you still need to specify the xlim and ylim arguments
#' @param variable  character, indicating variable name
#' @param xlim numeric of length = 2, with minimum and maximum longitude coordinates, in decimal degrees, of the bounding box selected.
#' @param ylim same as xlim, but for the selection of the latitudinal range.
#' @param path.to.obs Default to NULL, if not, indicate the absolute path to the directory containing a reanalysis dataset, for example ERA5. To automatically load W5E5. specify W5E5
#' @param years.proj Numerical range, years to select for projections
#' @param years.hist Numerical range, years to select for historical simulations and observations
#' @param domain specify the CORDEX-CORE domain (e.g AFR-22, EAS-22). Used with path.to.data = CORDEX-CORE. Default is NULL
#' @param aggr.m character, monthly aggregation. One of none, mean or sum
#' @param bias.correction logical
#' @param uppert  numeric of length=1, upper threshold
#' @param lowert numeric of length=1, lower threshold
#' @param season list, containing seasons to select. For example, list(1:6, 7:12)
#' @param consecutive logical, to use in conjunction with lowert or uppert
#' @param duration A parameter that can be set to either "max" or a specific number. It is relevant only when 'consecutive' is set to TRUE. For instance, to calculate the count of consecutive days with Tmax (maximum temperature) above 35°C, lasting for more than 3 days, you can set 'uppert' to 35, 'consecutive' to TRUE, and 'duration' to 3.
#' @param frequency logical value. This parameter is relevant only when 'consecutive' is set to TRUE and 'duration' is not set to "max". For instance, if you want to determine the count of heatwaves, defined as the number of days with Tmax (maximum temperature) exceeding 35°C for a minimum of 3 consecutive days, set 'uppert' to 35, 'consecutive' to TRUE, 'duration' to 3, and 'frequency' to TRUE.
#' @param n.sessions numeric, number of sessions to use in parallel processing for loading the data. Default to 6. Increasing the number of sessions will not necessarily results in better performances. Leave as default unless necessary
#' @param chunk.size numeric, indicating the number of chunks. The smaller the better when working with limited RAM
#' @param overlap numeric, amount of overlap needed to create the composite. Default 0.25
#' @param method character, bias-correction method to use. One of eqm (Empirical Quantile Mapping), qdm (Quantile Delta Mapping) or scaling. Default to eqm. When using the scaling method, the multiplicative approach is automatically applied only when the variable is precipitation.
#' @param cross_validation character, one of none or 3fold. Whether 3-fold cross validation should be used to avoid overfitting during bias-correction. Default to "none"
#' @param window character, one of none or monthly. Whether bias correction should be applied on a monthly or annual basis. Monthly is the preferred option when performing bias-correction using daily data
#' @importFrom magrittr %>%
#' @return list with SpatRaster. To explore the output run attributes(output)
#' @export


load_data_and_model_biases <-
  function(variable,
           country = NULL,
           years.hist = NULL,
           years.proj,
           path.to.data,
           path.to.obs = NULL,
           xlim,
           ylim,
           aggr.m = "none",
           chunk.size,
           overlap = 0.25,
           season,
           lowert = NULL,
           uppert = NULL,
           consecutive = F,
           duration = "max",
           frequency = F,
           bias.correction = F,
           domain = NULL,
           n.sessions = 6,
           method = "eqm",
           cross_validation = "none",
           window = "monthly") {
    # calculate number of chunks based on xlim and ylim
    if (missing(chunk.size) | missing(season)) {
      cli::cli_abort("chunk.size and season must be specified")
    }

    if (missing(xlim) | missing(ylim)) {
      cli::cli_abort(
        "xlim and ylim need to be specified to load data in spatial chunks. The country argument is just used for cropping the final raster"
      )
    }

    country_shp = if (!is.null(country) &
                      !inherits(country, "sf")) {
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

    lon_range <-
      c(sf::st_bbox(country_shp)[[1]], sf::st_bbox(country_shp)[[3]])
    lat_range <-
      c(sf::st_bbox(country_shp)[[2]], sf::st_bbox(country_shp)[[4]])

    x_chunks <- seq(from = xlim[1], to = xlim[2], by = chunk.size)
    y_chunks <- seq(from = ylim[1], to = ylim[2], by = chunk.size)
    x_chunks <- if (length(x_chunks) < 2)
      xlim
    else
      x_chunks
    y_chunks <- if (length(y_chunks) < 2)
      ylim
    else
      y_chunks

    #making sure the whole area is loaded
    if (x_chunks[length(x_chunks)] < lon_range[2])
      x_chunks[length(x_chunks) + 1] = lon_range[2]
    if (y_chunks[length(y_chunks)] < lat_range[2])
      y_chunks[length(y_chunks) + 1] = lat_range[2]
    # create empty list to store output
    out_list <- list()

    # loop over chunks
    for (i in 1:(length(x_chunks) - 1)) {
      for (j in 1:(length(y_chunks) - 1)) {
        # set xlim and ylim for current chunk
        xlim_chunk <- c(x_chunks[i] - overlap, x_chunks[i + 1])
        ylim_chunk <- c(y_chunks[j] - overlap, y_chunks[j + 1])
        cli::cli_progress_step(paste(
          paste0(
            "Loading data and calculating model biases for spatial CHUNK_",
            i,
            "_",
            j
          ),
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
              domain = domain,
              path.to.obs = path.to.obs,
              xlim = xlim_chunk,
              ylim = ylim_chunk,
              aggr.m = aggr.m,
              buffer = 0,
              n.sessions = n.sessions
            )  %>%

              # do ccs for current chunk
              model_biases(
                .,
                season = season,
                bias.correction = bias.correction,
                uppert = uppert,
                lowert = lowert,
                consecutive = consecutive,
                duration =  duration,
                frequency = frequency,
                n.sessions = 1,
                method = method,
                cross_validation = cross_validation,
                window = window
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
    rst_mbrs <- lapply(out_list, `[[`, 2)
    # Merge the extracted rasters using `Reduce` and set their names
    merge_rasters <- function(rst_list) {
      # Determine the resolution of each raster in the list
      resolutions <- sapply(rst_list, function(r)
        terra::res(r))

      # Check if all rasters have the same resolution
      if (length(unique(resolutions)) > 1) {
        cli::cli_alert_warning("Interpolation was required to merge the rasters.")
        # If resolutions differ, determine the smallest (finest) resolution among all rasters
        common_res <- max(resolutions)

        # Resample all rasters to the common resolution
        rst_list <- lapply(rst_list, function(r) {
          terra::resample(
            r,
            terra::rast(
              terra::ext(r),
              resolution = common_res,
              crs = terra::crs(r)
            ),
            method = "mode"
          )
        })
      }

      # Merge rasters
      merged_raster <-
        Reduce(function(x, y)
          terra::merge(x, y), rst_list)

      # Set names from the first raster in the list
      names <- names(rst_list[[1]])
      setNames(merged_raster, names)
    }

    cli::cli_process_done()

    rasters_mean <- merge_rasters(rst_mean)
    rasters_mbrs <- merge_rasters(rst_mbrs)

    invisible(structure(
      list(
        rasters_mean %>% terra::crop(., country_shp) %>% terra::mask(., country_shp),
        rasters_mbrs %>% terra::crop(., country_shp) %>% terra::mask(., country_shp),
        NULL
      ),
      class =  "CAVAanalytics_model_biases",
      components = list(
        "SpatRaster for ensemble biases",
        "SpatRaster for model biases",
        "data frame for temporal biases"
      )
    ))

  }
