#' Date selection
#'
#' automatically select common dates among C4R objects
#' @param data list containing C4R objects, which are the outputs of the loadGridata function
#' @export

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

#' Load data using xarray
#'
#' automatically using xarray and converting into C4R list for remote data access. Only works for data stored at UC server
#' @export

loadGridData_xarray <-
  function(dataset,
           var,
           lonLim,
           latLim,
           years,
           aggr.m = "none") {
    cli::cli_inform("This function requires reticulate and xarray!")
    cli::cli_progress_step("Downloading data")
    library(reticulate)
    xarray <- import("xarray")

    # opne the dataset. W5E5 has a different structure
    ds <- if (stringr::str_detect(dataset, "W5E5")) {
      xarray$open_dataset(dataset)$sel(
        lat = seq(
          from = latLim[2],
          to = latLim[1],
          by = -0.5
        ),
        lon = seq(lonLim[2], lonLim[1], by = -0.5),
        method = "nearest"
      )[[var]]


    } else {
      xarray$open_dataset(dataset)$sel(
        latitude = seq(
          from = latLim[2],
          to = latLim[1],
          by = -0.25
        ),
        longitude = seq(lonLim[2], lonLim[1], by = -0.25),
        method = "nearest"
      )[[var]]

    }

    # select years

    time_mask = (ds['time']$dt$year$values >= years[1]) &
      (ds['time']$dt$year$values <= years[length(years)])

    ds_time <- ds$sel(time = time_mask)

    # retrieve dates. Some RCMs have dates in cftime so it needs converstion
    dates <-
      if (any(stringr::str_detect(class(ds_time$time$values[[1]]), "cftime"))) {
        date <-
          xarray$cftime_range(start = ds_time$time$values[[1]],
                              periods = length(ds_time$time))

        date$strftime(date_format = "%Y-%m-%d")$values

      } else {
        ds_time$time$values

      }

    # building C4R object

    C4R <- if (stringr::str_detect(dataset, "W5E5")) {
      list(
        Variable = list(varName = var),
        Data = ds_time$values,
        xyCoords = list(x = as.numeric(rev(
          ds_time$lon$values
        )), y = as.numeric(rev(
          ds_time$lat$values
        ))),
        Dates = list(start = as.character(dates), end = as.character(dates))
      )

    } else {
      list(
        Variable = list(varName = var),
        Data = ds_time$values,
        xyCoords = list(x = as.numeric(rev(
          ds_time$longitude$values
        )), y = as.numeric(rev(
          ds_time$latitude$values
        ))),
        Dates = list(start = as.character(dates), end = as.character(dates))
      )


    }

    attributes(C4R$Data) <-
      list(dimensions = c("time", "lat", "lon"),
           dim = dim(C4R$Data))

    cli::cli_progress_done()

    if (aggr.m == "none") {
      invisible(C4R)
    } else {
      invisible(transformeR::aggregateGrid(C4R, aggr.m = list(FUN = aggr.m)))

    }

  }

#' make a raster
#'
#' Make a spatRaster from a C4R list with dim(Data)=2
#'
#' @param cl4.object A C4R list with the Data slot in two dimension
#' @param dimensions vector specifying which dimensions corresponds to lat and lon. The rest will be averaged
#' @param shape.file sf object for which to crop and mask the spatRaster
#' @return spatRaster
#' @export

make_raster <- function(cl4.object, dimensions, shape.file) {
  xmin <-
    if (is.null(cl4.object$xyCoords$lon))
      min(cl4.object$xyCoords$x)
  else
    min(cl4.object$xyCoords$lon[1,])
  xmax <-
    if (is.null(cl4.object$xyCoords$lon))
      max(cl4.object$xyCoords$x)
  else
    max(cl4.object$xyCoords$lon[1,])
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
    apply(cl4.object$Data, dimensions, mean, na.rm = TRUE)

  cl4.object$Data <- array_mean

  rasters <- terra::rast(cl4.object$Data,
                         extent = terra::ext(xmin, xmax, ymin, ymax)) %>%
    terra::flip(., direction = 'vertical') %>%
    terra::crop(., shape.file, snap = "out") %>%
    terra::mask(., shape.file)

  nms <-
    paste0(
      stringr::str_extract(cl4.object$Dates$start[1], "\\d{4}"),
      ".",
      stringr::str_extract(cl4.object$Dates$end[length(cl4.object$Dates$start)],  "\\d{4}")
    )
  names(rasters) <-  nms

  return(rasters)

}

#' Consecutive days
#'
#' Calculation of consecutive days. It can be used with aggregateGrid.
#'
#' @param col numeric vector
#' @param duration either "max" or "total".
#' @param lowert numeric. lower threshold
#' @param uppert numeric. upper threshold
#' @return numeric of length 1
#' @export
# functions for consecutive days
thrs_consec = function(col, duration, lowert, uppert) {
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

  #get only connsecutive days matching the threshold

  consec_days = consec$lengths[consec$values == TRUE]

  #return values out

  if (duration == "max") {
    val <- max(consec_days, na.rm = T)
    return(if(val=="-Inf") 0 else val)

  } else{
    return(sum(consec_days[consec_days > duration], na.rm = T))

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



#' Apply multivariate linear regression to a multimember grid
#'
#' This function can be used after performing annual aggregation and with a multigrid object. it applies multivariate linear regression per pixel if
#' spatial averages are not performed or for spatially aggregated data
#' @return array, without spatial averages, or dataframe, if spatial averages are performed
#'
#' @export

# multivariate
ens_trends <- function(c4R) {
  if (is.null(c4R$Members))
    cli::cli_abort(
      c("x" = "This list does not seem to contain several members. Consider applying models_trends")
    )

  mbrs <- dim(c4R$Data)[1]

  if (length(dim(c4R$Data)) > 2) {
    # in cases in which there is a spatial dimension
    if (dim(c4R$Data)[1] > 100)
      cli::cli_alert_warning("Check that your performed annual aggregation before using this function")

    cli::cli_progress_step(
      " Applying multivariate linear regression to the ensemble. Global test statistics calculated assuming uncorrelated response (for faster computation). P-value calculated using 999 iterations via PIT-trap resampling."
    )

    global.lm <- apply(c4R$Data, c(3, 4), function(y) {
      df <- reshape2::melt(y) %>%
        tidyr::pivot_wider(values_from = "value", names_from = "Var1") %>%
        dplyr::select(-Var2) %>%
        mvabund::mvabund()

      df_var = data.frame(time = 1:nrow(df))
      mnlm <- mvabund::manylm(df ~ time, data = df_var)
      out <- anova(mnlm, p.uni = "adjusted")

      sig.models <-
        names(out$uni.p[2,][out$uni.p[2,] < 0.05]) # names of models with significance (p.value < 0.05)
      colnames(mnlm$coefficients) <- paste0("X", 1:mbrs)
      coef_res <- mnlm$coefficients[2,  sig.models]
      prop_res <-
        if (length(coef_res) == 0)
          999
      else
        sum(ifelse(coef_res  >= 0, 1, -1)) # number of models, with significance, that shows an increaseor decraese. 999 assign to NA
      cbind(prop_res, out$table[2, 4])

    })
    return(global.lm)
  } else {
    # if we did spatial averages
    if (dim(c4R$Data)[2] > 100)
      cli::cli_alert_warning("Check that your performed annual aggregation before using this function")
    df <- reshape2::melt(c4R$Data) %>%
      tidyr::pivot_wider(values_from = "value", names_from = "Var1") %>%
      dplyr::select(-Var2) %>%
      mvabund::mvabund()

    df_var = data.frame(time = 1:nrow(df))
    mnlm <- mvabund::manylm(df ~ time, data = df_var)
    out <- anova(mnlm, p.uni = "adjusted")

    sig.models <-
      names(out$uni.p[2,][out$uni.p[2,] < 0.05]) # names of models with significance (p.value < 0.05)
    colnames(mnlm$coefficients) <- paste0("X", 1:mbrs)
    coef_res <- mnlm$coefficients[2,  sig.models]
    prop_res <-
      if (length(coef_res) == 0)
        999
    else
      sum(ifelse(coef_res  >= 0, 1, -1)) # number of models, with significance, that shows an increase or decraese. 999 assign to NA

    df_tm_series <- reshape2::melt(c4R$Data)  %>%
      dplyr::select(-Var2) %>%
      dplyr::group_by(Var1) %>%
      dplyr::mutate(date = seq(
        as.Date(c4R$Dates$start[[1]]),
        as.Date(c4R$Dates$start[[length(c4R$Dates$start)]]),
        by = "year"
      )) %>%
      dplyr::mutate(coef = prop_res, p.value = out$table[2, 4])

    return(df_tm_series)

  }
  cli::cli_process_done()

}

#' Apply linear regression to each member of multimember grid
#'
#' This function can be used after performing annual aggregation and with a multigrid object. it applies linear regression per pixel if
#' spatial averages are not performed or for spatially aggregated data.
#' @return array, without spatial averages, or dataframe, if spatial averages are performed
#'
#' @export


models_trends <- function(c4R, observation = F) {
  if (length(dim(c4R$Data)) > 2) {
    # in cases in which there is a spatial dimension
    cli::cli_progress_step(
      paste0(
        " Applying linear regression to ",
        ifelse(observation, "observation. ", "each ensemble member."),
        " P-value calculated using 999 iterations via residual (without replacement) resampling."
      )
    )
    # single model

    if (dim(c4R$Data)[ifelse(observation, 1, 2)] > 100)
      cli::cli_alert_warning("Check that your performed annual aggregation before using this function")


    ind.lm <-
      apply(c4R$Data, if (observation)
        c(2, 3)
        else
          c(1, 3, 4), function(y) {
            df <- reshape2::melt(y) %>%
              dplyr::mutate(time = 1:nrow(.))

            mod = mvabund::manylm(value ~ time, data = df)
            out <- anova(mod)
            return(cbind(mod$coefficients[2, 1], out$table[2, 4]))

          })

    cli::cli_process_done()
    return(ind.lm)
  } else {
    # when spatial averages are performed
    if (dim(c4R$Data)[ifelse(observation, 1, 2)] > 100)
      cli::cli_alert_warning("Check that your performed annual aggregation before using this function")
    if (!observation) {
      df_tm_series <- reshape2::melt(c4R$Data) %>%
        dplyr::group_by(Var1) %>%
        dplyr::mutate(
          coef = mvabund::manylm(value ~ Var2)$coefficients[2, 1],
          p.value = anova(mvabund::manylm(value ~ Var2))$table[2, 4]
        ) %>%
        dplyr::select(-Var2) %>%
        dplyr::mutate(date = seq(
          as.Date(c4R$Dates$start[[1]]),
          as.Date(c4R$Dates$start[[length(c4R$Dates$start)]]),
          by = "year"
        )) %>%
        dplyr::select(Var1, value, date, coef, p.value)

      return(df_tm_series)
    } else {
      df_tm_series <-
        data.frame(value = c4R$Data,
                   Var2 = 1:length(c4R$Data)) %>%
        dplyr::mutate(
          coef = mvabund::manylm(value ~ Var2)$coefficients[2, 1],
          p.value = anova(mvabund::manylm(value ~ Var2))$table[2, 4]
        ) %>%
        dplyr::select(-Var2) %>%
        dplyr::mutate(date = seq(
          as.Date(c4R$Dates$start[[1]]),
          as.Date(c4R$Dates$start[[length(c4R$Dates$start)]]),
          by = "year"
        )) %>%
        dplyr::select(value, date, coef, p.value)

      return(df_tm_series)

    }

  }
}

#' @export

ridgeline <- function(x,
                      num_grps = 10,
                      xlab = "Value",
                      ylab = "Group Intervals",
                      title = "",
                      legend_title = "z",
                      group_col,
                      z_col,
                      fill = NULL,
                      facet1 = NULL,
                      facet2=NULL) {
  if (missing(x)) {
    stop("Empty dataframe x. Please give a proper input.")
  }

  if (missing(group_col)) {
    stop("Group column not specified. Use group_col to specify group.")
  }

  if (missing(z_col)) {
    stop("Variable to plot is not specified. Use z_col to specify variable.")
  }

  df <- x
  ctgrp <- x <- NULL
  grp <- df[, group_col]
  z <- df[, z_col]
  f <- df[, fill]
  fc1 <- df[, facet1]
  fc2 <- df[, facet2]


  df2 <- data.frame(grp = grp,
                    z = z,
                    f = f,
                    fc1 = fc1,
                    fc2=fc2)
  df2$ctgrp <- cut(df2$grp, breaks = num_grps)


  ggplot2::ggplot(df2,
                  ggplot2::aes(y = ctgrp)) +
    ggridges::geom_density_ridges(
      ggplot2::aes(x = z, fill = if (!is.null(fill))
        f
        else
          NULL),
      scale = 1,
      rel_min_height = 0.01,
      alpha = .8,
      color = "white"
    ) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::ylab(ylab) +
    ggplot2::xlab(xlab) +
    if (!is.null(facet1) & !is.null(facet2) ) {
      ggplot2::facet_grid(fc1 ~ fc2)
    } else if (!is.null(facet1) & is.null(facet2)) {
      ggplot2::facet_grid(fc1 ~ .)
    } else if (is.null(facet1) & !is.null(facet2)) {
      ggplot2::facet_grid(fc2 ~ .)
    }
}

#' IPCC color palette
#'
#' automatically use the suggested IPCC colors for precipitation and temperature.
#' @param type character, one of tmp or pr.
#' @param divergent logical. If TRUE, divergent palette are used. Useful in combination with legend.range to assign central colors in the palette to zero values
#' @export
IPCC_palette <- function(type, divergent) {
  stopifnot(is.logical(divergent))
  match.arg(type, c("pr", "tmp"))
  if (type == "pr" & divergent) {
    c(
      rgb(84, 48, 5, maxColorValue = 255),
      rgb(140, 81, 10, maxColorValue = 255),
      rgb(191, 129, 45, maxColorValue = 255),
      rgb(223, 194, 125, maxColorValue = 255),
      rgb(246, 232, 195, maxColorValue = 255),
      "white",
      rgb(199, 234, 229, maxColorValue = 255),
      rgb(128, 205, 193, maxColorValue = 255),
      rgb(53, 151, 143, maxColorValue = 255),
      rgb(1, 102, 94, maxColorValue = 255),
      rgb(0, 60, 48, maxColorValue = 255)
    )
  } else if (type == "tmp" & divergent) {
    c(
      rgb(5, 48, 97, maxColorValue = 255),
      rgb(33, 102, 172, maxColorValue = 255),
      rgb(67, 147, 195, maxColorValue = 255),
      rgb(146, 197, 222, maxColorValue = 255),
      rgb(209, 229, 240, maxColorValue = 255),
      "white",
      rgb(253, 219, 199, maxColorValue = 255),
      rgb(244 , 165, 130, maxColorValue = 255),
      rgb(214, 96, 77, maxColorValue = 255),
      rgb(178, 24, 43, maxColorValue = 255),
      rgb(103, 0, 31, maxColorValue = 255)
    )

  } else if (type == "pr" & !divergent) {
    c(
      rgb(255, 255, 204, maxColorValue = 255),
      rgb(237, 248, 177, maxColorValue = 255),
      rgb(161, 218, 180, maxColorValue = 255),
      rgb(65, 182, 196, maxColorValue = 255),
      rgb(44, 127, 184, maxColorValue = 255),
      rgb(37, 52, 148, maxColorValue = 255),

    )
  } else {
    c(
      rgb(255, 255, 178, maxColorValue = 255),
      rgb(254, 204, 92, maxColorValue = 255),
      rgb(253, 141, 60, maxColorValue = 255),
      rgb(240, 59, 32, maxColorValue = 255),
      rgb(189, 0, 38, maxColorValue = 255),
      "#660000"
    )

  }
}
