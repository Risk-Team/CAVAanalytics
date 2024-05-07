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


#' make a raster
#'
#' Make a spatRaster from a C4R list
#'
#' @param cl4.object A C4R list
#' @param dimensions vector specifying which dimensions corresponds to lat and lon. The rest will be averaged
#' @param shape.file sf object for which to crop and mask the spatRaster
#' @param stat statistic to apply. Default is mean
#' @return spatRaster
#' @export

make_raster <-
  function(cl4.object, dimensions, shape.file, stat = "mean") {
    xmin <-
      if (is.null(cl4.object$xyCoords$lon))
        min(cl4.object$xyCoords$x)
    else
      min(cl4.object$xyCoords$lon[1, ])
    xmax <-
      if (is.null(cl4.object$xyCoords$lon))
        max(cl4.object$xyCoords$x)
    else
      max(cl4.object$xyCoords$lon[1, ])
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
      apply(cl4.object$Data, dimensions, stat, na.rm = TRUE)


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
#' @param lowert numeric. Lower threshold
#' @param uppert numeric. Upper threshold
#' @param frequency logical. Whether frequency or abosulte numbers should be returned. Only works with duration != max
#' @return numeric of length 1
#' @export
# functions for consecutive days

thrs_consec = function(col, duration, lowert, uppert, frequency) {
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

  if (duration == "max" &
      frequency)
    stop(
      "Not meaningful. By definition, the maximum duration of an event, let's say a dry spell, has frequency of 1"
    )

  #get only connsecutive days matching the threshold

  consec_days = consec$lengths[consec$values == TRUE]

  #return values out

  if (duration == "max") {
    val <- max(consec_days, na.rm = T)
    return(if (val == "-Inf")
      0
      else
        val)

  } else{
    if (!frequency)
      return(sum(consec_days[consec_days > duration], na.rm = T))
    else
      return(length(na.omit(consec_days[consec_days > duration])))

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


#' @export

agreement = function(array3d, threshold) {
  # Define the inner function find.agreement within the agreement function
  find.agreement = function(x, threshold) {
    # Calculate proportion of models predicting each sign of change (negative(-1), no change(0), positive(+1))
    sign.proportion = c(length(x[x < 0]) / length(x),
                        length(x[x == 0]) / length(x),
                        length(x[x > 0]) / length(x))
    names(sign.proportion) = c("-1", "0", "1")
    # Compare the set threshold to the maximum proportion of models agreeing on any one sign of change
    # If the max proportion is higher than threshold, return 1 (meaning there is agreement in signs among model)
    # Otherwise return 0 (no agreement meeting the set threshold)
    if (max(sign.proportion) > threshold) {
      return(1)
    } else {
      return(0)
    }
  }

  # Apply find.agreement to each element of the array3d over the second and third dimensions
  array1_agreement = apply(array3d, c(2, 3), find.agreement, threshold)
  return(array1_agreement)
}


#' @export

models_trends <- function(c4R, observation = F) {
  if (length(dim(c4R$Data)) > 2) {
    # in cases in which there is a spatial dimension
    cli::cli_progress_step(
      paste0(
        " Applying linear regression. P-value calculated using 999 iterations via residual (without replacement) resampling."
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

ridgeline <- suppressWarnings({
  suppressMessages({
    function(x,
             num_grps = 10,
             xlab = "Value",
             ylab = "Group Intervals",
             title = "",
             legend_title = "z",
             group_col,
             z_col,
             fill = NULL,
             facet1 = NULL,
             facet2 = NULL) {
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


      df2 <- data.frame(
        grp = grp,
        z = z,
        f = f,
        fc1 = fc1,
        fc2 = fc2
      )
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
        if (!is.null(facet1) & !is.null(facet2)) {
          ggplot2::facet_grid(fc1 ~ fc2)
        } else if (!is.null(facet1) & is.null(facet2)) {
          ggplot2::facet_grid(fc1 ~ .)
        } else if (is.null(facet1) & !is.null(facet2)) {
          ggplot2::facet_grid(fc2 ~ .)
        }
    }
  })
})

#' IPCC color palette
#'
#' automatically use the suggested IPCC colors for precipitation and temperature.
#' @param type character, one of tmp or pr.
#' @param divergent logical. If TRUE, divergent palette are used. Useful in combination with legend.range to assign central colors in the palette to zero values
#' @export
IPCC_palette <- function(type, divergent)
{
  stopifnot(is.logical(divergent))
  match.arg(type, c("pr", "tmp"))
  if (type == "pr" & divergent)
  {
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
  } else
    if (type == "tmp" & divergent)
    {
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

    } else
      if (type == "pr" & !divergent)
      {
        c(
          rgb(255, 255, 204, maxColorValue = 255),
          rgb(237, 248, 177, maxColorValue = 255),
          rgb(161, 218, 180, maxColorValue = 255),
          rgb(65, 182, 196, maxColorValue = 255),
          rgb(44, 127, 184, maxColorValue = 255),
          rgb(37, 52, 148, maxColorValue = 255)

        )
      } else
      {
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


#' @export

convert_vector_to_month_initials <- function(month_vector) {
  # Ensure the vector is treated as a sequence, including wrapping cases
  seq_length <- length(month_vector)
  if (seq_length > 1) {
    # Extract the first letter of each month
    month_initials <- substr(month.abb[month_vector], 1, 1)
  } else {
    # If only one month is given, use the three-letter abbreviation
    month_initials <- month.abb[month_vector]
  }

  # Collapse into a single string
  paste(month_initials, collapse = "")
}


#' @export
spatial_prep = function(data,
                        index,
                        ccs_sign = F,
                        stat,
                        ensemble,
                        obs = F,
                        trends = F) {
  lngth <-
    length(stringr::str_split(names(data[[1]]), "_")[[1]]) # season is always at the end of the string
  order <-
    unique(purrr::map_chr(stringr::str_split(names(data[[1]]), "_"), ~ .x[lngth])) # order of seasons

  if (!obs) {
    if (ensemble) {
      cli::cli_text(
        paste0(
          "{cli::symbol$arrow_right}",
          " Visualizing ensemble ",
          stat,
          if (ccs_sign)
            " and agreement in the sign of change"
          else
            ""
        )
      )
    } else {
      cli::cli_text(
        paste0(
          "{cli::symbol$arrow_right} Visualizing individual members, argument stat is ignored.",
          if (ccs_sign)
            "To visualize model agreement set ensemble to F "
          else
            ""
        )
      )
    }

    rs_df <-
      terra::as.data.frame(data[[index]], xy = TRUE, na.rm = TRUE) %>%
      tidyr::pivot_longer(cols = 3:ncol(.),
                          values_to = "value",
                          names_to = "long_name") %>%  {
                            if (ensemble)
                            {
                              # Extract scenario and time frame from column names
                              tidyr::separate_wider_delim(
                                .,
                                long_name,
                                delim = "_",
                                names = c("scenario", "time_frame", "season")
                              ) %>%
                                # Replace "." with "-" in time frame
                                dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-")) %>%
                                dplyr::mutate(., season = factor(season, levels = order))

                            } else
                            {
                              # Extract Member, scenario and time frame from column names
                              tidyr::separate_wider_delim(
                                .,
                                long_name,
                                delim = "_",
                                names = c("member", "scenario", "time_frame", "season")
                              ) %>%
                                # Replace "." with "-" in time frame
                                dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-")) %>%
                                dplyr::mutate(., season = factor(season, levels = order))

                            }
                          }

    if (ccs_sign) {
      rs_df_sign <-
        terra::as.data.frame(data[[4]], xy = TRUE, na.rm = TRUE) %>%
        tidyr::pivot_longer(
          cols = 3:ncol(.),
          values_to = "value",
          names_to = "long_name"
        ) %>%
        # Extract scenario and time frame from column names
        tidyr::separate_wider_delim(
          .,
          long_name,
          delim = "_",
          names = c("scenario", "time_frame", "season")
        ) %>%
        # Replace "." with "-" in time frame
        dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-")) %>%
        dplyr::mutate(., season = factor(season, levels = order))


      return(list(rs_df, rs_df_sign))


    } else {
      return(list(rs_df))
    }
  } else {
    # when obs is TRUE
    if (!trends) {
      cli::cli_text(paste0(
        "{cli::symbol$arrow_right}",
        " Visualizing observational dataset "
      ))

      cli::cli_alert_warning(" Argument ensemble and stat are ignored")
      rs_df <-
        terra::as.data.frame(data[[index]], xy = TRUE, na.rm = TRUE) %>%
        tidyr::pivot_longer(
          cols = 3:ncol(.),
          values_to = "value",
          names_to = "long_name"
        ) %>%
        # Extract scenario and time frame from column names
        tidyr::separate_wider_delim(
          long_name,
          delim = "_",
          names = c("scenario", "time_frame", "season")
        )  %>%
        # Replace "." with "-" in time frame
        dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-")) %>%
        dplyr::mutate(., season = factor(season, levels = order))

      return(list(rs_df))

    } else {
      # linear regression for observations
      cli::cli_text(
        paste0(
          "{cli::symbol$arrow_right}",
          " Visualizing linear regression results for the observational dataset "
        )
      )

      cli::cli_alert_warning(" Argument ensemble and stat are ignored")

      rs_df <-
        purrr::map(
          data[1:2],
          ~ terra::as.data.frame(.x, xy = TRUE, na.rm = TRUE) %>%
            tidyr::pivot_longer(
              cols = 3:ncol(.),
              values_to = "value",
              names_to = "long_name",
            ) %>%
            tidyr::separate_wider_delim(
              .,
              long_name,
              delim = "_",
              names = c("scenario", "type", "time_frame", "season")
            ) %>%
            # Replace "." with "-" in time frame
            dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-")) %>%
            dplyr::mutate(., season = factor(season, levels = order))
        )

      return(rs_df)
    }
  }

}



#' @export
spatial_plot = function(spatial_data,
                        sign,
                        ensemble,
                        palette,
                        bins,
                        intervals,
                        alpha,
                        plot_titles,
                        legend_range,
                        obs,
                        trends,
                        lwd)
{
  # Suppress warnings
  options(warn = -1)

  # Get countries data
  countries <-
    rnaturalearth::ne_countries(scale = "large", returnclass = "sf")

  palette <-
    if (is.null(palette))
      c("blue", "cyan", "green", "yellow", "orange", "red", "black")
  else
    palette

  if (!sign)  {
    legend_range <-
      if (is.null(legend_range))
        c(range(spatial_data[[1]]$value, na.rm = TRUE))
    else
      legend_range
  } else  {
    # when for ccs, then give full legend_range values

    legend_range <-
      if (is.null(legend_range))
        c(-max(abs(
          range(spatial_data[[1]]$value, na.rm = TRUE)
        )), +max(abs(
          range(spatial_data[[1]]$value, na.rm = TRUE)
        )))
    else
      legend_range

  }

  if (!obs)
  {
    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        fill = 'white',
        color = "black",
        data = countries,
        alpha = 0.5,
        lwd = lwd
      ) +
      ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value),
                           data = spatial_data[[1]],
                           alpha = alpha) +
      ggplot2::geom_sf(
        fill = NA,
        color = "black",
        lwd = lwd,
        data = countries
      ) + {
        if (!bins)
        {
          ggplot2::scale_fill_gradientn(
            colors = palette,
            limits = legend_range,
            na.value = "transparent",
            n.breaks = 10,
            guide = ggplot2::guide_colourbar(
              ticks.colour = "black",
              ticks.linewidth = 1,
              title.position = "top",
              title.hjust = 0.5
            )
          )
        } else
        {
          ggplot2::scale_fill_stepsn(
            colors = palette,
            limits = legend_range,
            na.value = "transparent",
            breaks = if (is.null(intervals))
              ggplot2::waiver()
            else
              intervals,
            guide = ggplot2::guide_colourbar(
              ticks.colour = "black",
              ticks.linewidth = 1,
              title.position = "top",
              title.hjust = 0.5
            )
          )
        }
      } +
      ggplot2::coord_sf(
        xlim = c(
          range(spatial_data[[1]]$x)[[1]] - 0.5,
          range(spatial_data[[1]]$x)[[2]] + 0.5
        ),
        ylim = c(
          range(spatial_data[[1]]$y)[[1]] - 0.5,
          range(spatial_data[[1]]$y)[[2]] + 0.5
        ),
        expand = F,
        ndiscr = 500
      ) + {
        if (ensemble)
        {
          ggh4x::facet_nested(scenario ~ season)
        } else
        {
          ggh4x::facet_nested(scenario ~ season + member)
        }
      } +
      ggplot2::labs(fill = plot_titles, x = "", y = "") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.background = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = if (ensemble)
          "right"
        else
          "bottom",
        legend.direction = if (ensemble)
          "vertical"
        else
          "horizontal",
        legend.key.height = if (ensemble)
          ggplot2::unit(1.2, 'cm')
        else
          ggplot2::unit(0.3, 'cm'),
        legend.key.width = if (ensemble)
          ggplot2::unit(0.3, 'cm')
        else
          ggplot2::unit(2, 'cm'),
        legend.box.spacing = ggplot2::unit(0, "pt"),
        legend.text =  if (ensemble)
          NULL
        else
          ggplot2::element_text(angle = 45, hjust = 1)
      ) + {
        if (sign)
          ggplot2::geom_point(
            data = dplyr::filter(spatial_data[[2]], value == 1),
            size = 0.1,
            alpha = 0.4,
            color = "black",
            ggplot2::aes(x, y)
          )

      }

    return(p)

  } else
  {
    # when obs is TRUE

    if (!trends)
    {
      p <- ggplot2::ggplot() +
        ggplot2::geom_sf(
          fill = 'white',
          color = "black",
          data = countries,
          alpha = 0.5,
          lwd = lwd
        ) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value),
                             data = spatial_data[[1]],
                             alpha = alpha) +
        ggplot2::geom_sf(
          fill = NA,
          color = "black",
          lwd = lwd,
          data = countries
        ) + {
          if (!bins)
          {
            ggplot2::scale_fill_gradientn(
              colors = palette,
              limits = legend_range,
              na.value = "transparent",
              n.breaks = 10,
              guide = ggplot2::guide_colourbar(
                ticks.colour = "black",
                ticks.linewidth = 1,
                title.position = "top",
                title.hjust = 0.5,
                label.hjust = 1
              )
            )
          } else
          {
            ggplot2::scale_fill_stepsn(
              colors = palette,
              limits = legend_range,
              na.value = "transparent",
              breaks = if (is.null(intervals))
                ggplot2::waiver()
              else
                intervals,
              guide = ggplot2::guide_colourbar(
                ticks.colour = "black",
                ticks.linewidth = 1,
                title.position = "top",
                title.hjust = 0.5,
                label.hjust = 1
              )
            )

          }

        } +
        ggplot2::coord_sf(
          xlim = c(
            range(spatial_data[[1]]$x)[[1]] - 0.5,
            range(spatial_data[[1]]$x)[[2]] + 0.5
          ),
          ylim = c(
            range(spatial_data[[1]]$y)[[1]] - 0.5,
            range(spatial_data[[1]]$y)[[2]] + 0.5
          ),
          expand = F,
          ndiscr = 500
        ) +
        ggh4x::facet_nested(scenario  ~ season) +
        ggplot2::labs(fill = plot_titles, x = "", y = "") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          strip.text.y = ggplot2::element_blank(),
          plot.background = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          legend.position = "right",
          legend.key.height = ggplot2::unit(1, 'cm'),
          legend.key.width = ggplot2::unit(0.3, 'cm'),
          legend.box.spacing = ggplot2::unit(0.2, "pt")
        )

      return(p)

    } else
    {
      # when linear regression is visualized

      p <- ggplot2::ggplot() +
        ggplot2::geom_sf(
          fill = 'white',
          color = "black",
          data = countries,
          alpha = 0.5,
          lwd = lwd
        ) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value),
                             data = spatial_data[[1]],
                             alpha = alpha) +
        ggplot2::geom_sf(
          fill = NA,
          color = "black",
          lwd = lwd,
          data = countries
        ) + {
          if (!bins)
          {
            ggplot2::scale_fill_gradientn(
              colors = palette,
              limits = legend_range,
              na.value = "transparent",
              n.breaks = 10,
              guide = ggplot2::guide_colourbar(
                ticks.colour = "black",
                ticks.linewidth = 1,
                title.position = "top",
                title.hjust = 0.5
              )
            )
          } else
          {
            ggplot2::scale_fill_stepsn(
              colors = palette,
              limits = legend_range,
              na.value = "transparent",
              breaks = if (is.null(intervals))
                ggplot2::waiver()
              else
                intervals,
              guide = ggplot2::guide_colourbar(
                ticks.colour = "black",
                ticks.linewidth = 1,
                title.position = "top",
                title.hjust = 0.5
              )
            )
          }
        } +
        ggplot2::geom_point(
          data = dplyr::filter(spatial_data[[2]], value < 0.05),
          size = 0.1,
          alpha = 0.4,
          color = "black",
          ggplot2::aes(x, y)
        ) +
        ggplot2::coord_sf(
          xlim = c(
            range(spatial_data[[2]]$x)[[1]] - 0.5,
            range(spatial_data[[2]]$x)[[2]] + 0.5
          ),
          ylim = c(
            range(spatial_data[[2]]$y)[[1]] - 0.5,
            range(spatial_data[[2]]$y)[[2]] + 0.5
          ),
          expand = F,
          ndiscr = 500
        ) +
        ggh4x::facet_nested(scenario  ~ season) +
        ggplot2::labs(fill = plot_titles, x = "", y = "") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          strip.text.y = ggplot2::element_blank(),
          plot.background = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          legend.position = "right",
          legend.key.height = ggplot2::unit(1, 'cm'),
          legend.key.width = ggplot2::unit(0.3, 'cm'),
          legend.box.spacing = ggplot2::unit(0.2, "pt")
        )

      return(p)

    }

  }

}

#' @export
temporal_plot = function(data,
                         index,
                         ensemble,
                         spatial.aggr = F,
                         plot_titles,
                         palette,
                         legend_range,
                         obs = F)
{
  lngth <-
    length(stringr::str_split(names(data[[1]]), "_")[[1]]) # season is always at the end of the string
  order <-
    unique(purrr::map_chr(stringr::str_split(names(data[[1]]), "_"), ~ .x[lngth])) # order of seasons

  df.processed <-  if (spatial.aggr)
  {
    cli::cli_text(paste0(
      "{cli::symbol$arrow_right}",
      " Visualizing annual time series "
    ))
    data[[index]] %>%
      dplyr::group_by(date, experiment, Var1, season) %>%
      dplyr::summarise(value = median(value)) %>% # spatial aggregation
      dplyr::mutate(season = factor(season, levels = order)) # ordering seasons

  } else {
    if (!obs) {
      cli::cli_text(paste0(
        "{cli::symbol$arrow_right}",
        " Visualizing annual anomaly time series "
      ))
    } else {
      cli::cli_text(
        paste0(
          "{cli::symbol$arrow_right}",
          " Visualizing annual time series for the observational dataset"
        )
      )
    }
    data[[index]] %>%
      dplyr::mutate(season = factor(season, levels = order))

  }

  if (!obs)
  {
    palette <-
      if (is.null(palette))
        RColorBrewer::brewer.pal(min(length(unique(
          data[[index]]$experiment
        )), RColorBrewer::brewer.pal.info["Set2", "maxcolors"]),
        "Set2")
    else
      palette
    cli::cli_alert_warning(" Arguments stat, bins,intervals, alpha and lwd are ignored")
    if (ensemble)
    {
      p <-  df.processed %>%
        dplyr::group_by(date, experiment, season) %>%
        dplyr::summarise(sd = sd(value),
                         value = mean(value)) %>%
        ggplot2::ggplot() +
        ggplot2::geom_line(
          ggplot2::aes(y = value,
                       x = date,
                       color = experiment),
          linetype = "dotted",
          alpha = 0.5,
          linewidth = 0.9
        ) +
        ggplot2::geom_smooth(
          ggplot2::aes(y = value,
                       x = date,
                       color = experiment),
          se = F,
          linewidth = 1,
          method = "gam",
          formula = y ~ x
        ) +
        ggplot2::geom_ribbon(
          ggplot2::aes(
            y = value,
            x = date,
            ymin = value - sd,
            ymax = value + sd,
            fill = experiment
          ),
          alpha = 0.15,
          show.legend = F
        ) +
        ggplot2::facet_wrap(season ~ .) +
        ggplot2::scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          legend.position = "bottom",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          legend.title = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        ) +
        ggplot2::labs(x = "Year", y = plot_titles) +
        ggplot2::scale_color_manual(values = palette) +
        ggplot2::scale_fill_manual(values = palette) +
        if (!is.null(legend_range))
        {
          ggplot2::ylim(legend_range[1], legend_range[2])
        }

      return(p)


    } else
    {
      # individual models
      p <- df.processed  %>%
        ggplot2::ggplot() +
        ggplot2::geom_line(
          ggplot2::aes(y = value,
                       x = date,
                       color = experiment),
          linetype = "dotted",
          alpha = 0.7
        ) +
        ggplot2::geom_smooth(
          ggplot2::aes(y = value,
                       x = date,
                       color = experiment),
          se = F,
          linewidth = 0.5,
          method = "gam",
          formula = y ~ x
        ) +
        ggplot2::facet_grid(season ~ Var1) +
        ggplot2::scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          legend.position = "bottom",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          legend.title = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        ) +
        ggplot2::labs(x = "Year", y = plot_titles) +
        ggplot2::scale_color_manual(values = palette) +
        ggplot2::scale_fill_manual(values = palette) +
        if (!is.null(legend_range))
        {
          ggplot2::ylim(legend_range[1], legend_range[2])
        }

      return(p)
    }
  } else
  {
    # when obs is TRUE
    palette <- if (is.null(palette))
      "black"
    else
      palette


    cli::cli_alert_warning(" Arguments stat, bins,intervals, alpha and lwd are ignored")
    p <- df.processed  %>%
      dplyr::group_by(date, experiment, season) %>%
      dplyr::summarise(value = mean(value)) %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(
        ggplot2::aes(y = value,
                     x = date,
                     color = experiment),
        linetype = "dotted",
        alpha = 0.7,
        linewidth = 0.7
      ) +
      ggplot2::geom_smooth(
        ggplot2::aes(y = value,
                     x = date,
                     color = experiment),
        se = F,
        linewidth = 0.8,
        method = "gam",
        formula = y ~ x
      ) +
      ggplot2::facet_wrap( ~ season) +
      ggplot2::scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      ) +
      ggplot2::labs(x = "Year", y = plot_titles) +
      ggplot2::scale_color_manual(values = palette) +
      if (!is.null(legend_range))
      {
        ggplot2::ylim(legend_range[1], legend_range[2])
      }

    return(p)

  }

}


#' @export

spatiotemporal_plot = function(data,
                               index,
                               ensemble,
                               plot_titles,
                               palette,
                               legend_range,
                               n.groups,
                               obs = F)
{
  lngth <-
    length(stringr::str_split(names(data[[1]]), "_")[[1]]) # season is always at the end of the string
  order <-
    unique(purrr::map_chr(stringr::str_split(names(data[[1]]), "_"), ~ .x[lngth])) # order of seasons

  cli::cli_text(paste0("{cli::symbol$arrow_right}",
                       " Visualizing frequencies "))

  df.processed <-   data[[index]] %>%
    dplyr::mutate(season = factor(season, levels = order))

  if (!obs)
  {
    cli::cli_alert_warning(
      " Arguments bins, stat, alpha and lwd are ignored. Change number of group intervals with n.groups"
    )


    palette <-
      if (is.null(palette))
        RColorBrewer::brewer.pal(min(length(unique(
          data[[index]]$experiment
        )), RColorBrewer::brewer.pal.info["Set2", "maxcolors"]),
        "Set2")
    else
      palette

    if (ensemble)
    {
      p <-
        suppressWarnings(
          suppressMessages(
            df.processed  %>%
              ridgeline(
                .,
                group_col = 'date',
                z_col = 'value',
                num_grps = n.groups,
                fill = 'experiment',
                facet1 = 'season'
              ) +
              ggplot2::theme_bw() +
              ggplot2::theme(
                legend.position = "bottom",
                legend.title = ggplot2::element_blank()
              ) +
              ggplot2::labs(x = plot_titles) +
              ggplot2::scale_fill_manual(values =  palette) +
              if (!is.null(legend_range))
              {
                ggplot2::xlim(legend_range[1], legend_range[2])
              }
          )
        )

      return(p)

    } else
    {
      # when ensemble is FALSE for individual models and spatiotemporal
      p <-
        suppressWarnings(
          suppressMessages(
            df.processed %>%
              ridgeline(
                .,
                group_col = 'date',
                z_col = 'value',
                num_grps = n.groups,
                fill = 'experiment',
                facet1 = 'Var1',
                facet2 = 'season'
              ) +
              ggplot2::theme_bw() +
              ggplot2::theme(
                legend.position = "bottom",
                legend.title = ggplot2::element_blank()
              ) +
              ggplot2::labs(x = plot_titles) +
              ggplot2::scale_fill_manual(values =  palette) +
              if (!is.null(legend_range))
              {
                ggplot2::xlim(legend_range[1], legend_range[2])
              }
          )
        )

      return(p)

    }

  } else
  {
    # when obs is TRUE
    cli::cli_alert_warning(
      " Arguments bins,palette, intervals,alpha, lwd and ensemble are ignored. Change number of group intervals with n.groups"
    )

    p <-
      suppressWarnings(
        suppressMessages(
          df.processed %>%
            ridgeline(
              .,
              group_col = 'date',
              z_col = 'value',
              num_grps = n.groups,
              facet1 =  'season'
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(legend.position = "none") +
            ggplot2::labs(x = plot_titles) +
            if (!is.null(legend_range))
            {
              ggplot2::xlim(legend_range[1], legend_range[2])
            }
        )
      )

    return(p)


  }

}
