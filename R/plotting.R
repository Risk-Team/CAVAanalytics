#' Plot CAVAanalytics Results
#'
#' @title Plot CAVAanalytics Results
#' @description Generic plotting function for CAVAanalytics outputs
#' @param rst Output from CAVAanalytics functions
#' @param palette Color palette to use
#' @param ... Additional arguments passed to specific methods
#' @return A ggplot2 object
#' @importFrom ggplot2 ggplot geom_raster aes scale_fill_gradientn theme_bw theme labs
#' @export
plotting <- function(rst, palette, ...) {
  UseMethod("plotting")
}

# Constants --------------------------------------------------
DEFAULT_LINE_WIDTH <- 0.1
DEFAULT_ALPHA <- NA
POINT_SIZE <- 0.1
PLOT_TITLES = "default"
# Projections --------------------------------------------------
#' @rdname plotting
#' @method plotting CAVAanalytics_projections
#' @param palette Color palette to use. Default is NULL
#' @param legend_range Fix legend limits. Default is NULL
#' @param plot_titles Title of the plot legend. Default is "default"
#' @param ensemble Whether to visualize ensemble mean or individual models. Default is TRUE
#' @param bins Whether to visualize colors as gradient or bins. Default is FALSE
#' @param intervals Control number of bins when bins=TRUE. Default is NULL
#' @param alpha Transparency of colors. Default is NA
#' @param stat Statistic to compute ("mean" or "sd"). Default is "mean"
#' @param spatiotemporal Whether to show frequencies without aggregation. Default is FALSE
#' @param temporal Whether to show temporal aggregation. Default is FALSE
#' @param lwd Width of country boundaries. Default is 0.1
#' @param n.groups Number of groups for spatiotemporal plots. Default is 3
#' @export

plotting.CAVAanalytics_projections <-
  function(rst,
           palette = NULL,
           legend_range = NULL,
           plot_titles =PLOT_TITLES,
           ensemble = TRUE,
           bins = FALSE,
           intervals = NULL,
           alpha = DEFAULT_ALPHA,
           stat = "mean",
           spatiotemporal = FALSE,
           temporal = FALSE,
           lwd = DEFAULT_LINE_WIDTH,
           n.groups = 3) {
    # check -------------------------------------------------------------------

    stopifnot(is.logical(ensemble))
    stopifnot(is.logical(bins))
    match.arg(stat, choices = c("mean", "sd"))
    stopifnot(is.logical(spatiotemporal))
    stopifnot(is.logical(temporal))

    if (spatiotemporal &
        temporal)
      cli::cli_abort("spatiotemporal and temporal cannot be both equal TRUE")

    # start code -------------------------------------------------------------------
    cli::cli_progress_step("Plotting")

    p <-  if (!temporal & !spatiotemporal)  {
      if (ensemble &
          stat == "mean") {
        spatial_prep(rst, 1, ccs_sign = F, stat, ensemble) %>%
          spatial_plot(
            .,
            sign = F,
            ensemble,
            palette,
            bins,
            intervals,
            alpha,
            plot_titles,
            legend_range,
            obs = F,
            trends = F,
            lwd
          )
      } else if (ensemble & stat == "sd") {
        spatial_prep(rst, 2, ccs_sign = F, stat, ensemble) %>%
          spatial_plot(
            .,
            sign = F,
            ensemble,
            palette,
            bins,
            intervals,
            alpha,
            plot_titles,
            legend_range,
            obs = F,
            trends = F,
            lwd
          )
      } else {
        # individual models
        spatial_prep(rst, 3, ccs_sign = F, stat, ensemble) %>%
          spatial_plot(
            .,
            sign = F,
            ensemble,
            palette,
            bins,
            intervals,
            alpha,
            plot_titles,
            legend_range,
            obs = F,
            trends = F,
            lwd
          )
      }
    } else if (temporal & !spatiotemporal) {
      # when temporal is TRUE
      temporal_plot(rst,
                    4,
                    ensemble,
                    spatial.aggr = T,
                    plot_titles,
                    palette,
                    legend_range)
    } else {
      spatiotemporal_plot(rst,
                          4,
                          ensemble,
                          plot_titles,
                          palette,
                          legend_range,
                          n.groups)
    }

    cli::cli_progress_done()
    return(p)

  }

# climate change signal ---------------------------------------------------


#' @rdname plotting
#' @method plotting CAVAanalytics_ccs
#' @param palette Color palette to use. Default is NULL
#' @param legend_range Fix legend limits. Default is NULL
#' @param plot_titles Title of the plot legend. Default is "default"
#' @param ensemble Whether to visualize ensemble mean or individual models. Default is TRUE
#' @param bins Whether to visualize colors as gradient or bins. Default is FALSE
#' @param intervals Control number of bins when bins=TRUE. Default is NULL
#' @param alpha Transparency of colors. Default is NA
#' @param stat Statistic to compute ("mean" or "sd"). Default is "mean"
#' @param spatiotemporal Whether to show frequencies without aggregation. Default is FALSE
#' @param temporal Whether to show temporal aggregation. Default is FALSE
#' @param lwd Width of country boundaries. Default is 0.1
#' @export
plotting.CAVAanalytics_ccs <-
  function(rst,
           palette = NULL,
           legend_range = NULL,
           plot_titles = PLOT_TITLES,
           ensemble = TRUE,
           bins = FALSE,
           intervals = NULL,
           alpha = DEFAULT_ALPHA,
           stat = "mean",
           spatiotemporal = FALSE,
           temporal = FALSE,
           lwd = DEFAULT_LINE_WIDTH,
           point_size=POINT_SIZE) {
    # check inputs ------------------------------------------------------------

    stopifnot(is.logical(ensemble))
    stopifnot(is.logical(bins))
    match.arg(stat, choices = c("mean", "sd"))
    stopifnot(is.logical(spatiotemporal))
    stopifnot(is.logical(temporal))

    if (spatiotemporal)
      cli::cli_abort("Feature not meaningful for this object type")

    # start code -------------------------------------------------------------------
    cli::cli_progress_step("Plotting")

    p <-  if (!temporal)  {
      if (ensemble &
          stat == "mean") {
        spatial_prep(
          data = rst,
          index = 1,
          ccs_sign = T,
          stat,
          ensemble
        ) %>%
          spatial_plot(
            .,
            sign = T,
            ensemble,
            palette,
            bins,
            intervals,
            alpha,
            plot_titles,
            legend_range,
            obs = F,
            trends = F,
            lwd,
            point_size
          )
      } else if (ensemble & stat == "sd") {
        spatial_prep(
          data = rst,
          index = 2,
          ccs_sign = T,
          stat,
          ensemble
        ) %>%
          spatial_plot(
            .,
            sign = T,
            ensemble,
            palette,
            bins,
            intervals,
            alpha,
            plot_titles,
            legend_range,
            obs = F,
            trends = F,
            lwd,
            point_size
          )
      } else {
        # individual models
        spatial_prep(
          data = rst,
          index = 3,
          ccs_sign = T,
          stat,
          ensemble
        ) %>%
          spatial_plot(
            .,
            sign = F,
            ensemble,
            palette,
            bins,
            intervals,
            alpha,
            plot_titles,
            legend_range,
            obs = F,
            trends = F,
            lwd,
            point_size
          )
      }
    } else {
      # when temporal is TRUE
      temporal_plot(
        data = rst,
        index = 5,
        ensemble,
        spatial.aggr = F,
        plot_titles,
        palette,
        legend_range
      )
    }

    cli::cli_progress_done()
    return(p)

  }


# observations ------------------------------------------------------------

#' @rdname plotting
#' @method plotting CAVAanalytics_observations
#' @param palette Color palette to use. Default is NULL
#' @param legend_range Fix legend limits. Default is NULL
#' @param plot_titles Title of the plot legend. Default is "default"
#' @param ensemble Whether to visualize ensemble mean or individual models. Default is FALSE
#' @param bins Whether to visualize colors as gradient or bins. Default is FALSE
#' @param intervals Control number of bins when bins=TRUE. Default is NULL
#' @param alpha Transparency of colors. Default is NA
#' @param spatiotemporal Whether to show frequencies without aggregation. Default is FALSE
#' @param temporal Whether to show temporal aggregation. Default is FALSE
#' @param lwd Width of country boundaries. Default is 0.1
#' @param n.groups Number of groups for spatiotemporal plots. Default is 3
#' @param point_size Size of dots for significance of trends. Default is 0.1
#' @export
plotting.CAVAanalytics_observations <-
  function(rst,
           palette = NULL,
           legend_range = NULL,
           plot_titles = PLOT_TITLES,
           ensemble = FALSE,
           bins = FALSE,
           intervals = NULL,
           alpha = DEFAULT_ALPHA,
           spatiotemporal = FALSE,
           temporal = FALSE,
           lwd = DEFAULT_LINE_WIDTH,
           n.groups = 3,
           point_size=POINT_SIZE) {
    # check inputs ------------------------------------------------------------

    stopifnot(is.logical(bins))
    stopifnot(is.logical(spatiotemporal))
    stopifnot(is.logical(temporal))
    if (spatiotemporal &
        temporal)
      cli::cli_abort("spatiotemporal and temporal cannot be both equal TRUE")


    # start of code -----------------------------------------------------------
    cli::cli_progress_step("Plotting")

    p <- if (length(rst) == 2) {
      # linear regression was not applied
      if (!temporal & !spatiotemporal) {
        spatial_prep(
          data = rst,
          index = 1,
          ccs_sign = F,
          stat,
          ensemble,
          obs = T
        ) %>%
          spatial_plot(
            .,
            sign = F,
            ensemble,
            palette,
            bins,
            intervals,
            alpha,
            plot_titles,
            legend_range,
            obs = T,
            trends = F,
            lwd,
            point_size
          )
      } else {
        if (temporal) {
          temporal_plot(
            data = rst,
            index = 2,
            ensemble,
            spatial.aggr = F,
            plot_titles,
            palette,
            legend_range,
            obs = T
          )
        } else {
          # spatiotemporal
          spatiotemporal_plot(rst,
                              2,
                              ensemble,
                              plot_titles,
                              palette,
                              legend_range,
                              n.groups,
                              obs = T)
        }
      }

    } else {
      # when linear regression is used
      if (!temporal & !spatiotemporal) {
        spatial_prep(
          data = rst,
          index = 1,
          ccs_sign = F,
          stat,
          ensemble,
          obs = T,
          trends = T
        ) %>%
          spatial_plot(
            .,
            sign = F,
            ensemble,
            palette,
            bins,
            intervals,
            alpha,
            plot_titles,
            legend_range,
            obs = T,
            trends = T,
            lwd,
            point_size
          )
      } else {
        # when spatiotemproal or temporal is TRUE
        if (temporal) {
          temporal_plot(
            data = rst,
            index = 3,
            ensemble,
            spatial.aggr = F,
            plot_titles,
            palette,
            legend_range,
            obs = T
          )
        } else {
          # spatiotemporal
          spatiotemporal_plot(rst,
                              3,
                              ensemble,
                              plot_titles,
                              palette,
                              legend_range,
                              n.groups,
                              obs = T)
        }

      }

    }

    cli::cli_progress_done()

    return(p)

  }


# biases ------------------------------------------------------------------


#' @rdname plotting
#' @method plotting CAVAanalytics_model_biases
#' @param palette Color palette to use. Default is NULL
#' @param legend_range Fix legend limits. Default is NULL
#' @param plot_titles Title of the plot legend. Default is "default"
#' @param ensemble Whether to visualize ensemble mean or individual models. Default is TRUE
#' @param bins Whether to visualize colors as gradient or bins. Default is FALSE
#' @param intervals Control number of bins when bins=TRUE. Default is NULL
#' @param alpha Transparency of colors. Default is NA
#' @param temporal Whether to show temporal aggregation. Default is FALSE
#' @param spatiotemporal Whether to show frequencies without aggregation. Default is FALSE
#' @param lwd Width of country boundaries. Default is 0.1
#' @export

plotting.CAVAanalytics_model_biases <-
  function(rst,
           palette = NULL,
           legend_range = NULL,
           plot_titles = PLOT_TITLES,
           ensemble = TRUE,
           bins = FALSE,
           intervals = NULL,
           alpha = DEFAULT_ALPHA,
           temporal = F,
           spatiotemporal = F,
           lwd = DEFAULT_LINE_WIDTH) {
    # check -------------------------------------------------------------------
    if (spatiotemporal)
      cli::cli_abort("Not meaningful for model biases")
    stopifnot(is.logical(ensemble))
    stopifnot(is.logical(bins))
    stopifnot(is.logical(temporal))

    # start code --------------------------------------------------------------

    if (ensemble) {
      cli::cli_text(
        if (temporal)
          "{cli::symbol$arrow_right} Visualizing annual time series for ensemble bias"
        else
          "{cli::symbol$arrow_right} Visualizing spatial ensemble bias"
      )
    } else {
      cli::cli_text(
        if (temporal)
          "{cli::symbol$arrow_right} Visualizing annual time series individual member biases"
        else
          "{cli::symbol$arrow_right} Visualizing individual member spatial biases"
      )
    }

    # retrieve the right spatraster based on the ensemble argument

    lngth <-
      length(stringr::str_split(names(rst[[1]]), "_")[[1]]) # season is always at the end of the string
    order <-
      purrr::map_chr(stringr::str_split(names(rst[[1]]), "_"), ~ .x[lngth]) # order of seasons


    rst <-
      if (ensemble &
          !temporal)
        rst[[1]]
    else if (!ensemble & !temporal)
      rst[[2]]
    else
      rst[[3]]


    if (!temporal) {
      # Set default colors for legend
      colors <-
        if (is.null(palette))
          c("blue", "cyan", "green", "white", "orange", "red", "black")
      else
        palette

      # Set default range for legend
      legend_range <-
        if (is.null(legend_range))
          c(-max(abs(range(
            terra::values(rst), na.rm = TRUE
          ))), +max(abs(range(
            terra::values(rst), na.rm = TRUE
          ))))
      else
        legend_range

      # Suppress warnings
      options(warn = -1)

      # Get countries data
      countries <-
        rnaturalearth::ne_countries(scale = "large", returnclass = "sf")

      # Convert SpatRaster to dataframe

      cli::cli_progress_step("Plotting")

      rs_df <-
        terra::as.data.frame(rst, xy = TRUE, na.rm = TRUE) %>%
        tidyr::pivot_longer(cols = 3:ncol(.),
                            values_to = "value",
                            names_to = "long_name") %>%  {
                              if (ensemble) {
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

                              } else {
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

      p <- ggplot2::ggplot() +
        ggplot2::geom_sf(
          fill = 'white',
          color = "black",
          data = countries,
          alpha = 0.5,
          lwd = lwd
        ) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value),
                             data = rs_df,
                             alpha = alpha) +
        ggplot2::geom_sf(
          fill = NA,
          color = "black",
          data = countries,
          lwd = lwd
        ) +
        {
          if (!bins) {
            ggplot2::scale_fill_gradientn(
              colors = colors,
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
          } else {
            ggplot2::scale_fill_stepsn(
              colors = colors,
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
          xlim = c(range(rs_df$x)[[1]] - 0.5, range(rs_df$x)[[2]] + 0.5),
          ylim = c(range(rs_df$y)[[1]] - 0.5, range(rs_df$y)[[2]] + 0.5),
          expand = F,
          ndiscr = 500
        ) +
        {
          if (ensemble) {
            ggh4x::facet_nested(season ~ .)
          } else {
            ggh4x::facet_nested(. ~ season + member)
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
        )

      cli::cli_progress_done()

      return(p)

    } else {
      # to look at temporal trends
      palette <- if (is.null(palette))
        "black"
      else
        palette
      if (is.null(rst))
        cli::cli_abort(c("x" = "Not allowed with load_data_and_model_biases"))

      cli::cli_alert_warning(" Arguments bins, intervals, and alpha are ignored")
      cli::cli_progress_step("Plotting")
      if (ensemble) {
        p <- rst %>%
          dplyr::mutate(., season = factor(season, levels = order)) %>%
          dplyr::mutate(value = value - obs_value) %>%
          dplyr::group_by(date, season) %>%
          dplyr::summarise(sd = sd(value), value = mean(value)) %>%
          ggplot2::ggplot() +
          ggplot2::geom_hline(yintercept = 0,
                              linetype = "dashed",
                              color = "red") +
          ggplot2::geom_line(ggplot2::aes(y = value, x = date),
                             color = palette,
                             alpha = 0.5) +
          ggplot2::geom_ribbon(
            ggplot2::aes(
              y = value,
              x = date,
              ymin = value - sd,
              ymax = value + sd
            ),
            fill = palette,
            alpha = 0.1,
            show.legend = F
          ) +
          ggplot2::scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
          ggplot2::theme_bw() +
          ggplot2::theme(
            legend.position = "bottom",
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            legend.title = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank()
          ) +
          ggplot2::facet_wrap(~ season) +
          ggplot2::labs(x = "Year", y = plot_titles) +
          if (!is.null(legend_range)) {
            ggplot2::scale_y_continuous(limits = legend_range)
          }

      } else {
        p <- rst %>%
          dplyr::mutate(., season = factor(season, levels = order)) %>%
          dplyr::mutate(value = value - obs_value) %>%
          ggplot2::ggplot() +
          ggplot2::geom_hline(yintercept = 0,
                              linetype = "dashed",
                              color = "red") +
          ggplot2::geom_line(ggplot2::aes(y = value, x = date), alpha = 0.5) +
          ggplot2::scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
          ggplot2::facet_grid(season ~ Var1) +
          ggplot2::theme_bw() +
          ggplot2::labs(x = "Year", y = plot_titles) +
          ggplot2::theme(
            legend.position = "bottom",
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            legend.title = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank()
          ) +
          if (!is.null(legend_range)) {
            ggplot2::scale_y_continuous(limits = legend_range)
          }



      }
      cli::cli_process_done()
      return(p)
    }


  }

# Helper functions --------------------------------------------------

#' @noRd
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


      ggplot2::ggplot(df2, ggplot2::aes(y = ctgrp)) +
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


#' @noRd
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

#' @noRd
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



#' @noRd
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
                        lwd,
                        point_size)
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
            size = point_size,
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
          size = point_size,
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

#' @noRd
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
        )), RColorBrewer::brewer.pal.info["Set2", "maxcolors"]), "Set2")
    else
      palette
    cli::cli_alert_warning(" Arguments stat, bins,intervals, alpha and lwd are ignored")
    if (ensemble)
    {
      p <-  df.processed %>%
        dplyr::group_by(date, experiment, season) %>%
        dplyr::summarise(sd = sd(value), value = mean(value)) %>%
        ggplot2::ggplot() +
        ggplot2::geom_line(
          ggplot2::aes(y = value, x = date, color = experiment),
          linetype = "dotted",
          alpha = 0.5,
          linewidth = 0.9
        ) +
        ggplot2::geom_smooth(
          ggplot2::aes(y = value, x = date, color = experiment),
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
          ggplot2::aes(y = value, x = date, color = experiment),
          linetype = "dotted",
          alpha = 0.7
        ) +
        ggplot2::geom_smooth(
          ggplot2::aes(y = value, x = date, color = experiment),
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
        ggplot2::aes(y = value, x = date, color = experiment),
        linetype = "dotted",
        alpha = 0.7,
        linewidth = 0.7
      ) +
      ggplot2::geom_smooth(
        ggplot2::aes(y = value, x = date, color = experiment),
        se = F,
        linewidth = 0.8,
        method = "gam",
        formula = y ~ x
      ) +
      ggplot2::facet_wrap(~ season) +
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

#' @noRd

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

  cli::cli_text(paste0("{cli::symbol$arrow_right}", " Visualizing frequencies "))

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
        )), RColorBrewer::brewer.pal.info["Set2", "maxcolors"]), "Set2")
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



#' Clean text from plots
#'
#' Remove facets labels from ggplot object
#'
#' @param position character indicating which facets to remove (both, x or y).
#' @export
remove_facets = function(position = "both") {
  if (position == "both")
    ggplot2::theme(
      strip.text.x = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "black", fill =
                                             NA)
    )
  else if (position == "x")

    ggplot2::theme(strip.text.x = ggplot2::element_blank())
  else if (position == "y")

    ggplot2::theme(strip.text.y = ggplot2::element_blank())
  else

    cli::cli_abort(c("x" = "position can be equal to both, x or y"))

}

#' Customise facet text
#'
#' Customise facets labels in ggplot object. It only works when ensemble is TRUE
#'
#' @param current_label character indicating the current facet label.
#' @param new_label character indicating which new label to use.
#' @param position character indicating which facets to change (x or y).
#' @export

rename_facets = function(current_label, new_label, position = "x") {
  cli::cli_alert_warning(c("!" = "Customise_facets can only be used with ensemble equal TRUE"))

  names(new_label) = current_label

  if (position == "x")
    ggh4x::facet_nested(scenario ~ season, labeller = ggplot2::labeller(season = new_label))
  else if (position == "y")

    ggh4x::facet_nested(scenario ~ season, labeller = ggplot2::labeller(scenario = new_label))
  else

    cli::cli_abort(c("x" = "position can be equal to x or y"))

}
