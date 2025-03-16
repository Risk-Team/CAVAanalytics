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
DEFAULT_COLORS <- c("blue", "cyan", "green", "white", "orange", "red", "black")
DEFAULT_LINE_WIDTH <- 0.1
DEFAULT_ALPHA <- NA
DEFAULT_DATE_BREAKS <- "4 years"
DEFAULT_DATE_FORMAT <- "%Y"

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
           plot_titles = "default",
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
           plot_titles = "default",
           ensemble = TRUE,
           bins = FALSE,
           intervals = NULL,
           alpha = DEFAULT_ALPHA,
           stat = "mean",
           spatiotemporal = FALSE,
           temporal = FALSE,
           lwd = DEFAULT_LINE_WIDTH) {
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
            lwd
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
            lwd
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
            lwd
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
#' @export
plotting.CAVAanalytics_observations <-
  function(rst,
           palette = NULL,
           legend_range = NULL,
           plot_titles = "default",
           ensemble = FALSE,
           bins = FALSE,
           intervals = NULL,
           alpha = DEFAULT_ALPHA,
           spatiotemporal = FALSE,
           temporal = FALSE,
           lwd = DEFAULT_LINE_WIDTH,
           n.groups = 3) {
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
            lwd
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
            lwd
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
           plot_titles = "default",
           ensemble = TRUE,
           bins = FALSE,
           intervals = NULL,
           alpha = DEFAULT_ALPHA,
           temporal = FALSE,
           spatiotemporal = FALSE,
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
          DEFAULT_COLORS
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
        create_base_map(countries, lwd) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value),
                             data = rs_df,
                             alpha = alpha) +
        create_color_scale(colors, legend_range, bins, intervals) +
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
        create_common_theme(ensemble)

      cli::cli_progress_done()

      return(p)

    } else {
      # to look at temporal trends
      palette <- if (is.null(palette))
        DEFAULT_COLORS[1]
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
          dplyr::summarise(sd = sd(value),
                           value = mean(value)) %>%
          ggplot2::ggplot() +
          ggplot2::geom_hline(yintercept = 0,
                              linetype = "dashed",
                              color = "red") +
          ggplot2::geom_line(ggplot2::aes(y = value,
                                          x = date),
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
          ggplot2::scale_x_date(date_breaks = DEFAULT_DATE_BREAKS, date_labels = DEFAULT_DATE_FORMAT) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            legend.position = "bottom",
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            legend.title = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank()
          ) +
          ggplot2::facet_wrap( ~ season) +
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
          ggplot2::geom_line(ggplot2::aes(y = value,
                                          x = date),
                             alpha = 0.5) +
          ggplot2::scale_x_date(date_breaks = DEFAULT_DATE_BREAKS, date_labels = DEFAULT_DATE_FORMAT) +
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

#' Create base map with country boundaries
#' @noRd
create_base_map <- function(countries, lwd = 0.1) {
  list(
    ggplot2::geom_sf(
      fill = 'white',
      color = "black", 
      data = countries,
      alpha = 0.5,
      lwd = lwd
    ),
    ggplot2::geom_sf(
      fill = NA,
      color = "black",
      data = countries,
      lwd = lwd
    )
  )
}

#' Create common theme elements
#' @noRd 
create_common_theme <- function(ensemble = TRUE) {
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
      legend.position = if (ensemble) "right" else "bottom",
      legend.direction = if (ensemble) "vertical" else "horizontal",
      legend.key.height = if (ensemble) ggplot2::unit(1.2, 'cm') else ggplot2::unit(0.3, 'cm'),
      legend.key.width = if (ensemble) ggplot2::unit(0.3, 'cm') else ggplot2::unit(2, 'cm'),
      legend.box.spacing = ggplot2::unit(0, "pt"),
      legend.text = if (ensemble) NULL else ggplot2::element_text(angle = 45, hjust = 1)
    )
}

#' Create color scale
#' @noRd
create_color_scale <- function(colors = DEFAULT_COLORS, legend_range, bins = FALSE, intervals = NULL) {
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
      breaks = if (is.null(intervals)) ggplot2::waiver() else intervals,
      guide = ggplot2::guide_colourbar(
        ticks.colour = "black",
        ticks.linewidth = 1,
        title.position = "top",
        title.hjust = 0.5
      )
    )
  }
}

#' Create spatial plot
#' @noRd
create_spatial_plot <- function(data, ensemble, colors, legend_range, bins, intervals, alpha, plot_titles, lwd) {
  countries <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
  
  ggplot2::ggplot() +
    create_base_map(countries, lwd) +
    ggplot2::geom_raster(
      ggplot2::aes(x = x, y = y, fill = value),
      data = data,
      alpha = alpha
    ) +
    create_color_scale(colors, legend_range, bins, intervals) +
    create_common_theme(ensemble) +
    ggplot2::labs(fill = plot_titles)
}

#' Create temporal plot 
#' @noRd
create_temporal_plot <- function(data, ensemble, plot_titles, palette, legend_range) {
  base_plot <- ggplot2::ggplot(data) +
    ggplot2::geom_line(ggplot2::aes(x = date, y = value), 
                       color = if(is.null(palette)) DEFAULT_COLORS[1] else palette) +
    ggplot2::scale_x_date(date_breaks = DEFAULT_DATE_BREAKS, 
                         date_labels = DEFAULT_DATE_FORMAT) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Year", y = plot_titles)
    
  if (!is.null(legend_range)) {
    base_plot + ggplot2::scale_y_continuous(limits = legend_range)
  } else {
    base_plot
  }
}

#' Validate common plotting parameters
#' @noRd
validate_plot_params <- function(ensemble, bins, spatiotemporal, temporal) {
  stopifnot(
    is.logical(ensemble),
    is.logical(bins),
    is.logical(spatiotemporal),
    is.logical(temporal)
  )
  
  if (spatiotemporal && temporal) {
    cli::cli_abort("spatiotemporal and temporal cannot be both equal TRUE")
  }
}
