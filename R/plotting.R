#' Visualize results
#'
#' Automatically plot results from CAVAanalytics main functions
#' @export

#' @import ggplot2
#' @param rst output of one of CAVAanalytics functions, such as projections. rst is of class SpatRaster
#' @param palette charachter. Color Palette
#' @param legend_range  numeric. Fix legend limits
#' @param plot_titles character. Title of the plot legend
#' @param ensemble logical. Whether to visualize the ensemble mean or each individual model
#' @param bins logical. Whether to visualize colors as a gradient or in bins
#' @param n.bins numeric. Controlling the number of bins when bins equal TRUE
#' @param alpha numeric. Transparency of colors
#' @param spatiotemporal logical. Whether computed yearly data should be visualized without spatial and temporal aggregation
#' @param temporal logical. Whether computed yearly data should be visualized temporally after spatial aggregation
#' @return ggplot object

plotting <-
  function(rst,
           palette,
           legend_range,
           plot_titles,
           ensemble,
           bins,
           n.bins,
           alpha,
           spatiotemporal,
           temporal,
           ...) {
    UseMethod("plotting")
  }



# projections -------------------------------------------------------------


#' @export

plotting.CAVAanalytics_projections <-
  function(rst,
           palette = NULL,
           legend_range = NULL,
           plot_titles = "default",
           ensemble = TRUE,
           bins = FALSE,
           n.bins = NULL,
           alpha = NA,
           stat = "mean",
           spatiotemporal = F,
           temporal = F,
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

    # Messages

    if (isTRUE(ensemble)) {
      if (!spatiotemporal & !temporal)
        cli::cli_text(paste0("{cli::symbol$arrow_right}", " Visualizing ensemble ", stat))
      else
        cli::cli_text(
          paste0(
            "{cli::symbol$arrow_right}",
            " Visualizing ensemble ",
            ifelse(temporal, "temporal ", "spatiotemporal "),
            "evolution. Argument stat is ignored"
          )
        )
    } else {
      if (!spatiotemporal | !temporal)
        cli::cli_text(
          "{cli::symbol$arrow_right} Visualizing individual members, argument stat is ignored"
        )
      else
        cli::cli_text(
          paste0(
            "{cli::symbol$arrow_right}",
            " Visualizing individual members ",
            ifelse(temporal, "temporal ", "spatiotemporal "),
            "evolution. Argument stat is ignored"
          )
        )
    }


    # retrieve the right raster stack based on the ensemble argument

    rst <- if (spatiotemporal | temporal) {
      rst[[4]]
    } else {
      if (isTRUE(ensemble) &
          stat == "mean")
        rst[[1]]
      else if (isTRUE(ensemble) & stat == "sd")
        rst[[2]]
      else
        rst[[3]]
    }

    if (!spatiotemporal & !temporal) {
      # Set default colors for legend
      colors <-
        if (is.null(palette))
          c("blue", "cyan", "green", "yellow", "orange", "red", "black")
      else
        palette

      # Set default range for legend
      legend_range <-
        if (is.null(legend_range))
          c(range(terra::values(rst), na.rm = TRUE))
      else
        legend_range

      # Suppress warnings
      options(warn = -1)

      # Get countries data
      countries <-
        rnaturalearth::ne_countries(scale = "large", returnclass = "sf")

      cli::cli_progress_step("Plotting")
      # Convert SpatRaster to dataframe
      rs_df <-
        terra::as.data.frame(rst, xy = TRUE, na.rm = TRUE) %>%
        tidyr::pivot_longer(cols = 3:ncol(.),
                            values_to = "value",
                            names_to = "long_name") %>%  {
                              if (ensemble) {
                                # Extract scenario, time frame and season from column names
                                tidyr::separate_wider_delim(
                                  .,
                                  long_name,
                                  delim = "_",
                                  names = c("scenario", "time_frame", "season")
                                ) %>%
                                  # Replace "." with "-" in time frame
                                  dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))

                              } else {
                                # Extract Member, scenario, time frame and season from column names
                                tidyr::separate_wider_delim(
                                  .,
                                  long_name,
                                  delim = "_",
                                  names = c("member", "scenario", "time_frame", "season")
                                ) %>%
                                  # Replace "." with "-" in time frame
                                  dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))

                              }
                            }

      p <- ggplot2::ggplot() +
        ggplot2::geom_sf(
          fill = 'white',
          color = "black",
          data = countries,
          alpha = 0.5
        ) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value),
                             data = rs_df,
                             alpha = alpha) +
        ggplot2::geom_sf(fill = NA,
                         color = "black",
                         data = countries) +
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
              n.breaks = ifelse(is.null(n.bins), 10, n.bins),
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
            ggh4x::facet_nested(scenario ~  season)
          } else {
            ggh4x::facet_nested(scenario ~  season + member)
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
      # when temporal or spatiotemporal is TRUE
      palette <-
        if (!is.null(palette))
          palette
      else
        c("blue", "red")
      if (is.null(rst))
        cli::cli_abort(c("x" = "Not allowed with load_data_and_projections"))

      if (spatiotemporal)  {
        cli::cli_alert_warning(
          " Arguments bins and stat are ignored. Change number of group intervals with n.groups"
        )

        if (ensemble) {
          cli::cli_progress_step("Plotting")
          p <-
            ridgeline(
              rst,
              group_col = 'date',
              z_col = 'value',
              num_grps = n.groups,
              fill = 'experiment',
              facet1 = 'season'
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(legend.position = "bottom",
                           legend.title = ggplot2::element_blank()) +
            ggplot2::labs(x = plot_titles) +
            ggplot2::scale_fill_manual(values =  palette) +
            if (!is.null(legend_range)) {
              ggplot2::xlim(legend_range[1], legend_range[2])
            }



          cli::cli_progress_done()

          return(p)

        } else {
          # when ensemble is FALSE for individual models and spatiotemporal
          cli::cli_progress_step("Plotting")
          p <-
            ridgeline(
              rst,
              group_col = 'date',
              z_col = 'value',
              num_grps = n.groups,
              fill = 'experiment',
              facet1 = 'Var1',
              facet2 = 'season'
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(legend.position = "bottom",
                           legend.title = ggplot2::element_blank()) +
            ggplot2::labs(x = plot_titles) +
            ggplot2::scale_fill_manual(values =  palette) +
            if (!is.null(legend_range)) {
              ggplot2::xlim(legend_range[1], legend_range[2])
            }

          cli::cli_progress_done()
          return(p)

        }

      } else {
        # when temporal is TRUE
        cli::cli_alert_warning(
          " Arguments bins, stat and legend_range are ignored. Change number of group intervals with n.groups"
        )
        if (ensemble) {
          cli::cli_progress_step("Plotting")
          p <- rst %>%
            dplyr::group_by(date, experiment, Var1, season) %>%
            dplyr::summarise(value = median(value)) %>%  # spatial aggregation
            dplyr::group_by(date, experiment, season) %>%
            dplyr::summarise(sd = sd(value),
                             value = mean(value)) %>%
            ggplot2::ggplot() +
            ggplot2::geom_line(
              ggplot2::aes(
                y = value,
                x = date,
                color = experiment
              ),
              linetype = "dotted",
              alpha = 0.5,
              linewidth = 0.9
            ) +
            ggplot2::geom_smooth(
              ggplot2::aes(
                y = value,
                x = date,
                color = experiment
              ),
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
            if (!is.null(palette)) {
              list(
                ggplot2::scale_color_manual(values = palette),
                ggplot2::scale_fill_manual(values = palette)
              )
            }

          cli::cli_progress_done()
          return(p)

        } else {
          # for individual models
          cli::cli_progress_step("Plotting")
          p <- rst %>%
            dplyr::group_by(Var1, date, experiment, season) %>%
            dplyr::summarise(value = mean(value)) %>%
            ggplot2::ggplot() +
            ggplot2::geom_line(
              ggplot2::aes(
                y = value,
                x = date,
                color = experiment
              ),
              linetype = "dotted",
              alpha = 0.7
            ) +
            ggplot2::geom_smooth(
              ggplot2::aes(
                y = value,
                x = date,
                color = experiment
              ),
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
            if (!is.null(palette)) {
              ggplot2::scale_color_manual(values = palette)
            }

          cli::cli_progress_done()
          return(p)

        }

      }

    }

  }


# climate change signal ---------------------------------------------------


#' @export

plotting.CAVAanalytics_ccs <-
  function(rst,
           palette = NULL,
           legend_range = NULL,
           plot_titles = "default",
           ensemble = TRUE,
           bins = FALSE,
           n.bins = NULL,
           alpha = NA,
           stat = "mean",
           spatiotemporal = F,
           temporal = F) {
    # check inputs ------------------------------------------------------------

    stopifnot(is.logical(ensemble))
    stopifnot(is.logical(bins))
    match.arg(stat, choices = c("mean", "sd"))
    stopifnot(is.logical(spatiotemporal))
    stopifnot(is.logical(temporal))

    if (spatiotemporal |
        temporal)
      cli::cli_abort("Feature not available for this object type")

    # Intermediate functions ------------------------------------------------------------

    spatial_prep = function(data, index, ...) {
      if (isTRUE(ensemble)) {
        cli::cli_text(
          paste0(
            "{cli::symbol$arrow_right}",
            " Visualizing ensemble ",
            stat,
            " and agreement in the sign of change"
          )
        )
      } else {
        cli::cli_text(
          "{cli::symbol$arrow_right} Visualizing individual members, argument stat is ignored. To visualize model agreement set ensemble to F "
        )
      }

      rsts = data[[index]]

      rs_df <-
        terra::as.data.frame(rsts, xy = TRUE, na.rm = TRUE) %>%
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
                                  dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))

                              } else {
                                # Extract Member, scenario and time frame from column names
                                tidyr::separate_wider_delim(
                                  .,
                                  long_name,
                                  delim = "_",
                                  names = c("member", "scenario", "time_frame", "season")
                                ) %>%
                                  # Replace "." with "-" in time frame
                                  dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))

                              }
                            }


      rs_df_sign <-
        terra::as.data.frame(data[[4]], xy = TRUE, na.rm = TRUE) %>%
        tidyr::pivot_longer(cols = 3:ncol(.),
                            values_to = "value",
                            names_to = "long_name") %>%
        # Extract scenario and time frame from column names
        tidyr::separate_wider_delim(
          .,
          long_name,
          delim = "_",
          names = c("scenario", "time_frame", "season")
        ) %>%
        # Replace "." with "-" in time frame
        dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))


      return(list(rs_df, rs_df_sign))



    }

    spatial_plot = function(spatial_data,
                            sign,
                            ...)  {
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

      legend_range <-
        if (is.null(legend_range))
          c(range(spatial_data[[1]]$value, na.rm = TRUE))
      else
        legend_range

      p <- ggplot2::ggplot() +
        ggplot2::geom_sf(
          fill = 'white',
          color = "black",
          data = countries,
          alpha = 0.5
        ) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value),
                             data = spatial_data[[1]],
                             alpha = alpha) +
        ggplot2::geom_sf(fill = NA,
                         color = "black",
                         data = countries) +
        {
          if (!bins) {
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
          } else {
            ggplot2::scale_fill_stepsn(
              colors = colors,
              limits = legend_range,
              na.value = "transparent",
              n.breaks = ifelse(is.null(n.bins), 10, n.bins),
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
        ) +
        {
          if (ensemble) {
            ggh4x::facet_nested(scenario ~ season)
          } else {
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
        ) +

        {
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
    }

    # start code -------------------------------------------------------------------
    cli::cli_progress_step("Plotting")

    p <-  if (isTRUE(ensemble) &
              stat == "mean") {
      spatial_prep(rst, 1) %>%
        spatial_plot(., sign = T)
    } else if (isTRUE(ensemble) & stat == "sd") {
      spatial_prep(rst, 2, ensemble) %>%
        spatial_plot(., sign = T)
    } else {
      spatial_prep(rst, 3, ensemble) %>%
        spatial_plot(., sign = F)
    }

    cli::cli_progress_done()
    return(p)

  }

# observations ------------------------------------------------------------

#' @export

plotting.CAVAanalytics_observations <-
  function(rst,
           palette = NULL,
           legend_range = NULL,
           plot_titles = "default",
           ensemble = FALSE,
           bins = FALSE,
           n.bins = NULL,
           alpha = NA,
           spatiotemporal = F,
           temporal = F,
           n.groups = 3) {
    # check inputs ------------------------------------------------------------

    stopifnot(is.logical(bins))
    stopifnot(is.logical(spatiotemporal))
    stopifnot(is.logical(temporal))
    if (spatiotemporal &
        temporal)
      cli::cli_abort("spatiotemporal and temporal cannot be both equal TRUE")

    # intermediate functions --------------------------------------------------


    temporal_plot = function(data_list, index) {
      cli::cli_alert_warning(" Arguments ensemble,bins,n.bins,alpha and legend_range are ignored")
      rst <- do.call(rbind, data_list[[index]])
      p <- rst %>%
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
        if (!is.null(palette)) {
          ggplot2::scale_color_manual(values = palette)
        }

      return(p)
    }


    spatiotemporal_plot = function(data_list,
                                   index,
                                   ...) {
      cli::cli_alert_warning(
        " Arguments bins,n.bins,alpha,palette and ensemble are ignored. Change number of group intervals with n.groups"
      )
      rst <- do.call(rbind, data_list[[index]])

      p <-
        suppressMessages(
          rst %>%
            ridgeline(
              rst,
              group_col = 'date',
              z_col = 'value',
              num_grps = n.groups,
              facet1 =  'season'
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(legend.position = "none") +
            ggplot2::labs(x = plot_titles) +
            if (!is.null(legend_range)) {
              ggplot2::xlim(legend_range[1], legend_range[2])
            }

        )

      return(p)

    }

    spatial_plot <-
      function(data_list,
               index,
               ...) {
        cli::cli_alert_warning(" Argument ensemble is ignored")
        rst <- data_list[[index]]

        # Convert SpatRaster to dataframe
        rs_df <-
          terra::as.data.frame(rst, xy = TRUE, na.rm = TRUE) %>%
          tidyr::pivot_longer(cols = 3:ncol(.),
                              values_to = "value",
                              names_to = "long_name") %>%
          # Extract scenario and time frame from column names
          tidyr::separate_wider_delim(
            long_name,
            delim = "_",
            names = c("scenario", "time_frame", "season")
          )  %>%
          # Replace "." with "-" in time frame
          dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))


        # Set default colors for legend
        palette <-
          if (is.null(palette))
            c("blue",
              "cyan",
              "green",
              "yellow",
              "orange",
              "red",
              "black")
        else
          palette

        # Set default range for legend
        legend_range <-
          if (is.null(legend_range))
            c(range(rs_df$value, na.rm = TRUE))
        else
          legend_range

        # Suppress warnings
        options(warn = -1)

        # Get countries data
        countries <-
          rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
        p <- ggplot2::ggplot() +
          ggplot2::geom_sf(
            fill = 'white',
            color = "black",
            data = countries,
            alpha = 0.5
          ) +
          ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value),
                               data = rs_df,
                               alpha = alpha) +
          ggplot2::geom_sf(fill = NA,
                           color = "black",
                           data = countries) +
          {
            if (!bins) {
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
            } else {
              ggplot2::scale_fill_stepsn(
                colors = palette,
                limits = legend_range,
                na.value = "transparent",
                n.breaks = ifelse(is.null(n.bins), 10, n.bins),
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
            xlim = c(range(rs_df$x)[[1]] - 0.5, range(rs_df$x)[[2]] + 0.5),
            ylim = c(range(rs_df$y)[[1]] - 0.5, range(rs_df$y)[[2]] + 0.5),
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


    spatial_plot_trend <- function(data_list,
                                   ...) {
      cli::cli_alert_warning(" Argument ensemble is ignored")

      palette <-
        if (is.null(palette))
          c("blue",
            "cyan",
            "green",
            "yellow",
            "orange",
            "red",
            "black")
      else
        palette

      # Set default range for legend
      legend_range <-
        if (is.null(legend_range))
          c(range(terra::values(data_list[[1]]), na.rm = TRUE)) # slope coef
      else
        legend_range

      # Get countries data
      countries <-
        rnaturalearth::ne_countries(scale = "large", returnclass = "sf")

      rs_df <-
        purrr::map(
          data_list[1:2],
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
            dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))
        )

      p <- ggplot2::ggplot() +
        ggplot2::geom_sf(
          fill = 'white',
          color = "black",
          data = countries,
          alpha = 0.5
        ) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value),
                             data = rs_df[[1]],
                             alpha = alpha) +
        ggplot2::geom_sf(fill = NA,
                         color = "black",
                         data = countries) +
        {
          if (!bins) {
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
          } else {
            ggplot2::scale_fill_stepsn(
              colors = palette,
              limits = legend_range,
              na.value = "transparent",
              n.breaks = ifelse(is.null(n.bins), 10, n.bins),
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
          data = dplyr::filter(rs_df[[2]], value < 0.05),
          size = 0.1,
          alpha = 0.4,
          color = "black",
          ggplot2::aes(x, y)
        ) +
        ggplot2::coord_sf(
          xlim = c(range(rs_df[[2]]$x)[[1]] - 0.5, range(rs_df[[2]]$x)[[2]] + 0.5),
          ylim = c(range(rs_df[[2]]$y)[[1]] - 0.5, range(rs_df[[2]]$y)[[2]] + 0.5),
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


    # start of code -----------------------------------------------------------
    cli::cli_progress_step("Plotting")

    p <- if (length(rst) == 2) {
      # linear regression was not applied
      if (!temporal & !spatiotemporal) {
        spatial_plot(rst, 1)
      } else {
        if (temporal) {
          temporal_plot(rst, 2)
        } else {
          # spatiotemporal
          spatiotemporal_plot(rst, 2)
        }
      }

    } else {
      # when linear regression is used
      if (!temporal & !spatiotemporal) {
        spatial_plot_trend(rst)
      } else {
        # when spatiotemproal or temporal is TRUE
        if (temporal) {
          temporal_plot(rst, 3)
        } else {
          # spatiotemporal
          spatiotemporal_plot(rst, 3)
        }

      }

    }

    cli::cli_progress_done()

    return(p)

  }


# biases ------------------------------------------------------------------


#' @export

plotting.CAVAanalytics_model_biases <-
  function(rst,
           palette = NULL,
           legend_range = NULL,
           plot_titles = "default",
           ensemble = TRUE,
           bins = FALSE,
           n.bins = NULL,
           alpha = NA,
           temporal = F,
           spatiotemporal = F) {
    # check -------------------------------------------------------------------
    if (spatiotemporal)
      cli::cli_abort("Not meaningful for model biases")
    stopifnot(is.logical(ensemble))
    stopifnot(is.logical(bins))
    stopifnot(is.logical(temporal))


    # messages

    # start code --------------------------------------------------------------

    if (ensemble) {
      cli::cli_text(
        if (temporal)
          "{cli::symbol$arrow_right} Visualizing the ensemble bias for spatially aggregated data"
        else
          "{cli::symbol$arrow_right} Visualizing the ensemble bias for temporally aggregated data"
      )
    } else {
      cli::cli_text(
        if (temporal)
          "{cli::symbol$arrow_right} Visualizing individual member biases for spatially aggregated data"
        else
          "{cli::symbol$arrow_right} Visualizing individual member biases for temporally aggregated data"
      )
    }


    # retrieve the right spatraster based on the ensemble argument

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
          ))),+max(abs(range(
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
                                  dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))

                              } else {
                                # Extract Member, scenario and time frame from column names
                                tidyr::separate_wider_delim(
                                  .,
                                  long_name,
                                  delim = "_",
                                  names = c("member", "scenario", "time_frame", "season")
                                ) %>%
                                  # Replace "." with "-" in time frame
                                  dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))

                              }
                            }

      p <- ggplot2::ggplot() +
        ggplot2::geom_sf(
          fill = 'white',
          color = "black",
          data = countries,
          alpha = 0.5
        ) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value),
                             data = rs_df,
                             alpha = alpha) +
        ggplot2::geom_sf(fill = NA,
                         color = "black",
                         data = countries) +
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
              n.breaks = ifelse(is.null(n.bins), 10, n.bins),
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

      cli::cli_alert_warning(" Arguments bins, n.bins, and alpha are ignored")
      cli::cli_progress_step("Plotting")
      if (ensemble) {
        p <- rst %>%
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
          dplyr::mutate(value = value - obs_value) %>%
          ggplot2::ggplot() +
          ggplot2::geom_hline(yintercept = 0,
                              linetype = "dashed",
                              color = "red") +
          ggplot2::geom_line(ggplot2::aes(y = value,
                                          x = date),
                             alpha = 0.5) +
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
