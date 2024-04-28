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
<<<<<<< Updated upstream
=======
#' @param spatiotemporal logical. Whether computed yearly data should be visualized without spatial and temporal aggregation. Frequencies are visualized
#' @param temporal logical. Whether computed yearly data should be visualized temporally after spatial aggregation (median of all pixels)
>>>>>>> Stashed changes
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
<<<<<<< Updated upstream
           stat = "mean") {
    # checking requirements
=======
           stat = "mean",
           spatiotemporal = F,
           temporal = F,
           n.groups = 3) {
    # check inputs ------------------------------------------------------------

>>>>>>> Stashed changes
    stopifnot(is.logical(ensemble))
    stopifnot(is.logical(bins))
    match.arg(stat, choices = c("mean", "sd"))

<<<<<<< Updated upstream
    # messages

    if (isTRUE(ensemble)) {
      cli::cli_text(paste0("{cli::symbol$arrow_right}", " Visualizing ensemble ", stat))
    } else {
      cli::cli_text(
        "{cli::symbol$arrow_right} Visualizing individual members, argument stat is ignored"
      )
    }



    # retrieve the right raster stack based on the ensemble argument

    rst <-
      if (isTRUE(ensemble) &
          stat == "mean")
        rst[[1]]
    else if (isTRUE(ensemble) & stat == "sd")
      rst[[2]]
    else
      rst[[3]]

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
    rs_df <- terra::as.data.frame(rst, xy = TRUE, na.rm = TRUE) %>%
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
=======
    if (spatiotemporal &
        temporal)
      cli::cli_abort("spatiotemporal and temporal cannot be both equal TRUE")


    # start code -------------------------------------------------------------------
    cli::cli_progress_step("Plotting")

    p <-  if (!temporal & !spatiotemporal)  {
      if (ensemble &
          stat == "mean") {
        spatial_prep(rst, 1, proj = T, stat, ensemble) %>%
          spatial_plot(.,
                       sign = F,
                       ensemble,
                       palette,
                       bins,
                       n.bins,
                       alpha,
                       plot_titles,
                       legend_range)
      } else if (ensemble & stat == "sd") {
        spatial_prep(rst, 2, proj = T, ensemble) %>%
          spatial_plot(.,
                       sign = F,
                       ensemble,
                       palette,
                       bins,
                       n.bins,
                       alpha,
                       plot_titles,
                       legend_range)
      } else {
        # individual models
        spatial_prep(rst, 3, proj = T, stat, ensemble) %>%
          spatial_plot(.,
                       sign = F,
                       ensemble,
                       palette,
                       bins,
                       n.bins,
                       alpha,
                       plot_titles,
                       legend_range)
      }
    } else if (temporal & !spatiotemporal) {
      # when temporal is TRUE
      temporal_plot(rst, 4, ensemble,  spatial.aggr = T, plot_titles, palette, legend_range)
    } else {
      spatiotemporal_plot(rst, 4, ensemble,  plot_titles, palette, legend_range, n.groups)
>>>>>>> Stashed changes

    return(p)

    cli::cli_progress_done()
    return(p)

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
           stat = "mean") {
    # checking requirements
    stopifnot(is.logical(ensemble))
    stopifnot(is.logical(bins))
    match.arg(stat, choices = c("mean", "sd"))

<<<<<<< Updated upstream
    # messages

    if (isTRUE(ensemble)) {
      cli::cli_text(paste0("{cli::symbol$arrow_right}", " Visualizing ensemble ", stat))
    } else {
      cli::cli_text(
        "{cli::symbol$arrow_right} Visualizing individual members, argument stat is ignored"
      )
    }


    # retrieve the right spatraster based on the ensemble argument

    rst <-
      if (isTRUE(ensemble) &
          stat == "mean")
        rst[[1]]
    else if (isTRUE(ensemble) & stat == "sd")
      rst[[2]]
    else
      rst[[3]]

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

    # Convert SpatRaster to dataframe

    cli::cli_progress_step("Plotting")

    rs_df <- terra::as.data.frame(rst, xy = TRUE, na.rm = TRUE) %>%
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
      )

    cli::cli_progress_done()

=======
    if (spatiotemporal)
      cli::cli_abort("Feature not available for this object type")

    # start code -------------------------------------------------------------------
    cli::cli_progress_step("Plotting")

    p <-  if (!temporal)  {
      if (ensemble &
          stat == "mean") {
        spatial_prep(data = rst,
                     index = 1,
                     proj = F,
                     stat,
                     ensemble) %>%
          spatial_plot(.,
                       sign = T,
                       ensemble,
                       palette,
                       bins,
                       n.bins,
                       alpha,
                       plot_titles,
                       legend_range)
      } else if (ensemble & stat == "sd") {
        spatial_prep(data = rst,
                     index = 2,
                     proj = F,
                     stat,
                     ensemble) %>%
          spatial_plot(.,
                       sign = T,
                       ensemble,
                       palette,
                       bins,
                       n.bins,
                       alpha,
                       plot_titles,
                       legend_range)
      } else {
        # individual models
        spatial_prep(data = rst,
                     index = 3,
                     proj = F,
                     stat,
                     ensemble) %>%
          spatial_plot(.,
                       sign = F,
                       ensemble,
                       palette,
                       bins,
                       n.bins,
                       alpha,
                       plot_titles,
                       legend_range)
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
>>>>>>> Stashed changes
    return(p)

  }

<<<<<<< Updated upstream

# trends ------------------------------------------------------------------



#' @export


plotting.CAVAanalytics_trends <-
=======
# observations ------------------------------------------------------------

#' @export

plotting.CAVAanalytics_observations <-
>>>>>>> Stashed changes
  function(rst,
           palette = NULL,
           legend_range = NULL,
           plot_titles = "default",
<<<<<<< Updated upstream
           ensemble = TRUE,
           bins = FALSE,
           n.bins = NULL,
           alpha = NA,
           frequencies = F,
           n.groups = 3,
           spatial_aggr = F) {
    # checking requirements
    stopifnot(is.logical(ensemble))
    stopifnot(is.logical(bins))
    stopifnot(is.logical(frequencies))
    stopifnot(is.logical(spatial_aggr))

    if (frequencies &
        spatial_aggr)
      cli::cli_abort("frequencies and spatial_aggr cannot be both equal TRUE")

    # retrieve the right spatRaster based on how trends was run
    if (!frequencies & !spatial_aggr) {
      if (length(rst) > 4) {
        # trends was run on projections
        historical <- FALSE
        if (ensemble) {
          members <-
            length(unique(stringr::str_match(names((
              rst[[3]]
            )), "Member.\\d")))
          rst <- rst[1:2]

        } else {
          rst <- rst[3:4]
        }

      } else {
        # trends was run for observations
        historical <- TRUE
        rst <- rst[1:2]
      }

      # Messages
      if (!historical) {
        if (ensemble) {
          cli::cli_text(paste0(
            "{cli::symbol$arrow_right}",
            ifelse(
              frequencies,
              " Visualizing ensemble, frequencies ",
              " Visualizing ensemble "
            )
          ))
        } else {
          cli::cli_text(paste0(
            "{cli::symbol$arrow_right}",
            ifelse(
              frequencies,
              " Visualizing individual members (frequencies)",
              " Visualizing individual members "
            )
          ))
        }
      } else {
        # when historical is TRUE

        cli::cli_alert_warning(
          paste0(
            "Argument ensemble is ignored, trends were applied to observations.",
            ifelse(frequencies, " Visualizing frequencies", "")
          )
        )
      }

      # Set default colors for legend
      colors <-
        if (is.null(palette))
          c("blue", "cyan", "green", "yellow", "orange", "red", "black")
      else
        palette

      # Set default range for legend
      legend_range <- if (is.null(legend_range)) {
        if (!ensemble | historical) {
          c(range(terra::values(rst[[1]], na.rm = TRUE)))
        } else {
          c(-members, members)
        }
      } else {
        legend_range
=======
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


>>>>>>> Stashed changes
      }


      # Get countries data
      countries <-
        rnaturalearth::ne_countries(scale = "large", returnclass = "sf")

      # Convert spatraster to dataframe for both coefficients and p.values

      cli::cli_progress_step("Plotting")

      rs_df <-
        purrr::map(
          rst,
          ~ terra::as.data.frame(.x, xy = TRUE, na.rm = TRUE) %>%
            tidyr::pivot_longer(
              cols = 3:ncol(.),
              values_to = "value",
              names_to = "long_name",
            ) %>%
            {
              if (!historical) {
                if (ensemble) {
                  # Extract scenario and time frame from column names
                  tidyr::separate_wider_delim(
                    .,
                    long_name,
                    delim = "_",
                    names = c("scenario", "type", "time_frame", "season")
                  ) %>%
                    # Replace "." with "-" in time frame
                    dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-")) %>%
                    dplyr::mutate(., value = ifelse(value == 999, NA, value)) # 999 is the value assigned instead of NA

                } else {
                  # Extract Member, scenario and time frame from column names
                  tidyr::separate_wider_delim(
                    .,
                    long_name,
                    delim = "_",
                    names = c("member", "scenario", "type", "time_frame", "season")
                  )  %>%
                    # Replace "." with "-" in time frame
                    dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))

                }
              } else {
                # when historical
                tidyr::separate_wider_delim(
                  .,
                  long_name,
                  delim = "_",
                  names = c("scenario", "type", "time_frame", "season")
                ) %>%
                  # Replace "." with "-" in time frame
                  dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))

              }
            }
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
        {
          if (ensemble | historical) {
            ggh4x::facet_nested(scenario  ~ season)
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
          legend.key.height = ggplot2::unit(if (ensemble)
            1
            else
              0.3, 'cm'),
          legend.key.width = ggplot2::unit(if (ensemble)
            0.3
            else
              1.5, 'cm'),
          legend.box.spacing = ggplot2::unit(0, "pt"),
          legend.text =  ggplot2::element_text(angle = if (ensemble)
            360
            else
              45, hjust = 1)
        )

      cli::cli_process_done()

      return(p)
    } else {
      # when frequencies or spatial_aggr is set as T
      if (is.null(rst[[5]]))
        cli::cli_abort(c("x" = "Not allowed with load_data_and_trends"))

      if (frequencies) {
        # when frequencies is TRUE

        if (length(rst) > 4) {
          # trends were run on projections
          cli::cli_alert_warning(
            " Arguments bins and legend_range are ignored. Change number of group intervals with n.groups"
          )

          if (ensemble) {
            rst <- rst[[5]]
            p <-
              suppressMessages(
                ridgeline(
                  rst,
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
                  if (!is.null(palette)) {
                    ggplot2::scale_fill_manual(values = palette)
                  }
              )

            return(p)

          } else {
            # for individual models
            rst <- rst[[5]]
            p <-
              suppressMessages(
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
                  ggplot2::theme(
                    legend.position = "bottom",
                    legend.title = ggplot2::element_blank()
                  ) +
                  ggplot2::labs(x = plot_titles) +
                  if (!is.null(palette)) {
                    ggplot2::scale_fill_manual(values = palette)
                  }

              )


            return(p)

          }

        } else {
          # when trends is run for the historical period and frequencies is true
          cli::cli_alert_warning(
            " Arguments bins, legend_range, ensemble and palette are ignored. Change number of group intervals with n.groups"
          )
          rst <- do.call(rbind, rst[[3]])
          p <-
            suppressMessages(
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
                if (!is.null(palette)) {
                  ggplot2::scale_fill_manual(values = palette)
                }
            )

          return(p)


        }

      } else {
        # when spatial_aggr is TRUE

        if (length(rst) > 4) {
          cli::cli_alert_warning(" Arguments bins and legend_range are ignored")
          # trends were run on projections
          if (ensemble) {
            rst[[5]] %>%
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
                alpha = 0.5
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
              ggplot2::geom_ribbon(
                ggplot2::aes(
                  y = value,
                  x = date,
                  ymin = value - sd,
                  ymax = value + sd,
                  fill = experiment
                ),
                alpha = 0.1,
                show.legend = F
              ) +
              ggplot2::facet_wrap( ~ season) +
              ggplot2::scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
              ggplot2::theme_bw() +
              ggplot2::theme(
                legend.position = "bottom",
                axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                legend.title = ggplot2::element_blank()
              ) +
              ggplot2::labs(x = "Year", y = plot_titles) +
              if (!is.null(palette)) {
                list(
                  ggplot2::scale_color_manual(values = palette),
                  ggplot2::scale_fill_manual(values = palette)
                )
              }

          } else {
            # for individual models
            rst[[5]] %>%
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
                legend.title = ggplot2::element_blank()
              ) +
              ggplot2::labs(x = "Year", y = plot_titles) +
              if (!is.null(palette)) {
                ggplot2::scale_color_manual(values = palette)
              }

          }

        } else {
          # when trends is run for the historical period and spatial_aggr is true
          cli::cli_alert_warning(" Arguments bins, ensemble and legend_range are ignored")
          rst[[3]][[1]] %>%
            dplyr::group_by(date, experiment, season) %>%
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
            ggplot2::facet_wrap( ~ season) +
            ggplot2::scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
            ggplot2::theme_bw() +
            ggplot2::theme(
              legend.position = "bottom",
              axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
              legend.title = ggplot2::element_blank()
            ) +
            ggplot2::labs(x = "Year", y = plot_titles) +
            if (!is.null(palette)) {
              list(
                ggplot2::scale_color_manual(values = palette),
                ggplot2::scale_fill_manual(values = palette)
              )
            }

        }
      }
    }
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
           alpha = NA) {
    # checking requirements
    stopifnot(is.logical(bins))

    # messages

    cli::cli_text(
      "{cli::symbol$arrow_right} Argument ensemble is ignored when visualizing observations "
    )

    # retrieve the right raster stack based on the ensemble argument

    rst <- rst[[1]]

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
    rs_df <- terra::as.data.frame(rst, xy = TRUE, na.rm = TRUE) %>%
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
              title.hjust = 0.5,
              label.hjust = 1
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
        strip.text.y=ggplot2::element_blank(),
        plot.background = ggplot2::element_blank(),
        panel.background =ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "right",
        legend.key.height = unit(1, 'cm'),
        legend.key.width = unit(0.3, 'cm'),
        legend.box.spacing = unit(0.2, "pt")
      )

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
           trends = F) {
    # checking requirements
    stopifnot(is.logical(ensemble))
    stopifnot(is.logical(bins))

    # messages

    if (ensemble) {
      cli::cli_text(
        if (trends)
          "{cli::symbol$arrow_right} Visualizing the ensemble bias for spatially aggregated data"
        else
          "{cli::symbol$arrow_right} Visualizing the ensemble bias for temporally aggregated data"
      )
    } else {
      cli::cli_text(
        if (trends)
          "{cli::symbol$arrow_right} Visualizing individual member biases for spatially aggregated data"
        else
          "{cli::symbol$arrow_right} Visualizing individual member biases for temporally aggregated data"
      )
    }


    # retrieve the right spatraster based on the ensemble argument

    rst <-
      if (ensemble &
          !trends)
        rst[[1]]
    else if (!ensemble & !trends)
      rst[[2]]
    else
      rst[[3]]

    if (!trends) {
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

      if (is.null(rst))
        cli::cli_abort(c("x" = "Not allowed with load_data_and_model_biases"))

      cli::cli_alert_warning(" Arguments bins, n.bins, palette and alpha are ignored")
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
                             alpha = 0.5) +
          ggplot2::geom_ribbon(
            ggplot2::aes(
              y = value,
              x = date,
              ymin = value - sd,
              ymax = value + sd
            ),
            alpha = 0.1,
            show.legend = F
          ) +
          ggplot2::scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
          ggplot2::theme_bw() +
          ggplot2::theme(
            legend.position = "bottom",
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            legend.title = ggplot2::element_blank()
          ) +
          ggplot2::facet_wrap( ~ season) +
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
            legend.title = ggplot2::element_blank()
          ) +
          if (!is.null(legend_range)) {
            ggplot2::scale_y_continuous(limits = legend_range)
          }



      }
      cli::cli_process_done()
      return(p)
    }


  }
