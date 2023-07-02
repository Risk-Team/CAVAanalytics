#' Step three: Visualization
#'
#' Automatically plot results from CAVAanalytics step two
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
#' @return ggplot object
#' @examples
#'load_data(country = "Somalia", variable="tas", years.hist=2000, years.proj=2010,
#'               path.to.data = "CORDEX-CORE", domain="AFR-22") %>%
#' projections(., season = 1:12) %>%
#' plotting(plot_titles="hurs", ensemble=T)


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

#' @export

plotting.CAVAanalytics_projections <-
  function(rst,
           palette = NULL,
           legend_range = NULL,
           plot_titles,
           ensemble,
           bins = FALSE,
           n.bins = NULL,
           alpha = NA,
           stat = "mean") {
    # checking requirements
    stopifnot(is.logical(ensemble))
    stopifnot(is.logical(bins))
    match.arg(stat, choices = c("mean", "sd"))

    # messages

    if (isTRUE(ensemble)) {
      cli::cli_alert_info(paste0("Visualizing ensemble ", stat))
    } else {
      cli::cli_alert_info("Visualizing individual members, argument stat is ignored")
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
      rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

    cli::cli_progress_step("Plotting")
    # Convert SpatRaster to dataframe
    rs_df <- terra::as.data.frame(rst, xy = TRUE, na.rm = TRUE) %>%
      tidyr::pivot_longer(cols = 3:ncol(.),
                          values_to = "value",
                          names_to = "long_name") %>%  {
                            if (ensemble) {
                              # Extract scenario and time frame from column names
                              dplyr::mutate(
                                .,
                                scenario = stringr::str_extract(long_name, ".*_") %>%   stringr::str_remove(., "_"),
                                time_frame =  stringr::str_extract(long_name, "_.*") %>%   stringr::str_remove(., "_")
                              ) %>%
                                # Replace "." with "-" in time frame
                                dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))

                            } else {
                              # Extract Member, scenario and time frame from column names
                              dplyr::mutate(
                                .,
                                member =   stringr::str_extract(long_name, "Member \\d+"),
                                scenario =  stringr::str_extract(long_name, "_.*_") %>%   stringr::str_remove_all(., "_"),
                                time_frame =  stringr::str_extract(long_name, "_\\d+.*") %>%   stringr::str_remove(., "_")
                              ) %>%
                                # Replace "." with "-" in time frame
                                dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))

                            }
                          }

    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        fill = 'antiquewhite1',
        color = "black",
        data = countries
      ) +
      ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value),
                           data = rs_df,
                           alpha = alpha) +
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
              label.theme = ggplot2::element_text(angle = 45),
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
              label.theme = ggplot2::element_text(angle = 45),
              label.hjust = 1
            )
          )

        }

      } +
      ggplot2::coord_sf(
        xlim = c(range(rs_df$x)[[1]] - 1, range(rs_df$x)[[2]] + 1),
        ylim = c(range(rs_df$y)[[1]] - 1, range(rs_df$y)[[2]] + 1),
        expand = F,
        ndiscr = 500
      ) +
      {
        if (ensemble) {
          ggplot2::facet_grid(time_frame ~ scenario)
        } else {
          ggh4x::facet_nested(scenario ~ time_frame   + member)
        }
      } +
      ggplot2::labs(fill = plot_titles, x = "", y = "") +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::theme(
        plot.background = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = 'aliceblue'),
        panel.border = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.height = ggplot2::unit(0.5, 'cm'),
        legend.key.width = ggplot2::unit(2, 'cm'),
        legend.box.spacing = ggplot2::unit(0, "pt")
      )

    cli::cli_progress_done()

    return(p)

  }


#' @export

plotting.CAVAanalytics_ccs <-
  function(rst,
           palette = NULL,
           legend_range = NULL,
           plot_titles,
           ensemble,
           bins = FALSE,
           n.bins = NULL,
           alpha = NA,
           stat = "mean") {
    # checking requirements
    stopifnot(is.logical(ensemble))
    stopifnot(is.logical(bins))
    match.arg(stat, choices = c("mean", "sd"))

    # messages

    if (isTRUE(ensemble)) {
      cli::cli_alert_info(paste0(" Visualizing ensemble ", stat))
    } else {
      cli::cli_alert_info(" Visualizing individual members, argument stat is ignored")
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
      rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

    # Convert SpatRaster to dataframe

    cli::cli_progress_step("Plotting")

    rs_df <- terra::as.data.frame(rst, xy = TRUE, na.rm = TRUE) %>%
      tidyr::pivot_longer(cols = 3:ncol(.),
                          values_to = "value",
                          names_to = "long_name") %>%  {
                            if (ensemble) {
                              # Extract scenario and time frame from column names
                              dplyr::mutate(
                                .,
                                scenario = stringr::str_split(long_name, "_") %>%  purrr::map_chr(., 2),
                                time_frame =  stringr::str_split(long_name, "_") %>%  purrr::map_chr(., 3)
                              ) %>%
                                # Replace "." with "-" in time frame
                                dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))

                            } else {
                              # Extract Member, scenario and time frame from column names
                              dplyr::mutate(
                                .,
                                member =   stringr::str_split(long_name, "_") %>%  purrr::map_chr(., 1),
                                scenario =  stringr::str_split(long_name, "_") %>%  purrr::map_chr(., 2),
                                time_frame =  stringr::str_split(long_name, "_") %>%  purrr::map_chr(., 3)
                              ) %>%
                                # Replace "." with "-" in time frame
                                dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))

                            }
                          }

    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        fill = 'antiquewhite1',
        color = "black",
        data = countries
      ) +
      ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value),
                           data = rs_df,
                           alpha = alpha) +
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
              label.theme = ggplot2::element_text(angle = 45),
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
              label.theme = ggplot2::element_text(angle = 45),
              label.hjust = 1
            )
          )
        }
      } +
      ggplot2::coord_sf(
        xlim = c(range(rs_df$x)[[1]] - 1, range(rs_df$x)[[2]] + 1),
        ylim = c(range(rs_df$y)[[1]] - 1, range(rs_df$y)[[2]] + 1),
        expand = F,
        ndiscr = 500
      ) +
      {
        if (ensemble) {
          ggplot2::facet_grid(time_frame ~ scenario)
        } else {
          ggh4x::facet_nested(scenario ~ time_frame   + member)
        }
      } +
      ggplot2::labs(fill = plot_titles, x = "", y = "") +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::theme(
        plot.background = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = 'aliceblue'),
        panel.border = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.height = ggplot2::unit(0.5, 'cm'),
        legend.key.width = ggplot2::unit(2, 'cm'),
        legend.box.spacing = ggplot2::unit(0, "pt")
      )

    cli::cli_progress_done()

    return(p)

  }



#' @export

plotting.CAVAanalytics_trends <-
  function(rst,
           palette = NULL,
           legend_range = NULL,
           plot_titles,
           ensemble,
           bins = FALSE,
           n.bins = NULL,
           alpha = NA,
           frequencies,
           n.groups = 3,
           spatial_aggr) {
    # checking requirements
    stopifnot(is.logical(ensemble))
    stopifnot(is.logical(bins))
    stopifnot(is.logical(frequencies))
    stopifnot(is.logical(spatial_aggr))

    if (frequencies & spatial_aggr) cli::cli_abort("frequencies and spatial_aggr cannot be both equal TRUE")

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
          cli::cli_alert_info(paste0(
            ifelse(
              frequencies,
              " Visualizing ensemble, frequencies ",
              " Visualizing ensemble "
            )
          ))
        } else {
          cli::cli_alert_info(paste0(
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
      }


      # Get countries data
      countries <-
        rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

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
                  dplyr::mutate(
                    .,
                    scenario = stringr::str_split(long_name, "_") %>%  purrr::map_chr(., 1),
                    time_frame =  stringr::str_split(long_name, "_") %>%  purrr::map_chr(., 3)
                  ) %>%
                    # Replace "." with "-" in time frame
                    dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-")) %>%
                    dplyr::mutate(., value = ifelse(value == 999, NA, value)) # 999 is the value assigned instead of NA

                } else {
                  # Extract Member, scenario and time frame from column names
                  dplyr::mutate(
                    .,
                    member =   stringr::str_split(long_name, "_") %>%  purrr::map_chr(., 1),
                    scenario =  stringr::str_split(long_name, "_") %>%  purrr::map_chr(., 2),
                    time_frame =  stringr::str_split(long_name, "_") %>%  purrr::map_chr(., 4)
                  ) %>%
                    # Replace "." with "-" in time frame
                    dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))

                }
              } else {
                # when historical

                dplyr::mutate(
                  .,
                  scenario = stringr::str_split(long_name, "_") %>%  purrr::map_chr(., 1),
                  time_frame =  stringr::str_split(long_name, "_") %>%  purrr::map_chr(., 3)
                ) %>%
                  # Replace "." with "-" in time frame
                  dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))

              }
            }
        )

      p <- ggplot2::ggplot() +
        ggplot2::geom_sf(
          fill = 'antiquewhite1',
          color = "black",
          data = countries
        ) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value),
                             data = rs_df[[1]],
                             alpha = alpha) +
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
                label.theme = ggplot2::element_text(angle = 45),
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
                label.theme = ggplot2::element_text(angle = 45),
                label.hjust = 1
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
          xlim = c(range(rs_df[[2]]$x)[[1]] - 1, range(rs_df[[2]]$x)[[2]] + 1),
          ylim = c(range(rs_df[[2]]$y)[[1]] - 1, range(rs_df[[2]]$y)[[2]] + 1),
          expand = F,
          ndiscr = 500
        ) +
        {
          if (ensemble | historical) {
            ggplot2::facet_grid(time_frame ~ scenario)
          } else {
            ggh4x::facet_nested(scenario ~ time_frame   + member)
          }
        } +
        ggplot2::labs(fill = plot_titles, x = "", y = "") +
        ggplot2::theme_bw(base_size = 12) +
        ggplot2::theme(
          plot.background = ggplot2::element_blank(),
          panel.background = ggplot2::element_rect(fill = 'aliceblue'),
          panel.border = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.height = ggplot2::unit(0.5, 'cm'),
          legend.key.width = ggplot2::unit(2, 'cm'),
          legend.box.spacing = ggplot2::unit(0, "pt")
        )

      cli::cli_process_done()

      return(p)
    } else {  # when frequencies or spatial_aggr is set as T

      if (frequencies) {# when frequencies is TRUE
      cli::cli_alert_warning(" Arguments bins, legend_range, plot_titles and palette are ignored. Change number of group intervals with n.groups")

      if (length(rst) > 4) {
        # trends were run on projections
        if (ensemble) {
          members <-
            length(unique(stringr::str_match(names((
              rst[[3]]
            )), "Member.\\d")))
          rst <- rst[[5]]
          plts <-
            suppressMessages(
              purrr::map(
                unique(rst$experiment),
                ~ ridgeline(
                  dplyr::filter(rst, experiment == .x),
                  group_col = 'date',
                  z_col = 'value',
                  num_grps = n.groups
                ) +
                  ggplot2::ggtitle(.x) +
                  ggplot2::theme_bw() +
                  ggplot2::theme(
                    plot.title = ggplot2::element_text(hjust = 0.5),
                    legend.position = "none",
                    legend.key.height = ggplot2::unit(0.2, 'cm'),
                    legend.key.width = ggplot2::unit(1, 'cm')
                  )
              )
            )

          p <- patchwork::wrap_plots(plts)
          return(p)

        } else { # for individual models
          rst <- rst[[6]]
          plts <-
            suppressMessages(purrr::map(
              unique(rst$experiment),
              ~ purrr::map(unique(rst$Var1), function(model)

                ridgeline(
                  dplyr::filter(rst, experiment == .x, Var1 == model),
                  group_col = 'date',
                  z_col = 'value',
                  num_grps = n.groups
                ) +
                  ggplot2::ggtitle(paste0(model, "_", .x)) +
                  ggplot2::theme_bw() +
                  ggplot2::theme(
                    plot.title = ggplot2::element_text(hjust = 0.5),
                    legend.position = "none",
                    legend.key.height = ggplot2::unit(0.2, 'cm'),
                    legend.key.width = ggplot2::unit(1, 'cm')
                  ))

            ))

          # Combine plots
          p <-
            patchwork::wrap_plots(plts[[1]]) /  patchwork::wrap_plots(plts[[2]])
          return(p)

        }

      } else {
        # when trends is run for the historical period and frequencies is true

        rst <- rst[[3]][[1]]
        p <-
          suppressMessages(
            ridgeline(
              rst,
              group_col = 'date',
              z_col = 'value',
              num_grps = n.groups
            ) +
              ggplot2::ggtitle("obs") +
              ggplot2::theme_bw() +
              ggplot2::theme(
                plot.title = ggplot2::element_text(hjust = 0.5),
                legend.position = "none",
                legend.key.height = ggplot2::unit(0.2, 'cm'),
                legend.key.width = ggplot2::unit(1, 'cm')
              )
          )

        return(p)


      }

      } else { # when spatial_aggr is TRUE

    cli::cli_alert_warning(" Arguments bins and legend_range are ignored")

        if (length(rst) > 4) {
          # trends were run on projections
          if (ensemble) {

            rst[[6]] %>%
            dplyr::group_by(date, experiment) %>%
              dplyr::summarise(sd=sd(value)/sqrt(length(unique(rst[[6]]$Var1))), value=mean(value)) %>%
              ggplot2::ggplot()+
              ggplot2::geom_line(ggplot2::aes(y=value, x=date, color=experiment))+
              ggplot2::geom_point(ggplot2::aes(y=value, x=date, color=experiment), size = 2, alpha=0.3) +

              ggplot2::geom_ribbon(ggplot2::aes(y=value, x=date, ymin = value - sd,
                                                ymax = value + sd, fill = experiment), alpha = 0.1, show.legend = F) +
              ggplot2::geom_label(
                ggplot2:: aes(x=date, y=value, label = round(value, digits = 0), fill=experiment),
                size=2,
                nudge_x = 0.1,
                nudge_y = 0.1,
                color="black",
                show.legend = FALSE,
                alpha=0.5
              ) +
              ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
              ggplot2::theme_bw()+
              ggplot2::theme(legend.position = "bottom", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.title = ggplot2::element_blank())+
              ggplot2::labs(x = "Year", y = plot_titles)+
              if (!is.null(palette)) {
               list(ggplot2::scale_color_manual(values = palette),
                ggplot2::scale_fill_manual(values = palette))
              }

          } else { # for individual models
            rst[[6]] %>%
            dplyr::group_by(Var1, date, experiment) %>%
              dplyr::summarise(value=mean(value)) %>%
              ggplot2::ggplot()+
              ggplot2::geom_line(ggplot2::aes(y=value, x=date, color=experiment))+
              ggplot2::geom_point(ggplot2::aes(y=value, x=date, color=experiment), size = 2, alpha=0.3) +
              ggplot2::facet_wrap(~Var1, ncol = 2)+
              ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
              ggplot2::theme_bw()+
              ggplot2::theme(legend.position = "bottom", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1), legend.title = ggplot2::element_blank())+
              ggplot2::labs(x = "Year", y = plot_titles)+
              if (!is.null(palette)) {
                ggplot2::scale_color_manual(values = palette)
              }

          }

        } else {
          # when trends is run for the historical period and spatial_aggr is true

       # to complete
        }
}
    }
  }
