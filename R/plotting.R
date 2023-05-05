#' Step three: Visualization
#'
#' Automatically plot results from CAVAanalytics step two
#' @export

#' @import ggplot2
#' @param rst output of one of CAVAanalytics functions, such as projections. rst is of class RasterStack
#' @param palette charachter. Color Palette
#' @param legend_range  numeric. Fix legend limits
#' @param plot_titles character. Title of the plot legend
#' @param ensemble logical. Whether to visualize the ensemble mean or each individual model
#' @param alpha numeric. Transparency of the raster colors
#' @return ggplot object
#' @examples
#'load_data(country = "Somalia", variable="tas", years.hist=2000, years.proj=2010,
#'               path.to.data = "CORDEX-CORE", domain="AFR-22") %>%
#' projections(., season = 1:12) %>%
#' plotting(plot_titles="hurs", ensemble=T)


plotting <- function(rst, palette, legend_range, plot_titles, ensemble, bins, n.bins, alpha, ...) {
UseMethod("plotting")
}

#' @export

plotting.CAVAanalytics_projections <- function(rst, palette=NULL, legend_range=NULL, plot_titles, ensemble, bins=FALSE, n.bins=NULL,alpha=NA, stat="mean") {

  # checking requirements
  stopifnot(is.logical(ensemble))
  stopifnot(is.logical(bins))
  match.arg(stat, choices = c("mean", "sd"))

  # messages

  if (isTRUE(ensemble)) {
    message(Sys.time(), "\n", paste0("Visualizing ensemble ", stat))
  } else {message(Sys.time(), "\n", "Visualizing individual members, argument stat is ignored")}

  message(Sys.time(), "\n", "Prepare for plotting")

  # retrieve the right raster stack based on the ensemble argument

  rst <- if (isTRUE(ensemble) & stat=="mean") rst[[1]] else if (isTRUE(ensemble) & stat=="sd") rst[[2]] else rst[[3]]

  # Set default colors for legend
  colors <- if (is.null(palette)) c("blue", "cyan", "green", "yellow", "orange", "red", "black") else palette

  # Set default range for legend
  legend_range <- if (is.null(legend_range)) c(range(raster::values(rst), na.rm = TRUE)) else legend_range

  # Suppress warnings
  options(warn = -1)

  # Get countries data
  countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  # Convert raster to dataframe
  rs_df <- raster::as.data.frame(rst, xy = TRUE, na.rm = TRUE) %>%
    tidyr::pivot_longer(
      cols = 3:ncol(.),
      values_to = "value",
      names_to = "long_name"
    ) %>% {
      if(ensemble) {
        # Extract scenario and time frame from column names
        dplyr::mutate(., scenario = stringr::str_extract(long_name, ".*_") %>%  stringr::str_remove(., "_"),
                      time_frame =  stringr::str_extract(long_name, "_.*") %>%  stringr::str_remove(., "_")) %>%
          # Replace "." with "-" in time frame
          dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))

      } else {
        # Extract Member, scenario and time frame from column names
        dplyr::mutate(., member=   stringr::str_extract(long_name, "Member\\.\\d+"),
               scenario =  stringr::str_extract(long_name, "_.*_") %>%  stringr::str_remove_all(., "_"),
               time_frame =  stringr::str_extract(long_name, "_\\d+.*") %>%  stringr::str_remove(., "_")) %>%
          # Replace "." with "-" in time frame
          dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))

      }
    }

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(fill = 'antiquewhite1',
            color = "black",
            data = countries) +
    ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value),
                data = rs_df, alpha=alpha) +
    {
      if(!bins) {
        ggplot2::scale_fill_gradientn(
          colors = colors,
          limits = legend_range,
          na.value = "transparent",
          n.breaks = 10,
          guide=ggplot2::guide_colourbar(ticks.colour = "black",
                                         ticks.linewidth = 1, title.position="top", title.hjust=0.5, label.theme = ggplot2::element_text(angle = 45),  label.hjust = 1))
      } else {

        ggplot2::scale_fill_stepsn(
          colors = colors,
          limits = legend_range,
          na.value = "transparent",
          n.breaks = ifelse(is.null(n.bins), 10, n.bins),
          guide=ggplot2::guide_colourbar(ticks.colour = "black",
                                         ticks.linewidth = 1, title.position="top", title.hjust=0.5, label.theme = ggplot2::element_text(angle = 45),  label.hjust = 1))

      }

    }+
    ggplot2::coord_sf(
      xlim = c(range(rs_df$x)[[1]] - 1, range(rs_df$x)[[2]] + 1),
      ylim = c(range(rs_df$y)[[1]] - 1, range(rs_df$y)[[2]] + 1),
      expand = F,
      ndiscr = 500
    ) +
    {
      if(ensemble) {

        ggplot2::facet_grid(time_frame ~ scenario )
      } else {

        ggh4x::facet_nested(scenario ~ time_frame   + member)
      }
    } +
    ggplot2::labs(fill = plot_titles, x="", y="") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      plot.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = 'aliceblue'),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks= ggplot2::element_blank(),
      axis.title= ggplot2::element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.height= ggplot2::unit(0.5, 'cm'),
      legend.key.width= ggplot2::unit(2, 'cm'),
      legend.box.spacing=ggplot2::unit(0, "pt")
    )

  message(Sys.time(), " Done")

  return(p)

}


#' @export

plotting.CAVAanalytics_ccs <- function(rst, palette=NULL, legend_range=NULL, plot_titles, ensemble, bins=FALSE, n.bins=NULL, alpha=NA, stat="mean") {

  # checking requirements
  stopifnot(is.logical(ensemble))
  stopifnot(is.logical(bins))
  match.arg(stat, choices = c("mean", "sd"))

  # messages

  if (isTRUE(ensemble)) {
    message(Sys.time(), "\n", paste0("Visualizing ensemble ", stat))
  } else {message(Sys.time(), "\n", "Visualizing individual members, argument stat is ignored")}

  message(Sys.time(), "\n", "Prepare for plotting")

  # retrieve the right raster stack based on the ensemble argument

  rst <- if (isTRUE(ensemble) & stat=="mean") rst[[1]] else if (isTRUE(ensemble) & stat=="sd") rst[[2]] else rst[[3]]

  # Set default colors for legend
  colors <- if (is.null(palette)) c("blue", "cyan", "green", "yellow", "orange", "red", "black") else palette

  # Set default range for legend
  legend_range <- if (is.null(legend_range)) c(range(raster::values(rst), na.rm = TRUE)) else legend_range

  # Suppress warnings
  options(warn = -1)

  # Get countries data
  countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  # Convert raster to dataframe

  rs_df <- raster::as.data.frame(rst, xy = TRUE, na.rm = TRUE) %>%
    tidyr::pivot_longer(
      cols = 3:ncol(.),
      values_to = "value",
      names_to = "long_name"
    ) %>% {
      if(ensemble) {
        # Extract scenario and time frame from column names
        dplyr::mutate(., scenario = stringr::str_split(long_name, "_") %>% purrr::map_chr(., 2),
                      time_frame =  stringr::str_split(long_name, "_") %>% purrr::map_chr(., 3)) %>%
          # Replace "." with "-" in time frame
          dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))

      } else {
        # Extract Member, scenario and time frame from column names
        dplyr::mutate(., member=   stringr::str_split(long_name, "_") %>% purrr::map_chr(., 1),
                      scenario =  stringr::str_split(long_name, "_") %>% purrr::map_chr(., 2),
                      time_frame =  stringr::str_split(long_name, "_") %>% purrr::map_chr(., 3)) %>%
          # Replace "." with "-" in time frame
          dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))

      }
    }

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(fill = 'antiquewhite1',
            color = "black",
            data = countries) +
    ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value),
                data = rs_df) +
    {
      if(!bins) {

        ggplot2::scale_fill_gradientn(
          colors = colors,
          limits = legend_range,
          na.value = "transparent",
          n.breaks = 10,
          guide=ggplot2::guide_colourbar(ticks.colour = "black",
                                         ticks.linewidth = 1, title.position="top", title.hjust=0.5, label.theme = ggplot2::element_text(angle = 45),  label.hjust = 1))
      } else {

        ggplot2::scale_fill_stepsn(
          colors = colors,
          limits = legend_range,
          na.value = "transparent",
          n.breaks = ifelse(is.null(n.bins), 10, n.bins),
          guide=ggplot2::guide_colourbar(ticks.colour = "black",
                                         ticks.linewidth = 1, title.position="top", title.hjust=0.5, label.theme = ggplot2::element_text(angle = 45),  label.hjust = 1))
      }
    } +
    ggplot2::coord_sf(
      xlim = c(range(rs_df$x)[[1]] - 1, range(rs_df$x)[[2]] + 1),
      ylim = c(range(rs_df$y)[[1]] - 1, range(rs_df$y)[[2]] + 1),
      expand = F,
      ndiscr = 500
    ) +
    {
      if(ensemble) {

        ggplot2::facet_grid(time_frame ~ scenario )
      } else {

        ggh4x::facet_nested(scenario ~ time_frame   + member)
      }
    } +
    ggplot2::labs(fill = plot_titles, x="", y="") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      plot.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = 'aliceblue'),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks= ggplot2::element_blank(),
      axis.title= ggplot2::element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.height= ggplot2::unit(0.5, 'cm'),
      legend.key.width= ggplot2::unit(2, 'cm'),
      legend.box.spacing=ggplot2::unit(0, "pt")
    )

  message(Sys.time(), " Done")

  return(p)

}



#' @export

plotting.CAVAanalytics_trends <- function(rst, palette=NULL, legend_range=NULL, plot_titles, ensemble, bins=FALSE, n.bins=NULL,alpha=NA, spatial_average) {

  # checking requirements
  stopifnot(is.logical(ensemble))
  stopifnot(is.logical(bins))
  stopifnot(is.logical(spatial_average))


  # messages

  if (isTRUE(ensemble)) {
    message(Sys.time(), "\n", paste0(ifelse(spatial_average,"Visualizing ensemble, spatial averages ", "Visualizing ensemble ")))
  } else {message(Sys.time(), "\n", paste0(ifelse(spatial_average,"Visualizing individual members after spatial averages ", "Visualizing individual members ")))}

  message(Sys.time(), "\n", "Prepare for plotting")

  # retrieve the right raster stack based on how trends was run

  if (length(rst)>4) { # trends were ru on projections
    if (ensemble) {
      members <- length(unique(stringr::str_match(names((rst[[3]])), "Member.\\d")))
      rst <- rst[1:2]

    } else {

      rst <- rst[3:4]
    }

  } # to be compledted for observaions

  # Set default colors for legend
  colors <- if (is.null(palette)) c("blue", "cyan", "green", "yellow", "orange", "red", "black") else palette

  # Set default range for legend
  legend_range <- if (is.null(legend_range)) {
    if (!ensemble) {
      c(range(raster::values(rst[[1]]), na.rm = TRUE))} else {

        c(-members, members)
      }
  } else {
    legend_range}

  # Suppress warnings
  #options(warn = -1)

  # Get countries data
  countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  # Convert raster to dataframe for both coefficients and p.values

  rs_df <-
    purrr::map(
      rst,
      ~ raster::as.data.frame(.x, xy = TRUE, na.rm = TRUE) %>%
        tidyr::pivot_longer(
          cols = 3:ncol(.),
          values_to = "value",
          names_to = "long_name",
        ) %>%
        {
          if(ensemble) {
            # Extract scenario and time frame from column names
            dplyr::mutate(., scenario = stringr::str_split(long_name, "_") %>% purrr::map_chr(., 1),
                          time_frame =  stringr::str_split(long_name, "_") %>% purrr::map_chr(., 3)) %>%
              # Replace "." with "-" in time frame
              dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-")) %>%
              dplyr::mutate(., value=ifelse(value==999, NA, value)) # 999 is the value assigned instead of NA

          } else {
            # Extract Member, scenario and time frame from column names
            dplyr::mutate(., member=   stringr::str_split(long_name, "_") %>% purrr::map_chr(., 1),
                          scenario =  stringr::str_split(long_name, "_") %>% purrr::map_chr(., 2),
                          time_frame =  stringr::str_split(long_name, "_") %>% purrr::map_chr(., 4)) %>%
              # Replace "." with "-" in time frame
              dplyr::mutate(., time_frame =  stringr::str_replace(time_frame, "\\.", "-"))

          }
        })

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(fill = 'antiquewhite1',
                     color = "black",
                     data = countries) +
    ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value),
                         data = rs_df[[1]],  alpha=alpha) +
    {
      if(!bins) {

        ggplot2::scale_fill_gradientn(
          colors = colors,
          limits = legend_range,
          na.value = "transparent",
          n.breaks = 10,
          guide=ggplot2::guide_colourbar(ticks.colour = "black",
                                         ticks.linewidth = 1, title.position="top", title.hjust=0.5, label.theme = ggplot2::element_text(angle = 45),  label.hjust = 1))
      } else {

        ggplot2::scale_fill_stepsn(
          colors = colors,
          limits = legend_range,
          na.value = "transparent",
          n.breaks = ifelse(is.null(n.bins), 10, n.bins),
          guide= ggplot2::guide_colourbar(ticks.colour = "black",
                                          ticks.linewidth = 1, title.position="top", title.hjust=0.5, label.theme = ggplot2::element_text(angle = 45),  label.hjust = 1))
      }
    } +
    ggplot2::geom_point(
      data = dplyr::filter(rs_df[[2]], value < 0.05),
      size = 0.1,
      alpha=0.4,
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
      if(ensemble) {

        ggplot2::facet_grid(time_frame ~scenario  )
      } else {

        ggh4x::facet_nested(scenario ~ time_frame   + member)
      }
    } +
    ggplot2::labs(fill = plot_titles, x="", y="") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      plot.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = 'aliceblue'),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks= ggplot2::element_blank(),
      axis.title= ggplot2::element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.height= ggplot2::unit(0.5, 'cm'),
      legend.key.width= ggplot2::unit(2, 'cm'),
      legend.box.spacing=ggplot2::unit(0, "pt")
    )

  message(Sys.time(), " Done")

  return(p)

}



