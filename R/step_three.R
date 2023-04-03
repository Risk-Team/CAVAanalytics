#' Step three: Visualization
#'
#' Automatically plot results from cavaR step two
#' @export

#' @import ggplot2
#' @param rst output of one of cavaR functions, such as projections. rst is of class RasterStack
#' @param palette charachter. Color Palette
#' @param legend.range  numeric. Fix legend limits
#' @param plot_titles character. Title of the plot legend
#' @param ensemble logical. Whether to visualize the ensemble mean or each individual model
#' @return ggplot object
#' @examples
#'load_data(country = "Somalia", variable="tas", years.hist=2000, years.proj=2010,
#'               path.to.data = "CORDEX-CORE", domain="AFR-22") %>%
#' projections(., season = 1:12) %>%
#' plotting(plot_titles="hurs", ensemble=T)


plotting <- function(rst, palette, legend.range, plot_titles, ensemble, ...) {
UseMethod("plotting")
}

#' @export

plotting.cavaR_projections <- function(rst, palette=NULL, legend_range=NULL, plot_titles, ensemble, stat="mean") {

  # checking requirements
  stopifnot(is.logical(ensemble))
  match.arg(stat, choices = c("mean", "sd"))

  # messages

  if (isTRUE(ensemble)) {
    message(Sys.time(), "\n", paste0("Visualizing ensemble ", stat))
  } else {message(Sys.time(), "\n", "Visualizing individual members")}

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

  p <- ggplot() +
    scale_fill_gradientn(
      colors = colors,
      limits = legend_range,
      na.value = "transparent",
      n.breaks = 10,
      guide=guide_colourbar(ticks.colour = "black",
                            ticks.linewidth = 1, title.position="top", title.hjust=0.5)) +
    geom_sf(fill = 'antiquewhite1',
            color = "black",
            data = countries) +
    geom_raster(aes(x = x, y = y, fill = value),
                data = rs_df,
                alpha = 0.7) +
    coord_sf(
      xlim = c(range(rs_df$x)[[1]] - 2, range(rs_df$x)[[2]] + 2),
      ylim = c(range(rs_df$y)[[1]] - 2, range(rs_df$y)[[2]] + 2),
      expand = F,
      ndiscr = 500
    ) +
    {
      if(ensemble) {

        facet_grid(scenario ~ time_frame )
      } else {

        ggh4x::facet_nested(scenario ~ time_frame   + member)
      }
    } +
    labs(fill = plot_titles, x="", y="") +
    theme_bw(base_size = 12) +
    theme(
      plot.background = element_blank(),
      panel.background = element_rect(fill = 'aliceblue'),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks= element_blank(),
      axis.title= element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.key.height= unit(0.5, 'cm'),
      legend.key.width= unit(2, 'cm'),
      legend.box.spacing=unit(0, "pt")
    )

  message(Sys.time(), " Done")

  return(p)

}




