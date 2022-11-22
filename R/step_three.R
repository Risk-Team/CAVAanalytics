#' Step three: Visualization
#'
#' Automatically plot results from cavaR step two
#' @export
#' @import stringr
#' @import ggplot2
#' @import dplyr
#' @import rnaturalearth
#' @import RColorBrewer
#' @importFrom raster as.data.frame
#' @param rst output of one of cavaR functions, such as projections. rst is of class RasterStack
#' @param palette charachter. Color Palette
#' @param legend.range  numeric. Fix legend limits
#' @param plot_titles character. Title of teh plot legend
#' @return ggplot object
#' @examples
#' fpath <- system.file("extdata/", package="chatR")
#' exmp <- load_data(country = "Moldova", variable="hurs", years.hist=2000, years.proj=2010, path.to.data = fpath) %>%
#' projections(., season = 1:12) %>%
#' plotting(plot_titles="hurs")
#' exmp
#'
#'


plotting <- function(rst, palette=NULL, legend.range=NULL, plot_titles) {


  # for legends
  colors <- if(is.null(palette)) c("blue", "cyan", "green", "yellow", "orange", "red", "black") else palette

  legend_range = if (is.null(legend.range)) c(range(raster::values(rst), na.rm = TRUE)) else legend.range

  options(warn = -1)

  countries <-
    ne_countries(scale = "medium", returnclass = "sf")


  rs_df <- raster::as.data.frame(rst, xy = TRUE, na.rm = TRUE) %>%
    tidyr::pivot_longer(
      cols = 3:ncol(.),
      values_to = "value",
      names_to = "long_name"
    )  %>%
    mutate(
      scenario = str_extract(long_name, ".*_") %>% str_remove(., "_"),
      time_frame = str_extract(long_name, "_.*")%>% str_remove(., "_")
    ) %>%
    mutate(time_frame = str_replace(time_frame, "\\.", "-"))

  p <- ggplot() +
    scale_fill_gradientn(
      colors = colors,
      limits = legend_range,
      na.value = "transparent",
      n.breaks = 10
    ) +
    geom_sf(fill = 'antiquewhite1',
            color = "black",
            data = countries) +
    geom_raster(aes(x = x, y = y, fill = value),
                data = rs_df,
                alpha = 0.7) +
    coord_sf(
      xlim = c(range(rs_df$x)[[1]] - 4, range(rs_df$x)[[2]] + 4),
      ylim = c(range(rs_df$y)[[1]] - 4, range(rs_df$y)[[2]] + 4),
      expand = F,
      ndiscr = 500
    ) +
    # geom_point(
    #   data = filter(rs_df.p, value < 0.05),
    #   size = 0.3,
    #   shape = 19,
    #   color = "black",
    #   aes(x, y)
    # ) +
    facet_grid(scenario ~ time_frame) +
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
      axis.ticks= element_blank()
    ) +
    guides(fill = guide_colourbar(
      barwidth = 0.5,
      barheight = 10,
      ticks.colour = "black",
      ticks.linewidth = 2
    ))

  message(Sys.time(), " Done")

  return(p)

}

