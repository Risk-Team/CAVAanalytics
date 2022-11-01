#' Climate projections visualization
#'
#' Automatically plot climate models projections and useful statistics.
#' @export
#' @import stringr
#' @import ggplot2
#' @import purrr
#' @import furrr
#' @import dplyr
#' @import transformeR
#' @import rnaturalearthdata
#' @import rnaturalearth
#' @import RColorBrewer
#' @importFrom downscaleR biasCorrection
#' @importFrom glue glue_collapse
#' @importFrom climate4R.indices linearTrend
#' @importFrom raster crop mask stack extent subset flip

#' @param data output of load_data
#' @param bias.correction logical
#' @param uppert  numeric of length=1, upper threshold
#' @param lowert numeric of length=1, lower threshold
#' @param season Numerical, seasons to select. For example, 1:12
#' @param scaling.type character, default to "additive". Indicates whether to use multiplicative or additive approach for bias correction
#' @param consecutive logical, to use in conjuunction with lowert or uppert
#' @param duration character, either "max" or "total".
#' @return raster
#' @examples
#' fpath <- system.file("extdata/", package="chatR")
#' exmp <- load_data(country = "Moldova", variable="hurs", n.cores=6,
#'               path.to.rcps = fpath) %>%
#' projections(., season = 1:12)
#'
#'


projections <-
  function(data,
           bias.correction=F,
           uppert = NULL,
           lowert = NULL,
           season,
           scaling.type="additive",
           consecutive = FALSE,
           duration = "max") {
    # checking inputs requirement
    if (data[[4]] != "C4R.dataframe")
      stop("The input data does not seem to be the output of the chatR loading function")
    stopifnot(is.logical(consecutive),
              is.logical(bias.correction))

    if (!is.null(lowert) &
        !is.null(uppert))
      stop("select only one threshold")
    if (consecutive &
        (is.null(uppert)) &
        is.null(lowert))
      stop("Specify a threshold for which you want to calculate consecutive days")
    stopifnot(duration == "max" | duration == "total")

    if (!any(str_detect(colnames(data[[1]]),"obs"))) {
      warning("Bias correction cannot be performed, set as F")
      bias.correction=F
    }

    # retrieving information
    mod.numb <- dim(data[[1]]$models_mbrs[[1]]$Data) [1]
    datasets <- data[[1]]
    country_shp <- data[[2]]
    var <- datasets$models_mbrs[[1]]$Variable$varName

    # messages

    if (is.null(uppert) & is.null(lowert)) {
      mes = paste0(
        "Calculation of ",
        ifelse(var == "pr", "total ", "mean "),
        ifelse(bias.correction, "bias-corrected ", " "),
        var
      )
    }

    if ((!is.null(uppert) | !is.null(lowert)) & !consecutive) {
      mes = paste0(
        "Calculation of number of days with ",
        var,
        ifelse(
          !is.null(lowert),
          paste0(" below threshold of ", lowert),
          paste0(" above threshold of ", uppert)
        ),
        ifelse(bias.correction, " after bias-correction with scaling type", scaling.type, "")
      )
    }


    if ((!is.null(uppert) |
         !is.null(lowert)) & (consecutive & duration == "max")) {
      mes = paste0(
        "Calculation of maximum length of consecutive number of days ",
        ifelse(
          !is.null(lowert),
          paste0("below ", lowert),
          paste0("above ", uppert)
        ),
        ifelse(bias.correction, " after bias-correction with scaling type", scaling.type, "")
      )
    }

    if (
      (!is.null(uppert) |
       !is.null(lowert)) & (consecutive & duration == "total")) {
      mes = paste0(
        var,
        ". Calculation of total total number of consecutive days with duration longer than 6 days, ",
        ifelse(
          !is.null(lowert),
          paste0("below threshold of ", lowert),
          paste0("above threshold of ", uppert)
        ),
        ifelse(bias.correction, " after bias-correction with scaling type", scaling.type, "")
      )
    }
    # cores
    future::plan(future::multisession, workers = 2)

    # initialising

    if (any(str_detect(colnames(datasets), "obs"))) {

      datasets <- datasets %>%
        mutate_at(c("models_mbrs", "obs"), ~ map(., ~ subsetGrid(., season =
                                                                   season)))
    } else {

      datasets <- datasets %>%
        mutate_at(c("models_mbrs"), ~ map(., ~ subsetGrid(., season =
                                                            season)))
    }
    message(Sys.time(),
            " projections, season ",
            glue_collapse(season, "-"),
            ". ",
            mes)


    data_list <- datasets %>%
      filter(RCP != "historical") %>%
      {
        if (bias.correction) {
          message(
            paste(
              Sys.time(),
              " Performing bias correction with the scaling",
              " method, scaling type ", scaling.type, " for each model separately and then calculating the ensemble mean. Season",
              glue_collapse(season, "-")
            )
          )
          mutate(.,
                 models_mbrs = future_map(models_mbrs, function(x) {
                   if (var == "pr") {
                     bc <-
                       suppressMessages(downscaleR::biasCorrection(
                         y = obs[[1]],
                         x = filter(datasets, RCP == "historical")$models_mbrs[[1]],
                         newdata = x,
                         precipitation = TRUE,
                         method = "scaling",
                         scaling.type = scaling.type
                       ))
                   } else {
                     bc <-
                       suppressMessages(downscaleR::biasCorrection(
                         y = obs[[1]],
                         x = filter(datasets, RCP == "historical")$models_mbrs[[1]],
                         newdata = x,
                         precipitation = FALSE,
                         method ="scaling",
                         scaling.type = scaling.type
                       ))
                   }
                   out <-
                     transformeR::intersectGrid.time(x, bc, which.return = 2)
                   out$Dates$start <- x$Dates$start
                   out$Dates$end <-  x$Dates$end
                   return(out)
                 }))
        } else
          .
      }  %>%  # computing annual aggregation. if threshold is specified, first apply threshold
      mutate(
        models_agg_y = future_map(models_mbrs, function(x)
          suppressMessages(transformeR::aggregateGrid(# perform aggregation based on seasonended output
            x, aggr.y =
              if (var == "pr" &
                  !consecutive &
                  (is.null(uppert) & is.null(lowert))) {
                list(FUN = "sum")
              } else if (var != "pr" &
                         !consecutive &
                         (is.null(lowert) & is.null(uppert))) {
                list(FUN = "mean")
              } else if (consecutive) {
                list(
                  FUN = thrs_consec,
                  duration = duration,
                  lowert = lowert,
                  uppert = uppert
                )
              } else if (!consecutive) {
                list(FUN = thrs,
                     uppert = uppert,
                     lowert = lowert)
              }))
        ),
        rst = map2(RCP, models_agg_y, function(x, y) {
          y <- suppressMessages(transformeR::aggregateGrid(y, aggr.mem = list(FUN = "mean", na.rm = TRUE)))
          arry_mean <-
            apply(y$Data, c(2, 3), mean, na.rm = TRUE)
          y$Data <- arry_mean
          rs <- make_raster(y)
          names(rs) <- paste0(x, "_", names(rs)) %>%  str_remove(., "X")
          return(rs)
        })
      )

    rst=stack(data_list$rst) %>%
      crop(., country_shp, snap = "out") %>%
      mask(., country_shp) %>%
      stack()

    return(rst)

  }


#' @param rst output of one of cavaR functions, such as projections. rst is of class RasterStack
#' @param palette charachter. Color Palette
#' @param legend.range  numeric. Fix legend limits
#' @param plot_titles character. Title of teh plot legend
#' @return ggplot object
#' @examples
#' fpath <- system.file("extdata/", package="chatR")
#' exmp <- load_data(country = "Moldova", variable="hurs", n.cores=6,
#'               path.to.rcps = fpath) %>%
#' projections(., season = 1:12) %>%
#' plotting(plot_titles="hurs")
#'
#'


plotting <- function(rst, palette=NULL, legend.range=NULL, plot_titles) {


  # for legends
  colors <- if(is.null(palette)) rev(c("blue", "cyan", "green", "yellow", "orange", "red", "black")) else palette

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


  # if (trends) {
  #   rs_df.p <- as.data.frame(rs_tot_p, xy = TRUE, na.rm = TRUE) %>%
  #     tidyr::pivot_longer(
  #       cols = 3:ncol(.),
  #       values_to = "value",
  #       names_to = "long_name"
  #     )  %>%
  #     mutate(
  #       scenario = str_extract(long_name, ".*_"),
  #       time_frame = str_extract(long_name, "2\\d+_\\d+")
  #     ) %>%
  #     mutate(time_frame = str_replace(time_frame, "_", "-"))
  # } else {
  #   rs_df.p = data.frame(x = double(),
  #                        y = double(),
  #                        value = double())
  # }

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

