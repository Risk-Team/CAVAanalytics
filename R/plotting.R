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
#' @param legend.range numeric of length=2,
#' @param trends logical, if TRUE applies linear regression
#' @param season Numerical, seasons to select. For example, 1:12
#' @param palette character vector indicating colors to be used
#' @param method character, default to "scaling". Indicates the bias correction method
#' @param prov.country character, country for which provinces are highlighted
#' @param consecutive logical, to use in conjuunction with lowert or uppert
#' @param duration character, either "max" or "total".
#' @return ggplot object
#' @examples
#' fpath <- system.file("extdata/", package="chatR")
#' exmp <- load_data(country = "Moldova", variable="hurs", n.cores=6,
#'               path.to.rcps = fpath)
#' projections(exmp, season = 1:12)
#'
#'


projections <-
  function(data,
           bias.correction=F,
           uppert = NULL,
           lowert = NULL,
           legend.range = NULL,
           trends=F,
           season,
           palette = NULL,
           method = "scaling",
           prov.country = NULL,
           consecutive = FALSE,
           duration = "max") {
    # checking inputs requirement
    if (data[[4]] != "C4R.dataframe")
      stop("The input data does not seem to be the output of the chatR loading function")
    stopifnot(is.logical(consecutive),
              is.logical(bias.correction),
              is.logical(trends))

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

    if (!trends & is.null(uppert) & is.null(lowert)) {
      mes = paste0(
        "Calculation of ",
        ifelse(var == "pr", "total ", "mean "),
        ifelse(bias.correction, "bias-corrected ", " "),
        var
      )
    }

    if (!trends &
        (!is.null(uppert) | !is.null(lowert)) & !consecutive) {
      mes = paste0(
        "Calculation of number of days with ",
        var,
        ifelse(
          !is.null(lowert),
          paste0(" below threshold of ", lowert),
          paste0(" above threshold of ", uppert)
        ),
        ifelse(bias.correction, " after bias-correction", "")
      )
    }

    if (trends & is.null(uppert) & is.null(lowert)) {
      mes = paste0(
        "Calculation of yearly increase in ",
        ifelse(var == "pr", "total ", "mean "),
        var,
        ifelse(bias.correction, " after bias-correction", "")
      )
    }
    if (trends &
        (!is.null(uppert) | !is.null(lowert)) & !consecutive) {
      mes = paste0(
        var,
        ". Calculation of yearly increase in number of days ",
        ifelse(
          !is.null(lowert),
          paste0("below ", lowert),
          paste0("above ", uppert)
        ),
        ifelse(bias.correction, " after bias-correction ", " ")
      )
    }
    if (!trends &
        (!is.null(uppert) |
         !is.null(lowert)) & (consecutive & duration == "max")) {
      mes = paste0(
        "Calculation of maximum length of consecutive number of days ",
        ifelse(
          !is.null(lowert),
          paste0("below ", lowert),
          paste0("above ", uppert)
        ),
        ifelse(bias.correction, " after bias-correction", "")
      )
    }
    if (trends &
        (!is.null(uppert) |
         !is.null(lowert)) & (consecutive & duration == "max")) {
      mes = paste0(
        "Calculation of yearly increase in maximum length of consecutive number of days ",
        ifelse(
          !is.null(lowert),
          paste0("below ", lowert),
          paste0("above ", uppert)
        ),
        ifelse(bias.correction, " after bias-correction", "")
      )
    }
    if (!trends &
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
        ifelse(bias.correction, " after bias-correction", "")
      )
    }
    if (trends &
        (!is.null(uppert) |
         !is.null(lowert)) & (consecutive & duration == "total")) {
      mes = paste0(
        var,
        ". Calculation of yearly increase in total total number of consecutive days with duration longer than 6 days, ",
        ifelse(
          !is.null(lowert),
          paste0("below threshold of ", lowert),
          paste0("above threshold of ", uppert)
        ),
        ifelse(bias.correction, " after bias-correction", "")
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
              " Performing bias correction with the",
              method,
              " method for each model separately and then calculating the ensemble mean. Season",
              glue_collapse(season, "-")
            )
          )
          mutate(.,
                 models_mbrs = future_map(models_mbrs, function(x) {
                   if (var == "pr") {
                     bc <-
                       downscaleR::biasCorrection(
                         y = obs[[1]],
                         x = filter(datasets, RCP == "historical")$models_mbrs[[1]],
                         newdata = x,
                         precipitation = TRUE,
                         method = method,
                         scaling.type = "multiplicative"
                       )
                   } else {
                     bc <-
                       downscaleR::biasCorrection(
                         y = obs[[1]],
                         x = filter(datasets, RCP == "historical")$models_mbrs[[1]],
                         newdata = x,
                         precipitation = FALSE,
                         method = method,
                         scaling.type = "additive"
                       )
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
        models_tmp = map(models_mbrs, function(x)  {
          # divide the data in 30 years time frame for projections
          yrs <- list(2010:2039, 2040:2069, 2070:2099)
          map(yrs, ~ transformeR::subsetGrid(x, years = .x))
        }),
        models_agg_y = future_map(models_tmp, function(x)
          map(
            x,
            ~ suppressMessages(transformeR::aggregateGrid(# perform aggregation based on seasonended output
              .x, aggr.y =
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
          )),
        models_mean = future_map(models_agg_y, function(x)
          map(
            x, ~ suppressMessages(transformeR::aggregateGrid(.x, aggr.mem = list(FUN = "mean", na.rm = TRUE)))
          )),
        rs_tot = map(models_mean, ~ map(.x, function(y) {
          if (!trends) {
            # when trends is not specified
            # Average the 30 years data
            arry_mean <-
              apply(y$Data, c(2, 3), mean, na.rm = TRUE)
            y$Data <- arry_mean
            rs <- make_raster(y)
            return(rs)
          } else {
            # for trends
            options(warn = -1)
            trendgrid <-
              suppressMessages(linearTrend(y, p = 0.9) %>% transformeR::subsetGrid(var = "b")  %>% make_raster)
            trendgrid_pvalues <-
              suppressMessages(linearTrend(y, p = 0.9) %>% transformeR::subsetGrid(var = "pval")  %>% make_raster)
            names(trendgrid)  <-
              paste0(names(trendgrid), "_b")
            names(trendgrid_pvalues)  <-
              paste0(names(trendgrid_pvalues), "_pval")
            rs <- stack(trendgrid, trendgrid_pvalues)
            return(rs)
          }
        }))
      ) %>%
      mutate(rs_tot = map(rs_tot, raster::stack))

    rs_tot <- stack(data_list$rs_tot)

    names(rs_tot) <-
      c(paste0("RCP2.6_", names(rs_tot)[1:ifelse(trends, 6, 3)]), paste0("RCP8.5_", names(rs_tot)[ifelse(trends, 7, 4):ifelse(trends, 12, 6)]))

    rs_tot <- rs_tot %>%
      crop(., country_shp, snap = "out") %>%
      mask(., country_shp) %>%
      stack()

    if (trends)  {
      rs_tot_p <-  subset(rs_tot, grep("pval", names(rs_tot)))
      rs_tot <- subset(rs_tot, grep("b", names(rs_tot)))


    }

    # for legends
    colors <-
      rev(c("blue", "cyan", "green", "yellow", "orange", "red", "black"))

    colors_pr <-
      c("red", "orange", "yellow", "green", "cyan", "blue", "purple")

    legend_range = if (is.null(legend.range))
      c(range(raster::values(rs_tot), na.rm = TRUE))
    else
      legend.range

    options(warn = -1)
    col <-
      if (var == "pr") {
        if (is.null(lowert))
          colors_pr
        else
          rev(colors_pr)
      }
    else {
      if (is.null(lowert))
        rev(colors)
      else
        colors
    }

    countries <-
      ne_countries(scale = "medium", returnclass = "sf")

    provinces <-
      if (!is.null(prov.country))
        ne_states(country = prov.country, returnclass = "sf")
    else
      ne_countries(scale = "medium", returnclass = "sf")

    colors <- if (!is.null(palette))
      palette
    else
      col

    plot_titles <- if (is.null(lowert) & is.null(uppert)) {
      if (var == "pr" & isFALSE(trends))
        "mm"
      else if (var == "pr" & trends)
        "mm/year"
      else if (var != "pr" & isFALSE(trends))
        "°C"
      else if (var != "pr" & trends)
        "°C/year"
    } else {
      if (trends)
        "N° days/year"
      else
        "N° days"
    }

    rs_df <- raster::as.data.frame(rs_tot, xy = TRUE, na.rm = TRUE) %>%
      tidyr::pivot_longer(
        cols = 3:ncol(.),
        values_to = "value",
        names_to = "long_name"
      )  %>%
      mutate(
        scenario = str_extract(long_name, "RCP\\d.\\d"),
        time_frame = str_extract(long_name, "2\\d+_\\d+")
      ) %>%
      mutate(time_frame = str_replace(time_frame, "_", "-"))

    if (trends) {
      rs_df.p <- as.data.frame(rs_tot_p, xy = TRUE, na.rm = TRUE) %>%
        tidyr::pivot_longer(
          cols = 3:ncol(.),
          values_to = "value",
          names_to = "long_name"
        )  %>%
        mutate(
          scenario = str_extract(long_name, "RCP\\d.\\d"),
          time_frame = str_extract(long_name, "2\\d+_\\d+")
        ) %>%
        mutate(time_frame = str_replace(time_frame, "_", "-"))
    } else {
      rs_df.p = data.frame(x = double(),
                           y = double(),
                           value = double())
    }

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
      geom_sf(
        fill = NA,
        color = "black",
        data = provinces,
        lwd = 0.2
      ) +
      coord_sf(
        xlim = c(range(rs_df$x)[[1]] - 2, range(rs_df$x)[[2]] + 2),
        ylim = c(range(rs_df$y)[[1]] - 2, range(rs_df$y)[[2]] + 2),
        expand = F,
        ndiscr = 500
      ) +
      geom_point(
        data = filter(rs_df.p, value < 0.05),
        size = 0.3,
        shape = 19,
        color = "black",
        aes(x, y)
      ) +
      facet_grid(scenario ~ time_frame) +
      labs(x = "Longitude", y = "Latitude", fill = plot_titles) +
      theme_bw(base_size = 12) +
      theme(
        plot.background = element_blank(),
        panel.background = element_rect(fill = 'aliceblue'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1,
          size = 8
        ),
        axis.text.y = element_text(size = 8)
      ) +
      guides(fill = guide_colourbar(
        barwidth = 0.5,
        barheight = 10,
        ticks.colour = "black",
        ticks.linewidth = 2
      ))

    message(Sys.time(), " Done")

    return(p)

  } # end of function



