#' Converting C4R lists into CAVAanalytics list
#'
#' Converting a series of C4R lists (output of loadeR::loadeGridData) into a CAVAanalytics list

#' @param historical list containing a list of C4R lists, one per model or NULL. For example, list(list(mod_1, mod_2)) where mod_1 and mod_2 are C4R lists.
#' @param projections named list containing a list of C4R lists or NULL. For example, list(rcp26=list(mod_1, mod_2), rcp85= list(mod_1, mod_2)) where mod_1 and mod_2 are C4R lists.
#' @param obs default is NULL. List containing a C4R list. Example list(dataset) where dataset is a C4R list
#' @importFrom magrittr %>%
#' @export


C4R_to_CAVA <- function(obs = NULL,
                        projections,
                        historical = NULL) {
  check.args <- function(projections, historical, obs) {
    if (!is.null(projections))
      stopifnot(is.list(projections))
    if (!is.null(historical))
      stopifnot(is.list(historical))

    if (!is.null(projections) & is.null(names(projections)))

      cli::cli_abort(
        c("x" = "Projections needs to contain named lists. For example, rcp26 and rcp85. Set names before proceedings")
      )

    if (!is.null(historical) & length(historical) != 1)

      cli::cli_abort(
        c("x" = "historical should be a nested list of length one, containing C4R lists. For example, if there are 3 models, historical should be length(historical[[1]])==3)")
      )

    if (!is.null(obs) & length(obs) != 1)

      cli::cli_abort(
        c("x" = "obs should be a nested list of length one, containing a C4R list. For example, to access the Data component you should obs[[1]]$Data")
      )

    if (!is.null(projections) & !is.null(historical)) {
      if (length(projections[[1]]) != length(historical[[1]]))
        cli::cli_abort(
          c("x" = "The number of models differ between projections and historical experiments")
        )
    }

    if (!is.null(projections)) {
      if (length(projections[[1]]) < 2)
        cli::cli_abort(
          c("x" = "Transforming C4R lists into a CAVAanalytics data frame requires at least two models")
        )
    }

    if (!is.null(historical)) {
      if (length(historical[[1]]) < 2)
        cli::cli_abort(
          c("x" = "Transforming C4R lists into a CAVAanalytics data frame requires at least two models")
        )
    }

  } # end of check args

  geo_localize <- function(projections, historical) {
    if (!is.null(projections) | !is.null(historical))  {
      coords <-
        if (!is.null(historical))
          historical[[1]][[1]]$xyCoords
      else
        projections[[1]][[1]]$xyCoords
      # Check if $xy is the same for all components
      if (!is.null(historical)) {
        check.hist <-
          all(sapply(historical[[1]], function(mbr)
            identical(mbr$xyCoords, coords)))
        if (check.hist) {

        } else {
          cli::cli_abort("Spatial extent in the historical experiment are not the same across C4R lists")
        }
      }
      if (!is.null(projections)) {
        check.proj <- all(sapply(projections, function(i) {
          sapply(i, function(mbr)
            identical(mbr$xyCoords, coords))
        }))
        if (check.proj) {

        } else {
          cli::cli_abort("Spatial extent in the projections experiment are not the same across C4R lists")
        }
      }
    }

    country_shp <- sf::st_bbox(c(
      xmin = min(coords$x),
      xmax = max(coords$x),
      ymax = max(coords$y),
      ymin = min(coords$y)
    )) %>%
      sf::st_as_sfc() %>%
      data.frame(geometry = .) %>%
      sf::st_as_sf()

    return(country_shp)

  }

  # checking

  check.args(projections = projections,
             historical = historical,
             obs = obs)
  country_shp <-
    geo_localize(projections = projections, historical = historical)

  # making the dataset

  experiment <-
    c(if (is.null(historical))
      NULL
      else
        "historical", if (!is.null(projections))
          names(projections)
      else
        NULL)

  models <- c(historical, projections)

  cli::cli_progress_step("Making CAVAanalytics list")

  models_df <-
    dplyr::tibble(experiment = experiment, models = models) %>%
    dplyr::mutate(models_mbrs = purrr::map(models, common_dates)) %>%
    dplyr::select(-models) %>%
    dplyr::mutate(obs = obs)

  cli::cli_process_done()

  if (!is.null(historical) | !is.null(projections)) {
    # Getting temporal resolution
    temp_res <- purrr::map(1:nrow(models_df), function(i) {
      dates <- models_df$models_mbrs[[i]]$Dates$start
      dates <- as.Date(dates)
      table(lubridate::year(dates), lubridate::month(dates))
    })

    names(temp_res) <- models_df$experiment

    # Checking temporal resolution
    differ_check <- lapply(1:nrow(models_df), function(n) {
      sapply(temp_res[[n]], function(x) {
        any(x != x[1])
      })
    }) %>% unlist()

    first_ncol <- ncol(temp_res[[1]])

    # Check the number of columns of the remaining tables
    same_ncol <-
      all(sapply(temp_res, function(table)
        ncol(table) == first_ncol))

    if (any(differ_check) | !same_ncol) {
      cli::cli_bullets(c("!" = "There might be temporal inconsistency in your data, check .[[3]]"))
    }
  } else {
    temp_res <- NULL
  }
  # returning object
  invisible(structure(
    list(models_df, country_shp, temp_res),
    class = "CAVAanalytics_list",
    components = list("data.frame with list columns", "bbox", "temporal resolution")
  ))

}
