# Data path availability ---------------------------------------------------

#' Test whether a local file or HTTP(S) URL is reachable
#'
#' Uses [file.exists()] for filesystem paths. For URLs, opens a short read-only
#' connection ([base::url()]) and reads one byte so redirects are followed and
#' TLS is validated like other R download code. THREDDS `dodsC` dataset URLs
#' often return HTTP 400 on the bare path, so `dodsC` endpoints are probed
#' with an OPeNDAP `.dds` suffix first.
#'
#' @param path Character path or URL.
#' @return Logical scalar.
#' @noRd
.path_is_reachable <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    return(FALSE)
  }
  if (stringr::str_detect(path, "^https?://")) {
    .url_reachable(path)
  } else {
    file.exists(path)
  }
}

#' @noRd
.url_reachable <- function(url) {
  probe_one <- function(u) {
    tryCatch(
      {
        con <- url(u, "rb")
        on.exit(close(con), add = TRUE)
        suppressWarnings(readBin(con, "raw", n = 1L))
        TRUE
      },
      error = function(e) FALSE
    )
  }

  candidates <- if (
    stringr::str_detect(url, "/dodsC/") &&
      !stringr::str_detect(url, stringr::regex("\\.dds(\\?|$)"))
  ) {
    c(paste0(url, ".dds"), url)
  } else {
    url
  }

  for (u in candidates) {
    if (isTRUE(probe_one(u))) {
      return(TRUE)
    }
  }
  FALSE
}

#' @noRd
.cordex_domain_choices <- function() {
  c(
    "AFR-22",
    "SEA-22",
    "AUS-22",
    "EAS-22",
    "CAM-22",
    "SAM-22",
    "WAS-22",
    "NAM-22",
    "EUR-22",
    "CAS-22"
  )
}

#' @noRd
.validate_domain_for_cordex_check <- function(domain) {
  if (is.null(domain)) {
    cli::cli_abort("{.arg domain} is required when checking CORDEX model paths.")
  }
  match.arg(domain, choices = .cordex_domain_choices())
}

#' ERA5 and W5E5 paths for hub or THREDDS (same as [load_data()] / [load_data_hub()]).
#'
#' @noRd
.standard_obs_check_paths <- function(kind = c("hub", "thredds")) {
  kind <- match.arg(kind)
  if (kind == "hub") {
    p <- c(load_obs_paths.hub("ERA5"), load_obs_paths.hub("W5E5"))
  } else {
    p <- c(load_obs_paths.thredds("ERA5"), load_obs_paths.thredds("W5E5"))
  }
  dplyr::tibble(path = p, role = c("ERA5", "W5E5"))
}

#' Human-readable count of CORDEX URLs vs ERA5/W5E5 for CLI messages.
#'
#' @noRd
.check_paths_breakdown_phrase <- function(roles) {
  n_model <- sum(roles == "model")
  n_re <- sum(roles %in% c("ERA5", "W5E5"))
  if (n_model == 0L) {
    cli::format_inline("{n_re} reanalysis path{?s} (ERA5 & W5E5)")
  } else {
    cli::format_inline(
      "{n_model} CORDEX simulation{?s} + {n_re} reanalysis (ERA5 & W5E5)"
    )
  }
}

#' Summarise path checks and return a tibble
#'
#' @noRd
.check_paths_report <- function(paths, roles, kind_label) {
  if (length(paths) == 0L) {
    cli::cli_alert_info("No {kind_label} paths to check.")
    return(invisible(
      dplyr::tibble(path = character(), role = character(), exists = logical())
    ))
  }

  result <- dplyr::tibble(path = paths, role = roles) %>%
    dplyr::mutate(exists = purrr::map_lgl(path, .path_is_reachable))

  n_ok <- sum(result$exists)
  n_tot <- nrow(result)
  brk <- .check_paths_breakdown_phrase(roles)
  if (n_ok == n_tot) {
    cli::cli_alert_success(
      "{kind_label}: all {n_tot} path{?s} reachable ({brk})."
    )
  } else {
    cli::cli_alert_warning(
      "{kind_label}: {n_ok} of {n_tot} path{?s} reachable ({brk})."
    )
    missing <- result$path[!result$exists]
    cli::cli_bullets(
      stats::setNames(as.list(missing), rep("x", length(missing)))
    )
  }

  invisible(result)
}


#' Check that CORDEX and observation paths exist on the HUB filesystem
#'
#' Uses the same inventory and path rules as [load_data_hub()] (including
#' rewriting JupyterHub prefixes to the GPFS data root). Lists every model path
#' in the inventory for the given `domain` and `database` (all experiments:
#' historical, rcp26, rcp85). Does not load grid data and does not use years.
#'
#' @param database `CORDEX-CORE`, `CORDEX-CORE-BC`, or `NULL` to check only the
#'   standard reanalysis paths (ERA5 and W5E5 on the hub filesystem).
#' @param domain CORDEX domain (e.g. `AFR-22`). Required when `database` is set.
#'
#' ERA5 and W5E5 are always included (same locations as [load_data_hub()] with
#' `path.to.obs = "ERA5"` or `"W5E5"`).
#'
#' @return Invisibly, a tibble with columns `path`, `role` (`model`, `ERA5`, or
#'   `W5E5`), and `exists`. The CLI reports how many paths are reachable.
#'
#' @seealso [load_data_hub()], [check_data_thredds()]
#' @export
check_data_hub <- function(
  database = "CORDEX-CORE",
  domain = NULL
) {
  has_models <- !is.null(database)

  model_paths <- character()
  if (has_models) {
    if (!database %in% c("CORDEX-CORE", "CORDEX-CORE-BC")) {
      cli::cli_abort(
        "Only CORDEX-CORE and CORDEX-CORE-BC are supported for {.fn check_data_hub}"
      )
    }
    domain <- .validate_domain_for_cordex_check(domain)
    files <- load_model_paths.hub(
      domain,
      years.hist = NULL,
      years.proj = NULL,
      path.to.data = database
    )
    model_paths <- unlist(files, use.names = FALSE)
  }

  obs <- .standard_obs_check_paths("hub")
  paths <- c(model_paths, obs$path)
  roles <- c(rep("model", length(model_paths)), obs$role)

  .check_paths_report(paths, roles, "HUB")
}


#' Check that CORDEX and observation URLs are reachable on THREDDS
#'
#' Uses the same inventory and URLs as [load_data()] when
#' `path.to.data` is `CORDEX-CORE` or `CORDEX-CORE-BC`. Lists every model URL
#' for the given `domain` and dataset (all experiments). Performs a light HTTP
#' check only (does not open datasets). Does not use years.
#'
#' @param path.to.data `CORDEX-CORE`, `CORDEX-CORE-BC`, or `NULL` to check only
#'   the standard THREDDS URLs for ERA5 and W5E5. Local directory paths are not
#'   supported here.
#' @param domain CORDEX domain (e.g. `AFR-22`). Required when `path.to.data` is set.
#'
#' ERA5 and W5E5 are always included (same URLs as [load_data()] with
#' `path.to.obs = "ERA5"` or `"W5E5"`).
#'
#' @return Invisibly, a tibble with columns `path`, `role` (`model`, `ERA5`, or
#'   `W5E5`), and `exists`. The CLI reports how many paths are reachable.
#'
#' @seealso [load_data()], [check_data_hub()]
#' @export
check_data_thredds <- function(
  path.to.data = "CORDEX-CORE",
  domain = NULL
) {
  has_models <- !is.null(path.to.data)

  if (has_models && !path.to.data %in% c("CORDEX-CORE", "CORDEX-CORE-BC")) {
    cli::cli_abort(c(
      "x" = "{.arg path.to.data} must be CORDEX-CORE, CORDEX-CORE-BC, or NULL.",
      "i" = "For local directories, use {.code file.exists()} or list files yourself."
    ))
  }

  model_paths <- character()
  if (has_models) {
    domain <- .validate_domain_for_cordex_check(domain)
    files <- load_model_paths.thredds(
      domain,
      years.hist = NULL,
      years.proj = NULL,
      path.to.data = path.to.data
    )
    model_paths <- unlist(files, use.names = FALSE)
  }

  obs <- .standard_obs_check_paths("thredds")
  paths <- c(model_paths, obs$path)
  roles <- c(rep("model", length(model_paths)), obs$role)

  .check_paths_report(paths, roles, "THREDDS")
}
