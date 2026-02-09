.onAttach <- function(libname, pkgname) {
  # Show version
  packageStartupMessage(
    cli::format_message(
      "{.pkg CAVAanalytics} v{packageVersion(pkgname)}"
    )
  )

  # Check for announcements (once per session, fail silently)
  if (is.null(getOption("CAVAanalytics.announcement_checked"))) {
    options(CAVAanalytics.announcement_checked = TRUE)

    tryCatch(
      {
        url <- "https://api.github.com/repos/Risk-Team/CAVAanalytics/issues?labels=announcement&state=open&per_page=1"
        resp <- readLines(url, warn = FALSE)
        resp <- paste(resp, collapse = "")
        issues <- jsonlite::fromJSON(resp)

        if (length(issues) > 0 && nrow(issues) > 0) {
          title <- issues$title[1]
          body <- issues$body[1]

          msg <- cli::format_message(c(
            "",
            "!" = "{.strong {title}}",
            if (!is.na(body) && nzchar(trimws(body))) "i" = trimws(body)
          ))
          packageStartupMessage(msg)
        }
      },
      error = function(e) NULL
    )
  }
}
