library(jsonlite)
library(cranlogs)
library(DT)
library(dplyr)
library(gh)
library(purrr)

fetch_cran_data <- function(author, pkgs = NULL, size = 20) {
  info_full <- pkgsearch::pkg_search(author, size = size)
  purrr::map_dfr(pkgs, function(pkg) {
    info <- info_full[info_full$package == pkg, ]
    website <- pick_primary_website(info$url, pkg = pkg)
    tibble::tibble(
      Package = pkg,
      Version = info$version,
      Title = info$title,
      Description = info$description,
      Downloads_30days = info$downloads_last_month,
      Website = website
    )
  }) |>
    arrange(desc(Downloads_30days))
}

fetch_runiverse_data <- function(author, pkgs = NULL, pkgs_exclude = NULL) {
  info_full <- jsonlite::fromJSON(
    paste0("https://", author, ".r-universe.dev/api/packages")
  ) |>
    tibble::as_tibble()
  if (!is.null(pkgs_exclude)) {
    pkgs <- setdiff(pkgs, pkgs_exclude)
  }
  info <- info_full |>
    mutate(
      Downloads_30days = `_downloads`$count,
      Website = vapply(URL, pick_primary_website, character(1))
    ) |>
    select(Package, Version, Title, Description, Downloads_30days, Website) |>
    arrange(desc(Downloads_30days))
  if (!is.null(pkgs_exclude)) {
    info <- dplyr::filter(info, !(Package %in% pkgs_exclude))
  }
  if (!is.null(pkgs)) {
    info <- dplyr::filter(info, Package %in% pkgs)
  }
  info
}

make_dt_with_hex <- function(
  data,
  hex_height = 60,
  pageLength = 25,
  buttons = FALSE
) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr must be installed")
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("DT must be installed")
  }
  data_clean <- data |>
    mutate(
      Package = paste0("<strong>", Package, "</strong>"),
      Version = as.character(Version),
      CRAN_url = sprintf("https://cran.r-project.org/package=%s", Package),
      # If Website is missing, use CRAN as default
      Website = ifelse(is.na(Website) | Website == "", CRAN_url, Website),
      HexURL = paste0(Website, "/logo.png"),
      HexHTML = sprintf(
        '<a href="%s" target="_blank"><img src="%s" height="%d"></a>',
        Website,
        HexURL,
        hex_height
      ),
      Website = HexHTML
    ) |>
    select(
      Package,
      # Version,
      Title,
      # Description,
      Downloads_30days,
      Website #,
      # CRAN_url
    ) |>
    rename(`Downloads (30-day)` = Downloads_30days) |>
    as.data.frame()

  dt_options <- list(
    pageLength = pageLength,
    autoWidth = TRUE,
    dom = "t"
  )

  if (buttons) {
    dt_options$dom <- "Bfrtip"
    dt_options$buttons <- c("copy", "csv", "excel")
  }

  DT::datatable(
    data_clean,
    escape = FALSE,
    rownames = FALSE,
    options = dt_options,
    class = "stripe hover row-border order-column"
  )
}

pick_primary_website <- function(raw_url, pkg = NULL) {
  if (is.null(raw_url) || length(raw_url) == 0 || is.na(raw_url)) {
    return(NA_character_)
  }

  # Split on comma/newline
  urls <- unlist(strsplit(raw_url, ",|\\n"))
  urls <- trimws(urls)
  urls <- urls[nzchar(urls)]

  if (length(urls) == 0) {
    return(NA_character_)
  }

  # 1) Remove GitHub repo URLs (github.com)
  non_github <- urls[!grepl("github\\.com", urls, ignore.case = TRUE)]

  # Allow fallback later
  github_only <- length(non_github) == 0

  if (!github_only) {
    candidates <- non_github

    # ---- (A) Prefer URLs ending in the package name
    if (!is.null(pkg)) {
      # Normalize trailing slash
      ends_with_pkg <- grepl(paste0("/", pkg, "/?$"), candidates)
      if (any(ends_with_pkg)) {
        return(candidates[which(ends_with_pkg)[1]])
      }
    }

    # ---- (B) Next: prefer pkgdown default host (github.io)
    ghio <- grepl("github\\.io", candidates, ignore.case = TRUE)
    if (any(ghio)) {
      return(candidates[which(ghio)[1]])
    }

    # ---- (C) Otherwise: first non-github URL
    return(candidates[1])
  }

  # 2) If everything is github.com, return first as fallback
  urls[1]
}
