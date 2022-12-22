#' API Errors
#'
#' @importFrom httr stop_for_status
#'
#' @param api_response the response to our request

api_erros <- function(api_response) {
  tryCatch(
    stop_for_status(api_response),
    http_404 = function(c) "URL Not Found",
    http_403 = function(c) "Access Forbidden.",
    http_400 = function(c) "Bad Request",
    http_500 = function(c) "Internal Server Error"
  )
}

#' Try to get a token from env var
#' @export
token_from_env_var <- function() {
  pat <- Sys.getenv("GITHUB_PAT")

  if (pat == "") {
    log_error("Could not find env var GITHUB_PAT for GitHub PAT.")
    abort("Could not find GitHub PAT.")
  }

  log_info("Using GitHub PAT from env var GITHUB_PAT.")
  invisible(pat)
}

#' Validate the token
#' @param pat GitHub personal access token
#' @export
validate_gh_pat <- function(pat) {
  response <- GET(
    url = "https://api.github.com/user",
    add_headers(Authorization = paste("token", pat))
  )
  if (isFALSE(response$status_code == 200)) {
    log_error("The token does not appear to be valid or does not have permissions for the request.")
    abort("The token does not appear to be valid or does not have permissions for the request.")
  }

  TRUE
}

#' Get github pat
#'
#' @importFrom logger log_error log_info
#' @importFrom rlang abort
#'
#' @param gh_pat character, a github token
#' default: GITHUB_PAT
#'
#' @export

get_gh_pat <- function(gh_pat = NULL) {

  if (is.null(gh_pat)) {
    gh_pat <- token_from_env_var()
  }

  validate_gh_pat(gh_pat)
  invisible(gh_pat)
}


#' Get github login
#'
#' @inheritParams get_gh_pat
#'
#' @export

get_gh_login <- function(gh_pat = NULL) {

  if (is.null(gh_pat)) {
    gh_pat <- token_from_env_var()
  }

  validate_gh_pat(gh_pat)
  response <- GET(
    url = "https://api.github.com/user",
    add_headers(Authorization = paste("token", gh_pat))
    )

  content(response)$login
}

#' Wrangle repository data
#' Helper function to return a tidy data frame of
#' repository data.
#'
#' @param gh_response a response from github
#'
#' @importFrom tibble enframe
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr bind_rows select mutate across everything
#' @importFrom rlang .data

wrangle_repo_data <- function(gh_response) {
  gh_response |>
    unlist(recursive = FALSE) |>
    lapply(function(x) {
      enframe(x) |>
        pivot_wider(names_from = "name", values_from = "value")
    }) |>
    bind_rows() |>
    select(.data$name, .data$owner, .data$private, .data$html_url) |>
    mutate(owner = sapply(.data$owner, "[[", 1)) |>
    mutate(across(everything(), unlist))
}
