#' List all repositories which the authenticated user has access to
#'
#' @param token a github personal access token on an env var storing it.
#' If `NULL` (the default) the function tries to fetch the token from `GITHUB_PAT`
#' env var.
#' @param visibility can be one of: private, public, or all
#'
#' @importFrom httr GET content add_headers
#' @importFrom logger log_info
#' @return a data frame of repositories
#' @export
#'
#' @examples
#' \dontrun{list_repositories(visibility = "all")}

list_repositories <- function(token = NULL, visibility = "all") {

  token <- paste0("token ", get_gh_pat(gh_pat = token))

  url <- "https://api.github.com/user/repos"
  status_code <- 200
  response_content <- list("ok")
  response_list <- list()
  page <- 1

  while (length(response_content) > 0) {
    log_info(sprintf("Fetching page %s", page))
    q <- list(visibility = visibility, page = page)
    response <- GET(url, query = q, add_headers(Authorization = token))
    response_content <- content(response)
    status_code <- response$status_code
    response_list[[page]] <- response_content
    page <- page + 1
  }

  log_info("Wrangling response list")
  wrangle_repo_data(gh_response = response_list)

  }

#' List usernames of collaborators for repository
#' @param token a github pat
#' @param owner github username
#' @param repo github repository
#' @param affiliation can be one of: outside, direct, all
#'
#' @importFrom tibble tibble
#'
#' @return collaborators of a repository
#' @export
#'
#' @examples
#' \dontrun{list_collabs(owner = GITHUBREPOOWNER, repo = REPONAME, affiliation = "all")}

list_collabs <-
  function(token = NULL,
           owner,
           repo,
           affiliation) {
    q <- list(affiliation = affiliation)
    token <- paste0("token ", get_gh_pat(token))

    url <-
      sprintf("https://api.github.com/repos/%s/%s/collaborators",
              owner,
              repo)
    api_response <-
      GET(url, query = q, add_headers(Authorization = token))
    api_erros(api_response)
    all_data <- content(api_response)
    tibble("Collaborators" = sapply(all_data,  "[[", 1))
  }
