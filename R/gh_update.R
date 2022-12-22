#' Updating Description of some repository
#' @param token a github pat
#' @param user github username
#' @param repo github repository
#' @param desc the updated description
#' @importFrom httr POST
#'
#' @export
#'
#' @examples
#' \dontrun{update_desc(user = GITHUBUSERNAME, repo = REPONAME, desc = "new description")}

update_desc <-
  function(token = NULL, user, repo, desc) {
    token <- paste0("token ", get_gh_pat(token))

    url <- sprintf("https://api.github.com/repos/%s/%s", user, repo)

    q <- toJSON(list(description = desc),
                          auto_unbox = TRUE)

    api_response <-
      POST(url, body = q, add_headers(Authorization = token))

    api_erros(api_response)
  }

#' Adding collaborators to a repository
#'
#' @param token a github pat
#' @param owner owner of the repository
#' @param repo name of the repository
#' @param user user being added to repository
#' @param permission can be one of: pull, triage, push, maintain, admin
#'
#' @importFrom jsonlite toJSON
#' @importFrom httr PUT
#'
#' @export
#'
#' @examples
#' \dontrun{add_collabs(owner = GITHUBREPOOWNER, repo = REPONAME,
#' user = GITHUBUSERNAME, permission = "admin")}

add_collabs <-
  function(token = NULL,
           owner,
           repo,
           user,
           permission) {
    token <- paste0("token ", get_gh_pat(token))

    url <-
      sprintf("https://api.github.com/repos/%s/%s/collaborators/%s",
              owner,
              repo,
              user)

    q <-
      toJSON(list(permission = permission),  auto_unbox = TRUE)

    api_response <-
      PUT(url, body = q, add_headers(Authorization = token))

    api_erros(api_response)

  }
