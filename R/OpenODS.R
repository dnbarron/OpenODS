#' Open ODS
#'
#' Organisational API for the NHS
#'
#' See \url{http://www.openods.co.uk}
#'
#' @param path String: Path for query.
#' @param query String: Query parameter.
#'
#' @import httr
#'
#' @return Object of class \code{openods}, a list with three elements:
#'           \emph{content} The parsed content returned by the API
#'           \emph{path} The path used by \code{GET}
#'           \emph{response} The raw JSON data returned by the API
#' @export
#'
#' @examples
#' ods_api("api/organisations", query = "RADCLIFFE")
ods_api <- function(path, query = NULL){
  requireNamespace("httr")

  url <- modify_url("https://api.openods.co.uk", path = path, query = query)

  resp <- GET(url)

  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)

  if (http_error(resp)) {
    stop(
      sprintf(
        "OpenODS API request failed [%s]\n%s\n<%s>",
        status_code(resp),
        parsed$errorText,
        url
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "openods"
  )
}

#' Print method for openods class
#'
#' @param x An \code{openods} object
#' @param ...
#'
#' @return Nothing
#' @export
#'
#' @examples
#' print(x)
print.openods <- function(x, ...) {
  cat("<OpenODS ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}


#' Retrieve role types
#'
#' @param code String: if \code{code = NULL} (the default), will return a tibble with all available role
#'   codes and their names.  If \code{code} is a valid role type code, will return information about
#'   that role.
#'
#' @return A tibble with two columns, code and name
#' @export
#'
#' @examples
#' ods_roles()
#' ods_roles("RO0101")
ods_roles <- function(code = NULL){

  if (is.null(code)){
    path <- "api/role-types"
  } else path <- paste("api/role-types", code, sep = "/")
  resp <- ods_api(path = path)

  if (length(resp$content) == 1) {
    cont <- resp$content[[1]]
    codes <- purrr::map_chr(cont, 'code')
    name <- purrr::map_chr(cont, 'name')
  } else {
    cont <- resp$content
    codes <- cont$code
    name <- cont$name
  }

  tibble::tibble(codes = codes, name = name)
}
