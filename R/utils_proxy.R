#-------------------------------------------------------------------------------
# Util functions related to the use of proxies
#-------------------------------------------------------------------------------

#' Pipe wrapper for httr2::req_proxy
#'
#' @param req
#' @param proxy
#'
#' @returns

req_proxy_if_set <- function(req,
                             proxy = getOption("restatis.proxy")) {

  if (is.null(proxy)) return(req)

  httr2::req_proxy(req = req,
                   url = proxy$url,
                   port = proxy$port,
                   username = proxy$username,
                   password = proxy$password,
                   auth = proxy$auth)

}

#-------------------------------------------------------------------------------

#' Set Global Proxy Settings For 'restatis'
#'
#' @param url
#' @param port
#' @param username
#' @param password
#' @param auth
#'
#' @returns
#' @export
#'
#' @examples
gen_set_proxy <- function(url,
                          port = NULL,
                          username = NULL,
                          password = NULL,
                          auth = "basic") {

  options(restatis.proxy = list(url = url,
                                port = port,
                                username = username,
                                password = password,
                                auth = auth))

}

#-------------------------------------------------------------------------------

# User does in the Console or in R-Profile:
# gen_set_proxy("localhost", 8888)
# getOption() # => Check settings
