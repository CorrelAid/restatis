#' access_check: Check if you can access the database(s)
#'
#' @description Function to check if the user can access the wished database(s). Testing for Genesis and Zensus.
#'
#' @param database Character string. Indicator if the Genesis database, Zensus database, or all available databases are checked. Default option is 'all'.
#'
#' @return Character string. Information if the user can access the wished database(s). "Yes" indicates a positive result, "No" indicates a negative result.
#' @export
#'
#' @examples
#' \dontrun{
#' # Check if you can access all databases
#' object <- access_check(database = "all")
#' object <- access_check()
#'
access_check <- function(database = c("all", "genesis", "zensus", "regio")){

  gen_fun <- test_database_function(database)

  #-----------------------------------------------------------------------------

  if ("gen_api" %in% gen_fun){

    par_list <- list(
      endpoint = "helloworld/whoami",
      username = gen_auth_get(database = "genesis")$username,
      password = gen_auth_get(database = "genesis")$password
    )

    results_raw_1 <- do.call("gen_api", par_list)

    results_json_1 <- test_if_json(results_raw_1)

    results_json_1 <- test_if_okay(results_json_1)

  }

  if ("gen_zensus_api" %in% gen_fun){

    par_list <- list(
      endpoint = "helloworld/whoami",
      username = gen_auth_get(database = "zensus")$username,
      password = gen_auth_get(database = "zensus")$password
    )

    results_raw_2 <- do.call("gen_zensus_api", par_list)

    results_json_2 <- test_if_json(results_raw_2)

    results_json_2 <- test_if_okay(results_json_2)

  }

  if ("gen_regio_api" %in% gen_fun){

    par_list <- list(
      endpoint = "helloworld/whoami",
      username = gen_auth_get(database = "regio")$username,
      password = gen_auth_get(database = "regio")$password
    )

    results_raw_3 <- do.call("gen_regio_api", par_list)

    results_json_3 <- test_if_json(results_raw_3)

    results_json_3 <- test_if_okay(results_json_3)

  }

  #-----------------------------------------------------------------------------

  # Return the result
  return(
    paste(
      if("gen_api" %in% gen_fun){ "Genesis:"},
      if("gen_api" %in% gen_fun){ results_json_1},

      if("gen_zensus_api" %in% gen_fun){ "Zensus:"},
      if("gen_zensus_api" %in% gen_fun){ results_json_2},

      if("gen_regio_api" %in% gen_fun){ "Regio:"},
      if("gen_regio_api" %in% gen_fun){ results_json_3}
    )
  )

}
