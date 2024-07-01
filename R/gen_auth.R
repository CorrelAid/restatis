#' gen_auth_save
#'
#' @param database The database to store credentials for ('all', 'zensus', 'genesis', 'regio').
#'
#' @description Save credentials of the different databases
#' @details Username and password are encrypted and saved as RDS in the
#'   package config directory. A random string is generated and stored in the
#'   session environment variable `RESTATIS_KEY`. This string is used as the key
#'   to encrypt and decrypt the entered credentials. To avoid having to save
#'   authentication in future sessions, `RESTATIS_KEY` can be added to .Renviron.
#'   The usethis package includes a helper function for editing .Renviron files
#'   from an R session with [usethis::edit_r_environ()].
#'
#' @export
#'
gen_auth_save <- function(database = c("all", "genesis", "zensus", "regio")) {

  #-----------------------------------------------------------------------------

  if(database == "genesis"){

    username <- gen_auth_ask("username")
    password <- gen_auth_ask("password")

    auth_path <- gen_auth_path("auth.rds")

    key <- httr2::secret_make_key()

    Sys.setenv(RESTATIS_KEY = key)

    message("Saving 'GENESIS' database credentials to ",
            auth_path,
            "\n\n",
            "Please add the following line to your .Renviron, ",
            "e.g. via `usethis::edit_r_environ()`, ",
            "to use the specified username and password across sessions:\n\n",
            "RESTATIS_KEY=",
            key,
            "\n\n")

    dir.create(gen_auth_path(), showWarnings = FALSE, recursive = TRUE)

    httr2::secret_write_rds(list(username = username, password = password),
                            path = auth_path,
                            key = "RESTATIS_KEY")

    # Logincheck
    gen_logincheck(database = "genesis")

  #-----------------------------------------------------------------------------

  } else if (database == "zensus"){

    username <- gen_auth_ask("username")
    password <- gen_auth_ask("password")

    auth_path <- gen_auth_path("auth_zensus.rds")

    key <- httr2::secret_make_key()

    Sys.setenv(ZENSUS_KEY = key)

    message("Saving 'Zensus' database credentials to ",
            auth_path,
            "\n\n",
            "Please add the following line to your .Renviron, ",
            "e.g. via `usethis::edit_r_environ()`, ",
            "to use the specified username and password across sessions:\n\n",
            "ZENSUS_KEY=",
            key,
            "\n\n")

    dir.create(gen_auth_path(), showWarnings = FALSE, recursive = TRUE)

    httr2::secret_write_rds(list(username = username, password = password),
                            path = auth_path,
                            key = "ZENSUS_KEY")

    # Logincheck
    gen_logincheck(database = "zensus")

  #-----------------------------------------------------------------------------

  } else if (database == "regio"){

    username <- gen_auth_ask("username")
    password <- gen_auth_ask("password")

    auth_path <- gen_auth_path("auth_regio.rds")

    key <- httr2::secret_make_key()

    Sys.setenv(REGIO_KEY = key)

    message("Saving 'Regionalstatistik' database credentials to ",
            auth_path,
            "\n\n",
            "Please add the following line to your .Renviron, ",
            "e.g. via `usethis::edit_r_environ()`, ",
            "to use the specified username and password across sessions:\n\n",
            "REGIO_KEY=",
            key,
            "\n\n")

    dir.create(gen_auth_path(), showWarnings = FALSE, recursive = TRUE)

    httr2::secret_write_rds(list(username = username, password = password),
                            path = auth_path,
                            key = "REGIO_KEY")

    # Logincheck
    gen_logincheck(database = "regio")

  #-----------------------------------------------------------------------------

  } else if (database == "all"){

    message("~~ Saving credentials for 'GENESIS' database.")

    username <- gen_auth_ask("username")
    password <- gen_auth_ask("password")

    auth_path <- gen_auth_path("auth.rds")

    key <- httr2::secret_make_key()

    Sys.setenv(RESTATIS_KEY = key)

    message("Saving 'GENESIS' database credentials to ",
            auth_path,
            "\n\n",
            "Please add the following line to your .Renviron, ",
            "e.g. via `usethis::edit_r_environ()`, ",
            "to use the specified username and password across sessions:\n\n",
            "RESTATIS_KEY=",
            key,
            "\n\n")

    dir.create(gen_auth_path(), showWarnings = FALSE, recursive = TRUE)

    httr2::secret_write_rds(list(username = username, password = password),
                            path = auth_path,
                            key = "RESTATIS_KEY")

    # Logincheck
    gen_logincheck(database = "genesis")

    #-------------------------------------------------------------------------

    message("~~ Saving credentials for 'Zensus' database.")

    username <- gen_auth_ask("username")
    password <- gen_auth_ask("password")

    auth_path <- gen_auth_path("auth_zensus.rds")

    key <- httr2::secret_make_key()

    Sys.setenv(ZENSUS_KEY = key)

    message("Saving 'Zensus' database credentials to ",
            auth_path,
            "\n\n",
            "Please add the following line to your .Renviron, ",
            "e.g. via `usethis::edit_r_environ()`, ",
            "to use the specified username and password across sessions:\n\n",
            "ZENSUS_KEY=",
            key,
            "\n\n")

    dir.create(gen_auth_path(), showWarnings = FALSE, recursive = TRUE)

    httr2::secret_write_rds(list(username = username, password = password),
                            path = auth_path,
                            key = "ZENSUS_KEY")

    # Logincheck
    gen_logincheck(database = "zensus")

    #-------------------------------------------------------------------------

    message("~~ Saving credentials for 'Regionalstatistik' database.")

    username <- gen_auth_ask("username")
    password <- gen_auth_ask("password")

    auth_path <- gen_auth_path("auth_regio.rds")

    key <- httr2::secret_make_key()

    Sys.setenv(REGIO_KEY = key)

    message("Saving 'Regionalstatistik' database credentials to ",
            auth_path,
            "\n\n",
            "Please add the following line to your .Renviron, ",
            "e.g. via `usethis::edit_r_environ()`, ",
            "to use the specified username and password across sessions:\n\n",
            "REGIO_KEY=",
            key,
            "\n\n")

    dir.create(gen_auth_path(), showWarnings = FALSE, recursive = TRUE)

    httr2::secret_write_rds(list(username = username, password = password),
                            path = auth_path,
                            key = "REGIO_KEY")

    # Logincheck
    gen_logincheck(database = "regio")

  #-----------------------------------------------------------------------------

  } else {

    stop("Invalid 'database' argument. Please choose 'genesis', 'zensus', 'regio' or 'all'.")

  }

}

#-------------------------------------------------------------------------------

#' gen_auth_get
#'
#' @param database Takes values of 'all', 'genesis', 'zensus' and 'regio'
#'
#' @return Credentials for the database(s) chosen by the user
#' @export
#'
gen_auth_get <- function(database = c("all", "genesis", "zensus", "regio")) {

  if("genesis" %in% database && !("all" %in% database)){

    if(is.null(sys.call(-1))){

      message("Retrieving credentials for 'GENESIS' database.")

    }

    auth_path <- gen_auth_path("auth.rds")

    if (!(file.exists(auth_path) && nzchar(Sys.getenv("RESTATIS_KEY")))) {

      stop(paste0("'GENESIS' database credentials not found. ",
                  "Please run 'gen_auth_save()' to store 'GENESIS' database username and password."),
           call. = FALSE)

    }

    return(httr2::secret_read_rds(auth_path, "RESTATIS_KEY"))

    #---------------------------------------------------------------------------

  } else if ("zensus" %in% database && !("all" %in% database)){

    if(is.null(sys.call(-1))){

      message("Retrieving credentials for 'Zensus' database.")

    }

    auth_path <- gen_auth_path("auth_zensus.rds")

    if (!(file.exists(auth_path) && nzchar(Sys.getenv("ZENSUS_KEY")))) {

      stop(paste0("'Zensus' database credentials not found. ",
                  "Please run 'gen_auth_save()' to store 'Zensus' database username and password."),
           call. = FALSE)

    }

    return(httr2::secret_read_rds(auth_path, "ZENSUS_KEY"))

    #---------------------------------------------------------------------------

  } else if ("regio" %in% database && !("all" %in% database)){

    if(is.null(sys.call(-1))){

      message("Retrieving credentials for 'Regionalstatistik' database.")

    }

    auth_path <- gen_auth_path("auth_regio.rds")

    if (!(file.exists(auth_path) && nzchar(Sys.getenv("REGIO_KEY")))) {

      stop(paste0("'Regionalstatistik' database credentials not found. ",
                  "Please run 'gen_auth_save()' to store 'Regionalstatistik' database username and password."),
           call. = FALSE)

    }

    return(httr2::secret_read_rds(auth_path, "REGIO_KEY"))

    #---------------------------------------------------------------------------

  } else if ("all" %in% database){

    if(is.null(sys.call(-1))){

      message("Retrieving credentials for 'GENESIS' database.")

    }

    auth_path <- gen_auth_path("auth.rds")

    if (!(file.exists(auth_path) && nzchar(Sys.getenv("RESTATIS_KEY")))) {

      stop(paste0("'GENESIS' database credentials not found. ",
                  "Please run 'gen_auth_save()' to store 'GENESIS' database username and password."),
           call. = FALSE)

    }

    return(httr2::secret_read_rds(auth_path, "RESTATIS_KEY"))

    #---------------------------------------------------------------------------

    if(is.null(sys.call(-1))){

      message("Retrieving credentials for 'Zensus' database.")

    }

    auth_path <- gen_auth_path("auth_zensus.rds")

    if (!(file.exists(auth_path) && nzchar(Sys.getenv("ZENSUS_KEY")))) {

      stop(paste0("'Zensus' database credentials not found. ",
                  "Please run 'gen_auth_save()' to store 'Zensus' database username and password."),
           call. = FALSE)

    }

    return(httr2::secret_read_rds(auth_path, "ZENSUS_KEY"))

    #---------------------------------------------------------------------------

    if(is.null(sys.call(-1))){

      message("Retrieving credentials for 'Regionalstatistik' database.")

    }

    auth_path <- gen_auth_path("auth_regio.rds")

    if (!(file.exists(auth_path) && nzchar(Sys.getenv("REGIO_KEY")))) {

      stop(paste0("'Regionalstatistik' database credentials not found. ",
                  "Please run 'gen_auth_save()' to store 'Regionalstatistik' database username and password."),
           call. = FALSE)

    }

    return(httr2::secret_read_rds(auth_path, "REGIO_KEY"))

  } else {

    stop("Invalid database argument. Please choose 'genesis', 'zensus', 'regio' or 'all'.")

  }

}

#-------------------------------------------------------------------------------

#' gen_auth_ask
#'
#' @param credential_type Type of credential to ask for
#'
#' @return The user response
#'
gen_auth_ask <- function(credential_type) {

  val <- askpass::askpass(paste0("Please enter your ", credential_type, ": "))

  if (is.null(val)) {

    stop("Cancelled by user.", call. = FALSE)

  }

  return(val)

}

#-------------------------------------------------------------------------------

#' gen_auth_path
#'
#' @param ... Optional arguments for file.path()
#'
#' @return A file path for the storage of config files
#'
gen_auth_path <- function(...) {

  return(file.path(tools::R_user_dir("restatis", "config"), ...))

}

