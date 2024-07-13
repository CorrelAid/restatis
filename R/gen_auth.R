#' gen_auth_save
#'
#' @description Save credentials of the different databases for further convenient use
#'
#' @param database Character string. The database to store credentials for ('all', 'genesis', 'zensus' or 'regio').
#'
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
#' @examples
#' \dontrun{
#' gen_auth_save("zensus")
#' }
#'
#'
gen_auth_save <- function(database = c("all", "genesis", "zensus", "regio")) {

  if (missing(database)) stop("You have to specify a value for parameter 'database'.",
                              call. = FALSE)

  #-----------------------------------------------------------------------------

  if (database == "genesis"){

    username <- gen_auth_ask("username")
    password <- gen_auth_ask("password")

    auth_path <- gen_auth_path("auth.rds")

    key <- httr2::secret_make_key()

    Sys.setenv(RESTATIS_KEY = key)

    message("Saving GENESIS database credentials to ",
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

    want_token_resp <- askpass::askpass("Do you want to specifiy a Zensus 2022 API token for API access? Type 'yes'.\n
                                        If you want to specify username and password, type any character.")
    want_token <- ifelse(trimws(tolower(want_token_resp)) == "yes", TRUE, FALSE)

    if (isTRUE(want_token)) {

      username <- gen_auth_ask("API token")
      password <- NULL

    } else {

      username <- gen_auth_ask("username")
      password <- gen_auth_ask("password")

    }

    auth_path <- gen_auth_path("auth_zensus.rds")

    key <- httr2::secret_make_key()

    Sys.setenv(ZENSUS_KEY = key)

    message("Saving Zensus 2022 database credentials to ",
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

    message("Saving regionalstatistik.de database credentials to ",
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

    message("~~ Saving credentials for GENESIS database.")

    username <- gen_auth_ask("username")
    password <- gen_auth_ask("password")

    auth_path <- gen_auth_path("auth.rds")

    key <- httr2::secret_make_key()

    Sys.setenv(RESTATIS_KEY = key)

    message("Saving GENESIS database credentials to ",
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

    message("~~ Saving credentials for Zensus 2022 database.")

    username <- gen_auth_ask("username")
    password <- gen_auth_ask("password")

    auth_path <- gen_auth_path("auth_zensus.rds")

    key <- httr2::secret_make_key()

    Sys.setenv(ZENSUS_KEY = key)

    message("Saving Zensus 2022 database credentials to ",
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

    message("~~ Saving credentials for regionalstatistik.de database.")

    username <- gen_auth_ask("username")
    password <- gen_auth_ask("password")

    auth_path <- gen_auth_path("auth_regio.rds")

    key <- httr2::secret_make_key()

    Sys.setenv(REGIO_KEY = key)

    message("Saving regionalstatistik.de database credentials to ",
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

    stop("Invalid 'database' argument. Please choose 'all', 'genesis', 'zensus' or 'regio'.",
         call. = FALSE)

  }

}

#-------------------------------------------------------------------------------

#' gen_auth_get
#'
#' @description Function to retrieve the credentials stored via \code{gen_auth_save()}
#'
#' @param database Character string. The database to get the credentials for ('all', 'genesis', 'zensus' and 'regio').
#'
#' @return Credentials for the database(s) chosen by the user
#' @export
#'
#' @examples
#' \dontrun{
#' gen_auth_get("all")
#' }
#'
#'
gen_auth_get <- function(database = c("all", "genesis", "zensus", "regio")) {

  #-----------------------------------------------------------------------------

  if (length(database) == 1) {

    if (!(database %in% c("all", "genesis", "zensus", "regio"))) {

      stop("Misspecification of parameter 'database': Must only be 'all', 'genesis', 'zensus' or 'regio'.",
           call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------

  if (length(database) > 1) {

    if ("all" %in% database) {

      stop("If you want to specify 'all', do not specify further databases (i.e., just set database to 'all').",
           call. = FALSE)

    }

    #---------------------------------------------------------------------------

    if (!all(database %in% c("genesis", "zensus", "regio"))) {

      stop("Misspecification of parameter 'database': Must only be 'genesis', 'zensus' or 'regio'.",
           call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------

  if (length(database) == 1) {

    #---------------------------------------------------------------------------

    if (database == "genesis") {

      auth_path <- gen_auth_path("auth.rds")

      if (!(file.exists(auth_path) && nzchar(Sys.getenv("RESTATIS_KEY")))) {

        stop(paste0("GENESIS database credentials not found. ",
                    "Please run 'gen_auth_save()' to store GENESIS database username and password."),
             call. = FALSE)

      }

      return(httr2::secret_read_rds(auth_path, "RESTATIS_KEY"))

    #---------------------------------------------------------------------------

    } else if (database == "zensus") {

      auth_path <- gen_auth_path("auth_zensus.rds")

      if (!(file.exists(auth_path) && nzchar(Sys.getenv("ZENSUS_KEY")))) {

        stop(paste0("Zensus 2022 database credentials not found. ",
                    "Please run 'gen_auth_save()' to store Zensus 2022 database username and password."),
             call. = FALSE)

    }

    return(httr2::secret_read_rds(auth_path, "ZENSUS_KEY"))

  #-------------------------------------------------------------------------------

  } else if (database == "regio") {

      auth_path <- gen_auth_path("auth_regio.rds")

      if (!(file.exists(auth_path) && nzchar(Sys.getenv("REGIO_KEY")))) {

        stop(paste0("regionalstatistik.de database credentials not found. ",
                    "Please run 'gen_auth_save()' to store regionalstatistik.de database username and password."),
             call. = FALSE)

      }

      return(httr2::secret_read_rds(auth_path, "REGIO_KEY"))

  #-----------------------------------------------------------------------------

  } else if (database == "all") {

      auth_path <- gen_auth_path("auth.rds")

      if (!(file.exists(auth_path) && nzchar(Sys.getenv("RESTATIS_KEY")))) {

        stop(paste0("GENESIS database credentials not found. ",
                    "Please run 'gen_auth_save()' to store GENESIS database username and password."),
             call. = FALSE)

      }

      message("Credentials for database GENESIS:\n")
      print(httr2::secret_read_rds(auth_path, "RESTATIS_KEY"))

      #---------------------------------------------------------------------------

      auth_path <- gen_auth_path("auth_zensus.rds")

      if (!(file.exists(auth_path) && nzchar(Sys.getenv("ZENSUS_KEY")))) {

        stop(paste0("Zensus 2022 database credentials not found. ",
                    "Please run 'gen_auth_save()' to store Zensus 2022 database username and password."),
             call. = FALSE)

      }

      message("Credentials for database Zensus 2022:\n")
      print(httr2::secret_read_rds(auth_path, "ZENSUS_KEY"))

      #---------------------------------------------------------------------------

      auth_path <- gen_auth_path("auth_regio.rds")

      if (!(file.exists(auth_path) && nzchar(Sys.getenv("REGIO_KEY")))) {

        stop(paste0("regionalstatistik.de database credentials not found. ",
                    "Please run 'gen_auth_save()' to store regionalstatistik.de database username and password."),
             call. = FALSE)

      }

      message("Credentials for database regionalstatistik.de:\n")
      print(httr2::secret_read_rds(auth_path, "REGIO_KEY"))

    } # End of 'else if (database == "all")'

  } # End of 'if (length(database) == 1) '

  #-----------------------------------------------------------------------------

  if (length(database) > 1) {

    if ("genesis" %in% database) {

      auth_path <- gen_auth_path("auth.rds")

      if (!(file.exists(auth_path) && nzchar(Sys.getenv("RESTATIS_KEY")))) {

        stop(paste0("GENESIS database credentials not found. ",
                    "Please run 'gen_auth_save()' to store GENESIS database username and password."),
             call. = FALSE)

      }

      message("Credentials for database GENESIS:\n")
      print(httr2::secret_read_rds(auth_path, "RESTATIS_KEY"))

    }

    #---------------------------------------------------------------------------

    if ("zensus" %in% database) {

      auth_path <- gen_auth_path("auth_zensus.rds")

      if (!(file.exists(auth_path) && nzchar(Sys.getenv("ZENSUS_KEY")))) {

        stop(paste0("Zensus 2022 database credentials not found. ",
                    "Please run 'gen_auth_save()' to store Zensus 2022 database username and password."),
             call. = FALSE)

      }

      message("Credentials for database Zensus 2022:\n")
      print(httr2::secret_read_rds(auth_path, "ZENSUS_KEY"))

    }

    #---------------------------------------------------------------------------

    if ("regio" %in% database) {

      auth_path <- gen_auth_path("auth_regio.rds")

      if (!(file.exists(auth_path) && nzchar(Sys.getenv("REGIO_KEY")))) {

        stop(paste0("regionalstatistik.de database credentials not found. ",
                    "Please run 'gen_auth_save()' to store regionalstatistik.de database username and password."),
             call. = FALSE)

      }

      message("Credentials for database regionalstatistik.de:\n")
      print(httr2::secret_read_rds(auth_path, "REGIO_KEY"))

    }

    #---------------------------------------------------------------------------

  } # End of '(length(database) > 1)'

}

#-------------------------------------------------------------------------------

#' gen_auth_ask
#'
#' @param credential_type Character string. Type of credential to ask for
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

