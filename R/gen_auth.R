#' Save database credentials
#'
#' @description Save credentials of the different databases for further convenient use
#'
#' @param database Character string. The database to store credentials for ('all', 'genesis', 'zensus' or 'regio').
#' @param use_token Boolean. Do you want to (if possible) set an API token instead of password + username? Note: This is not supported by regionalstatistik.de. Defaults to FALSE.
#'
#' @details Username and password are encrypted and saved as RDS in the
#'   package config directory. A random string is generated and stored in the
#'   session environment variable `GENESIS_KEY`. This string is used as the key
#'   to encrypt and decrypt the entered credentials. To avoid having to save
#'   authentication in future sessions, `GENESIS_KEY` can be added to .Renviron.
#'   The `{usethis}` package includes a helper function for editing .Renviron files
#'   from an R session with [usethis::edit_r_environ()].
#'
#' @return Path to the RDS file in which credentials are saved, invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' gen_auth_save("zensus")
#' }
#'
gen_auth_save <- function(database,
                          use_token = FALSE) {

  if (missing(database)) stop("You have to specify a value for parameter 'database'.",
                              call. = FALSE)

  if (database == "regio" & isTRUE(use_token)) {

    warning("regionalstatistik.de does not support API tokens. Defaulting to username and password.",
            call. = FALSE)

  }

  if (database == "bayern" & isTRUE(use_token)) {

    warning("statistikdaten.bayern.de does not support API tokens. Defaulting to username and password.",
            call. = FALSE)

  }

  #-----------------------------------------------------------------------------

  if (database %in% c("regio", "bayern")) {

    insert_and_save_credentials(database, use_token)

    gen_logincheck(database = database)

  #-----------------------------------------------------------------------------

  } else if (database %in% c("zensus", "genesis")) {

    insert_and_save_credentials(database, use_token)

    gen_logincheck(database = database)

  } else if (database == "all"){

    #---------------------------------------------------------------------------

    walk_arguments <- list(ui_menu_database = c("GENESIS", "Zensus 2022", "regionalstatistik.de"),
                           db_names = c("genesis", "zensus", "regio"),
                           use_token = use_token)

    purrr::pwalk(.l = walk_arguments,
                 .f = function(ui_menu_database,
                               db_names,
                               use_token) {

                   message("~~ Saving credentials for the ", ui_menu_database, " database.")

                   insert_and_save_credentials(database = db_names,
                                               use_token = use_token)

                   gen_logincheck(database = db_names)

                 })

  #-----------------------------------------------------------------------------

  } else {

    stop("Invalid 'database' argument. Please choose 'all', 'genesis', 'zensus' or 'regio'.",
         call. = FALSE)

  }

}

#-------------------------------------------------------------------------------

#' Retrieve database credentials
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
gen_auth_get <- function(database = NULL) {

  if(is.null(database)){

    database <- "all"

  }

  #-----------------------------------------------------------------------------

  if (length(database) == 1) {

    if (!(database %in% c("all", "genesis", "zensus", "regio", "bayern"))) {

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

    if (!all(database %in% c("genesis", "zensus", "regio", "bayern"))) {

      stop("Misspecification of parameter 'database': Must only be 'genesis', 'zensus' or 'regio'.",
           call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------

  if (length(database) == 1) {

    #---------------------------------------------------------------------------

    if (database == "genesis") {

      auth_path <- gen_auth_path("auth_genesis.rds")

      if (!(file.exists(auth_path) && nzchar(Sys.getenv("GENESIS_KEY")))) {

        stop(paste0("GENESIS database credentials not found. ",
                    "Please run 'gen_auth_save()' to store GENESIS database username and password."),
             call. = FALSE)

      }

      return(httr2::secret_read_rds(auth_path, "GENESIS_KEY"))

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

  } else if(database == "bayern") {

    auth_path <- gen_auth_path("auth_bayern.rds")

    if (!(file.exists(auth_path) && nzchar(Sys.getenv("BAYERN_KEY")))) {

      stop(paste0("statistikdaten.bayern.de database credentials not found. ",
                  "Please run 'gen_auth_save()' to store statistikdaten.bayern.de database username and password."),
           call. = FALSE)

    }

    return(httr2::secret_read_rds(auth_path, "BAYERN_KEY"))

  } else if (database == "all") {

      auth_path <- gen_auth_path("auth_genesis.rds")

      if (!(file.exists(auth_path) && nzchar(Sys.getenv("GENESIS_KEY")))) {

        warning(paste0("GENESIS database credentials not found. ",
                       "Please run 'gen_auth_save()' to store GENESIS database username and password."),
                call. = FALSE)

      } else {

        message("Credentials for database GENESIS:\n")
        print(httr2::secret_read_rds(auth_path, "GENESIS_KEY"))

      }

      #---------------------------------------------------------------------------

      auth_path <- gen_auth_path("auth_zensus.rds")

      if (!(file.exists(auth_path) && nzchar(Sys.getenv("ZENSUS_KEY")))) {

        warning(paste0("Zensus 2022 database credentials not found. ",
                    "Please run 'gen_auth_save()' to store Zensus 2022 database username and password."),
                call. = FALSE)

      } else {

        message("Credentials for database Zensus 2022:\n")
        print(httr2::secret_read_rds(auth_path, "ZENSUS_KEY"))

      }

      #---------------------------------------------------------------------------

      auth_path <- gen_auth_path("auth_regio.rds")

      if (!(file.exists(auth_path) && nzchar(Sys.getenv("REGIO_KEY")))) {

        warning(paste0("regionalstatistik.de database credentials not found. ",
                    "Please run 'gen_auth_save()' to store regionalstatistik.de database username and password."),
                call. = FALSE)

      } else {

        message("Credentials for database regionalstatistik.de:\n")
        print(httr2::secret_read_rds(auth_path, "REGIO_KEY"))

      }

      #---------------------------------------------------------------------------

      auth_path <- gen_auth_path("auth_bayern.rds")

      if (!(file.exists(auth_path) && nzchar(Sys.getenv("BAYERN_KEY")))) {

        warning(paste0("statistikdaten.bayern.de database credentials not found. ",
                       "Please run 'gen_auth_save()' to store statistikdaten.bayern.de database username and password."),
                call. = FALSE)

      } else {

        message("Credentials for database statistikdaten.bayern.de:\n")
        print(httr2::secret_read_rds(auth_path, "BAYERN_KEY"))

      }

    } # End of 'else if (database == "all")'

  } # End of 'if (length(database) == 1) '

  #-----------------------------------------------------------------------------

  if (length(database) > 1) {

    if ("genesis" %in% database) {

      auth_path <- gen_auth_path("auth_genesis.rds")

      if (!(file.exists(auth_path) && nzchar(Sys.getenv("GENESIS_KEY")))) {

        stop(paste0("GENESIS database credentials not found. ",
                    "Please run 'gen_auth_save()' to store GENESIS database username and password."),
             call. = FALSE)

      }

      message("Credentials for database GENESIS:\n")
      print(httr2::secret_read_rds(auth_path, "GENESIS_KEY"))

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

    if ("bayern" %in% database) {

      auth_path <- gen_auth_path("auth_bayern.rds")

      if (!(file.exists(auth_path) && nzchar(Sys.getenv("BAYERN_KEY")))) {

        stop(paste0("statistikdaten.bayern.de database credentials not found. ",
                    "Please run 'gen_auth_save()' to store statistikdaten.bayern.de database username and password."),
             call. = FALSE)

      }

      message("Credentials for database statistikdaten.bayern.de:\n")
      print(httr2::secret_read_rds(auth_path, "BAYERN_KEY"))

    }

    #---------------------------------------------------------------------------

  } # End of '(length(database) > 1)'

}

#-------------------------------------------------------------------------------

#' Helper function for authentication purposes
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

#' Helper function for authentication purposes
#'
#' @param ... Optional arguments for file.path()
#'
#' @return A file path for the storage of config files
#'
gen_auth_path <- function(...) {

  return(file.path(tools::R_user_dir("restatis", "config"), ...))

}
