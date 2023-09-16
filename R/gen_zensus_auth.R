#' Save authentication ZENSUS
#'
#' See Details.
#'
#' Zensus username and password are encrypted and saved as RDS in the
#'   package config directory.
#'
#' A random string is generated and stored in the session environment
#'   variable `ZENSUS_KEY`. This string is used as the key to encrypt and
#'   decrypt the entered Genesis credentials.
#'
#' To avoid having to save authentication in future sessions, `ZENSUS_KEY` can
#'   be added to .Renviron. The usethis package includes a helper function for
#'   editing .Renviron files from an R session with [usethis::edit_r_environ()].
#'
#' @export
gen_zensus_auth_save <- function() {

  username <- gen_auth_ask("username")
  password <- gen_auth_ask("password")

  auth_path <- gen_auth_path("auth_ZENSUS.rds")

  key <- httr2::secret_make_key()

  Sys.setenv(ZENSUS_KEY = key)

  message(
    "Saving credentials to ",
    auth_path,
    "\n\n",
    "Please add the following line to your .Renviron, ",
    "e.g. via `usethis::edit_r_environ()`, ",
    "to use the specified username and password across sessions:\n\n",
    "ZENSUS_KEY=",
    key,
    "\n\n"
  )

  dir.create(gen_auth_path(), showWarnings = FALSE, recursive = TRUE)

  httr2::secret_write_rds(
    list(username = username, password = password),
    path = auth_path,
    key = "ZENSUS_KEY"
  )
}

gen_zensus_auth_get <- function() {

  auth_path <- gen_auth_path("auth_ZENSUS.rds")

  if (!(file.exists(auth_path) && nzchar(Sys.getenv("ZENSUS_KEY")))) {
    stop(
      "Zensus credentials not found.\n",
      "Please run `gen_auth_save()` to store Zensus username and password.\n",
      call. = FALSE
    )
  }

  httr2::secret_read_rds(auth_path, "ZENSUS_KEY")
}
