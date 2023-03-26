#' Save authentication
#'
#' See Details.
#'
#' Destatis username and password are encrypted and saved as RDS in the
#'   package config directory.
#'
#' A random string is generated and stored in the session environment
#'   variable `RESTATIS_KEY`. This string is used as the key to encrypt and
#'   decrypt the entered Destatis credentials.
#'
#' To avoid having to save authentication in future sessions, `RESTATIS_KEY` can
#'   be added to .Renviron. The usethis package includes a helper function for
#'   editing .Renviron files from an R session with [usethis::edit_r_environ()].
#'
#' @export
gen_auth_save <- function() {
  username <- gen_auth_ask("username")
  password <- gen_auth_ask("password")

  auth_path <- gen_auth_path("auth.rds")

  key <- httr2::secret_make_key()

  Sys.setenv(RESTATIS_KEY = key)

  message(
    "Saving credentials to ",
    auth_path,
    "\n\n",
    "Please add the following line to your .Renviron, ",
    "e.g. via `usethis::edit_r_environ()`, ",
    "to use the specified username and password across sessions:\n\n",
    "RESTATIS_KEY=",
    key,
    "\n\n"
  )

  dir.create(gen_auth_path(), showWarnings = FALSE, recursive = TRUE)

  httr2::secret_write_rds(
    list(username = username, password = password),
    path = auth_path,
    key = "RESTATIS_KEY"
  )
}

gen_auth_get <- function() {
  auth_path <- gen_auth_path("auth.rds")

  if (!(file.exists(auth_path) && nzchar(Sys.getenv("RESTATIS_KEY")))) {
    stop(
      "Destatis credentials not found.\n",
      "Please run `gen_auth_save()` to store Destatis username and password.\n",
      call. = FALSE
    )
  }

  httr2::secret_read_rds(auth_path, "RESTATIS_KEY")
}

gen_auth_ask <- function(credential_type) {
  val <- askpass::askpass(paste0("Please enter your ", credential_type, ": "))

  if (is.null(val)) {
    stop("Cancelled by user", call. = FALSE)
  }

  val
}

gen_auth_path <- function(...) {
  file.path(tools::R_user_dir("restatis", "config"), ...)
}
