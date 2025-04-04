% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_qualitysigns.R
\name{gen_signs}
\alias{gen_signs}
\title{gen_signs}
\usage{
gen_signs(
  database = c("all", "genesis", "zensus", "regio"),
  error.ignore = FALSE,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{database}{Character string. Indicator if the GENESIS ('genesis'), Zensus 2022 ('zensus') or regionalstatistik.de ('regio') database is called. Default option is 'all'.}

\item{error.ignore}{Boolean. Indicator if the function should stop if an error occurs or no object for the request is found or if it should produce a token as response. Default option is 'FALSE'. Important note: This does not apply to errors generated by the API call itself (especially server or client errors with status codes 5xx or 4xx). In this case, the function call will error even if error.ignore is set to TRUE.}

\item{verbose}{Boolean. In case of success, should a message be printed? Defaults to 'TRUE'.}

\item{...}{Additional parameters for the API call. These parameters are only affecting the call itself, no further processing. For more details see \code{vignette("additional_parameter")}.}
}
\value{
A list of all current used special signs.
}
\description{
Function to list all currently used special signs (e.g., 0, *, X, (), p, ...) and their meaning in GENESIS, Zensus 2022 and/or regionalstatistik.de.
}
