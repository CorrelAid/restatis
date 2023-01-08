.onLoad <- function(libname, pkgname) {
  gen_api <<- memoise::memoise(gen_api)
}

# (YAB): The list extraction that happens in the following function
# might be eligible for refactoring into a smaller function to avoid
# repetition
# Also, why is "Main4" missing?

forming_evas <- function(list_of){
  list_of$Main <- apply(list_of, 1, function(x){
    evas_list_long_20220724$Titel[evas_list_long_20220724$EVAS == substr(x["Code"], 1, 1)]
  })

  list_of$Main2 <- apply(list_of, 1, function(x){
    evas_list_long_20220724$Titel[evas_list_long_20220724$EVAS == substr(x["Code"], 1, 2)]
  })

  list_of$Main3 <- apply(list_of, 1, function(x){
    evas_list_long_20220724$Titel[evas_list_long_20220724$EVAS == substr(x["Code"], 1, 3)]
  })

  list_of$Main5 <- apply(list_of, 1, function(x){
    evas_list_long_20220724$Titel[evas_list_long_20220724$EVAS == substr(x["Code"], 1, 5)]
  })

  nestedlist <- split(list_of, list_of$Main, drop = TRUE)
  nestedlist <- lapply(nestedlist, function(x){
    split(x, x["Main2"], drop = TRUE)
  })
  nestedlist <- lapply(nestedlist, function(x){
    lapply(x, function(y){
      split(y, y["Main3"])
    })})
  nestedlist <- lapply(nestedlist, function(x){
    lapply(x, function(y){
      lapply(y, function(z){
        split(z, z["Main5"])
      })
    })
  })

  aba <- lapply(
    nestedlist, function(d){
      lapply(d,  function(z){
        lapply(z,  function(y){
          lapply(y,  function(x){
            x[!(
              names(x)
              %in% c("Main", "Main2", "Main3", "Main5"))]})
        })
      })
    })
  return(aba)
}

#-------------------------------------------------------------------------------

check_function_input <- function(code = NULL,
                                 term = NULL,
                                 sortcriterion = NULL,
                                 category = NULL,
                                 detailed = NULL,
                                 type = NULL,
                                 date = NULL,
                                 similarity = NULL)

  #-----------------------------------------------------------------------------

  if (!is.null(code)) {

    if (length(code) != 1L) {

      stop("Parameter 'code' must be a single string.", call. = FALSE)

    }

    if (!is.null(code) & !is.character(code)) {

      stop("Parameter 'code' has to be of type 'character' or NULL.", call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------

  if(!is.null(term)) {

    if (length(term) != 1L) {

      stop("Parameter 'term' must be a single string.", call. = FALSE)

    }

    if (!is.null(term) & !is.character(term)) {

      stop("Parameter 'term' has to be of type 'character' or NULL.", call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------

  if(!is.null(sortcriterion)) {

    if(!is.character(sortcriterion)) {

      stop("Parameter 'sortcriterion' has to be of type 'character'.")

    }

  }

  #-----------------------------------------------------------------------------

  if(!is.null(similarity)) {

    if(!is.logical(similarity)) {

      stop("Parameter 'similarity' has to be of type 'logical'.")

    }

  }












