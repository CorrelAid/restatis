.onLoad <- function(libname, pkgname) {
  gen_api <<- memoise::memoise(gen_api)
}

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

  nestedlist <- split(list_of, list_of$Main, drop = T)
  nestedlist <- lapply(nestedlist, function(x){
    split(x, x["Main2"], drop = T)
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

