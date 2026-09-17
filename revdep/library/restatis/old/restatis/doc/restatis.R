## ----eval=FALSE---------------------------------------------------------------
#  library(restatis)

## ----eval=FALSE---------------------------------------------------------------
#  restatis::gen_alternative_terms(term = "*busfahrer*", database = "genesis")
#  

## ----eval=FALSE---------------------------------------------------------------
#  gen_alternative_terms(term = "*bus*", database = "genesis")
#  

## ----eval=FALSE---------------------------------------------------------------
#  search_results <- gen_find(term = "busfahrer",
#                             detailed = FALSE,
#                             ordering = TRUE,
#                             category = "all",
#                             database = "genesis")
#  
#  search_results
#  

## ----eval=FALSE---------------------------------------------------------------
#  gen_metadata(code = search_results$Cubes$Code[1],
#               category = search_results$Cubes$Object_Type[1],
#               database = "genesis")
#  

## ----eval=FALSE---------------------------------------------------------------
#  gen_cube(search_results$Cubes$Code[1], database = "genesis")
#  

## ----eval=FALSE---------------------------------------------------------------
#  gen_modified_data(code = "62361", date = "week_before", database = "genesis")
#  

