set_redactor(function(request) {

  request_string <- request$url

  first_part <- paste0(strsplit(request_string, split = "\\?")[[1]][1], "?")
  second_part <- strsplit(request_string, split = "\\?")[[1]][2]
  split <- strsplit(second_part, split = "&") %>% unlist
  credentials <- grepl(split, grepl, pattern = "password|username")
  string <- split[!credentials]
  final_string <- paste0(first_part,
                         paste0("username=ABCDEF&password=1234abcd&username=ABCDEF&password=1234abcd"),
                         paste0(string, collapse = "&"))
  final_string <- gsub("https://www-genesis.destatis.de/genesisWS/rest/2020", "/api", final_string)

  request$url <- final_string

  return(request)

})
