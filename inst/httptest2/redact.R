set_redactor(function (x) {
  gsub_response(x, "https://www-genesis.destatis.de/genesisWS/rest/2020", "api/")
})
