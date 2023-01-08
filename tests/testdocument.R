#-------------------------------------------------------------------------------

options(httptest2.verbose = TRUE)

# TODO
# Test different server failures, like 500
# Test what happens if there are no search results
# Wieso kann er nicht mehrere fixtures in einen Ordner schieben? zB alle "Catalogue"
# Return data.frame, not list? => for results

#-------------------------------------------------------------------------------

object <- restatis::running_out_of_terms("forst*", TRUE)

# object <- restatis::modified_data(code = "61111", date = "01.01.2022")
# object <- restatis::catalogue(code = "611*", detailed = TRUE, category = "tables")
# object <- restatis::catalogue(code = "12*", category = "tables", detailed = TRUE)

# Fehlermeldung wenn man keine category wählt oder wenn code = "5*"
# Error in order(y) : unimplemented type 'list' in 'orderVector1'
# Eventuell has_internet checken?
