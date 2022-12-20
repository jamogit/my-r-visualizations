library(httr)
library(jsonlite)


BASEURL = "https://avoindata.eduskunta.fi"
TABLESURL = paste0(BASEURL, "/api/v1/tables/")
ROWCOUNTSURL = paste0(BASEURL, "/api/v1/tables/counts")
# Syntax for rows: https://avoindata.eduskunta.fi/api/v1/tables/{tableName}/rows

# Query tables
res <- GET(TABLESURL)
data <- fromJSON(rawToChar(res$content))


# Query VaskiData

res <- GET(paste0(TABLESURL, "/VaskiData/rows/"))
data <- fromJSON(rawToChar(res$content))



rows <- data[["rowData"]]
