library(httr)
# url <- "https://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/naa/tp"
url <- "https://pxdata.stat.fi:443/PxWeb/api/v1/fi/StatFin/vtp/statfin_vtp_pxt_11sf.px"
body <- list(query = list("Regions", "*", "ContentsCode", "B1GM", "Year", c(2010:2020)))
json_query = '{
  "query": [
    {
      "code": "Taloustoimi",
      "selection": {
        "filter": "item",
        "values": [
          "B1GMH"
        ]
      }
    },
    {
      "code": "Vuosi",
      "selection": {
        "filter": "item",
        "values": [
          "2022"
        ]
      }
    },
    {
      "code": "Tiedot",
      "selection": {
        "filter": "item",
        "values": [
          "cp"
        ]
      }
    }
  ],
  "response": {
    "format": "json"
  }
}'


response <- POST(url, body = body, encode = "json")
data <- content(response, "parsed")



library(httr)
url <- "https://pxdata.stat.fi:443/PxWeb/api/v1/fi/StatFin/vtp/statfin_vtp_pxt_11sf.px"
body <- list(query = list("TALN100", "KOK", "1", "2010", "2020"))
response <- POST(url, body = body, encode = "json")
data <- content(response, "parsed")
library(jsonlite)
library(dplyr)

# Muunna JSON-muotoinen data.frame-objektiksi
df <- fromJSON(toJSON(data$dataset))

# Parsi tarvittavat sarakkeet
df_parsed <- df %>%
  select("Tilasto", "Vuosi", "Arvo") %>%
  filter(Tilasto == "Tuotos, markkinahintaan")

# Tulosta ensimmäiset 10 riviä
head(df_parsed, 10)
