library(ggplot2)
library(dplyr)
library(geofi)
library(pxweb)
library(showtext)
font_add_google(name = "Saira Condensed", regular.wt = 100, family = "sans-serif")
showtext_auto()



library(plotly)
spinrates <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/spinrates.csv",
                      stringsAsFactors = FALSE)

p <- ggplot(spinrates, aes(x=velocity, y=spinrate)) +
  geom_tile(aes(fill = swing_miss)) +
  scale_fill_distiller(palette = "YlGnBu") +
  labs(title = "Likelihood of swinging and missing on a fastball",
       y = "spin rate (rpm)")

ggplotly(p)




df <- get_municipality_pop()


# PXWEB query 
pxweb_query_list <- 
  list("Alue"=c("*"),
       "Tiedot"=c("*"),
       "Vuosi"=c("2021"))

# Download data 
px_data <- 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/fi/Postinumeroalueittainen_avoin_tieto/uusin/paavo_pxt_12f8.px",
            query = pxweb_query_list)

# Convert to data.frame 
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")


px_data_frame$OnkoKunta <- stringr::str_starts(px_data_frame$Alue, "MK")

df2 <- px_data_frame |> 
  filter(
    OnkoKunta == TRUE
  )


plotdata <- df2 |> 
  select(
    `Alue`,
    `Talouksien mediaanitulot (TR)`,
    `Asumisväljyys (TE)`,
    `Ylemmän korkeakoulututkinnon suorittaneet (KO)`
  )



p <- ggplot(df2, aes(x = `Asumisväljyys (TE)`,
                     y = Alue)) +
  geom_tile(aes(fill = `Talouksien mediaanitulot (TR)`)) +
  scale_fill_distiller(palette = "YlGnBu") +
  labs(title = "Likelihood of swinging and missing on a fastball")

p

ggplotly(p)
