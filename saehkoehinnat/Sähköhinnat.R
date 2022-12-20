library(readxl)
library(TSstudio)
library(xts)
library(zoo)
library(tidyverse)
library(lubridate)
library(timetk)
library(finnishgrid)
library(ggplot2)
library(plotly)
library(gganimate)


# Helped function to get yearly data. Splits the temporal request in half
getYearlyData <- function(year) {
  
  #year = 2021
  user_key = Sys.getenv("FINNISHGRID_KEY")  #  https://data.fingrid.fi/open-data-forms/registration/  
  
  firstHalf_start = paste0(year, "-01-01T00:00:00+0200")
  firstHalf_end = paste0(year, "-06-30T23:59:59+0200")
  secondHalf_start = paste0(year, "-07-01T00:00:00+0200")
  secondHalf_end = paste0(year, "-12-31T23:59:59+0200")
  
  
  elec_cons <- electricity_consumption_FI_RTD(start_time = firstHalf_start, 
                                              end_time = firstHalf_end, 
                                              user_key = user_key)  
  firstHalf_cons <- elec_cons |>
    dplyr::group_by(Pvm = as_date(start_time)) |>
    dplyr::summarise(Consumption = sum(value)) |>
    filter( year(Pvm) == year )
  
  elec_cons <- electricity_consumption_FI_RTD(start_time = secondHalf_start, 
                                              end_time = secondHalf_end, 
                                              user_key = user_key)
  secondHalf_cons <- elec_cons |>
    dplyr::group_by(Pvm = as.Date(start_time)) |>
    dplyr::summarise(Consumption = sum(value)) |>
    filter( year(Pvm) == year )
  
  consumption <- rbind(firstHalf_cons, secondHalf_cons)
  
  cons_xts <- timetk::tk_xts(consumption) # Muunnos aikasarjaksi

  
  # Production
  elec_prod <- electricity_production_FI_RTD(start_time = firstHalf_start, 
                                             end_time = firstHalf_end, 
                                             user_key = user_key)
  
  firstHalf_prod <- elec_prod |>
    dplyr::group_by(Pvm = as_date(start_time)) |>
    dplyr::summarise(Production = sum(value)) |>
    filter( year(Pvm) == year )
  
  
  elec_prod <- electricity_production_FI_RTD(start_time = secondHalf_start, 
                                             end_time = secondHalf_end, 
                                             user_key = user_key)
  
  secondHalf_prod <- elec_prod |>
    dplyr::group_by(Pvm = as_date(start_time)) |>
    dplyr::summarise(Production = sum(value)) |>
    filter( year(Pvm) == year )
  
  
  production <- rbind(firstHalf_prod, secondHalf_prod) |>
    arrange(Pvm)
  
  prod_xts <- timetk::tk_xts(production) # Muunnos aikasarjaksi
  
  
  
  xts_merged <- xts::merge.xts(cons_xts, prod_xts)
  
  return(xts_merged)
}


sahko2011 <- getYearlyData(2011)
sahko2012 <- getYearlyData(2012)
sahko2013 <- getYearlyData(2013)
sahko2014 <- getYearlyData(2014)
sahko2015 <- getYearlyData(2015)
sahko2016 <- getYearlyData(2016)
sahko2017 <- getYearlyData(2017)
sahko2018 <- getYearlyData(2018)
sahko2019 <- getYearlyData(2019)
sahko2020 <- getYearlyData(2020)
sahko2021 <- getYearlyData(2021)
sahko2022 <- getYearlyData(2022)

all <- rbind(sahko2011,
             sahko2012,
             sahko2013,
             sahko2014,
             sahko2015,
             sahko2016,
             sahko2017,
             sahko2018,
             sahko2019,
             sahko2020,
             sahko2021,
             sahko2022)

# Persist
write.zoo(all, "my-r-visualizations/sähköhinnat/sahko.csv")


# Lasketaan kolmen kuukauden liukuva keskiarvo
rolling3M <- rollapply(all, width = 90, FUN = mean, align = "center")

merged <- merge.xts(all, rolling3M)

p <- ts_plot(merged,
             title = "Title",
             type = "multiple")
p
df <- fortify(merged)



# Palette: https://coolors.co/palette/ff9f1c-479653-8aa1b1-0d122f
colourConsumption = "#FF9F1C"
colourProduction = "#479653"
colourBackground = "#0D122F"
colourText = "#8AA1B1"

plotData <- df #|> filter(year(df$Index) > 2017)



p <- ggplot(plotData) +
  geom_line(aes(x = Index, y = Consumption), colour = colourConsumption, show.legend = T) +
  geom_line(aes(x = Index, y = Consumption.1), size = 1.5, alpha = 0.5, colour = colourConsumption) +
  geom_line(aes(x = Index, y = Production), colour = colourProduction) +
  geom_line(aes(x = Index, y = Production.1), size = 1.5, alpha = 0.5, colour = colourProduction) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = colourBackground),
    panel.background = element_rect(fill = colourBackground),
    axis.text = element_text(colour = colourText),
    panel.grid.major= element_line(colour = colourText),
    text = element_text(colour = colourText),
    axis.line = element_blank()
  ) +
  xlab("Aika") +
  ylab("Megawattia") +
  labs(
    title = "Sähkönkulutus ja tuotanto Suomessa",
    subtitle = paste("Aika:", "{frame_along}"),
    # subtitle = "Päiväkohtainen kulutus ja tuotanto sekä 90 päivän keskiarvo vuodesta 2011 alkaen, megawattia",
    caption = "Lähde: Fingrid"
  ) +
  transition_reveal(Index)
  
p
