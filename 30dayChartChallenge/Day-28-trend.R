library(finnishgrid)
library(ggplot2)
library(xts)
library(zoo)
library(dplyr)
library(TSstudio)
library(h2o)
library(lubridate)
library(showtext)
font_add_google(name = "Saira Condensed", regular.wt = 100, family = "sans-serif")
showtext_auto()


user_key = Sys.getenv("FINGRID_OPENDATA_API_KEY")
start_time = "2017-01-01T00:00:00+0200"
end_time = "2023-04-27T00:00:00+0200"

tmp <- get_data(api_number = 74,  # energy production = 74. 124 = consumption
                start_time = start_time, 
                end_time = end_time, 
                user_key = user_key)


tmp$Date <- as.Date(tmp$start_time)
tmp$End <- as.Date(tmp$end_time)

df <- tmp |> 
  select(
    Date,
    value
  ) |> 
  filter(
    Date >= "2017-01-01" &
      Date < "2023-04-27"
  ) |> 
  group_by(Date) |> 
  summarise(Value = sum(value)) |> 
  arrange(Date)

df.xts <- timetk::tk_xts(df)

# Lasketaan 30 päivän liukuva keskiarvo
df_xts_ma <- rollapply(df.xts, width = 90, FUN=mean, align = "center")
df_xts_ma2 <- rollapply(df.xts, width = 356, FUN=mean, align = "right")

# Yhdistetään datat
df_merged <- xts::merge.xts(df.xts, df_xts_ma, df_xts_ma2)


df <- zoo::fortify.zoo(df_merged)

df <- df |> mutate(
  Day = factor(wday(df$Index, label = TRUE), ordered = FALSE ),
  Month = factor(month(df$Index, label = TRUE), ordered = FALSE),
  lag365 = lag(df$Value, n = 365)
) |> 
  filter(
    !is.na(lag365)
  )

# Trainset
h <- 365
train_df <- df[1:(nrow(df) - h), ]
test_df <- df[(nrow(df) - h + 1):nrow(df), ]

forecast_df <- data.frame(
  Index = seq.Date(from = max(df$Index) + 1, length.out = h, by = "day")
)
forecast_df$Day <- factor(wday(forecast_df$Index, label = TRUE), ordered = FALSE)
forecast_df$Month <- factor(month(forecast_df$Index, label = TRUE), ordered = FALSE)
forecast_df$lag365 <- tail(df$Value, 365)


# Model ----
h2o.init(max_mem_size = "3G")

train_h <- as.h2o(train_df)
test_h <- as.h2o(test_df)
forecast_h <- as.h2o(forecast_df)

X <- c("Index", "Day", "Month", "lag365")
y <- "Value"


automl <- h2o.automl(training_frame = train_h, x = X, y = y, nfolds = 5, max_runtime_secs = 60*20, seed = 12)

test_h$pred <- h2o.predict(automl@leader, test_h)
test_1 <- as.data.frame(test_h)

mape_automl <- mean(abs(test_1$Value - test_1$pred) / test_1$Value)
mape_automl

ggplot(test_1) +
  geom_line(aes(x = Index, y = Value)) +
  geom_line(aes(x = Index, y = pred))

forecast_h$pred <- h2o.predict(automl@leader, forecast_h)

final_forecast <- as.data.frame(forecast_h)



df_temp1 <- df |>
  transmute(
    Date = Index,
    Value,
    `90 days moving average` = Value.1,
    `365 days moving average` = Value.2,
    Group = 'Actual'
  )
df_temp2 <- final_forecast |>
  transmute(
    Date = as.Date(Index),
    Value = pred,
    `90 days moving average` = NA,
    `365 days moving average` = NA,
    Group = 'Predicted'
  )

df_final <- union_all(df_temp1, df_temp2)



ggplot(df_final, aes(x = Date, y = Value, group = Group, colour = Group)) +
  geom_point(aes(x = Date, y = Value), size = 0.5) +
  geom_line(aes(x = Date, y = df_final$`90 days moving average`), size = 2, colour = "slateblue", alpha = 0.2) +
  geom_line(aes(x = Date, y = `365 days moving average`), size = 2, colour = "hotpink", show.legend = F) +
  # geom_line(size = 1.0, data = df, aes(y = `Value.2`), colour = "sienna1", show.legend = T) +
  theme_minimal() +
  labs(
    title = "Electricity production in Finland from 2018-01-01 to 2023-04-26",
    subtitle = "90 and 365 days' moving averages and machine learning (H2O.automl) estimates for the next 365 days",
    x = "Date",
    y = "Production, MWh/day",
    caption = "Source: Fingrid via {finnishgrid}   |   #30DayChartChallenge - Trend - Day 28 | Credits: Rami Crispin's book for time-series analysis  |  DataViz: @janmoilanen_ek"
  ) +
  theme(
    text = element_text(family = "sans-serif", size = 40, colour = "steelblue4"),
    axis.text.x = element_text(size = 40, colour = "steelblue4"),
    axis.title.x = element_text(size = 40,  vjust = 1),
    axis.title.y = element_text(angle = 90, size = 30, vjust = 1),
    axis.text.y = element_text(size = 40, hjust = 0, margin = margin(0, 10, 0, 10, "pt"), colour = "steelblue4"),
    panel.grid = element_line(colour = "grey90"),
    axis.ticks = element_line(),
    plot.background = element_rect(fill = "white", colour = "white"),
    plot.title = element_text(size = 60),
    plot.margin = margin(1, 1, 1, 1, "in"),
    plot.subtitle = element_text(),
    legend.title = element_blank()
  ) +
  scale_x_date()

ggsave("Day28-Trend-4K.png", units = c("px"), width = 3840, height = 2160)
