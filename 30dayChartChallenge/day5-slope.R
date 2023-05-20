library(zoo)
library(xts)
library(lubridate)
library(readr)
library(dplyr)
library(TSstudio)
library(ggplot2)
library(showtext)

font_add_google(name = "Saira Condensed", regular.wt = 100, family = "sans-serif")

showtext_auto()



df <- read_delim("tyollisyys.csv", delim = ";",
                 escape_double = FALSE,
                 col_types = cols(Kuukausi = col_date(format = "%YM%m")), 
                         trim_ws = TRUE)

df$Kuukausi <- zoo::as.yearmon(df$Kuukausi)


df_xts <- xts::as.xts(df)


plot(df_xts)
ts_plot(df_xts)

ts_decompose(df_xts)

ts_seasonal(df_xts - decompose(df_xts)$trend, type = "box")

ts_surface(df_xts)

ts_lags(df_xts)

acf(df_xts)


# Feature engineering: trend as rising slope, 12 month lag & seasonal component
df <- df |> mutate(month = factor(lubridate::month(Kuukausi, label = TRUE), ordered = FALSE),
                   lag12 = lag(df$`Työllisyysaste, 15-64-vuotiaat, %`, n = 12)
                     )
df$trend <- 1:nrow(df)
df$trend_sqr <- df$trend^2


plot(df)



# Train ----
h = 12
train_df <- df[1:(nrow(df) - h), ]
test_df <- df[(nrow(df) - h + 1):nrow(df), ]

# Forecast-setti
forecast_df <- data.frame(
  Kuukausi = as.yearmon(max(df$Kuukausi) + seq(1, h)/h),
  trend = seq(from = max(df$trend) + 1, length.out = h, by = 1)
)

forecast_df$trend_sqr <- forecast_df$trend^2
forecast_df$month <- factor(lubridate::month(forecast_df$Kuukausi, label = TRUE), ordered = FALSE)
forecast_df$lag12 <- tail(df$`Työllisyysaste, 15-64-vuotiaat, %`, 12)


# Model benchmark

lm_model <- lm(`Työllisyysaste, 15-64-vuotiaat, %`~ month + lag12 + trend + trend_sqr, data = train_df)
summary(lm_model)


test_df$yhat <- predict(lm_model, newdata = test_df)
mape_lm_model <- mean(abs(test_df$`Työllisyysaste, 15-64-vuotiaat, %` - test_df$yhat) / test_df$`Työllisyysaste, 15-64-vuotiaat, %`)
mape_lm_model


forecast_df$pred <- predict(lm_model, forecast_df)


ggplot() +
  geom_line(data = df, aes(x = Kuukausi, y = `Työllisyysaste, 15-64-vuotiaat, %`), colour = "#FFBA08", size = 0.5, alpha = 0.3) +
  geom_point(data = df, aes(x = Kuukausi, y = `Työllisyysaste, 15-64-vuotiaat, %`), colour = "#FFBA08", size = 2, alpha = 0.3) +
  geom_smooth(data = df, aes(x = Kuukausi, y = `Työllisyysaste, 15-64-vuotiaat, %`), colour = "#FFBA08", span = 0.5, se = FALSE, alpha = 0.3) +
  geom_line(data = forecast_df, aes(x = Kuukausi, y = pred), colour = "#DC2F02", size = 0.5) +
  geom_point(data = forecast_df, aes(x = Kuukausi, y = pred), colour = "#DC2F02", shape = 21, stroke = 1, size = 2) +
  labs(
    title = "15-64-vuotiaiden työllisyysaste Suomessa tammikuusta 2010 huhtikuulle 2024*",
    subtitle = "Employment rate (%) in Finland from January 2010 to April 2024*, persons of aged 15-64",
    x = "Aika",
    y = "Työllisyysaste, %",
    caption = "Data: Tilastokeskus, työvoimatutkimus   |   #30DayChartChallenge - Slope - Day 5   |   DataViz & forecast: @janmoilanen_ek",
    tag = "* = Linear regression forecast\nR² = 0,86"
  ) +
  theme_void() +
  theme(
    text = element_text(family = "sans-serif", size = 50, colour = "#FFBA08"),
    axis.text.x = element_text(angle = 50, size = 50, hjust = 1),
    axis.title.x = element_text(size = 60, margin = margin(1, 0, 1, 0, "cm")),
    axis.text.y = element_text(size = 50, hjust = 0, margin = margin(0, 10, 0, 10, "pt")),
    axis.title.y = element_text(angle = 90, size = 60, hjust = 1, margin = margin(0, 1, 0, 0, "cm")),
    # axis.line = element_line(),
    # axis.ticks = element_line(colour = "#6A040F"),
    panel.grid.major = element_line(colour = "#6A040F"),
    panel.grid.minor.y = element_line(colour = "#6A040F"),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#370617"),
    plot.margin = margin(0.5, 1, 0.5, 1, unit = "in"),
    plot.title = element_text(),
    plot.caption = element_text(size = 40),
    plot.subtitle = element_text(size = 40, margin = margin(1, 0, 1, 0, "cm")),
    legend.position = "top",
    legend.text = element_text(size = 60),
    legend.title = element_blank(),
    plot.tag.position = "topright",
    plot.tag = element_text(colour = "#DC2F02", size = 30)
  ) +
  scale_x_continuous(breaks = seq(from = 2010, to = 2027, by = 1))

ggsave("Day5-SlopePlot-4K.png", units = c("px"), width = 3840, height = 2160)
