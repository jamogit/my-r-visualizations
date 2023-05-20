library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(gganimate)
library(showtext)
font_add_google(name = "Saira Condensed", regular.wt = 100, family = "sans-serif")
showtext_auto()

# Wrangle ----

gei2013 <- read_excel("gender-equality-index.xlsx", sheet = "2013 (2010 data)", skip = 1)
gei2015 <- read_excel("gender-equality-index.xlsx", sheet = "2015 (2012 data)", skip = 1)
gei2017 <- read_excel("gender-equality-index.xlsx", sheet = "2017 (2015 data)", skip = 1)
gei2019 <- read_excel("gender-equality-index.xlsx", sheet = "2019 (2017 data)", skip = 1)
gei2020 <- read_excel("gender-equality-index.xlsx", sheet = "2020 (2018 data)", skip = 1)
gei2021 <- read_excel("gender-equality-index.xlsx", sheet = "2021 (2019 data)", skip = 1)
gei2022 <- read_excel("gender-equality-index.xlsx", sheet = "2022 (2020 data)", skip = 1)


df <- rbind(gei2013, gei2015, gei2017, gei2019, gei2020, gei2021, gei2022)
# df <- rbind(gei2022)

df <- df |> 
  select(
    Year = `Index year`,
    Country,
    Index = `Gender Equality Index`,
    WORK,
    MONEY,
    KNOWLEDGE,
    TIME,
    POWER,
    HEALTH
  )

df_pivot <- pivot_longer(df, cols = c(WORK, MONEY, KNOWLEDGE, TIME, POWER, HEALTH, Index))

df.countries <- data.frame(
  Countrycode = c("EU","BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR","HR","IT","CY","LV","LT","LU","HU","MT","NL","AT","PL","PT","RO","SI","SK","FI","SE","EU27"),
  Countryname = c("European Union","Belgium","Bulgaria","Czechia","Denmark","Germany","Estonia","Ireland","Greece","Spain","France","Croatia","Italy","Cyprus","Latvia","Lithuania","Luxembourg","Hungary","Malta","Netherlands","Austria","Poland","Portugal","Romania","Slovenia","Slovakia","Finland","Sweden","EU27")
)

plotdata <- inner_join(df_pivot, df.countries, by = c("Country" = "Countrycode"))
plotdata <- plotdata |> filter(name == "Index")
plotdata$Country <- as.factor(plotdata$Country)
plotdata$Countryname <- as.factor(plotdata$Countryname)
plotdata$name <- as.factor(plotdata$name)
plotdata$Year <- as.integer(plotdata$Year)
plotdata2 <- plotdata |>
  filter(Country != "EU27") |>
  arrange(Countryname, desc(value))
plotdata2$id <- seq(1, nrow(plotdata2))

# Get the name and the y position of each label
label_data <- plotdata2
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)


remove(gei2013, gei2015, gei2017, gei2019, gei2020, gei2021, gei2022, df, df_pivot, df.countries, angle, number_of_bar, plotdata)


# Plot ----

a <- ggplot(plotdata2, aes(x = Countryname, y = value, fill = as.factor(Year), group = Year, colour = Countryname)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
  ylim(-20, 100) +
  coord_polar() +
  # geom_text(data = label_data, aes(x = Countryname, y = value + 10, label = Countryname, hjust = hjust), color = "white",
  #           fontface = "bold", alpha=0.6, size=2.5, angle = label_data$angle, inherit.aes = FALSE) +
  # facet_wrap(~Year + forcats::fct_reorder(name, value), ncol = 6, scales = "free")
  # transition_states(Year, state_length = 0, transition_length = 2) +
  # enter_fade() +
  # exit_fade() +
  # ease_aes('quadratic-in-out')
  theme_void() +
  geom_hline(yintercept = 0, colour = "black", linetype = "solid", size = 1, show.legend = FALSE) +
  geom_hline(yintercept = 100, colour = "grey20", linetype = "solid", size = 0.25, show.legend = FALSE) +
  geom_hline(yintercept = 25, colour = "black", linetype = "solid", size = 0.25, show.legend = FALSE) +
  geom_hline(yintercept = 50, colour = "black", linetype = "solid", size = 0.25, show.legend = FALSE) +
  geom_hline(yintercept = 75, colour = "grey20", linetype = "solid", size = 0.25, show.legend = FALSE) +
  labs(
    title = "Gender Equality Index from 2013 to 2022 in EU Countries",
    subtitle = "The Gender Equality Index gives the EU and the Member States a score from 1 to 100. A score of 100 would mean that a country had reached full equality between women and men.",
    x = "Countries and years",
    y = "Index",
    caption = "Source: https://eige.europa.eu/gender-equality-index | #30DayChartChallenge - UNWomen - Day 24 | DataViz: @janmoilanen_ek | Creds: r-graph-gallery.com/297-circular-barplot-with-groups.html",
    tag = "Years\n2013, 2015, 2017, 2019, 2020, 2021, 2022",
    # tag = "Powered by R"
  ) +
  theme(
    text = element_text(family = "sans-serif", size = 50, colour = "white"),
    axis.text.x = element_text(size = 30),
    axis.title.x = element_text(size = 40,  vjust = 2.5),
    # axis.text.y = element_text(size = 30, hjust = 0, margin = margin(0, 10, 0, 10, "pt")),
    # panel.grid.major.y = element_line(colour = "grey20"),
    # axis.text.y = element_blank(),
    panel.grid.major.y = element_line(colour = "grey20"),
    panel.grid.major.x = element_line(colour = "grey20"),
    # axis.ticks = element_line(),
    # axis.title.y = element_text(angle = 90, size = 60, hjust = 1, margin = margin(0, 1, 0, 0, "cm")),
    # panel.border = element_blank(),
    plot.background = element_rect(fill = "#001219", colour = "#001219"),
    plot.title = element_text(),
    plot.subtitle = element_text(size = 30),
    plot.caption = element_text(size = 25, hjust = 0),
    plot.tag = element_text(size = 30),
    plot.tag.position = "right"
  ) +
  annotate("text", x = rep(last(plotdata2$Countryname),5), y = c(0, 25, 50, 75, 100), label = c("0", "25", "50", "75", "100"),
           colour = "white", size = 8 , angle = 0, hjust = 0) +
  # geom_text(data = label_data,
  #           aes(x = Countryname, y = round(value), label = round(value), hjust = hjust),
  #           color="white",
  #           size = 8,
  #           family = "sans-serif",
  #           angle = label_data$angle,
  #           inherit.aes = FALSE
  # ) +
  # geom_text(data=plotdata2, aes(x = id, y = -18, label= Countryname), colour = "white", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE) +
  scale_fill_ordinal()
a

ggsave("Day-24-UNWomen-4K.png", units = c("px"), width = 3840, height = 2160)
