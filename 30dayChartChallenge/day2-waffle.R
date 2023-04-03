library(dplyr)
library(hrbrthemes)
library(ggplot2)
library(waffle)
library(showtext)

font_add_google(name = "Big Shoulders Inline Text", regular.wt = 300, family = "sans-serif")
showtext_auto()

df_2011 <- read.csv2("Kansanedustajat-2011.txt", sep = "\t", encoding = "UTF-8")
df_2011$Vuosi <- 2011
df_2011 <- df_2011 |>  filter( !(Nimi %in% c("Urpalainen Anu", "Donner Jörn",
                                             "Palola Mikael", "Raatikainen Mika",
                                             "Wallin Harry", "Hänninen Katja",
                                             "Lepomäki Elina", "Peltokorpi Terhi",
                                             "Sumuvuori Johanna") ))
df_2015 <- read.csv2("Kansanedustajat-2015.txt", sep = "\t", encoding = "UTF-8")
df_2015$Vuosi <- 2015

df_2019 <- read.csv2("Kansanedustajat-2019.txt", sep = "\t", header = T, encoding = "UTF-8")
df_2019$Vuosi <- 2019

df1 <- df_2019 |> count(Vuosi, Puolue) |> transmute(Vuosi, Puolue, n) |> arrange(n)
df2 <- df_2015 |> count(Vuosi, `Puolue`) |> transmute(Vuosi, Puolue, n) |> arrange(n)
df3 <- df_2011 |> count(Vuosi, `Ryhmä`) |> transmute(Vuosi, Puolue = `Ryhmä`, n) |> arrange(n)
df4 <- data.frame(
  Vuosi = 2023,
  Puolue = c("Kokoomus", "Perussuomalaiset", "Sosiaalidemokraatit", "Keskusta", "Vihreät", "Vasemmistoliitto", "Muut"),
  n = c(48, 46, 43, 23, 13, 11, 16)
)


df <- rbind(df1, df2, df3, df4)


# Fix labels ----
df$Puolue <- ifelse(df$Puolue == "kok", "Kokoomus", df$Puolue)
df$Puolue <- ifelse(df$Puolue == "kesk", "Keskusta", df$Puolue)
df$Puolue <- ifelse(df$Puolue == "vihr", "Vihreät", df$Puolue)
df$Puolue <- ifelse(df$Puolue == "vas", "Vasemmistoliitto", df$Puolue)
df$Puolue <- ifelse(df$Puolue == "sd", "Sosiaalidemokraatit", df$Puolue)
df$Puolue <- ifelse(df$Puolue == "r", "RKP", df$Puolue)
df$Puolue <- ifelse(df$Puolue == "ps", "Perussuomalaiset", df$Puolue)
df$Puolue <- ifelse(df$Puolue == "kd", "Kristillisdemokraatit", df$Puolue)

df$Puolue <- ifelse(df$Puolue %in% c("Kokoomus", "Keskusta", "Sosiaalidemokraatit",
                                     "Perussuomalaiset", "Vihreät", "Vasemmistoliitto"),
                    df$Puolue, "Muut")
df <- df |> arrange(Puolue, desc(n))


cols <- c("Kokoomus" = "#003859", "Keskusta" = "#3bad2e", "Sosiaalidemokraatit"= "tomato",
          "Perussuomalaiset" = "#ffd500", "Vihreät" = "#006845", "Vasemmistoliitto" = "#f00a64",
          "Muut" = "violet")


ggplot(df, aes(x = Vuosi, fill = Puolue, values = n, colour = factor(Puolue))) +
  geom_waffle(color = "grey90", size = .25, n_rows = 10, flip = TRUE) +
  facet_wrap(~Vuosi, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 8, # make this multiplyer the same as n_rows
  expand = c(0,0)) +
  scale_fill_manual(values = cols) +
  coord_equal() +
  labs(
    title = "Eduskuntaryhmien voimasuhteet vaalivuosina 2011-2023*",
    subtitle = "Distribution of Members of Parliament in Finland by election year",
    x = "Vaalivuosi",
    y = "Lukumäärä",
    caption = "* 2023 alustava laskenta\nTietolähde: Wikipedia, Yle   |   #30DayChartChallenge - Waffle - Day 2   |   DataViz: @janmoilanen_ek    |    Credits: @hrbrmstr"
  ) +
  theme_void() +
  # ggthemes::theme_economist_white() +
  theme(
    text = element_text(family = "sans-serif", size = 40, colour = "black"),
    # axis.line = element_line(colour = "#FFF3B0"),
    # axis.title = element_text(size = 12),
    # axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 0),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks = element_line(colour = "#FFF3B0"),
    panel.grid = element_line(colour = "grey80"),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "grey90"),
    plot.margin = margin(1, 1, 0, 1, unit = "cm"),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(reverse = TRUE))


ggsave("EduskuntaWafflePlot-4K.png", units = c("px"), width = 3840, height = 2160)



