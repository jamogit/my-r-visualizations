library(dplyr)
library(ggplot2)
library(tidyr)
library(learningtower)
library(showtext)
library(plyr)

font_add_google(name = "Saira Condensed", regular.wt = 100, family = "sans-serif")
showtext_auto()

df <- load_student(2018)
df_fin <- filter(df, country == "FIN")
df_fin_male <- filter(df_fin, gender == "male")
df_fin_female <- filter(df_fin, gender == "female")
remove(df)



hist(df_fin_male$math)
hist(df_fin_female$math)

plot(df_fin$read)
plot(df_fin$science)

df <- df_fin |> transmute(
  Student = student_id,
  Score = (math + read + science) / 3,
  Gender = as.factor(df_fin$gender)
)


means <- ddply(df, "Gender", summarise, MeanScore=mean(Score))



ggplot(df, aes(x=Score, fill = Gender, colour = Gender, alpha = 0.5)) +
  geom_histogram(aes(y = ..density..), binwidth = 3.5, alpha = 0.1, position = "identity", bins = 300) +
  geom_density(aes(colour = `Gender`), alpha = 0.3, show.legend = F ) +
  geom_vline(data=means, aes(xintercept = MeanScore, colour = Gender), linetype="dashed", show.legend = F) +
  theme_void() +
  labs(
    title = "Students' distribution of PISA 2018 Overall Scores in Finland by Gender",
    subtitle = "Histograms and densities of students' average Scores over Math, Reading and Science by gender in 2018. Females manage to score higher.
Dashed line shows the group mean score.",
    x = "Score",
    y = "Density",
    caption = "Data: OECD via {learningtower}   |   #30DayChartChallenge - Humans - Day 8   |   DataViz: @janmoilanen_ek",
  ) +
  theme(
    text = element_text(family = "sans-serif", size = 50, colour = "#E9D8A6"),
    axis.text.x = element_text(angle = 50, size = 50, hjust = 1),
    axis.title.x = element_text(size = 60, margin = margin(1, 0, 1, 0, "cm")),
    axis.text.y = element_text(size = 30, hjust = 0, margin = margin(0, 10, 0, 10, "pt")),
    axis.title.y = element_text(angle = 90, size = 60, hjust = 1, margin = margin(0, 1, 0, 0, "cm")),
    # axis.line = element_line(),
    # axis.ticks = element_line(colour = "#6A040F"),
    # panel.grid.minor = element_line(colour = "#E9D8A6"),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#001219"),
    plot.margin = margin(0.5, 1, 0.5, 1, unit = "in"),
    plot.title = element_text(),
    plot.caption = element_text(size = 40),
    plot.subtitle = element_text(size = 40, margin = margin(1, 0, 1, 0, "cm")),
    legend.position = "top",
    legend.text = element_text(size = 60),
    legend.title = element_blank()
  ) +
  scale_colour_manual(values = c("#9B2226", "#005F73")) +
  scale_fill_manual(values = c("#9B2226", "#005F73")) +
  scale_x_continuous(breaks = seq(from = round(min(df$Score), 0), to = round(max(df$Score), 0), by = 50))

ggsave("Day8-HumansPlot-4K.png", units = c("px"), width = 3840, height = 2160)
