library(pxweb)
library(dplyr)
library(ggplot2)
library(stringi)
library(bslib)

library(showtext)
font_add_google(name = "IBM Plex Sans", regular.wt = 300, family = "sans-serif")
showtext_auto()

# Noudetaan data Tilastokeskuksen rajapinnasta pxweb-kirjaston avulla
pxweb_query_list <- list("Kuukausi"=c("*"), "Tiedot"=c("*"), "Hyödyke"=c("*"))
px_data <- pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/hin/khi/kk/statfin_khi_pxt_11xd.px", query = pxweb_query_list)
px.df.orig <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Konvertoidaan päivämäärä ja filtteröidään dataa
px.df <- px.df.orig %>%
  mutate(
    Date = as.Date.character(paste(sub("M", "-", Kuukausi), "-01", sep = "")),
    HyodykeTitle = stri_trans_totitle(Hyödyke, type = "sentence"),
    Level = stri_count(Hyödyke, fixed = ".")
  ) %>%
  filter(
    Hyödyke != "0 KULUTTAJAHINTAINDEKSI" & !is.na(`Vuosimuutos (%)`)
  )

# Apumuuttujia
cite.infl <- px_data$metadata[[1]]$source

# Luodaan ggplotti
p <- ggplot(px.df %>% filter( Level == 0), aes(Date, `Vuosimuutos (%)`)) +
  geom_area(alpha = 0.5) +
  geom_point(size=1, colour="black", alpha=0.5) + 
  geom_smooth(na.rm = TRUE, color="black", span=1.0) +
  facet_wrap(~HyodykeTitle, ncol = 4) +
  ggtitle(label = "Inflaatiotrendi tuotteittain", subtitle = "Kuluttajahintaindeksi (2010=100)") +
  labs(
    caption = paste("L\U00E4hde:", cite.infl, "\nVisualisointi: Jan Moilanen")
  ) +
  ylab("Vuosimuutos (%)") +
  xlab("Aika") +
  theme_minimal() +
  theme(
    text = element_text(family = "sans-serif", size = 16),
    legend.position = "none",
    plot.subtitle = element_text(face = "italic", size = 10),
    plot.caption = element_text(face = "italic", size = 10),
    axis.title.y = element_text(size = 10, hjust = 1),
    #axis.title.y.right = element_blank(),
    axis.title.x = element_text(size = 10, hjust = 1),
    panel.grid.major = element_line(colour = "grey70"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 10),
    axis.text.x = element_text(
      angle = 45,
      size = 8,
      hjust = 1,
    ),
    axis.text.y = element_text(
      vjust = 0.5,
      size = 8
    )
  ) +
  scale_y_continuous(expand = c(0,0), labels = scales::number_format(accuracy = 1, suffix = " %"), sec.axis = sec_axis(~., "Vuosimuutos (%)", labels = scales::number_format(accuracy = 1, suffix = " %")))
p
