library(pxweb)
library(dplyr)
library(ggplot2)
library(stringi)
library(bslib)
library(plotly)

library(showtext)
font_add_google(name = "Sulphur Point", regular.wt = 300, family = "sans-serif")
showtext_auto()

LEVEL = 0

# Noudetaan data Tilastokeskuksen rajapinnasta pxweb-kirjaston avulla
pxweb_query_list <- list("Kuukausi"=c("*"), "Tiedot"=c("*"), "Hyödyke"=c("*"))
px_data <- pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/hin/khi/kk/statfin_khi_pxt_11xd.px", query = pxweb_query_list)
px.df.orig <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

props <- "PxWeb-paketti: Mans Magnusson, Markus Kainu, Janne Huovari, and Leo Lahti (rOpenGov).  pxweb: R tools for PXWEB API.  URL: http://github.com/ropengov/pxweb"


# Konvertoidaan päivämäärä ja filtteröidään dataa
px.df <- px.df.orig %>%
  mutate(
    Date = as.Date.character(paste(sub("M", "-", Kuukausi), "-01", sep = "")),
    HyodykeTitle = stri_trans_totitle(Hyödyke, type = "sentence"),
    Level = stri_count(Hyödyke, fixed = ".")
  )
px.df.latest <- px.df %>% filter (Date == max(Date) & Hyödyke != "0 KULUTTAJAHINTAINDEKSI")
latest_change <- paste(format(px.df.latest$`Vuosimuutos (%)`, decimal.mark = ","), "%")
latest_date <- px.df.latest$Kuukausi


# Apumuuttujia
cite.infl <- px_data$metadata[[1]]$source
px_data_comments <- pxweb_data_comments(px_data)
px_data_comments_df <- as.data.frame(px_data_comments)

# Luodaan ggplotti
p <- ggplot(px.df %>% filter( Level == LEVEL & Hyödyke != "0 KULUTTAJAHINTAINDEKSI" & !is.na(`Vuosimuutos (%)`)), aes(Date, `Vuosimuutos (%)`)) +
  geom_area(alpha = 0.2, colour = "#4C6085", fill = "#4C6085") +
  geom_point(size=0.7, colour="black", alpha=0.5) + 
  geom_smooth(na.rm = TRUE, se = F, color="#4C6085", span=1.0, alpha = 0.5) +
  facet_wrap(~HyodykeTitle, ncol = 4) +
  ggtitle(label = paste("Inflaatio", latest_date, latest_change) , subtitle = "Inflaatio tavaroiden ja palveluiden pääryhmittäin (COICOP-luokituksen pääryhmät)") +
  labs(
    caption = paste("L\U00E4hde:", cite.infl, "\nVisualisointi: Jan Moilanen", "\n", props)
  ) +
  ylab("Vuosimuutos (%)") +
  xlab("Aika") +
  theme_minimal() +
  theme(
    text = element_text(family = "sans-serif", size = 20, colour = "#4C6085"),
    legend.position = "none",
    plot.subtitle = element_text(face = "italic", size = 20),
    plot.caption = element_text(face = "italic", size = 15),
    axis.title.y = element_text(size = 20, hjust = 1),
    axis.title.x = element_text(size = 20, hjust = 1),
    panel.border = element_blank(),
    panel.grid.major = element_line(colour = "grey70"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white"),
    strip.text = element_text(size = 20, colour = "#32322C"),
    axis.text.x = element_text(
      angle = 45,
      size = 15,
      hjust = 1,
    ),
    axis.text.y = element_text(
      vjust = 0.5,
      size = 15
    )
  ) +
  scale_y_continuous(expand = c(0,0), labels = scales::number_format(accuracy = 1, suffix = " %"), sec.axis = sec_axis(~., "Vuosimuutos (%)", labels = scales::number_format(accuracy = 1, suffix = " %")))
p

# Hover-efx-plotti
#ggplotly(p)


ggsave(
  "Inflaatio tuoteryhmittäin.png",
  width = 2500,
  height = 2000,
  units = "px",
  dpi = 100
)
