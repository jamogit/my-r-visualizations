
# font_add_google(name = "Sulphlibrary(pxweb)
library(dplyr)
library(ggplot2)
library(ggtext)
library(stringi)
library(bslib)
library(plotly)
library(showtext)

font_add_google(name = "Bebas Neue", regular.wt = 400, family = "sans-serif")
showtext_auto()


LEVEL = 0
LANG = "en"

# Noudetaan data Tilastokeskuksen rajapinnasta pxweb-kirjaston avulla
pxweb_query_list <- list("Kuukausi"=c("*"), "Tiedot"=c("*"), "Hyödyke"=c("*"))
px_data <- pxweb_get(url = paste0("https://statfin.stat.fi:443/PxWeb/api/v1/", LANG,"/StatFin/khi/statfin_khi_pxt_11xd.px"), query = pxweb_query_list)
px.df.orig <- as.data.frame(px_data, column.name.type = "code", variable.value.type = "text")

props <- "PxWeb: Mans Magnusson, Markus Kainu, Janne Huovari, and Leo Lahti (rOpenGov).  pxweb: R tools for PXWEB API.  URL: http://github.com/ropengov/pxweb"


# Konvertoidaan päivämäärä ja filtteröidään dataa
px.df <- px.df.orig %>%
  mutate(
    Date = as.Date.character(paste(sub("M", "-", Kuukausi), "-01", sep = "")),
    HyodykeTitle = stri_trans_totitle(Hyödyke, type = "sentence"),
    Level = stri_count(Hyödyke, fixed = ".")
  )
px.df.latest <- px.df %>% filter (Date == max(Date) & if(LANG == "fi") {Hyödyke != "0 KULUTTAJAHINTAINDEKSI"}
                                  else {Hyödyke != "0 CONSUMER PRICE INDEX"})
latest_change <- paste(format(px.df.latest$vuosimuutos, decimal.mark = ","), "%")
latest_date <- px.df.latest$Date


# Apumuuttujia
cite.infl <- px_data$metadata[[1]]$source
px_data_comments <- pxweb_data_comments(px_data)
px_data_comments_df <- as.data.frame(px_data_comments)


# Label Helper
katkoLabel <- function(label) {
  l = stri_length(label)
  if (l > 20) {
    a = stri_sub(label, 1, 30)
    b = katkoLabel(stri_sub(label, 31, l))
    return(stri_c(stri_replace_last_fixed(a, ' ', '\n '), b))
  }
  else
    return(label)
}

plotdata <- px.df %>% filter( Level == LEVEL & if(LANG == "fi") {Hyödyke != "0 KULUTTAJAHINTAINDEKSI"}
                              else {Hyödyke != "0 CONSUMER PRICE INDEX"} & !is.na(vuosimuutos)) |>
  mutate(
    Geom_area_colour = ifelse(vuosimuutos < 0, "red", "white")
  )


# Luodaan ggplotti
p <- ggplot(plotdata, aes(Date, vuosimuutos)) +
  geom_smooth(na.rm = TRUE, se = F, color="#FCD900", span=0.5, alpha = 0.5, size = 1.0) +
  geom_area(fill = "#FCD900", alpha = 0.5) +
  facet_wrap(~HyodykeTitle, ncol = 4, scales = "free") +
  labs(
    title = "<span style='color:red'>Inflation</span> in Finland by <span style='color:#ff8c00'>Commodity</span>",
    subtitle = paste("Consumer Price Index (CPI) by Commodity, annual change in prices from 2010 onwards","\n",
                     "Latest total annual change:", latest_change, "(", format(latest_date, "%B %Y") , ")" ),
    caption = paste("Source:", cite.infl, "\nVisualisation: Jan Moilanen", "\n", props)
  ) +
  ylab("Annual change (%)") +
  xlab("Time") +
  theme_minimal() +
  theme(
    text = element_text(family = "sans-serif", size = 40, colour = "white"),
    legend.position = "none",
    plot.title = element_markdown(),
    plot.subtitle = element_text(face = "italic", size = 30),
    plot.caption = element_text(face = "italic", size = 20),
    axis.title.y = element_text(size = 30, hjust = 0.0),
    axis.title.x = element_text(size = 30, hjust = 0.0),
    axis.line.x = element_line(colour = "#DFF6FF"),
    panel.border = element_blank(),
    panel.grid.major = element_line(colour = "grey30"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "#06283D"),
    strip.text = element_text(size = 29, colour = "#ff8c00", hjust = 0.0),
    axis.text.x = element_text(
      angle = 45,
      size = 20,
      hjust = 1,
      colour = "#DFF6FF"
    ),
    axis.text.y = element_text(
      vjust = 0.5,
      size = 20,
      colour = "#DFF6FF"
    )
  ) +
  scale_y_continuous(
                     labels = scales::number_format(accuracy = 1, suffix = " %")
                     )
p


ggsave(
  "Inflaatio tuoteryhmittäin.png",
  width = 3570,
  height = 2000,
  units = "px",
  dpi = 100
)

