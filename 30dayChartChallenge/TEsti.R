library(tidyverse)
library(janitor)
library(showtext)
library(MetBrewer)
library(scico)
library(ggtext)
library(patchwork)
library(gghighlight)
library(stringr)

df1 <- read.csv("https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/dataConsumerConfidence.csv") %>% 
  mutate(date=lubridate::my(Time)) %>% 
  select(-Time) %>% 
  pivot_longer(!date, names_to = "country", values_to = "value") %>% 
  na.omit()

#### MISC ####
font <- "Gudea"
font_add_google(family=font, font, db_cache = TRUE)
fa_path <- systemfonts::font_info(family = "Font Awesome 6 Brands")[["path"]]
font_add(family = "fa-brands", regular = fa_path)
theme_set(theme_minimal(base_family = font, base_size = 10))
bg <- "#F4F5F1"
txt_col <- "black"
showtext_auto(enable = TRUE)

caption_text  <- str_glue("**Design:** Gilbert Fontana<br>","**Data:** OECD, 2022")


p1 <- df1 %>% 
  ggplot() +
  geom_hline(yintercept = 100,linetype="solid", size=.25) +
  # geom_point(data=df1 %>% 
  #              group_by(country) %>% 
  #              slice_max(date),
  #            aes(x=date, y=value, color=country),shape=16) +
  geom_line(aes(x=date, y=value, color=country)) +
  gghighlight(use_direct_label = FALSE,
              unhighlighted_params = list(colour = alpha("grey85", 1))) +
  # geom_text(data=df1 %>%
  #             group_by(country) %>%
  #             slice_max(date),
  #           aes(x=date, y=value, color=country, label = round(value)),
  #           hjust = -.5, vjust = .5, size=2.5, family=font, fontface="bold") +
  scale_color_met_d(name="Redon") +
  scale_x_date(date_labels = "%y") +
  scale_y_continuous(breaks = c(90,95,100,105,110),
                     labels = c("","","100","","")
  ) +
  #facet_wrap(~ country) +
  facet_wrap(~  factor(country, levels=c('USA','China','Japan','Germany', 'UK','France', 'Italy', 'South Korea', 'Australia'))) +
  coord_cartesian(clip = "off") +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(color=txt_col, size=7),
    strip.text.x = element_text(face="bold"),
    plot.title = element_markdown(hjust=.5,size=34, color=txt_col,lineheight=.8, face="bold", margin=margin(20,0,30,0)),
    plot.subtitle = element_markdown(hjust=.5,size=18, color=txt_col,lineheight = 1, margin=margin(10,0,30,0)),
    plot.caption = element_markdown(hjust=.5, margin=margin(60,0,0,0), size=8, color=txt_col, lineheight = 1.2),
    plot.caption.position = "plot",
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(10,10,10,10),
    legend.position = "none",
    legend.title = element_text(face="bold")
  )

p1

