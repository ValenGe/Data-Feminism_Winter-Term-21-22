# Inspired by
# Stats with Matt (2019) ggplot2 meets W. E. B. Du Bois [Online] https://www.statswithmatt.com/post/ggplot2-meets-w-e-b-du-bois/. Last accessed 13.03.22

library(tidyverse)
library(readxl)
afro_b <- read_excel("afrobarometer_living conditions.xlsx")

afro_b_longer <- afro_b %>%
  rename(category = Category,
         ethn = `Race of respondent`,
         R7 = "R7\r") %>%
  pivot_longer(cols = starts_with("R"), names_to = "rnd_name", values_to = "perc") %>%
  filter(category != "Don't know") %>%
  mutate(pct = as.numeric(perc)*100,
         ethn = factor(ethn, levels = c("Black/African", "White/European")),
         category = ifelse(category == "Very Bad", "Very bad", category),
         category = factor(category, levels = c("NA", "Very good", "Fairly good", "Neither good nor bad", "Fairly bad", "Very bad")),
         year = NA,
         year = ifelse(rnd_name == "R2", 2004, year),
         year = ifelse(rnd_name == "R3", 2005, year),
         year = ifelse(rnd_name == "R4", 2008, year),
         year = ifelse(rnd_name == "R5", 2013, year),
         year = ifelse(rnd_name == "R6", 2016, year),
         year = ifelse(rnd_name == "R7", 2019, year))

library(ggplot2)
library(extrafont)

font_name <- "Bahnschrift"

# theme setting fully adapted from Stats with Matt (2019)

theme_du_bois <- function() {
  theme_gray(base_family = font_name) %+replace%
    theme(
      plot.background = element_rect(
        fill = "antiquewhite2",
        color = "antiquewhite2"
      ),
      panel.background = element_rect(
        fill = "antiquewhite2",
        color = "antiquewhite2"
      ),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      ),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

afro_b_ts_svg <- afro_b_longer %>%
  filter(category != "NA") %>%
  mutate(ethn_2 = NA,
         ethn_2 = ifelse(ethn == "Black/African", "BLACK/AFRICAN.", ethn_2),
         ethn_2 = ifelse(ethn == "White/European", "WHITE/EUROPEAN.", ethn_2)) %>%
  ggplot(aes(x = year, y = pct, fill = category)) +
  geom_area(stat = "identity", 
            position = "fill") +
  theme_du_bois() +
  scale_fill_manual(
    values = rep(
      c("tan", "cornsilk2", "royalblue", "gold", "firebrick3"),
      times = 2
    )) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(2004, 2007, 2010, 2013, 2016, 2019)) +
  labs(y = "Percentages",
       x = "Year") +
  theme(
    plot.title = element_text(size = 14, 
                              family = font_name),
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(family = font_name),
    legend.position = "bottom",
    legend.key = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(vjust = -1,
                              size = 12.5,
                              family = font_name)
  ) +
  facet_wrap(~ethn_2)
afro_b_ts_svg

ggsave("afro_b_ts.svg",
       device = "svg",
       plot = afro_b_ts_svg,
)

afro_b_longer_sub <- afro_b_longer %>%
  filter(rnd_name == "R7")

pct_labels <- paste0(afro_b_longer_sub$pct, "%")
pct_labels[which(pct_labels == "50%")] <- ""
pct_labels[which(pct_labels == "0.5%")] <- ".5%"

afro_b_pre <- ggplot(
  data = afro_b_longer_sub,
  mapping = aes(
    x = 1,
    y = pct,
    fill = ethn:category
  )
) +
  geom_bar(
    width = 1,
    stat = "identity"
  ) +
  coord_polar(
    theta = "y",
    start = 1.3 * pi / 2
  ) +
  labs(x = NULL,
       y = NULL
  ) +
  scale_fill_manual(
    values = rep(
      c("NA", "tan", "cornsilk2", "royalblue", "gold", "firebrick3"),
      times = 2
    ),
    breaks = paste0(
      "Black/African:",
      c("Very bad", "Fairly bad", "Neither good nor bad", "Fairly good", "Very good", "Don't know")
    ),
    labels = c("Very bad", "Fairly bad", "Neither good nor bad", "Fairly good", "Very good", "Don't know"),
    guide = guide_legend(
      title = NULL,
      nrow = 3,
      ncol = 2,
      keywidth = 0.7,
      keyheight = 0.7,
    )
  ) +
  theme_du_bois()

afro_b_svg <- afro_b_pre + geom_text(
  aes(
    x = 1.4,
    label = pct_labels,
    family = font_name
  ),
  position = position_stack(vjust = 0.5),
  size = 2.5
) +
  annotate(
    "text",
    label = c("BLACK/AFRICAN.", "WHITE/EUROPEAN."),
    x = 1.55,
    y = c(203, 50),
    size = 3,
    family = font_name
  ) +
  theme(
    plot.title = element_text(size = 14),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.background = element_blank(),
    legend.text = element_text(
      size = 6,
      margin = margin(r = 70, unit = "pt")
    ),
    legend.position = c(0.6, 0.5),
    legend.key = element_blank()
  )
afro_b_svg

ggsave("afro_b.svg",
       device = "svg",
       plot = afro_b_svg,
       )
