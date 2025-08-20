library(tidyverse)
library(readxl)

# Figure 3 ----------

load("colors.RData")
load("geo.RData")

figure5 <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/PRC_HICP_MIDX/M.I15.CP08.FR+DE+IT+NL+ES" |> 
  rsdmx::readSDMX() |>
  as_tibble() |>
  left_join(geo, by = "geo") |>
  mutate(date = as.Date(paste0(obsTime, "-01"))) |>
  filter(date >= as.Date("1996-01-01"),
         date <= as.Date("2024-07-01")) |>
  group_by(Geo) |>
  mutate(obsValue = 100*obsValue/obsValue[date == as.Date("1996-01-01")])

figure5 |>
  left_join(colors, by = c("Geo" = "country")) %>%
  ggplot(.) + geom_line(aes(x = date, y = obsValue, color = color)) + 
  theme_minimal() + xlab("") + ylab("") +
  scale_x_date(breaks = as.Date(paste0(seq(1996, 2030, 2), "-01-01")),
               labels = date_format("%Y")) +
  scale_y_log10(breaks = seq(0, 200, 5)) +
  scale_color_identity() +
  ggimage::geom_image(data = . %>%
                        group_by(date) %>%
                        filter(n() == 5) %>%
                        arrange(obsValue) %>%
                        mutate(dist = min(obsValue[2]-obsValue[1], obsValue[3]-obsValue[2],
                                          obsValue[4]-obsValue[3], obsValue[5]-obsValue[4])) %>%
                        arrange(-dist, date) %>%
                        head(5) %>%
                        mutate(image = paste0("flags/", str_to_lower(gsub(" ", "-", Geo)), ".png")),
                      aes(x = date, y = obsValue, image = image), asp = 1.5) +
  theme(legend.position = "none") +
  labs(caption = "Source: Eurostat, author’s calculations")


ggsave("figure5.png", width = 1.25*6, height = 1.25*3.375, bg = "white", dpi = 150)
ggsave("figure5.pdf", width = 1.25*6, height = 1.25*3.375, dpi = 150)

