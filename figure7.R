library(tidyverse)
library(readxl)

load("colors.RData")
load("geo.RData")



figure7 <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/NAMA_10_CO3_P3/A.PD15_EUR.TOTAL.FR+DE+IT+EA20+ES" |> 
  rsdmx::readSDMX() |>
  as_tibble() |>
  left_join(geo, by = "geo") |>
  mutate(Geo = ifelse(geo == "EA20", "Europe", Geo)) |>
  mutate(date = as.Date(paste0(obsTime, "-01-01"))) |>
  filter(date >= as.Date("1999-01-01"),
         date <= as.Date("2023-01-01")) |>
  group_by(Geo) |>
  mutate(obsValue = 100*obsValue/obsValue[date == as.Date("1999-01-01")])

figure7 %>%
  left_join(colors, by = c("Geo" = "country")) %>%
  ggplot + geom_line(aes(x = date, y = obsValue, color = color)) +
  theme_minimal()  +
  ggimage::geom_image(data = . %>%
               group_by(date) %>%
               filter(n() == 5) %>%
               arrange(obsValue) %>%
               mutate(dist = min(obsValue[2]-obsValue[1],obsValue[3]-obsValue[2],
                                 obsValue[4]-obsValue[3],obsValue[5]-obsValue[4])) %>%
               arrange(-dist, date) %>%
               head(5) %>%
               mutate(image = paste0("flags/", str_to_lower(gsub(" ", "-", Geo)), ".png")),
             aes(x = date, y = obsValue, image = image), asp = 1.5) +
  scale_color_identity() + xlab("") + ylab("") +
  scale_x_date(breaks = as.Date(paste0(seq(1999, 2030, 2), "-01-01")),
               labels = scales::date_format("%Y")) +
  theme(legend.position = c(0.2, 0.85),
        legend.title = element_blank()) +
  scale_y_log10(breaks = seq(10, 300, 10)) +
  labs(caption = "Source: Eurostat, calculs de l'auteur")


ggsave("figure7.png", width = 1.25*6, height = 1.25*3.375, bg = "white", dpi = 150)
ggsave("figure7.pdf", width = 1.25*6, height = 1.25*3.375, dpi = 150)

