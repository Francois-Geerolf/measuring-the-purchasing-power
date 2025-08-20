library(tidyverse)
library(readxl)

load("colors.RData")
load("geo.RData")

POP <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/NAMA_10_PE/A.THS_PER.POP_NC.FR+DE+IT+EA20+ES" %>% 
  rsdmx::readSDMX() %>%
  as_tibble() %>%
  select(geo, obsTime, POP = obsValue)

figure6 <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/NASA_10_NF_TR/A.CP_MNAC.PAID.B6G.S14_S15.FR+DE+IT+EA20+ES" %>% 
  rsdmx::readSDMX() %>%
  as_tibble() %>%
  left_join(POP, by = c("geo", "obsTime")) %>%
  mutate(obsValue = obsValue/POP) %>%
  left_join(geo, by = "geo") %>%
  mutate(Geo = ifelse(geo == "EA20", "Europe", Geo)) %>%
  mutate(date = as.Date(paste0(obsTime, "-01-01"))) %>%
  filter(date >= as.Date("1999-01-01"),
         date <= as.Date("2023-01-01")) %>%
  select(Geo, date, obsValue) %>%
  group_by(Geo) %>%
  mutate(obsValue = 100*obsValue/obsValue[date == as.Date("1999-01-01")]) %>%
  ungroup()

figure6 %>%
  left_join(colors, by = c("Geo" = "country")) %>%
  ggplot(data = .) + geom_line(aes(x = date, y = obsValue, color = color)) + 
  theme_minimal() + xlab("") + ylab("") +
  scale_x_date(breaks = as.Date(paste0(seq(1999, 2030, 2), "-01-01")),
               labels = scales::date_format("%Y")) +
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

ggsave("figure6.png", width = 1.25*6, height = 1.25*3.375, bg = "white", dpi = 150)
ggsave("figure6.pdf", width = 1.25*6, height = 1.25*3.375, dpi = 150)

