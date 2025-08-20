library(tidyverse)
library(readxl)
library(curl)
library(scales)

# ig_d("insee", "t_pib_vol", "deflator-P3M-P51M-1999")
# https://fgeerolf.com/data/insee/t_pib_vol.html

temp <- tempfile()

curl_download("https://www.insee.fr/fr/statistiques/fichier/8196636/t_pib_vol.xls",
              destfile = temp,
              quiet = F)

t_pib_vol <- read_excel(temp, skip = 7, sheet = 2) %>%
  rename(TIME_PERIOD = ...1) %>%
  mutate(date = zoo::as.yearqtr(TIME_PERIOD, format = "%YT%q")) %>%
  select(-TIME_PERIOD) %>%
  filter(!is.na(date)) %>%
  gather(variable, value, -date) %>%
  filter(!is.na(value))


curl_download("https://www.insee.fr/fr/statistiques/fichier/8182876/t_pib_val.xls",
              destfile = temp,
              quiet = F)

# https://www.insee.fr/fr/statistiques/fichier/8196636/t_pib_vol.xls
# https://www.insee.fr/fr/statistiques/fichier/8182876/t_pib_vol.xls

t_pib_val <- read_excel(temp, skip = 7, sheet = 2) %>%
  rename(TIME_PERIOD = ...1) %>%
  mutate(date = zoo::as.yearqtr(TIME_PERIOD, format = "%YT%q")) %>%
  select(-TIME_PERIOD) %>%
  filter(!is.na(date)) %>%
  gather(variable, value, -date) %>%
  filter(!is.na(value))

t_pib_vol %>%
  filter(variable %in% c("P3M", "P51M")) %>%
  rename(vol = value) %>%
  left_join(t_pib_val, by = c("date", "variable")) %>%
  rename(val = value) %>%
  mutate(deflator = val/vol,
         date = zoo::as.Date(date)) %>%
  filter(date >= as.Date("1999-01-01"),
         date <= as.Date("2024-01-01")) %>%
  group_by(variable) %>%
  arrange(date) %>%
  mutate(deflator = 100*deflator/deflator[1]) %>%
  mutate(Variable = case_when(variable == "P3M" ~ "Household consumption deflator",
                              variable == "P51M" ~ "Household investment deflator")) %>%
  ggplot + theme_minimal() + ylab("") + xlab("") +
  geom_line(aes(x = date, y = deflator, color = Variable)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.3, 0.9)) +
  scale_x_date(breaks = seq(1999, 2100, 5) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  scale_y_log10(breaks = seq(0, 1000, 10)) +
  geom_label(data = . %>% filter(date == max(date)),
             aes(x = date, y = deflator, color = Variable, label = round(deflator, 1))) +
  labs(caption = "Source: Insee, author’s calculations")

ggsave("figure1.png", width = 1.25*6, height = 1.25*3.375, bg = "white", dpi = 150)
ggsave("figure1.pdf", width = 1.25*6, height = 1.25*3.375, dpi = 150)

