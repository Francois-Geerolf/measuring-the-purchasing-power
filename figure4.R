library(tidyverse)
library(readxl)

# CPI and HICP --------

figure1_idbanks <- c("001764780", "001763105")

CPI_HICP <- paste(figure1_idbanks, collapse = "+") |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", i = _) |>
  rsdmx::readSDMX() |>
  tibble::as_tibble() |>
  transmute(date = as.Date(paste0(TIME_PERIOD, "-01-01")),
            OBS_VALUE = as.numeric(OBS_VALUE),
            variable = case_when(grepl("harmonisé", TITLE_FR) ~ "Harmonized Index of Consumer Prices (HICP)",
                                 T ~ "Consumer Price Index (CPI)"))

# Household consumption deflator -------

temp <- tempfile()

curl::curl_download("https://www.insee.fr/fr/statistiques/fichier/8068592/T_CONSO_EFF_FONCTION.xlsx",
                    destfile = temp)

deflator <- read_excel(temp, skip = 3, sheet = "IPRIX2020") %>%
  rename(fonction = ...1, variable = ...2) %>%
  gather(year, OBS_VALUE, -fonction, -variable) %>%
  filter(!is.na(OBS_VALUE),
         fonction %in% c("CP06")) %>%
  transmute(date = as.Date(paste0(year, "-01-01")),
            OBS_VALUE,
            variable = "Household Consumption Deflator")


# Figure 4 ----------

figure4 <- deflator %>%
  bind_rows(CPI_HICP) %>%
  filter(date >= as.Date("1996-01-01")) %>%
  group_by(variable) %>%
  arrange(date) %>%
  mutate(OBS_VALUE = 100*OBS_VALUE/OBS_VALUE[1])

figure4 %>%
  ggplot() + ylab("") + xlab("") + theme_minimal() +
  geom_line(aes(x = date, y = OBS_VALUE, color = variable)) +
  scale_x_date(breaks = seq(1996, 2100, 4) %>% paste0("-01-01") %>% as.Date,
               labels = scales::date_format("%Y")) +
  theme(legend.position = c(0.25, 0.85),
        legend.title = element_blank()) +
  scale_y_log10(breaks = seq(0, 500, 5),
                labels = scales::dollar_format(accuracy = 1, prefix = "")) +
  labs(caption = "Source: Insee, author’s calculations")

ggsave("figure4.png", width = 1.25*6, height = 1.25*3.375, bg = "white", dpi = 150)
ggsave("figure4.pdf", width = 1.25*6, height = 1.25*3.375, dpi = 150)


