library (dplyr)
library(tidyr)

lista.no.paises <- readxl::read_excel(
  "data/undesa_pd_2020_ims_stock_by_sex_destination_and_origin.xlsx",
  sheet = 3, range = "B11:D59") %>% 
  select(1,3) %>% 
  rename("no.paises" = "...1", "code" = "Location code of destination")

paises.mayor.migracion <- readxl::read_excel(
  "data/undesa_pd_2020_ims_stock_by_sex_destination_and_origin.xlsx",
  sheet = 2, range = "A11:AE37062", guess_max = 37051) %>%
  filter(!`Location code of destination` %in% lista.no.paises$code) %>%
  select(2, 4, 6, "2020...14", "2020...21", "2020...28") %>%
  rename_with(~ c("destination", "code", "origin", "total.migration.2020",
                  "male.migration.2020","female.migration.2020"),
              .cols = c(1,2,3,4,5,6)) %>%
  filter(origin == "Costa Rica") %>%
  arrange(desc(total.migration.2020)) %>%
  mutate(rank = row_number())

# Guardar el ranking completo de destinos en data/procesados
dir.create("data/procesados", showWarnings = FALSE)
readr::write_csv(paises.mayor.migracion,
                 "data/procesados/destinos_principales_2020.csv")

