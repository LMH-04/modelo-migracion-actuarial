library(dplyr)
library(readr)

# Países a estudiar: Costa Rica (código M49 188) y los cinco principales
# destinos según el ranking generado por 01_Filtro_mayor_migracion.R
destinos <- read_csv("data/procesados/destinos_principales_2020.csv") %>%
  filter(posicion <= 5)

codigos.paises <- c(188, destinos$codigo)

anio.tabla <- 2023

# Lee una tabla de vida de WPP 2024 y conserva solo los países y el año
# de interés. Se leen únicamente las columnas necesarias.
leer.tabla.vida <- function(archivo) {
  read_csv(archivo,
           col_select = c(LocID, Location, Time, Sex, AgeGrpStart,
                          qx, px, lx, ex)) %>%
    filter(LocID %in% codigos.paises, Time == anio.tabla)
}

tablas.mortalidad <- bind_rows(
  leer.tabla.vida("data/WPP2024_Life_Table_Complete_Medium_Female_1950-2023.csv"),
  leer.tabla.vida("data/WPP2024_Life_Table_Complete_Medium_Male_1950-2023.csv")
) %>%
  rename(codigo = LocID, pais = Location, anio = Time, edad = AgeGrpStart) %>%
  mutate(sexo = if_else(Sex == "Female", "Mujer", "Hombre")) %>%
  select(pais, codigo, sexo, edad, anio, qx, px, lx, ex) %>%
  arrange(pais, sexo, edad)

# Guardar el extracto en data/procesados
write_csv(tablas.mortalidad, "data/procesados/tablas_mortalidad_2023.csv")
