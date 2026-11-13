# Modelo actuarial de emigración y retorno a Costa Rica

## Descripción

Proyecto individual del curso CA-0204 — Herramientas de Ciencia de Datos I, Universidad de Costa Rica, II ciclo 2026.

El proyecto busca desarrollar un modelo en R que permita comparar probabilidades de supervivencia y esperanza de vida restante de personas costarricenses bajo diferentes escenarios de residencia internacional.

## Escenarios

1. Permanencia en Costa Rica.
2. Emigración permanente hacia otro país.
3. Emigración temporal y posterior retorno a Costa Rica.

## Herramientas

- R y RStudio
- tidyverse
- ggplot2
- Quarto
- Git y GitHub

## Fuentes de datos

### Migración: International Migrant Stock (UN DESA)

Incluidas en `data/`:

- `undesa_pd_2020_ims_stock_by_sex_destination_and_origin.xlsx`: fuente principal para seleccionar los cinco destinos.
- `undesa_pd_2024_ims_stock_by_sex_destination_and_origin.xlsx`: el corredor Costa Rica–Estados Unidos no aparece desagregado.

### Mortalidad: World Population Prospects 2024 (UN DESA)

Tablas de vida por edad simple (0–100+), estimaciones 1950–2023, variante media. Por su tamaño (~770 MB cada una) **no se incluyen en el repositorio**: deben descargarse y colocarse en `data/` (se pueden descomprimir).

- Mujeres: [WPP2024_Life_Table_Complete_Medium_Female_1950-2023.csv.gz](https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Life_Table_Complete_Medium_Female_1950-2023.csv.gz)
- Hombres: [WPP2024_Life_Table_Complete_Medium_Male_1950-2023.csv.gz](https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Life_Table_Complete_Medium_Male_1950-2023.csv.gz)

Página de descarga: [WPP 2024 Download Center (CSV)](https://population.un.org/wpp/downloads?folder=Standard%20Projections&group=CSV%20format), sección *Life Tables*.

## Metodología

Se utilizarán tablas de mortalidad por edad y sexo para calcular probabilidades de supervivencia y esperanza de vida restante mediante funciones actuariales desarrolladas en R.

Los resultados representarán escenarios hipotéticos y no predicciones individuales.

## Estado del proyecto

En desarrollo. Pendiente de recopilación y validación de fuentes de datos.
