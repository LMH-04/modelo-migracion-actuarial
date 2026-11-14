# cod

Código del proyecto. Los scripts están numerados por orden de ejecución y se corren desde la raíz del proyecto (abriendo `modelo-migracion-actuarial.Rproj`).

| Script | Qué hace | Genera |
|---|---|---|
| `01_Filtro_mayor_migracion.R` | Ranking de destinos de la emigración costarricense (IMS 2020) | `data/procesados/destinos_principales_2020.csv` |
| `02_Filtro_tablas_mortalidad.R` | Tablas de vida 2023 de Costa Rica y los cinco principales destinos (WPP 2024) | `data/procesados/tablas_mortalidad_2023.csv` |
| `03_Funciones_actuariales.R` | Funciones de qx, trayectoria migratoria, supervivencia y esperanza de vida curtata | — (solo define funciones) |
| `04_Modelo_migracion.R` | Escenarios del ejemplo ilustrativo y sensibilidad | CSV en `res/` |
| `05_Graficos.R` | Gráficos del reporte | PNG en `res/` |
| `reporte.qmd` | Reporte ejecutivo en Quarto | `res/reporte.pdf` |

El script 02 necesita las tablas de vida de WPP 2024, que no están en el repositorio (ver `data/README.md`).
