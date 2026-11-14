library(tidyverse)
library(scales)

# Datos de entrada
tabla        <- read_csv("data/procesados/tablas_mortalidad_2023.csv", show_col_types = FALSE)
supervivencias <- read_csv("res/supervivencia_ejemplo.csv", show_col_types = FALSE)
trayectorias <- read_csv("res/trayectoria_ejemplo.csv", show_col_types = FALSE)
sensibilidad <- read_csv("res/sensibilidad_duracion.csv", show_col_types = FALSE)

# Nombres en español de cada código M49, en el orden del ranking
nombres.paises <- tibble(
  codigo = c(188, 840, 558, 591, 724, 124),
  nombre = c("Costa Rica", "Estados Unidos", "Nicaragua", "Panamá",
             "España", "Canadá")
)
orden.paises <- nombres.paises$nombre

# Colores: Costa Rica en negro y los destinos con la paleta Okabe-Ito,
# apta para daltonismo
colores.paises <- c("Costa Rica"     = "black",
                    "Estados Unidos" = "#0072B2",
                    "Nicaragua"      = "#D55E00",
                    "Panamá"         = "#009E73",
                    "España"         = "#CC79A7",
                    "Canadá"         = "#E69F00")

orden.alternativas <- c("Permanece en Costa Rica", "Retorno tras 5 años",
                        "Retorno tras 20 años", "Emigración permanente")

fuente <- "Fuente: UN DESA, World Population Prospects 2024 (tablas de vida 2023)."

# Etiquetas del eje de qx con coma decimal y sin ceros sobrantes (0,001)
etiqueta.qx <- label_number(decimal.mark = ",", drop0trailing = TRUE)

tema <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        legend.position = "bottom")

guardar <- function(grafico, archivo, alto = 5) {
  ggsave(file.path("res", archivo), grafico,
         width = 8, height = alto, dpi = 300, bg = "white")
}


# Gráfico 1. Probabilidad de muerte por edad en los seis países ----------------

datos.qx <- tabla %>%
  filter(edad < 100) %>%
  left_join(nombres.paises, by = "codigo") %>%
  mutate(nombre = factor(nombre, levels = orden.paises),
         sexo = factor(sexo, levels = c("Mujer", "Hombre"),
                       labels = c("Mujeres", "Hombres")))

grafico.qx <- ggplot(datos.qx, aes(edad, qx, colour = nombre)) +
  geom_line(aes(linewidth = nombre == "Costa Rica")) +
  scale_linewidth_manual(values = c(`TRUE` = 1.1, `FALSE` = 0.5), guide = "none") +
  scale_colour_manual(values = colores.paises) +
  scale_y_log10(labels = etiqueta.qx) +
  facet_wrap(~ sexo) +
  labs(title = "Nicaragua tiene la mayor mortalidad y España la menor; Costa Rica queda en medio",
       subtitle = "Probabilidad anual de muerte qx por edad, escala logarítmica, 2023",
       x = "Edad (años)", y = "Probabilidad de muerte (qx)",
       colour = NULL, caption = fuente) +
  tema

guardar(grafico.qx, "grafico_01_qx_paises.png")


# Gráfico 2. Línea de tiempo de las alternativas (hombres) --------------------
# Se muestran los dos extremos: España (menor mortalidad) y Nicaragua (mayor)

destinos.ejemplo <- c(724, 558)

datos.trayectoria <- trayectorias %>%
  filter(destino %in% destinos.ejemplo, sexo == "Hombre", edad < 100) %>%
  left_join(nombres.paises, by = c("residencia" = "codigo")) %>%
  left_join(rename(nombres.paises, nombre.destino = nombre),
            by = c("destino" = "codigo")) %>%
  mutate(alternativa = factor(alternativa, levels = orden.alternativas),
         nombre.destino = factor(nombre.destino, levels = c("España", "Nicaragua")))

# Se dibuja solo la emigración permanente. Las alternativas con retorno siguen
# esa misma línea hasta la edad de regreso (marcada) y luego la de Costa Rica.
emigracion.permanente <- datos.trayectoria %>%
  filter(alternativa == "Emigración permanente")

# Referencia: la trayectoria de permanecer en Costa Rica
referencia.cr <- datos.trayectoria %>%
  filter(alternativa == "Permanece en Costa Rica")

# Edades clave del ejemplo (parámetros del script 04: x = 22, d = 5, s = 5 y 20)
edad.emigracion <- 27

# Puntos donde la persona regresaría a Costa Rica en cada alternativa
puntos.retorno <- emigracion.permanente %>%
  filter(edad %in% c(32, 47)) %>%
  mutate(etiqueta = ifelse(edad == 32, "Regresa tras 5 años",
                           "Regresa tras 20 años"),
         # En Nicaragua la etiqueta va arriba del punto para no tapar la línea
         ajuste.h = ifelse(nombre.destino == "Nicaragua", 1.1, -0.1),
         ajuste.v = ifelse(nombre.destino == "Nicaragua", -0.8, 1.6))

grafico.trayectoria <- ggplot(emigracion.permanente, aes(edad, qx)) +
  geom_vline(xintercept = edad.emigracion, linetype = "dotted", colour = "grey40") +
  annotate("text", x = edad.emigracion, y = Inf, label = "Emigra a los 27",
           hjust = -0.05, vjust = 1.5, size = 3, colour = "grey30") +
  geom_line(data = referencia.cr, linetype = "dashed",
            colour = "grey40", linewidth = 0.5) +
  geom_line(aes(colour = nombre, group = 1), linewidth = 0.9) +
  geom_point(data = puntos.retorno, aes(colour = nombre), size = 2.5) +
  geom_text(data = puntos.retorno,
            aes(label = etiqueta, hjust = ajuste.h, vjust = ajuste.v),
            size = 2.8, colour = "grey20") +
  scale_colour_manual(values = c("Costa Rica" = "grey60",
                                 "España"     = "#CC79A7",
                                 "Nicaragua"  = "#D55E00")) +
  scale_y_log10(labels = etiqueta.qx) +
  facet_wrap(~ nombre.destino) +
  labs(title = "España reduce la mortalidad hasta los 60 años;\nNicaragua la aumenta en todas las edades",
       subtitle = "Probabilidad de muerte de un hombre que emigra a los 27. Línea punteada: permanecer en Costa Rica\nCon retorno, la trayectoria pasa a la línea punteada al regresar",
       x = "Edad (años)", y = "Probabilidad de muerte (qx), escala log.",
       colour = "País de residencia", caption = fuente) +
  tema

guardar(grafico.trayectoria, "grafico_02_linea_tiempo.png", alto = 5)


# Gráfico 3. Curvas de supervivencia de las alternativas ----------------------

datos.supervivencia <- supervivencias %>%
  left_join(nombres.paises, by = c("destino" = "codigo")) %>%
  mutate(nombre = factor(nombre, levels = orden.paises),
         alternativa = factor(alternativa, levels = orden.alternativas),
         sexo = factor(sexo, levels = c("Mujer", "Hombre"),
                       labels = c("Mujeres", "Hombres")))

grafico.supervivencia <- ggplot(datos.supervivencia,
                                aes(edad, tpx, colour = alternativa)) +
  geom_line(aes(linetype = alternativa == "Permanece en Costa Rica"),
            linewidth = 0.6) +
  scale_linetype_manual(values = c(`TRUE` = "dashed", `FALSE` = "solid"),
                        guide = "none") +
  scale_x_continuous(breaks = c(30, 60, 90)) +
  scale_colour_manual(values = c("Permanece en Costa Rica" = "black",
                                 "Retorno tras 5 años"     = "#56B4E9",
                                 "Retorno tras 20 años"    = "#0072B2",
                                 "Emigración permanente"   = "#D55E00")) +
  scale_y_continuous(labels = label_percent()) +
  facet_grid(sexo ~ nombre) +
  labs(title = "Las curvas de supervivencia se separan después de los 60 años",
       subtitle = "Probabilidad de sobrevivir desde los 22 años hasta cada edad, emigrando a los 27",
       x = "Edad (años)", y = "Probabilidad de supervivencia (tpx)",
       colour = NULL, caption = fuente) +
  tema +
  guides(colour = guide_legend(nrow = 2))

guardar(grafico.supervivencia, "grafico_03_supervivencia.png", alto = 5.5)


# Gráfico 4. Sensibilidad a los años en el extranjero -------------------------

datos.sensibilidad <- sensibilidad %>%
  left_join(nombres.paises, by = c("destino" = "codigo")) %>%
  mutate(nombre = factor(nombre, levels = orden.paises),
         sexo = factor(sexo, levels = c("Mujer", "Hombre"),
                       labels = c("Mujeres", "Hombres")))

# Etiqueta con el nombre del país al final de cada línea, en lugar de leyenda.
# En hombres, España y Canadá terminan casi en el mismo punto: se separan.
etiquetas.sensibilidad <- datos.sensibilidad %>%
  filter(anios.fuera == max(anios.fuera)) %>%
  mutate(ajuste = case_when(sexo == "Hombres" & nombre == "España" ~ -0.4,
                            sexo == "Hombres" & nombre == "Canadá" ~ 1.4,
                            TRUE ~ 0.5))

grafico.sensibilidad <- ggplot(datos.sensibilidad,
                               aes(anios.fuera, diferencia, colour = nombre)) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_line(linewidth = 0.8) +
  geom_text(data = etiquetas.sensibilidad,
            aes(label = nombre, vjust = ajuste),
            hjust = -0.1, size = 3) +
  scale_colour_manual(values = colores.paises, guide = "none") +
  scale_x_continuous(breaks = c(0, 25, 50, 75),
                     expand = expansion(mult = c(0.02, 0.4))) +
  facet_wrap(~ sexo) +
  labs(title = "El efecto de emigrar crece con los años que se pasan en el extranjero",
       subtitle = "Diferencia en la esperanza de vida a los 22 años frente a permanecer en Costa Rica, emigrando a los 27",
       x = "Años en el extranjero antes de regresar",
       y = "Diferencia en esperanza de vida (años)",
       caption = fuente) +
  tema

guardar(grafico.sensibilidad, "grafico_04_sensibilidad.png")
