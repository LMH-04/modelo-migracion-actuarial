library(dplyr)
library(readr)

source("cod/03_Funciones_actuariales.R")

tabla <- read_csv("data/procesados/tablas_mortalidad_2023.csv",
                  show_col_types = FALSE)

# Códigos M49 de los cinco principales destinos, en el orden del ranking
destinos <- read_csv("data/procesados/destinos_principales_2020.csv",
                     show_col_types = FALSE) %>%
  filter(posicion <= 5) %>%
  pull(codigo)

# Parámetros del ejemplo ilustrativo
x <- 22               # edad actual
d <- 5                # años antes de emigrar (emigra a los 27)
duraciones <- c(5, 20) # años en el extranjero antes de regresar

# Alternativas del ejemplo, según los años que se pasan en el extranjero:
# 0 años = nunca emigra, Inf = nunca regresa (emigración permanente)
alternativas <- c(0, duraciones, Inf)
names(alternativas) <- c("Permanece en Costa Rica",
                         paste("Retorno tras", duraciones, "años"),
                         "Emigración permanente")

edades <- 0:100

resultados     <- data.frame()   # e_x y diferencia de cada alternativa
supervivencias <- data.frame()   # curvas tpx de cada alternativa
trayectorias   <- data.frame()   # qx por edad y país de residencia
sensibilidad   <- data.frame()   # e_x según los años en el extranjero

for (sexo in c("Mujer", "Hombre")) {

  qx.cr <- obtener.qx(tabla, 188, sexo)
  e.cr  <- esperanza.vida(qx.cr, x)

  for (destino in destinos) {

    for (alternativa in names(alternativas)) {
      s      <- alternativas[[alternativa]]
      qx.alt <- trayectoria.qx(tabla, sexo, x, d, s, destino)
      e.alt  <- esperanza.vida(qx.alt, x)

      resultados <- bind_rows(resultados, data.frame(
        destino, sexo, alternativa, ex = e.alt, diferencia = e.alt - e.cr))

      supervivencias <- bind_rows(supervivencias, data.frame(
        destino, sexo, alternativa, supervivencia(qx.alt, x)))

      # Línea de tiempo: qx de cada edad y país en el que reside
      trayectorias <- bind_rows(trayectorias, data.frame(
        destino, sexo, alternativa, edad = edades, qx = qx.alt,
        residencia = ifelse(edades >= x + d & edades < x + d + s,
                            destino, 188)))
    }

    # Sensibilidad: de 0 años fuera hasta quedarse hasta la edad terminal
    for (s in 0:(100 - x - d)) {
      e.s <- esperanza.vida(trayectoria.qx(tabla, sexo, x, d, s, destino), x)
      sensibilidad <- bind_rows(sensibilidad, data.frame(
        destino, sexo, anios.fuera = s, ex = e.s, diferencia = e.s - e.cr))
    }
  }
}

# Guardar resultados en res/
write_csv(resultados,     "res/resultados_ejemplo.csv")
write_csv(supervivencias, "res/supervivencia_ejemplo.csv")
write_csv(sensibilidad,   "res/sensibilidad_duracion.csv")
write_csv(trayectorias,   "res/trayectoria_ejemplo.csv")
