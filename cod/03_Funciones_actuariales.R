library(dplyr)

# Funciones actuariales del modelo. Este script solo define funciones:
# las tablas se cargan en el script que las utilice (modelo, reporte o Shiny).
# Convenciones: edades enteras de 0 a 100, edad terminal omega = 100
# (qx = 1 a los 100 años) y esperanza de vida curtata.


# Devuelve el vector de qx (edades 0 a 100) de un país y sexo.
# El elemento [edad + 1] corresponde a la edad, porque R empieza en 1.
obtener.qx <- function(tabla, pais.tabla, sexo.tabla) {
  qx <- tabla %>%
    filter(pais == pais.tabla, sexo == sexo.tabla) %>%
    arrange(edad) %>%
    pull(qx)

  if (length(qx) != 101) {
    stop("No hay tabla completa para ", pais.tabla, " (", sexo.tabla, ")")
  }
  qx
}


# Construye el vector de qx de una trayectoria migratoria:
#   edades menores que x + d             -> Costa Rica
#   edades entre x + d y x + d + s - 1   -> país de destino
#   edades desde x + d + s               -> Costa Rica (solo si retorno = TRUE)
# Si retorno = FALSE, la persona se queda en el destino y s no se usa.
trayectoria.qx <- function(tabla, sexo, x, d, s, destino, retorno = TRUE) {
  qx.cr      <- obtener.qx(tabla, "Costa Rica", sexo)
  qx.destino <- obtener.qx(tabla, destino, sexo)

  edades <- 0:100
  edad.emigracion <- x + d
  edad.retorno <- if (retorno) x + d + s else Inf

  en.destino <- edades >= edad.emigracion & edades < edad.retorno
  ifelse(en.destino, qx.destino, qx.cr)
}


# Probabilidades de supervivencia tpx desde la edad x, para t = 0, 1, ..., 101 - x.
# tpx = (1 - q_x)(1 - q_{x+1})...(1 - q_{x+t-1}), con 0px = 1.
supervivencia <- function(qx, x) {
  if (x < 0 || x > 100) stop("La edad debe estar entre 0 y 100")

  q.desde.x <- qx[(x + 1):101]
  tpx <- c(1, cumprod(1 - q.desde.x))

  data.frame(t = 0:(101 - x), edad = x:101, tpx = tpx)
}


# Esperanza de vida curtata: e_x = suma de tpx para t = 1, ..., 100 - x.
esperanza.vida <- function(qx, x) {
  sup <- supervivencia(qx, x)
  sum(sup$tpx[sup$t >= 1])
}
