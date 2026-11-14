# Aplicación interactiva del modelo de emigración y retorno.
# Se ejecuta desde la raíz del proyecto con:
#   shiny::runApp("cod/06_App_shiny.R")
# (runApp trabaja desde la carpeta cod/, por eso las rutas usan ../)

library(shiny)
library(tidyverse)
library(scales)

source("03_Funciones_actuariales.R")

tabla <- read_csv("../data/procesados/tablas_mortalidad_2023.csv",
                  show_col_types = FALSE)

# Destinos: nombre en español y código M49, en el orden del ranking
destinos <- c("Estados Unidos" = 840, "Nicaragua" = 558, "Panamá" = 591,
              "España" = 724, "Canadá" = 124)

colores.paises <- c("Costa Rica"     = "grey60",
                    "Estados Unidos" = "#0072B2",
                    "Nicaragua"      = "#D55E00",
                    "Panamá"         = "#009E73",
                    "España"         = "#CC79A7",
                    "Canadá"         = "#E69F00")

fuente <- "Fuente: UN DESA, World Population Prospects 2024 (tablas de vida 2023)."

tema <- theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        legend.position = "bottom")

formato <- function(x, digitos = 2) {
  formatC(x, format = "f", digits = digitos, decimal.mark = ",")
}


# Números dentro de fórmulas: la coma decimal va entre llaves para que
# MathJax no la trate como separador y agregue un espacio
formato.mat <- function(x, digitos = 2) sub(",", "{,}", formato(x, digitos))

# Ejemplo numérico de la página de conceptos: hombre de 22 años en Costa Rica
qx.ejemplo  <- obtener.qx(tabla, 188, "Hombre")
sup.ejemplo <- supervivencia(qx.ejemplo, 22)
ejemplo <- list(
  q22 = qx.ejemplo[22 + 1],
  p5  = sup.ejemplo$tpx[sup.ejemplo$t == 5],
  p43 = sup.ejemplo$tpx[sup.ejemplo$t == 43],
  e22 = esperanza.vida(qx.ejemplo, 22)
)


# Interfaz ---------------------------------------------------------------------

# Página 1: conceptos actuariales necesarios para leer los resultados.
# Se muestran como un carrusel: una tarjeta a la vez; al tocarla, la tarjeta
# "cae" y aparece la siguiente.
estilo.tarjetas <- tags$style(HTML("
  .carrusel {
    position: relative; height: calc(100vh - 230px); max-height: 460px;
    max-width: 760px; margin: 0 auto;
  }
  .tarjeta {
    position: absolute; top: 0; left: 0; right: 0;
    background: #ffffff; border: 1px solid #e3e3e3; border-radius: 14px;
    border-top: 6px solid #0072B2; padding: 2.5vh 34px; cursor: pointer;
    box-shadow: 0 4px 14px rgba(0, 0, 0, 0.08);
    opacity: 0; transform: translateY(-40px); pointer-events: none;
    transition: transform 0.6s ease, opacity 0.6s ease;
  }
  .tarjeta.activa { opacity: 1; transform: none; pointer-events: auto; }
  .tarjeta.cae    { opacity: 0; transform: translateY(160px) rotate(8deg); }
  .tarjeta .numero  { color: #0072B2; font-weight: bold; font-size: 15px; }
  .tarjeta .simbolo { font-size: clamp(28px, 6vh, 44px); margin: 0.5vh 0; }
  .tarjeta .nombre  { font-size: clamp(19px, 3.6vh, 26px); font-weight: bold;
                      margin-bottom: 1vh; }
  .tarjeta .definicion { font-size: clamp(15px, 2.7vh, 19px); color: #444444; }
  .tarjeta .formula { font-size: clamp(16px, 3vh, 22px); }
  .tarjeta .ejemplo {
    background: #EAF3FA; border-radius: 8px; padding: 1vh 14px;
    font-size: clamp(14px, 2.6vh, 18px); color: #1d3c55;
  }
  .supuestos { font-size: clamp(15px, 2.7vh, 19px); color: #333333;
               margin: 0.5vh 0 1.5vh 0; padding-left: 22px; }
  .supuestos li { margin-bottom: 0.6vh; }
  .mini-tabla { margin: 1.5vh auto; font-size: clamp(15px, 2.7vh, 19px);
                border-collapse: collapse; }
  .mini-tabla th, .mini-tabla td {
    padding: 0.6vh 18px; text-align: center; border-bottom: 1px solid #e3e3e3;
  }
  .mini-tabla th { color: #0072B2; text-align: left; }
  .controles { text-align: center; margin-top: 10px; }
  .controles .btn { margin: 0 10px; }
  .punto {
    display: inline-block; width: 11px; height: 11px; margin: 0 5px;
    border-radius: 50%; background: #cccccc;
  }
  .punto.activo { background: #0072B2; }
"))

# Cambia de tarjeta: la actual cae y aparece la siguiente (o la anterior)
script.carrusel <- tags$script(HTML("
  var tarjetaActual = 0;
  function mostrarTarjeta(paso) {
    var tarjetas = document.querySelectorAll('.carrusel .tarjeta');
    var puntos = document.querySelectorAll('.punto');
    var anterior = tarjetas[tarjetaActual];
    anterior.classList.remove('activa');
    anterior.classList.add('cae');
    setTimeout(function() { anterior.classList.remove('cae'); }, 600);
    tarjetaActual = (tarjetaActual + paso + tarjetas.length) % tarjetas.length;
    tarjetas[tarjetaActual].classList.add('activa');
    puntos.forEach(function(p, i) { p.classList.toggle('activo', i === tarjetaActual); });
  }
  // Flechas del teclado, solo cuando la página de conceptos está visible
  document.addEventListener('keydown', function(e) {
    var carrusel = document.querySelector('.carrusel');
    if (!carrusel || carrusel.offsetParent === null) return;
    if (e.key === 'ArrowRight') mostrarTarjeta(1);
    if (e.key === 'ArrowLeft')  mostrarTarjeta(-1);
  });
"))

# Mini tabla de mortalidad para la tarjeta de qx (Costa Rica, hombres, 2023)
edades.ejemplo <- c(0, 22, 65, 90)
tabla.mortalidad.ejemplo <- tags$table(
  class = "mini-tabla",
  tags$tr(tags$th("Edad \\(x\\)"),
          lapply(edades.ejemplo, function(e) tags$td(e))),
  tags$tr(tags$th("\\(q_x\\)"),
          lapply(edades.ejemplo, function(e) tags$td(formato(qx.ejemplo[e + 1], 4))))
)

# Lista de supuestos para la primera tarjeta
lista.supuestos <- tags$ul(
  class = "supuestos",
  tags$li("En cada edad se aplica la mortalidad del país donde reside la persona."),
  tags$li("Se usan las tablas de mortalidad de 2023 de la ONU (UN DESA)."),
  tags$li("Los cambios de país ocurren en edades enteras."),
  tags$li("La persona puede fallecer antes de emigrar."),
  tags$li("Edad máxima de 100 años; se cuentan solo años completos de vida.")
)

total.tarjetas <- 5

tarjeta <- function(numero, simbolo, nombre, definicion, formula, ejemplo,
                    activa = FALSE) {
  div(class = if (activa) "tarjeta activa" else "tarjeta",
      onclick = "mostrarTarjeta(1)",
      div(class = "numero", paste(numero, "de", total.tarjetas)),
      div(class = "simbolo", simbolo),
      div(class = "nombre", nombre),
      div(class = "definicion", definicion),
      div(class = "formula", formula),
      div(class = "ejemplo", ejemplo))
}

pagina.conceptos <- fluidRow(
  column(
    10, offset = 1,
    withMathJax(),
    estilo.tarjetas,
    script.carrusel,
    h2("Supuestos y conceptos del modelo",
       style = "text-align: center; margin: 1vh 0 2vh 0; font-size: clamp(22px, 4vh, 30px);"),

    div(class = "carrusel",
      tarjeta(
        "1", NULL, "Supuestos del modelo",
        NULL,
        lista.supuestos,
        "Los resultados son comparaciones hipotéticas entre poblaciones, no predicciones individuales.",
        activa = TRUE
      ),
      tarjeta(
        "2", "\\(q_x\\)", "Probabilidad de muerte",
        "Probabilidad de que una persona de edad exacta \\(x\\) fallezca antes de cumplir \\(x+1\\) años.",
        tabla.mortalidad.ejemplo,
        "Es el dato de partida: viene directamente de las tablas de mortalidad de las Naciones Unidas (en la tabla, Costa Rica, hombres, 2023)."
      ),
      tarjeta(
        "3", "\\({}_tp_x\\)", "Probabilidad de supervivencia",
        "Probabilidad de que una persona de edad \\(x\\) siga con vida \\(t\\) años después.",
        "$${}_tp_x = \\prod_{j=0}^{t-1}\\left(1-q_{x+j}\\right)$$",
        paste0("Llegar de los 22 a los 65 años: \\({}_{43}p_{22} = ",
               formato.mat(ejemplo$p43, 4), "\\)")
      ),
      tarjeta(
        "4", "\\(e_x\\)", "Esperanza de vida curtata",
        "Años completos que, en promedio, le quedan por vivir a una persona de edad \\(x\\).",
        "$$e_x = \\sum_{t=1}^{100-x} {}_tp_x$$",
        paste0("Hombre de 22 años en Costa Rica: \\(e_{22} = ",
               formato.mat(ejemplo$e22), "\\) años")
      ),
      tarjeta(
        "5", "\\(q_a^M\\)", "Trayectoria migratoria",
        "En cada edad se aplica la mortalidad del país donde reside la persona.",
        "$$q_a^{M}=\\begin{cases} q_a^{CR}, & \\text{en Costa Rica} \\\\ q_a^{D}, & \\text{en el destino} \\end{cases}$$",
        "\\(\\Delta e_x = e_x^{M} - e_x^{CR}\\): si es positiva, el escenario tiene mayor esperanza de vida"
      )
    ),

    div(class = "controles",
        actionButton("anterior", "← Anterior", onclick = "mostrarTarjeta(-1)"),
        span(class = "punto activo"),
        lapply(2:total.tarjetas, function(i) span(class = "punto")),
        actionButton("siguiente", "Siguiente →", onclick = "mostrarTarjeta(1)")),

    helpText("Toque la tarjeta o use las flechas del teclado para avanzar.",
             fuente, style = "text-align: center; margin-top: 1vh;")
  )
)

# Página 2: simulador de escenarios.
# Diseño compacto para que todo quepa en la pantalla sin hacer scroll: el alto
# de los gráficos se ajusta al alto de la ventana (100vh = alto completo).
estilo.simulador <- tags$style(HTML("
  .simulador .well { padding: 12px 16px; margin-bottom: 0; }
  .simulador .well .form-group { margin-bottom: 6px; }
  .simulador .well .control-label { margin-bottom: 2px; }
  .simulador .indicador .etiqueta { font-size: 14px; color: #555555; }
  .simulador .indicador .valor    { font-size: 26px; font-weight: bold; }
  .simulador .descripcion { font-size: 14px; color: #555555; margin: 2px 0 6px 0; }
  .simulador .nav-tabs > li > a { padding: 6px 12px; }
"))

alto.grafico <- "calc(100vh - 250px)"

pagina.simulador <- div(
  class = "simulador",
  estilo.simulador,
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput("x", "Edad actual", min = 0, max = 90, value = 22, ticks = FALSE),
      radioButtons("sexo", "Sexo", choices = c("Mujer", "Hombre"),
                   selected = "Hombre", inline = TRUE),
      selectInput("destino", "País de destino", choices = destinos,
                  selected = 724),
      sliderInput("d", "Años antes de emigrar", min = 0, max = 99, value = 5,
                  ticks = FALSE),
      checkboxInput("retorno", "Regresa a Costa Rica", value = TRUE),
      conditionalPanel(
        condition = "input.retorno == true",
        sliderInput("s", "Años en el extranjero", min = 1, max = 100, value = 20,
                    ticks = FALSE)
      )
    ),

    mainPanel(
      width = 9,
      fluidRow(
        column(4, div(class = "indicador",
                      div(class = "etiqueta", "Permanecer en Costa Rica"),
                      div(class = "valor", textOutput("e.cr")))),
        column(4, div(class = "indicador",
                      div(class = "etiqueta", "Escenario migratorio"),
                      div(class = "valor", textOutput("e.escenario")))),
        column(4, div(class = "indicador",
                      div(class = "etiqueta", "Diferencia"),
                      div(class = "valor", textOutput("diferencia"))))
      ),
      div(class = "descripcion", textOutput("descripcion")),
      tabsetPanel(
        tabPanel("Trayectoria de mortalidad", plotOutput("grafico.qx", height = alto.grafico)),
        tabPanel("Supervivencia",             plotOutput("grafico.tpx", height = alto.grafico)),
        tabPanel("Comparación de destinos",   plotOutput("grafico.destinos", height = alto.grafico))
      )
    )
  )
)

# Portada: título y descripción del proyecto
estilo.portada <- tags$style(HTML("
  .portada { text-align: center; max-width: 860px; margin: 4vh auto 0 auto; }
  .portada .curso   { color: #0072B2; font-weight: bold;
                      font-size: clamp(13px, 2.4vh, 16px);
                      letter-spacing: 1px; text-transform: uppercase; }
  .portada .titulo  { font-size: clamp(26px, 5.5vh, 40px); font-weight: bold;
                      line-height: 1.2; margin: 1.5vh 0 1vh 0; }
  .portada .subtitulo { font-size: clamp(16px, 3vh, 22px); color: #555555;
                        margin-bottom: 2.5vh; }
  .portada .descripcion { font-size: clamp(15px, 2.8vh, 19px); color: #333333;
                          line-height: 1.45; }
  .portada .datos { margin: 2.5vh 0; }
  .portada .dato {
    display: inline-block; background: #EAF3FA; color: #1d3c55;
    border-radius: 20px; padding: 0.8vh 16px; margin: 4px;
    font-size: clamp(13px, 2.4vh, 16px);
  }
  .portada .autor { font-size: clamp(14px, 2.5vh, 17px); color: #555555;
                    margin-bottom: 2.5vh; }
  .portada .btn-comenzar {
    background: #0072B2; color: #ffffff; border: none;
    font-size: clamp(15px, 2.7vh, 18px); padding: 1vh 30px; border-radius: 24px;
  }
"))

pagina.inicio <- div(
  class = "portada",
  estilo.portada,
  div(class = "curso", "CA-0204 Herramientas de Ciencia de Datos I · Proyecto individual"),
  div(class = "titulo",
      "Esperanza de vida bajo escenarios de emigración y retorno a Costa Rica"),
  div(class = "subtitulo", "Un modelo actuarial programado en R"),
  div(class = "descripcion",
      "¿Cómo cambiarían la supervivencia y la esperanza de vida de una persona",
      "si viviera parte de su vida en otro país? El proyecto combina las tablas",
      "de mortalidad de Costa Rica y de los cinco principales destinos de la",
      "emigración costarricense para comparar tres escenarios: permanecer en",
      "el país, emigrar de forma permanente y emigrar con retorno."),
  div(class = "datos",
      span(class = "dato", "5 destinos: Estados Unidos, Nicaragua, Panamá, España y Canadá"),
      span(class = "dato", "3 escenarios migratorios"),
      span(class = "dato", "Datos de las Naciones Unidas (2020 y 2024)")),
  div(class = "autor", "Lemuel Méndez Hidalgo · Universidad de Costa Rica · II ciclo 2026"),
  actionButton("comenzar", "Comenzar →", class = "btn-comenzar")
)

ui <- navbarPage(
  "Emigración y esperanza de vida",
  id = "paginas",
  tabPanel("Inicio", pagina.inicio),
  tabPanel("1. Supuestos y conceptos", pagina.conceptos),
  tabPanel("2. Simulador", pagina.simulador)
)


# Servidor ---------------------------------------------------------------------

server <- function(input, output, session) {

  # Botón de la portada: pasa a la página de supuestos y conceptos
  observeEvent(input$comenzar, {
    updateNavbarPage(session, "paginas", selected = "1. Supuestos y conceptos")
  })

  # Años en el extranjero: Inf si no regresa
  anios.fuera <- reactive({
    if (input$retorno) input$s else Inf
  })

  nombre.destino <- reactive({
    names(destinos)[destinos == as.numeric(input$destino)]
  })

  # Verifica que la emigración ocurra dentro de la tabla (antes de los 100 años)
  validar <- reactive({
    validate(need(input$x + input$d < 100,
                  "La edad de emigración debe ser menor que 100 años."))
  })

  qx.cr <- reactive(obtener.qx(tabla, 188, input$sexo))

  qx.escenario <- reactive({
    validar()
    trayectoria.qx(tabla, input$sexo, input$x, input$d, anios.fuera(),
                   as.numeric(input$destino))
  })

  e.cr        <- reactive(esperanza.vida(qx.cr(), input$x))
  e.escenario <- reactive(esperanza.vida(qx.escenario(), input$x))

  # Indicadores (si los parámetros no son válidos se muestra una raya)
  parametros.validos <- reactive(input$x + input$d < 100)

  output$e.cr        <- renderText(paste(formato(e.cr()), "años"))
  output$e.escenario <- renderText({
    if (!parametros.validos()) return("—")
    paste(formato(e.escenario()), "años")
  })
  output$diferencia  <- renderText({
    if (!parametros.validos()) return("—")
    dif <- e.escenario() - e.cr()
    paste0(ifelse(dif >= 0, "+", ""), formato(dif), " años")
  })

  output$descripcion <- renderText({
    validar()
    edad.emigra <- input$x + input$d
    texto <- paste0("Persona de ", input$x, " años que emigra a ",
                    nombre.destino(), " a los ", edad.emigra, " años")
    if (input$retorno) {
      paste0(texto, " y regresa a Costa Rica a los ", edad.emigra + input$s, ".")
    } else {
      paste0(texto, " y no regresa.")
    }
  })

  # Gráfico: probabilidad de muerte por edad según el país de residencia
  output$grafico.qx <- renderPlot({
    validar()
    edades <- 0:99
    en.destino <- edades >= input$x + input$d &
                  edades <  input$x + input$d + anios.fuera()

    datos <- tibble(edad = edades,
                    qx = qx.escenario()[edades + 1],
                    residencia = ifelse(en.destino, nombre.destino(), "Costa Rica"))
    referencia <- tibble(edad = edades, qx = qx.cr()[edades + 1])

    ggplot(datos, aes(edad, qx)) +
      geom_line(data = referencia, linetype = "dashed", colour = "grey40") +
      geom_line(aes(colour = residencia, group = 1), linewidth = 1.1) +
      geom_vline(xintercept = input$x, colour = "grey70") +
      scale_colour_manual(values = colores.paises) +
      scale_y_log10(labels = label_number(decimal.mark = ",", drop0trailing = TRUE)) +
      labs(title = "Probabilidad de muerte por edad según el país de residencia",
           subtitle = "Línea punteada: permanecer en Costa Rica. Línea vertical: edad actual",
           x = "Edad (años)", y = "qx (escala log.)",
           colour = "País de residencia", caption = fuente) +
      tema
  })

  # Gráfico: curvas de supervivencia
  output$grafico.tpx <- renderPlot({
    datos <- bind_rows(
      supervivencia(qx.cr(), input$x) %>% mutate(alternativa = "Permanecer en Costa Rica"),
      supervivencia(qx.escenario(), input$x) %>% mutate(alternativa = "Escenario migratorio")
    )

    ggplot(datos, aes(edad, tpx, colour = alternativa, linetype = alternativa)) +
      geom_line(linewidth = 1.1) +
      scale_colour_manual(values = c("Permanecer en Costa Rica" = "black",
                                     "Escenario migratorio" = colores.paises[[nombre.destino()]])) +
      scale_linetype_manual(values = c("Permanecer en Costa Rica" = "dashed",
                                       "Escenario migratorio" = "solid")) +
      scale_y_continuous(labels = label_percent()) +
      labs(title = paste("Probabilidad de sobrevivir desde los", input$x, "años"),
           x = "Edad (años)", y = "tpx",
           colour = NULL, linetype = NULL, caption = fuente) +
      tema
  })

  # Gráfico: diferencia en la esperanza de vida con cada destino
  output$grafico.destinos <- renderPlot({
    validar()
    comparacion <- tibble(nombre = names(destinos), codigo = destinos) %>%
      rowwise() %>%
      mutate(diferencia = esperanza.vida(
        trayectoria.qx(tabla, input$sexo, input$x, input$d, anios.fuera(), codigo),
        input$x) - e.cr()) %>%
      ungroup() %>%
      mutate(nombre = fct_reorder(nombre, diferencia),
             seleccionado = codigo == as.numeric(input$destino))

    ggplot(comparacion, aes(diferencia, nombre, fill = seleccionado)) +
      geom_col(width = 0.7) +
      geom_vline(xintercept = 0, colour = "grey40") +
      geom_text(aes(label = formato(diferencia),
                    hjust = ifelse(diferencia >= 0, -0.15, 1.15)), size = 5) +
      scale_fill_manual(values = c(`TRUE` = "#0072B2", `FALSE` = "grey80"),
                        guide = "none") +
      scale_x_continuous(expand = expansion(mult = 0.25)) +
      labs(title = "Diferencia en la esperanza de vida frente a permanecer en Costa Rica",
           subtitle = "Mismos parámetros para los cinco destinos; en color, el destino seleccionado",
           x = "Diferencia en esperanza de vida (años)", y = NULL, caption = fuente) +
      tema
  })
}

shinyApp(ui = ui, server = server)
