# app.R
library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)

source("funciones_eleccion.R")

num_input <- function(x) {
  x <- gsub("\\.", "", as.character(x))
  x <- gsub(",", ".", x)
  as.numeric(x)
}

ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(
    tags$style(HTML("
      .bloque {border:1px solid #e5e7eb; border-radius:12px; padding:16px; margin-bottom:16px; background:white;}
      .estado {font-size:20px; font-weight:700;}
      .subtexto {color:#475569;}
      .valor {font-size:26px; font-weight:700;}
      .etiqueta {font-size:13px; color:#64748b;}
      .grid {display:grid; grid-template-columns: repeat(4, 1fr); gap:12px;}
      @media(max-width: 900px){.grid{grid-template-columns:1fr 1fr;}}
      @media(max-width: 600px){.grid{grid-template-columns:1fr;}}
      .alerta {border-left: 5px solid #b91c1c;}
    "))
  ),

  h2("Diagnóstico de conteo electoral"),
  p("Sube un Excel con una hoja nacional y una hoja distrital. Esta versión permite marcar excepciones: distritos donde la columna de votos es total final conocido, aunque el resto de distritos use votos contados."),

  layout_sidebar(
    sidebar = sidebar(
      fileInput("archivo", "Archivo Excel (.xlsx)", accept = c(".xlsx", ".xls")),
      uiOutput("selector_hojas"),
      radioButtons(
        "tipo_votos_distritos",
        "Regla general para la columna de votos de la hoja distritos:",
        choices = c(
          "Votos contados hasta ahora" = "contados",
          "Total final conocido o estimado" = "totales"
        ),
        selected = "contados"
      ),
      textInput(
        "total_validos",
        "Total nacional de votos válidos final u oficial, opcional",
        value = "",
        placeholder = "Ejemplo: 18000000"
      ),
      numericInput("n_sim", "Número de simulaciones", value = 10000, min = 1000, max = 100000, step = 1000),
      numericInput("n_eff", "Máximo n efectivo por distrito", value = 5000, min = 100, max = 100000, step = 500),
      numericInput("prob_corte", "Corte de probabilidad para estabilización", value = 0.995, min = 0.90, max = 0.9999, step = 0.001),
      numericInput("margen_pend", "Margen mínimo entre lo necesario y lo esperado", value = 0.03, min = 0, max = 0.20, step = 0.005),
      actionButton("calcular", "Calcular", class = "btn-primary")
    ),

    div(
      uiOutput("mensaje_estado"),

      div(class = "grid",
          div(class = "bloque", div(class = "etiqueta", "Conteo nacional válido"), div(class = "valor", textOutput("kpi_conteo"))),
          div(class = "bloque", div(class = "etiqueta", "Líder"), div(class = "valor", textOutput("kpi_lider"))),
          div(class = "bloque", div(class = "etiqueta", "Segundo necesita en pendientes"), div(class = "valor", textOutput("kpi_necesita"))),
          div(class = "bloque", div(class = "etiqueta", "Probabilidad simulada del ganador proyectado"), div(class = "valor", textOutput("kpi_prob")))
      ),

      div(class = "bloque",
          h4("Validación contra total nacional de votos válidos"),
          tableOutput("tabla_validacion")
      ),

      div(class = "bloque",
          h4("Resumen nacional"),
          tableOutput("tabla_nacional")
      ),

      div(class = "bloque",
          h4("Proyección territorial al 100%"),
          tableOutput("tabla_territorio"),
          plotOutput("grafico_pendientes", height = "360px")
      ),

      div(class = "bloque",
          h4("Simulación Monte Carlo al 100%"),
          tableOutput("tabla_simulacion"),
          plotOutput("grafico_simulacion", height = "300px")
      ),

      div(class = "bloque",
          h4("Datos nacionales válidos que interpretó la app"),
          p("Si el Excel contiene filas futuras con K=0 y R=0, no aparecerán aquí."),
          DTOutput("tabla_nacional_leida")
      ),

      div(class = "bloque",
          h4("Detalle distrital"),
          p("La columna tipo_votos_fila muestra si la fila fue tratada como votos contados o como total conocido."),
          DTOutput("tabla_distritos")
      )
    )
  )
)

server <- function(input, output, session) {

  hojas <- reactive({
    req(input$archivo)
    readxl::excel_sheets(input$archivo$datapath)
  })

  output$selector_hojas <- renderUI({
    req(hojas())
    lista <- hojas()
    tagList(
      selectInput("hoja_nacional", "Hoja nacional", choices = lista, selected = lista[1]),
      selectInput("hoja_distritos", "Hoja distritos", choices = lista, selected = ifelse(length(lista) >= 2, lista[2], lista[1]))
    )
  })

  datos <- eventReactive(input$calcular, {
    req(input$archivo, input$hoja_nacional, input$hoja_distritos)
    leer_datos(
      archivo = input$archivo$datapath,
      hoja_nacional = input$hoja_nacional,
      hoja_distritos = input$hoja_distritos,
      tipo_votos_distritos = input$tipo_votos_distritos,
      distritos_total_conocido = parse_lista_distritos(input$distritos_total_conocido)
    )
  })

  resultado <- reactive({
    d <- datos()
    analizar_eleccion(
      nacional = d$nacional,
      distritos = d$distritos,
      n_sim = num_input(input$n_sim),
      n_efectivo_max = num_input(input$n_eff),
      prob_corte = num_input(input$prob_corte),
      margen_pendiente = num_input(input$margen_pend),
      total_validos_nacional = num_input(input$total_validos)
    )
  })

  output$mensaje_estado <- renderUI({
    req(resultado())
    res <- resultado()
    div(class = "bloque",
        div(class = "estado", res$diagnostico$estado[1]),
        p(class = "subtexto", res$diagnostico$interpretacion[1])
    )
  })

  output$kpi_conteo <- renderText({
    req(resultado())
    formato_pct(resultado()$nacional$conteo_pct[1])
  })

  output$kpi_lider <- renderText({
    req(resultado())
    resultado()$diagnostico$lider[1]
  })

  output$kpi_necesita <- renderText({
    req(resultado())
    formato_pct(resultado()$diagnostico$pct_necesario_segundo_pendiente[1])
  })

  output$kpi_prob <- renderText({
    req(resultado())
    formato_pct(resultado()$diagnostico$prob_ganador_final_estimado[1])
  })

  output$tabla_validacion <- renderTable({
    req(resultado())
    x <- resultado()$validacion
    data.frame(
      indicador = c(
        "Total nacional válido informado",
        "Total distrital estimado por la app",
        "Diferencia absoluta",
        "Diferencia porcentual",
        "Diagnóstico"
      ),
      valor = c(
        formato_numero(x$total_validos_nacional),
        formato_numero(x$total_distrital_estimado),
        formato_numero(x$diferencia_absoluta),
        formato_pct(x$diferencia_pct),
        x$advertencia
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$tabla_nacional <- renderTable({
    req(resultado())
    x <- resultado()$nacional
    data.frame(
      indicador = c(
        "Conteo nacional válido",
        "Líder",
        "Segundo",
        "Diferencia en voto contado",
        "Ventaja acumulada sobre total",
        "Pendiente nacional",
        "¿Matemáticamente irreversible?",
        "Conteo mínimo requerido para irreversibilidad",
        "Porcentaje que necesita el segundo en lo pendiente"
      ),
      valor = c(
        formato_pct(x$conteo_pct),
        x$lider,
        x$segundo,
        formato_pct(x$diferencia_contada),
        formato_pct(x$ventaja_sobre_total),
        formato_pct(x$pendiente_total),
        ifelse(x$matematicamente_irreversible, "Sí", "No"),
        formato_pct(x$conteo_min_irreversible),
        formato_pct(x$pct_necesario_segundo_pendiente)
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$tabla_territorio <- renderTable({
    req(resultado())
    x <- resultado()$territorio$resumen
    data.frame(
      indicador = c(
        "Avance ponderado distrital",
        "Votos contados",
        "Total final estimado",
        "Votos pendientes estimados",
        "K actual territorial",
        "R actual territorial",
        "K final estimado al 100%",
        "R final estimado al 100%",
        "Ganador final estimado",
        "Diferencia final estimada",
        "K esperado entre votos pendientes",
        "R esperado entre votos pendientes",
        "K necesita en pendientes territoriales",
        "R necesita en pendientes territoriales"
      ),
      valor = c(
        formato_pct(x$avance_ponderado_distrital),
        formato_numero(x$votos_contados),
        formato_numero(x$votos_totales_estimados),
        formato_numero(x$votos_pendientes_estimados),
        formato_pct(x$k_actual_territorial_pct),
        formato_pct(x$r_actual_territorial_pct),
        formato_pct(x$k_final_estimado_pct),
        formato_pct(x$r_final_estimado_pct),
        x$ganador_final_estimado,
        formato_pct(x$diferencia_final_estimado),
        formato_pct(x$k_pendiente_esperado_pct),
        formato_pct(x$r_pendiente_esperado_pct),
        formato_pct(x$k_necesita_pendiente_territorial_pct),
        formato_pct(x$r_necesita_pendiente_territorial_pct)
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$tabla_simulacion <- renderTable({
    req(resultado())
    x <- resultado()$simulacion$resumen
    data.frame(
      indicador = c(
        "Simulaciones",
        "Probabilidad K",
        "Probabilidad R",
        "K media final",
        "K intervalo 95%",
        "R media final",
        "R intervalo 95%"
      ),
      valor = c(
        format(x$n_sim, big.mark = "."),
        formato_pct(x$prob_gana_k),
        formato_pct(x$prob_gana_r),
        formato_pct(x$k_pct_media),
        paste0(formato_pct(x$k_pct_p025), " - ", formato_pct(x$k_pct_p975)),
        formato_pct(x$r_pct_media),
        paste0(formato_pct(x$r_pct_p025), " - ", formato_pct(x$r_pct_p975))
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$grafico_pendientes <- renderPlot({
    req(resultado())
    d <- resultado()$territorio$detalle
    ggplot(d, aes(x = reorder(distrito, pendientes), y = pendientes)) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Distrito",
        y = "Votos pendientes estimados",
        title = "Votos pendientes estimados por distrito"
      ) +
      theme_minimal()
  })

  output$grafico_simulacion <- renderPlot({
    req(resultado())
    m <- resultado()$simulacion$muestras
    ggplot(m, aes(x = k_pct)) +
      geom_histogram(bins = 40) +
      geom_vline(xintercept = 0.5, linetype = "dashed") +
      scale_x_continuous(labels = scales::percent) +
      labs(
        x = "Porcentaje final simulado de K",
        y = "Frecuencia",
        title = "Distribución simulada del resultado final de K al 100%"
      ) +
      theme_minimal()
  })

  output$tabla_nacional_leida <- renderDT({
    req(datos())
    d <- datos()$nacional
    mostrar <- d %>%
      mutate(
        conteo_pct = formato_pct(conteo_pct),
        k_pct = formato_pct(k_pct),
        r_pct = formato_pct(r_pct)
      )
    datatable(mostrar, options = list(pageLength = 15, scrollX = TRUE))
  })

  output$tabla_distritos <- renderDT({
    req(resultado())
    d <- resultado()$territorio$detalle
    mostrar <- d %>%
      mutate(
        votos_reportados = round(votos_reportados),
        votos_contados = round(votos_contados),
        votos_totales_estimados = round(votos_totales_estimados),
        pendientes = round(pendientes),
        avance = formato_pct(avance),
        k_share = formato_pct(k_share),
        r_share = formato_pct(r_share),
        k_final_estimado_pct = formato_pct(k_final_estimado / (k_final_estimado + r_final_estimado)),
        r_final_estimado_pct = formato_pct(r_final_estimado / (k_final_estimado + r_final_estimado))
      ) %>%
      select(
        distrito,
        tipo_votos_fila,
        votos_reportados,
        votos_contados,
        votos_totales_estimados,
        pendientes,
        avance,
        k_share,
        r_share,
        k_final_estimado_pct,
        r_final_estimado_pct
      )

    datatable(mostrar, options = list(pageLength = 10, scrollX = TRUE))
  })
}

shinyApp(ui, server)
