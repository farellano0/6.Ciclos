# Calculadora de correlaciones de Pearson Winsorizado
# Aplicación Shiny basada en la paquetería wrs2

library(shiny)
library(readxl)
library(wrs2)
library(writexl)

ui <- fluidPage(
  titlePanel("Calculadora de correlaciones de Pearson Winsorizado"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "data_file",
        "Cargar archivo Excel",
        accept = c(".xls", ".xlsx"),
        buttonLabel = "Explorar"
      ),
      uiOutput("var_selector"),
      sliderInput(
        "winsor_prop",
        "Proporción de winsorización",
        min = 0,
        max = 0.4,
        value = 0.2,
        step = 0.05
      ),
      numericInput(
        "decimals",
        "Cantidad de decimales a mostrar",
        value = 3,
        min = 0,
        max = 6
      ),
      checkboxInput(
        "apa_style",
        "Formato APA (sin cero a la izquierda)",
        value = TRUE
      ),
      actionButton("compute", "Calcular"),
      downloadButton("download_results", "Descargar resultados (Excel)")
    ),
    mainPanel(
      h3("Matriz de correlaciones winsorizadas"),
      tableOutput("cor_table"),
      verbatimTextOutput("status_message")
    )
  )
)

server <- function(input, output, session) {
  raw_data <- reactive({
    req(input$data_file)
    read_excel(input$data_file$datapath)
  })

  output$var_selector <- renderUI({
    req(raw_data())
    numeric_vars <- names(Filter(is.numeric, raw_data()))

    if (length(numeric_vars) < 2) {
      return(helpText("El archivo necesita al menos dos variables numéricas para correlacionar."))
    }

    selectizeInput(
      "selected_vars",
      "Variables a correlacionar",
      choices = numeric_vars,
      selected = numeric_vars,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    )
  })

  formatted_correlation <- eventReactive(input$compute, {
    data <- raw_data()
    req(input$selected_vars)

    vars <- input$selected_vars
    validate(need(length(vars) >= 2, "Seleccione al menos dos variables."))

    win_prop <- input$winsor_prop

    cor_matrix <- matrix(NA_real_, nrow = length(vars), ncol = length(vars),
                         dimnames = list(vars, vars))

    for (i in seq_along(vars)) {
      for (j in i:length(vars)) {
        x <- data[[vars[[i]]]]
        y <- data[[vars[[j]]]]
        cor_value <- tryCatch(
          wincor(x, y, tr = win_prop),
          error = function(e) NA_real_
        )
        cor_matrix[i, j] <- cor_value
        cor_matrix[j, i] <- cor_value
      }
    }

    diag(cor_matrix) <- 1

    formatter <- function(x) {
      formatted <- formatC(x, digits = input$decimals, format = "f")
      if (isTRUE(input$apa_style)) {
        formatted <- sub("^0\\.", ".", formatted)
        formatted <- sub("^-0\\.", "-.", formatted)
      }
      formatted
    }

    data.frame(
      Variable = vars,
      cor_matrix |>
        apply(2, formatter),
      check.names = FALSE
    )
  })

  output$cor_table <- renderTable({
    formatted_correlation()
  }, rownames = FALSE)

  output$status_message <- renderPrint({
    if (is.null(input$data_file)) {
      "Cargue un archivo Excel para iniciar."
    } else if (is.null(input$selected_vars) || length(input$selected_vars) < 2) {
      "Seleccione al menos dos variables numéricas."
    } else {
      paste0(
        "Winsorización: ", sprintf("%d%%", round(input$winsor_prop * 100)),
        " | Decimales: ", input$decimals,
        if (isTRUE(input$apa_style)) " | Formato APA activado" else ""
      )
    }
  })

  output$download_results <- downloadHandler(
    filename = function() {
      "correlaciones_winsorizadas.xlsx"
    },
    content = function(file) {
      req(formatted_correlation())
      writexl::write_xlsx(formatted_correlation(), path = file)
    }
  )
}

shinyApp(ui, server)
