# app.R - Shiny app completa per confronto tra dati originali e sintetici
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)
library(shinydashboard)
library(shinyWidgets)
library(forcats)
library(gridExtra)

# Dataset predefiniti
data_iris <- iris
data_mtcars <- mtcars
data_diamonds <- ggplot2::diamonds[sample(nrow(ggplot2::diamonds), 1000), ]

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Analisi e Confronto Dati"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dataset", tabName = "dataset", icon = icon("database")),
      menuItem("Visualizzazione", tabName = "visualize", icon = icon("chart-line")),
      menuItem("Statistiche", tabName = "stats", icon = icon("calculator")),
      menuItem("Dati Sintetici", tabName = "synthetic", icon = icon("random"))
    )
  ),
  dashboardBody(
    tabItems(
      # Dataset tab
      tabItem(tabName = "dataset",
              fluidRow(
                box(
                  title = "Selezione Dataset",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  radioButtons("dataSource", "Sorgente dati:",
                               choices = c("Dataset predefinito" = "predefined",
                                           "Carica file CSV" = "upload"),
                               selected = "predefined"),
                  conditionalPanel(
                    condition = "input.dataSource == 'predefined'",
                    selectInput("dataset", "Seleziona dataset:",
                                choices = c("Iris" = "iris",
                                            "MTCars" = "mtcars",
                                            "Diamonds (sample)" = "diamonds",
                                            "CSV Esempio" = "sample"))
                  ),
                  conditionalPanel(
                    condition = "input.dataSource == 'upload'",
                    fileInput("fileUpload", "Carica file CSV",
                              accept = c("text/csv", 
                                         "text/comma-separated-values,text/plain", 
                                         ".csv")),
                    checkboxInput("header", "Il file ha l'intestazione", TRUE),
                    radioButtons("sep", "Separatore:",
                                 choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                                 selected = ","),
                    radioButtons("dec", "Decimale:",
                                 choices = c(Punto = ".", Virgola = ","),
                                 selected = ".")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Anteprima Dati",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("dataTable")
                )
              ),
              fluidRow(
                box(
                  title = "Informazioni Dataset",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  verbatimTextOutput("dataInfo")
                ),
                box(
                  title = "Struttura delle Variabili",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  verbatimTextOutput("dataStructure")
                )
              )
      ),
      
      # Visualizzazione tab
      tabItem(tabName = "visualize",
              fluidRow(
                box(
                  title = "Impostazioni Grafico",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  selectInput("plotType", "Tipo di grafico:",
                              choices = c("Scatter Plot" = "scatter",
                                          "Istogramma" = "histogram",
                                          "Boxplot" = "boxplot",
                                          "Barre" = "bar",
                                          "Densità" = "density")),
                  uiOutput("xAxisInput"),
                  uiOutput("yAxisInput"),
                  uiOutput("colorInput"),
                  uiOutput("facetInput"),
                  actionButton("updatePlot", "Aggiorna Grafico", 
                               class = "btn-success")
                ),
                box(
                  title = "Grafico",
                  status = "info",
                  solidHeader = TRUE,
                  width = 9,
                  plotlyOutput("plot", height = "600px")
                )
              )
      ),
      
      # Statistiche tab
      tabItem(tabName = "stats",
              fluidRow(
                tabBox(
                  id = "statsTabBox",
                  width = 12,
                  tabPanel("Statistiche Numeriche", 
                           fluidRow(
                             column(3,
                                    selectInput("numericStatsColumn", "Seleziona colonna numerica:", choices = NULL),
                                    actionButton("calculateNumericStats", "Calcola statistiche", class = "btn-primary")
                             ),
                             column(9,
                                    verbatimTextOutput("numericStatsSummary"),
                                    plotOutput("numericStatsPlot", height = "300px")
                             )
                           )
                  ),
                  tabPanel("Statistiche Categoriali", 
                           fluidRow(
                             column(3,
                                    selectInput("categoricalStatsColumn", "Seleziona colonna categoriale:", choices = NULL),
                                    actionButton("calculateCatStats", "Calcola statistiche", class = "btn-primary")
                             ),
                             column(9,
                                    verbatimTextOutput("categoricalStatsSummary"),
                                    plotOutput("categoricalStatsPlot", height = "300px")
                             )
                           )
                  ),
                  tabPanel("Correlazione", 
                           plotOutput("corrPlot", height = "500px"))
                )
              )
      ),
      
      # Dati Sintetici tab
      tabItem(tabName = "synthetic",
              fluidRow(
                box(
                  title = "Generazione Dati Sintetici",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  selectInput("syntheticMethod", "Metodo di generazione:",
                              choices = c("Campionamento con reinserimento" = "bootstrap",
                                          "Permutazione per colonna" = "permutation",
                                          "Perturbazione valori originali" = "perturbation")),
                  numericInput("syntheticSize", "Numero di record sintetici:", 
                               value = 100, min = 10, max = 10000),
                  checkboxGroupInput("includeColumnTypes", "Tipi di variabili da includere:",
                                     choices = c("Numeriche" = "numeric", 
                                                 "Categoriali" = "categorical"),
                                     selected = c("numeric", "categorical")),
                  conditionalPanel(
                    condition = "input.syntheticMethod == 'perturbation'",
                    sliderInput("numericNoisePct", "Livello di rumore per variabili numeriche (%):", 
                                min = 0, max = 50, value = 10)
                  ),
                  actionButton("generateSynthetic", "Genera Dati Sintetici", 
                               class = "btn-success"),
                  hr(),
                  conditionalPanel(
                    condition = "output.syntheticDataGenerated == true",
                    downloadButton("downloadSynthetic", "Scarica dati sintetici", class = "btn-info")
                  )
                ),
                box(
                  title = "Anteprima Dati Sintetici",
                  status = "info",
                  solidHeader = TRUE,
                  width = 8,
                  DTOutput("syntheticPreview")
                )
              ),
              fluidRow(
                tabBox(
                  id = "compareTabBox",
                  width = 12,
                  tabPanel("Confronto Variabili Numeriche", 
                           fluidRow(
                             column(3,
                                    selectInput("compareNumericColumn", "Seleziona variabile numerica:", choices = NULL),
                                    actionButton("updateNumericCompare", "Aggiorna confronto", class = "btn-primary")
                             ),
                             column(9,
                                    plotOutput("compareNumericPlot", height = "400px"),
                                    verbatimTextOutput("compareNumericStats")
                             )
                           )
                  ),
                  tabPanel("Confronto Variabili Categoriali", 
                           fluidRow(
                             column(3,
                                    selectInput("compareCategoricalColumn", "Seleziona variabile categoriale:", choices = NULL),
                                    selectInput("catVisualizationType", "Tipo di visualizzazione:",
                                                choices = c("Frequenze assolute" = "count", 
                                                            "Frequenze relative (%)" = "percent")),
                                    actionButton("updateCategoricalCompare", "Aggiorna confronto", class = "btn-primary")
                             ),
                             column(9,
                                    plotOutput("compareCategoricalPlot", height = "400px"),
                                    verbatimTextOutput("compareCategoricalStats")
                             )
                           )
                  ),
                  tabPanel("Tavola di contingenza", 
                           fluidRow(
                             column(3,
                                    selectInput("contingencyVar1", "Prima variabile:", choices = NULL),
                                    selectInput("contingencyVar2", "Seconda variabile:", choices = NULL),
                                    radioButtons("contingencySource", "Dati da analizzare:",
                                                 choices = c("Originali" = "original", 
                                                             "Sintetici" = "synthetic")),
                                    actionButton("calculateContingency", "Calcola", class = "btn-primary")
                             ),
                             column(9,
                                    verbatimTextOutput("contingencyTable"),
                                    plotOutput("contingencyPlot", height = "350px")
                             )
                           )
                  ),
                  tabPanel("Tabella comparativa", 
                           DTOutput("statsComparisonTable"))
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Caricamento del dataset
  data <- reactive({
    if (input$dataSource == "predefined") {
      switch(input$dataset,
             "iris" = data_iris,
             "mtcars" = data_mtcars,
             "diamonds" = data_diamonds,
             "sample" = {
               # Carica il file sample.csv dalla directory di lavoro
               tryCatch({
                 df <- read.csv("sample.csv", header = TRUE, sep = ",", dec = ".")
                 # Converti le colonne appropriate in fattori
                 df <- transform_data_types(df)
                 df
               }, error = function(e) {
                 # Se il file non esiste, restituisci un dataframe vuoto con la struttura corretta
                 data.frame(
                   ID = integer(),
                   Age = integer(),
                   Gender = character(),
                   City = character(),
                   Income = numeric(),
                   Score = numeric(),
                   Last_Purchase_Date = character(),
                   stringsAsFactors = FALSE
                 )
               })
             }
      )
    } else {
      req(input$fileUpload)
      tryCatch({
        df <- read.csv(input$fileUpload$datapath,
                       header = input$header,
                       sep = input$sep,
                       dec = input$dec)
        # Converti le colonne appropriate in fattori
        df <- transform_data_types(df)
        df
      }, error = function(e) {
        showNotification("Errore nel caricamento del file", type = "error")
        return(NULL)
      })
    }
  })
  
  # Funzione per ottimizzare i tipi di dati
  transform_data_types <- function(df) {
    for (col_name in colnames(df)) {
      col <- df[[col_name]]
      
      # Se è un carattere, verifica se è categoriale
      if (is.character(col)) {
        unique_vals <- unique(col)
        # Se ha pochi valori unici rispetto alle righe, lo trasformiamo in fattore
        if (length(unique_vals) <= min(20, nrow(df) * 0.3)) {
          df[[col_name]] <- as.factor(col)
        }
      }
      
      # Converti potenziali date
      if (is.character(col) && any(grepl("^\\d{2,4}[/-]\\d{1,2}[/-]\\d{1,2}$", col))) {
        tryCatch({
          df[[col_name]] <- as.Date(col)
        }, error = function(e) {
          # Non è una data, lasciamo com'è
        })
      }
    }
    return(df)
  }
  
  # Anteprima dati
  output$dataTable <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Informazioni sul dataset
  output$dataInfo <- renderPrint({
    req(data())
    df <- data()
    
    cat("Numero di righe:", nrow(df), "\n")
    cat("Numero di colonne:", ncol(df), "\n\n")
    
    num_cols <- sum(sapply(df, is.numeric))
    cat_cols <- sum(sapply(df, is.factor) | sapply(df, is.character))
    date_cols <- sum(sapply(df, function(x) inherits(x, "Date")))
    other_cols <- ncol(df) - num_cols - cat_cols - date_cols
    
    cat("Tipi di colonne:\n")
    cat("- Numeriche:", num_cols, "\n")
    cat("- Categoriali:", cat_cols, "\n")
    cat("- Date:", date_cols, "\n")
    cat("- Altro:", other_cols, "\n\n")
    
    cat("Riassunto dataset:\n")
    summary(df)
  })
  
  # Struttura del dataset
  output$dataStructure <- renderPrint({
    req(data())
    str(data())
  })
  
  # Aggiungi colonna tipo per ogni variabile
  get_column_types <- reactive({
    req(data())
    df <- data()
    
    types <- sapply(df, function(x) {
      if (is.numeric(x)) {
        return("numeric")
      } else if (is.factor(x) || is.character(x)) {
        return("categorical")
      } else if (inherits(x, "Date")) {
        return("date")
      } else {
        return("other")
      }
    })
    
    return(types)
  })
  
  # Liste di colonne per tipo
  get_numeric_columns <- reactive({
    req(data())
    column_types <- get_column_types()
    names(column_types[column_types == "numeric"])
  })
  
  get_categorical_columns <- reactive({
    req(data())
    column_types <- get_column_types()
    names(column_types[column_types == "categorical"])
  })
  
  # UI dinamici per la visualizzazione
  output$xAxisInput <- renderUI({
    req(data())
    cols <- colnames(data())
    
    if (input$plotType %in% c("histogram", "density")) {
      numeric_cols <- get_numeric_columns()
      selectInput("xAxis", "Variabile X:", choices = numeric_cols)
    } else if (input$plotType == "boxplot") {
      numeric_cols <- get_numeric_columns()
      selectInput("xAxis", "Variabile X (gruppi):", choices = c(get_categorical_columns(), get_numeric_columns()))
    } else {
      selectInput("xAxis", "Variabile X:", choices = cols)
    }
  })
  
  output$yAxisInput <- renderUI({
    req(data())
    if (input$plotType %in% c("scatter", "boxplot")) {
      numeric_cols <- get_numeric_columns()
      selectInput("yAxis", "Variabile Y:", choices = numeric_cols)
    } else if (input$plotType == "bar") {
      selectInput("yAxis", "Variabile Y (conteggio):", choices = colnames(data()))
    } else {
      # Per istogramma e densità non serve y-axis
      return(NULL)
    }
  })
  
  output$colorInput <- renderUI({
    req(data())
    cols <- colnames(data())
    selectInput("colorVar", "Variabile colore (opzionale):", 
                choices = c("Nessuna" = "", cols), 
                selected = "")
  })
  
  output$facetInput <- renderUI({
    req(data())
    categorical_cols <- get_categorical_columns()
    selectInput("facetVar", "Variabile facet (opzionale):", 
                choices = c("Nessuna" = "", categorical_cols), 
                selected = "")
  })
  
  # Generazione del grafico
  output$plot <- renderPlotly({
    req(data(), input$xAxis)
    req(input$updatePlot)
    
    isolate({
      df <- data()
      p <- ggplot(df)
      
      if (input$plotType == "scatter") {
        req(input$yAxis)
        p <- p + geom_point(aes_string(x = input$xAxis, y = input$yAxis))
      } else if (input$plotType == "histogram") {
        p <- p + geom_histogram(aes_string(x = input$xAxis), bins = 30)
      } else if (input$plotType == "density") {
        p <- p + geom_density(aes_string(x = input$xAxis))
      } else if (input$plotType == "boxplot") {
        req(input$yAxis)
        p <- p + geom_boxplot(aes_string(x = input$xAxis, y = input$yAxis))
      } else if (input$plotType == "bar") {
        p <- p + geom_bar(aes_string(x = input$xAxis))
      }
      
      # Aggiungi colore se selezionato
      if (!is.null(input$colorVar) && input$colorVar != "") {
        p <- p + aes_string(color = input$colorVar, fill = input$colorVar)
      }
      
      # Aggiungi facet se selezionato
      if (!is.null(input$facetVar) && input$facetVar != "") {
        p <- p + facet_wrap(as.formula(paste("~", input$facetVar)))
      }
      
      # Aggiungi tema e titoli
      p <- p + theme_minimal() +
        labs(title = paste("Grafico:", input$plotType),
             x = input$xAxis,
             y = if(input$plotType %in% c("scatter", "boxplot")) input$yAxis else "Conteggio")
      
      ggplotly(p)
    })
  })
  
  # Aggiornamento delle liste di colonne per statistiche
  observe({
    req(data())
    num_cols <- get_numeric_columns()
    cat_cols <- get_categorical_columns()
    
    updateSelectInput(session, "numericStatsColumn", choices = num_cols)
    updateSelectInput(session, "categoricalStatsColumn", choices = cat_cols)
    updateSelectInput(session, "contingencyVar1", choices = colnames(data()))
    updateSelectInput(session, "contingencyVar2", choices = colnames(data()))
  })
  
  # Calcolo statistiche numeriche
  observeEvent(input$calculateNumericStats, {
    req(data(), input$numericStatsColumn)
    
    output$numericStatsSummary <- renderPrint({
      column_data <- data()[[input$numericStatsColumn]]
      
      cat("Statistiche per", input$numericStatsColumn, ":\n\n")
      cat("N. osservazioni:", length(column_data), "\n")
      cat("N. valori mancanti:", sum(is.na(column_data)), "\n\n")
      
      cat("Distribuzione:\n")
      cat("- Minimo:", min(column_data, na.rm = TRUE), "\n")
      cat("- 1° Quartile:", quantile(column_data, 0.25, na.rm = TRUE), "\n")
      cat("- Media:", mean(column_data, na.rm = TRUE), "\n")
      cat("- Mediana:", median(column_data, na.rm = TRUE), "\n")
      cat("- 3° Quartile:", quantile(column_data, 0.75, na.rm = TRUE), "\n")
      cat("- Massimo:", max(column_data, na.rm = TRUE), "\n\n")
      
      cat("Variabilità:\n")
      cat("- Deviazione standard:", sd(column_data, na.rm = TRUE), "\n")
      cat("- Varianza:", var(column_data, na.rm = TRUE), "\n")
      cat("- Range:", diff(range(column_data, na.rm = TRUE)), "\n")
      cat("- IQR:", IQR(column_data, na.rm = TRUE), "\n")
    })
    
    output$numericStatsPlot <- renderPlot({
      column_data <- data()[[input$numericStatsColumn]]
      
      p <- ggplot(data(), aes_string(x = input$numericStatsColumn)) +
        geom_histogram(aes(y = ..density..), fill = "steelblue", color = "black", bins = 30) +
        geom_density(alpha = 0.2, fill = "red") +
        geom_vline(xintercept = mean(column_data, na.rm = TRUE), 
                   color = "red", linetype = "dashed", size = 1) +
        geom_vline(xintercept = median(column_data, na.rm = TRUE), 
                   color = "green", linetype = "dashed", size = 1) +
        theme_minimal() +
        labs(title = paste("Distribuzione di", input$numericStatsColumn),
             subtitle = "Linea rossa = media, Linea verde = mediana",
             x = input$numericStatsColumn, y = "Densità")
      
      # Aggiungiamo un boxplot
      p2 <- ggplot(data(), aes_string(y = input$numericStatsColumn)) +
        geom_boxplot(fill = "steelblue") +
        coord_flip() +
        theme_minimal() +
        labs(title = "Boxplot", x = "")
      
      # Combina i grafici
      gridExtra::grid.arrange(p, p2, ncol = 1, heights = c(2, 1))
    })
  })
  
  # Calcolo statistiche categoriali
  observeEvent(input$calculateCatStats, {
    req(data(), input$categoricalStatsColumn)
    
    output$categoricalStatsSummary <- renderPrint({
      column_data <- data()[[input$categoricalStatsColumn]]
      
      # Convertiamo in fattore se è una stringa
      if (is.character(column_data)) {
        column_data <- as.factor(column_data)
      }
      
      freq_table <- table(column_data, useNA = "ifany")
      prop_table <- prop.table(freq_table) * 100
      
      cat("Statistiche per", input$categoricalStatsColumn, ":\n\n")
      cat("N. osservazioni:", length(column_data), "\n")
      cat("N. valori mancanti:", sum(is.na(column_data)), "\n")
      cat("N. categorie uniche:", length(levels(column_data)), "\n\n")
      
      cat("Frequenze:\n")
      print(freq_table)
      cat("\n")
      
      cat("Percentuali:\n")
      print(round(prop_table, 2))
      cat("%\n\n")
      
      cat("Moda:", names(which.max(freq_table)), "\n")
    })
    
    output$categoricalStatsPlot <- renderPlot({
      column_data <- data()[[input$categoricalStatsColumn]]
      
      # Convertiamo in fattore se è una stringa
      if (is.character(column_data)) {
        column_data <- as.factor(column_data)
      }
      
      # Creiamo un dataframe per la visualizzazione
      plot_df <- data.frame(
        categoria = column_data
      )
      
      # Se ci sono troppi livelli, mostriamo solo i più frequenti
      if (length(levels(as.factor(column_data))) > 10) {
        plot_df$categoria <- forcats::fct_lump(plot_df$categoria, n = 10)
      }
      
      # Grafico a barre
      ggplot(plot_df, aes(x = categoria)) +
        geom_bar(aes(y = ..count.., fill = ..count..)) +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste("Distribuzione di", input$categoricalStatsColumn),
             x = input$categoricalStatsColumn, y = "Frequenza",
             fill = "Frequenza")
    })
  })
  
  # Grafico di correlazione
  output$corrPlot <- renderPlot({
    req(data())
    
    # Seleziona solo le colonne numeriche
    num_data <- data()[, get_numeric_columns(), drop = FALSE]
    
    if (ncol(num_data) > 1) {
      corr_matrix <- cor(num_data, use = "pairwise.complete.obs")
      
      # Prepara il dataframe per ggplot
      corr_df <- as.data.frame(as.table(corr_matrix))
      names(corr_df) <- c("Var1", "Var2", "Correlation")
      
      # Crea il grafico di correlazione
      ggplot(corr_df, aes(x = Var1, y = Var2, fill = Correlation)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(-1, 1)) +
        geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Matrice di Correlazione",
             x = "", y = "")
    } else {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Servono almeno due variabili numeriche per creare una matrice di correlazione") +
        theme_void()
    }
  })
  
  # Dati sintetici
  synthetic_data <- reactiveVal(NULL)
  
  # Flag per indicare se i dati sintetici sono stati generati
  output$syntheticDataGenerated <- reactive({
    return(!is.null(synthetic_data()))
  })
  outputOptions(output, "syntheticDataGenerated", suspendWhenHidden = FALSE)
  
  # Aggiorna le opzioni del menu per le colonne di confronto
  observe({
    req(data())
    
    num_cols <- get_numeric_columns()
    cat_cols <- get_categorical_columns()
    
    updateSelectInput(session, "compareNumericColumn", choices = num_cols)
    updateSelectInput(session, "compareCategoricalColumn", choices = cat_cols)
  })
  
  # Genera dati sintetici
  observeEvent(input$generateSynthetic, {
    req(data())
    
    # Ottieni i dati originali
    original_data <- data()
    synth_size <- min(input$syntheticSize, 10000)  # Limita la dimensione per sicurezza
    
    # Determina quali colonne includere
    column_types <- get_column_types()
    columns_to_include <- character(0)
    
    if ("numeric" %in% input$includeColumnTypes) {
      columns_to_include <- c(columns_to_include, names(column_types[column_types == "numeric"]))
    }
    
    if ("categorical" %in% input$includeColumnTypes) {
      columns_to_include <- c(columns_to_include, names(column_types[column_types == "categorical"]))
    }
    
    # Verifica che ci siano colonne selezionate
    if (length(columns_to_include) == 0) {
      showNotification("Seleziona almeno un tipo di variabile da includere", type = "warning")
      return()
    }
    
    # Seleziona le colonne da includere
    df_subset <- original_data[, columns_to_include, drop = FALSE]
    
    # Inizializza il dataframe sintetico
    synth_df <- NULL
    
    # Implementa il metodo selezionato
    if (input$syntheticMethod == "bootstrap") {
      # Campionamento con reinserimento dalle righe originali
      indices <- sample(1:nrow(original_data), size = synth_size, replace = TRUE)
      synth_df <- df_subset[indices, , drop = FALSE]
      
    } else if (input$syntheticMethod == "perturbation") {
      # Perturbazione dei valori originali
      # Prima campiona le righe come nel bootstrap
      indices <- sample(1:nrow(original_data), size = synth_size, replace = TRUE)
      synth_df <- df_subset[indices, , drop = FALSE]
      
      # Poi perturba ogni colonna numerica
      for (col in names(column_types[column_types == "numeric"])) {
        if (col %in% colnames(synth_df)) {
          # Calcoliamo il livello di rumore basato sul range della variabile
          noise_level <- sd(synth_df[[col]], na.rm = TRUE) * (input$numericNoisePct / 100)
          # Aggiungiamo rumore gaussiano
          synth_df[[col]] <- synth_df[[col]] + rnorm(nrow(synth_df), mean = 0, sd = noise_level)
        }
      }
      
    } else if (input$syntheticMethod == "permutation") {
      # Permutazione dei valori per ogni colonna (conserva la distribuzione marginale)
      synth_df <- data.frame(matrix(NA, nrow = synth_size, ncol = length(columns_to_include)))
      colnames(synth_df) <- columns_to_include
      
      for (col in columns_to_include) {
        original_values <- df_subset[[col]]
        # Campiona con reinserimento i valori originali per ogni colonna
        sampled_values <- sample(original_values, size = synth_size, replace = TRUE)
        synth_df[[col]] <- sampled_values
      }
    }
    
    # Memorizza i dati sintetici
    synthetic_data(synth_df)
    showNotification("Dati sintetici generati con successo", type = "message")
  })
  
  # Anteprima dati sintetici
  output$syntheticPreview <- renderDT({
    req(synthetic_data())
    datatable(head(synthetic_data(), 10), options = list(scrollX = TRUE))
  })
  
  # Download dei dati sintetici
  output$downloadSynthetic <- downloadHandler(
    filename = function() {
      paste("dati_sintetici_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(synthetic_data(), file, row.names = FALSE)
    }
  )
  
  # Confronto variabile numerica
  observeEvent(input$updateNumericCompare, {
    req(data(), synthetic_data(), input$compareNumericColumn)
    
    # Verifica che la colonna selezionata esista in entrambi i dataset
    if (!(input$compareNumericColumn %in% colnames(synthetic_data()))) {
      showNotification("Colonna non trovata nei dati sintetici", type = "error")
      return()
    }
    
    output$compareNumericPlot <- renderPlot({
      # Prepara i dati per il confronto
      orig_vals <- data()[[input$compareNumericColumn]]
      synth_vals <- synthetic_data()[[input$compareNumericColumn]]
      
      # Crea un dataframe combinato per il grafico
      combined_df <- data.frame(
        valore = c(orig_vals, synth_vals),
        tipo = c(rep("Originale", length(orig_vals)), rep("Sintetico", length(synth_vals)))
      )
      
      # Crea il grafico di confronto
      p1 <- ggplot(combined_df, aes(x = valore, fill = tipo)) +
        geom_density(alpha = 0.5) +
        theme_minimal() +
        labs(title = paste("Confronto distribuzione di", input$compareNumericColumn),
             subtitle = "Densità",
             x = input$compareNumericColumn, y = "Densità") +
        scale_fill_manual(values = c("Originale" = "blue", "Sintetico" = "red"))
      
      # Aggiungi boxplot per confronto
      p2 <- ggplot(combined_df, aes(x = tipo, y = valore, fill = tipo)) +
        geom_boxplot() +
        theme_minimal() +
        labs(title = "Confronto quartili",
             x = "", y = input$compareNumericColumn) +
        scale_fill_manual(values = c("Originale" = "blue", "Sintetico" = "red"))
      
      # Combina i grafici
      gridExtra::grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))
    })
    
    output$compareNumericStats <- renderPrint({
      # Estrai i valori originali e sintetici
      orig_vals <- data()[[input$compareNumericColumn]]
      synth_vals <- synthetic_data()[[input$compareNumericColumn]]
      
      # Calcola le statistiche
      cat("Statistiche comparative per", input$compareNumericColumn, ":\n\n")
      
      cat("Originale:\n")
      cat("- Media:", mean(orig_vals, na.rm = TRUE), "\n")
      cat("- Mediana:", median(orig_vals, na.rm = TRUE), "\n")
      cat("- Dev. std:", sd(orig_vals, na.rm = TRUE), "\n")
      cat("- Min:", min(orig_vals, na.rm = TRUE), "\n")
      cat("- Max:", max(orig_vals, na.rm = TRUE), "\n\n")
      
      cat("Sintetico:\n")
      cat("- Media:", mean(synth_vals, na.rm = TRUE), "\n")
      cat("- Mediana:", median(synth_vals, na.rm = TRUE), "\n")
      cat("- Dev. std:", sd(synth_vals, na.rm = TRUE), "\n")
      cat("- Min:", min(synth_vals, na.rm = TRUE), "\n")
      cat("- Max:", max(synth_vals, na.rm = TRUE), "\n\n")
      
      # Confronto statistico
      cat("Confronto:\n")
      cat("- Δ Media:", mean(synth_vals, na.rm = TRUE) - mean(orig_vals, na.rm = TRUE), "\n")
      cat("- Δ Mediana:", median(synth_vals, na.rm = TRUE) - median(orig_vals, na.rm = TRUE), "\n")
      cat("- Rapporto Dev. std:", sd(synth_vals, na.rm = TRUE) / sd(orig_vals, na.rm = TRUE), "\n")
      
      # Test di Kolmogorov-Smirnov per confrontare le distribuzioni
      ks_test <- tryCatch(
        ks.test(orig_vals, synth_vals),
        error = function(e) NULL
      )
      
      if (!is.null(ks_test)) {
        cat("\nTest di Kolmogorov-Smirnov:\n")
        cat("- D =", ks_test$statistic, "\n")
        cat("- p-value =", ks_test$p.value, "\n")
        cat("- Interpretazione:", ifelse(ks_test$p.value < 0.05, 
                                         "Le distribuzioni sono significativamente diverse (p < 0.05)",
                                         "Non c'è evidenza di differenze significative tra le distribuzioni"))
      }
    })
  })
  
  # Confronto variabile categoriale
  observeEvent(input$updateCategoricalCompare, {
    req(data(), synthetic_data(), input$compareCategoricalColumn)
    
    # Verifica che la colonna selezionata esista in entrambi i dataset
    if (!(input$compareCategoricalColumn %in% colnames(synthetic_data()))) {
      showNotification("Colonna non trovata nei dati sintetici", type = "error")
      return()
    }
    
    output$compareCategoricalPlot <- renderPlot({
      # Prepara i dati per il confronto
      orig_vals <- data()[[input$compareCategoricalColumn]]
      synth_vals <- synthetic_data()[[input$compareCategoricalColumn]]
      
      # Assicuriamoci che entrambi siano fattori con gli stessi livelli
      if (!is.factor(orig_vals)) orig_vals <- as.factor(orig_vals)
      if (!is.factor(synth_vals)) synth_vals <- as.factor(synth_vals)
      
      # Unifichiamo i livelli
      all_levels <- unique(c(levels(orig_vals), levels(as.factor(synth_vals))))
      orig_vals <- factor(orig_vals, levels = all_levels)
      synth_vals <- factor(synth_vals, levels = all_levels)
      
      # Calcola le frequenze
      if (input$catVisualizationType == "count") {
        # Frequenze assolute
        orig_freq <- table(orig_vals)
        synth_freq <- table(synth_vals)
        
        # Prepara i dati per il grafico
        plot_data <- data.frame(
          Categoria = rep(names(orig_freq), 2),
          Frequenza = c(as.numeric(orig_freq), as.numeric(synth_freq)),
          Tipo = rep(c("Originale", "Sintetico"), each = length(orig_freq))
        )
        
        # Crea il grafico
        ggplot(plot_data, aes(x = Categoria, y = Frequenza, fill = Tipo)) +
          geom_bar(stat = "identity", position = "dodge") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = paste("Confronto frequenze di", input$compareCategoricalColumn),
               x = input$compareCategoricalColumn, y = "Frequenza") +
          scale_fill_manual(values = c("Originale" = "blue", "Sintetico" = "red"))
      } else {
        # Frequenze relative (percentuali)
        orig_pct <- prop.table(table(orig_vals)) * 100
        synth_pct <- prop.table(table(synth_vals)) * 100
        
        # Prepara i dati per il grafico
        plot_data <- data.frame(
          Categoria = rep(names(orig_pct), 2),
          Percentuale = c(as.numeric(orig_pct), as.numeric(synth_pct)),
          Tipo = rep(c("Originale", "Sintetico"), each = length(orig_pct))
        )
        
        # Crea il grafico
        ggplot(plot_data, aes(x = Categoria, y = Percentuale, fill = Tipo)) +
          geom_bar(stat = "identity", position = "dodge") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = paste("Confronto percentuali di", input$compareCategoricalColumn),
               x = input$compareCategoricalColumn, y = "Percentuale (%)") +
          scale_fill_manual(values = c("Originale" = "blue", "Sintetico" = "red"))
      }
    })
    
    output$compareCategoricalStats <- renderPrint({
      # Estrai i valori originali e sintetici
      orig_vals <- data()[[input$compareCategoricalColumn]]
      synth_vals <- synthetic_data()[[input$compareCategoricalColumn]]
      
      # Assicuriamoci che entrambi siano fattori
      if (!is.factor(orig_vals)) orig_vals <- as.factor(orig_vals)
      if (!is.factor(synth_vals)) synth_vals <- as.factor(synth_vals)
      
      # Calcola le frequenze e percentuali
      orig_freq <- table(orig_vals)
      synth_freq <- table(synth_vals)
      orig_pct <- prop.table(orig_freq) * 100
      synth_pct <- prop.table(synth_freq) * 100
      
      # Unisci i risultati
      all_categories <- unique(c(names(orig_freq), names(synth_freq)))
      
      # Crea tabella comparativa
      cat("Confronto distribuzione per", input$compareCategoricalColumn, ":\n\n")
      
      comparison <- data.frame(
        Categoria = all_categories,
        Orig_Freq = numeric(length(all_categories)),
        Orig_Pct = numeric(length(all_categories)),
        Synth_Freq = numeric(length(all_categories)),
        Synth_Pct = numeric(length(all_categories)),
        Delta_Pct = numeric(length(all_categories))
      )
      
      for (i in 1:length(all_categories)) {
        cat <- all_categories[i]
        comparison$Orig_Freq[i] <- ifelse(cat %in% names(orig_freq), orig_freq[cat], 0)
        comparison$Orig_Pct[i] <- ifelse(cat %in% names(orig_pct), orig_pct[cat], 0)
        comparison$Synth_Freq[i] <- ifelse(cat %in% names(synth_freq), synth_freq[cat], 0)
        comparison$Synth_Pct[i] <- ifelse(cat %in% names(synth_pct), synth_pct[cat], 0)
        comparison$Delta_Pct[i] <- comparison$Synth_Pct[i] - comparison$Orig_Pct[i]
      }
      
      # Ordina per frequenza originale decrescente
      comparison <- comparison[order(-comparison$Orig_Freq), ]
      
      # Stampa la tabella
      print(comparison)
      
      # Test Chi-quadrato per verificare se la distribuzione è significativamente diversa
      if (length(all_categories) > 1) {
        # Creiamo tabelle di contingenza con le stesse categorie
        contingency <- matrix(0, nrow = 2, ncol = length(all_categories))
        rownames(contingency) <- c("Originale", "Sintetico")
        colnames(contingency) <- all_categories
        
        for (i in 1:length(all_categories)) {
          cat <- all_categories[i]
          contingency[1, i] <- ifelse(cat %in% names(orig_freq), orig_freq[cat], 0)
          contingency[2, i] <- ifelse(cat %in% names(synth_freq), synth_freq[cat], 0)
        }
        
        # Calcola il test chi-quadrato
        chi_test <- tryCatch(
          chisq.test(contingency),
          error = function(e) NULL,
          warning = function(w) NULL
        )
        
        if (!is.null(chi_test)) {
          cat("\nTest Chi-quadrato:\n")
          cat("- X² =", chi_test$statistic, "\n")
          cat("- p-value =", chi_test$p.value, "\n")
          cat("- Interpretazione:", ifelse(chi_test$p.value < 0.05, 
                                           "Le distribuzioni sono significativamente diverse (p < 0.05)",
                                           "Non c'è evidenza di differenze significative tra le distribuzioni"))
        } else {
          cat("\nImpossibile calcolare il test Chi-quadrato. Potrebbero esserci troppi zeri o frequenze attese troppo piccole.")
        }
      }
    })
  })
  
  # Tavola di contingenza
  observeEvent(input$calculateContingency, {
    req(input$contingencyVar1, input$contingencyVar2)
    
    # Seleziona il dataset appropriato
    df <- if (input$contingencySource == "original") data() else synthetic_data()
    
    # Verifica che entrambe le variabili esistano nel dataset
    if (!(input$contingencyVar1 %in% colnames(df)) || !(input$contingencyVar2 %in% colnames(df))) {
      showNotification("Una o entrambe le variabili non sono presenti nel dataset selezionato", type = "error")
      return()
    }
    
    var1 <- df[[input$contingencyVar1]]
    var2 <- df[[input$contingencyVar2]]
    
    # Calcola la tavola di contingenza
    output$contingencyTable <- renderPrint({
      # Crea la tabella di contingenza
      conting_table <- table(var1, var2)
      
      cat("Tavola di contingenza:\n")
      print(conting_table)
      
      cat("\nFrequenze relative (percentuali):\n")
      print(round(prop.table(conting_table) * 100, 2))
      
      cat("\nFrequenze relative per riga (percentuali):\n")
      print(round(prop.table(conting_table, 1) * 100, 2))
      
      # Test di indipendenza Chi-quadrato
      chi_test <- tryCatch(
        chisq.test(conting_table),
        error = function(e) NULL,
        warning = function(w) NULL
      )
      
      if (!is.null(chi_test)) {
        cat("\nTest Chi-quadrato di indipendenza:\n")
        cat("- X² =", chi_test$statistic, "\n")
        cat("- gradi di libertà =", chi_test$parameter, "\n")
        cat("- p-value =", chi_test$p.value, "\n")
        cat("- Interpretazione:", ifelse(chi_test$p.value < 0.05, 
                                         "Le variabili sono significativamente associate (p < 0.05)",
                                         "Non c'è evidenza di associazione significativa tra le variabili"))
      }
    })
    
    output$contingencyPlot <- renderPlot({
      # Crea un dataframe per il grafico
      plot_df <- data.frame(
        var1 = var1,
        var2 = var2
      )
      
      # Mosaicplot
      ggplot(plot_df) +
        geom_count(aes(x = var1, y = var2, color = ..n.., size = ..n..)) +
        scale_color_gradient(low = "lightblue", high = "darkblue") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste("Relazione tra", input$contingencyVar1, "e", input$contingencyVar2),
             subtitle = paste("Dataset:", ifelse(input$contingencySource == "original", "Originale", "Sintetico")),
             x = input$contingencyVar1, y = input$contingencyVar2)
    })
  })
  
  # Tabella comparativa generale
  output$statsComparisonTable <- renderDT({
    req(data(), synthetic_data())
    
    # Preparazione della tabella di confronto generale
    stats_df <- data.frame(
      Variabile = character(),
      Tipo = character(),
      Metrica = character(),
      Originale = numeric(),
      Sintetico = numeric(),
      Differenza = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Colonne numeriche
    num_cols <- intersect(get_numeric_columns(), colnames(synthetic_data()))
    
    for (col in num_cols) {
      orig_vals <- data()[[col]]
      synth_vals <- synthetic_data()[[col]]
      
      # Media
      mean_orig <- mean(orig_vals, na.rm = TRUE)
      mean_synth <- mean(synth_vals, na.rm = TRUE)
      stats_df <- rbind(stats_df, data.frame(
        Variabile = col,
        Tipo = "Numerica",
        Metrica = "Media",
        Originale = mean_orig,
        Sintetico = mean_synth,
        Differenza = mean_synth - mean_orig,
        stringsAsFactors = FALSE
      ))
      
      # Mediana
      median_orig <- median(orig_vals, na.rm = TRUE)
      median_synth <- median(synth_vals, na.rm = TRUE)
      stats_df <- rbind(stats_df, data.frame(
        Variabile = col,
        Tipo = "Numerica",
        Metrica = "Mediana",
        Originale = median_orig,
        Sintetico = median_synth,
        Differenza = median_synth - median_orig,
        stringsAsFactors = FALSE
      ))
      
      # Deviazione standard
      sd_orig <- sd(orig_vals, na.rm = TRUE)
      sd_synth <- sd(synth_vals, na.rm = TRUE)
      stats_df <- rbind(stats_df, data.frame(
        Variabile = col,
        Tipo = "Numerica",
        Metrica = "Dev. Std",
        Originale = sd_orig,
        Sintetico = sd_synth,
        Differenza = sd_synth - sd_orig,
        stringsAsFactors = FALSE
      ))
    }
    
    # Colonne categoriali
    cat_cols <- intersect(get_categorical_columns(), colnames(synthetic_data()))
    
    for (col in cat_cols) {
      orig_vals <- data()[[col]]
      synth_vals <- synthetic_data()[[col]]
      
      # Numero di categorie
      n_cats_orig <- length(unique(orig_vals))
      n_cats_synth <- length(unique(synth_vals))
      stats_df <- rbind(stats_df, data.frame(
        Variabile = col,
        Tipo = "Categoriale",
        Metrica = "N. Categorie",
        Originale = n_cats_orig,
        Sintetico = n_cats_synth,
        Differenza = n_cats_synth - n_cats_orig,
        stringsAsFactors = FALSE
      ))
      
      # Calcola l'entropia se possibile
      tryCatch({
        # Entropia (diversità delle categorie)
        entropy_orig <- -sum(prop.table(table(orig_vals)) * log(prop.table(table(orig_vals))))
        entropy_synth <- -sum(prop.table(table(synth_vals)) * log(prop.table(table(synth_vals))))
        stats_df <- rbind(stats_df, data.frame(
          Variabile = col,
          Tipo = "Categoriale",
          Metrica = "Entropia",
          Originale = entropy_orig,
          Sintetico = entropy_synth,
          Differenza = entropy_synth - entropy_orig,
          stringsAsFactors = FALSE
        ))
      }, error = function(e) {
        # Se non possiamo calcolare l'entropia, continuiamo
      })
    }
    
    # Formatting per la visualizzazione
    datatable(stats_df, options = list(pageLength = 20, scrollX = TRUE)) %>%
      formatRound(columns = c("Originale", "Sintetico", "Differenza"), digits = 4)
  })
}

# Run the application
shinyApp(ui = ui, server = server)