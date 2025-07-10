# ATTUARIO AI ASSISTANT - RAMO DANNI ----------------------------------------
# Versione 2.2 | Metodi Stocastici Integrati

# CONFIGURAZIONE -----------------------------------------------------------
# Setup iniziale
if (!require("httr")) install.packages("httr")
#' Inizializza l'ambiente per l'esecuzione dell'applicazione
initialize_environment <- function() {
  # Crea directory di log se non esiste
  log_dir <- tempdir()
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }
  
  # Inizializza file di log
  log_file <- file.path(log_dir, "attuario_ai.log")
  if (!file.exists(log_file)) {
    cat("", file = log_file)
  }
  
  # Verifica pacchetti richiesti
  required_packages <- c(
    "actuar", "dplyr", "ggplot2", "rstanarm", "groqR", "jsonlite", 
    "rmarkdown", "shiny", "scales", "future", "promises", "bslib"
  )
  
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    log_message(sprintf("Installazione pacchetti mancanti: %s", 
                        paste(missing_packages, collapse = ", ")))
    
    if (!requireNamespace("pacman", quietly = TRUE)) {
      install.packages("pacman", repos = "https://cran.rstudio.com/")
    }
    
    pacman::p_load(char = missing_packages, install = TRUE, quiet = TRUE)
  }
  
  # Verifica API key
  if (nchar(Sys.getenv("GROQ_API_KEY")) == 0) {
    log_message("GROQ_API_KEY non impostata, alcune funzionalità potrebbero non essere disponibili", "WARNING")
  }
  
  log_message("Ambiente inizializzato con successo")
  return(TRUE)
}

#' Funzione per generare un report basato su simulazione e analisi
generate_report <- function(sim_data, ia_results, report_type = "full") {
  if (is.null(sim_data) || is.null(ia_results)) {
    return("Dati insufficienti per generare il report")
  }
  
  # Crea un template di report basato sul tipo richiesto
  templates <- list(
    full = list(
      title = "Report Completo Analisi Scenari Catastrofali",
      sections = c("dati", "simulazione", "ia", "confronto", "raccomandazioni")
    ),
    summary = list(
      title = "Report Sintetico Analisi Scenari",
      sections = c("dati", "ia", "raccomandazioni")
    ),
    technical = list(
      title = "Report Tecnico Analisi Attuariale",
      sections = c("dati", "simulazione", "confronto")
    )
  )
  
  template <- templates[[report_type]]
  if (is.null(template)) {
    template <- templates[["full"]]
  }
  
  # Genera il contenuto del report
  content <- paste0(
    "# ", template$title, "\n\n",
    "Data: ", format(Sys.time(), "%d/%m/%Y %H:%M"), "\n\n"
  )
  
  # Aggiungi sezioni in base al template
  if ("dati" %in% template$sections) {
    content <- paste0(
      content,
      "## Dati di Input\n\n",
      "Scenario analizzato: ", ia_results$metadata$scenario_input, "\n\n"
    )
  }
  
  if ("simulazione" %in% template$sections && !is.null(sim_data)) {
    analysis <- analyze_risk(sim_data)
    content <- paste0(
      content,
      "## Risultati Simulazione\n\n",
      "- Distribuzione frequenza: ", sim_data$frequency_dist$dist, "\n",
      "- Distribuzione gravità: ", sim_data$severity_dist$dist, "\n",
      "- Media perdite: ", format(analysis$classical$mean_loss, big.mark = ","), "\n",
      "- VaR 99.5%: ", format(analysis$classical$VaR_995, big.mark = ","), "\n\n"
    )
  }
  
  if ("ia" %in% template$sections) {
    content <- paste0(
      content,
      "## Analisi IA\n\n",
      "- Probabilità evento: ", format(ia_results$probabilita_evento * 100, digits = 2), "%\n",
      "- Impatto minimo: ", ia_results$impatti_potenziali$min, " milioni\n",
      "- Impatto più probabile: ", ia_results$impatti_potenziali$piu_probabile, " milioni\n",
      "- Impatto massimo: ", ia_results$impatti_potenziali$max, " milioni\n\n"
    )
  }
  
  if ("confronto" %in% template$sections && !is.null(sim_data)) {
    analysis <- analyze_risk(sim_data)
    content <- paste0(
      content,
      "## Confronto Simulazione vs IA\n\n",
      "| Metrica | Simulazione | Analisi IA |\n",
      "|---------|-------------|------------|\n",
      "| Perdita media | ", format(analysis$classical$mean_loss/1e6, digits = 2), "M | ", 
      ia_results$impatti_potenziali$piu_probabile, "M |\n",
      "| Perdita massima | ", format(analysis$classical$VaR_995/1e6, digits = 2), "M | ", 
      ia_results$impatti_potenziali$max, "M |\n\n"
    )
  }
  
  if ("raccomandazioni" %in% template$sections) {
    content <- paste0(
      content,
      "## Raccomandazioni\n\n"
    )
    
    for (i in seq_along(ia_results$raccomandazioni_mitigazione)) {
      content <- paste0(
        content,
        i, ". ", ia_results$raccomandazioni_mitigazione[i], "\n"
      )
    }
  }
  
  return(content)
}
# Setup logging
log_file <- file.path(tempdir(), "attuario_ai.log")
log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message <- sprintf("[%s] %s: %s", timestamp, level, msg)
  cat(message, "\n", file = log_file, append = TRUE)
  if(level %in% c("ERROR", "WARNING")) cat(message, "\n")
}

# Installazione e caricamento pacchetti
tryCatch({
  if(!require(pacman)) install.packages("pacman", repos = "https://cran.rstudio.com/")
  pacman::p_load(
    actuar, dplyr, ggplot2, rstanarm, groqR, jsonlite, rmarkdown, shiny,
    scales, future, promises, bslib,
    quiet = TRUE
  )
  log_message("Pacchetti caricati con successo")
}, error = function(e) {
  log_message(sprintf("Errore nel caricamento pacchetti: %s", e$message), "ERROR")
  stop(e)
})

# FUNZIONI CORE -----------------------------------------------------------
#' Simulatore Sinistri con Coda Pesante
simula_scenario <- function(n = 1e4, 
                            freq_dist = list(dist = "poisson", lambda = 50),
                            sev_dist = list(dist = "pareto", shape = 2.5, scale = 1e4)) {
  
  # Standardizza i nomi delle distribuzioni
  freq_dist$dist <- tolower(freq_dist$dist)
  sev_dist$dist <- tolower(sev_dist$dist)
  
  # Correzione nomi distribuzioni
  freq_dist$dist <- gsub("binomiale negativa", "negative binomial", freq_dist$dist)
  sev_dist$dist <- gsub("lognormale", "lognormal", sev_dist$dist)
  
  # Validazione input
  if (!is.numeric(n) || n <= 0) stop("n deve essere un numero positivo")
  if (!is.list(freq_dist) || !is.list(sev_dist)) stop("freq_dist e sev_dist devono essere liste")
  
  # Validazione distribuzioni
  valid_freq_dist <- c("poisson", "negative binomial")
  valid_sev_dist <- c("pareto", "lognormal", "gamma")
  
  if (!freq_dist$dist %in% valid_freq_dist) 
    stop(sprintf("Distribuzione frequenza non valida. Usare: %s", 
                 paste(valid_freq_dist, collapse = ", ")))
  
  if (!sev_dist$dist %in% valid_sev_dist)
    stop(sprintf("Distribuzione gravità non valida. Usare: %s", 
                 paste(valid_sev_dist, collapse = ", ")))
  
  # Gestione parametri mancanti
  if (freq_dist$dist == "poisson") {
    if (is.null(freq_dist$lambda)) freq_dist$lambda <- 50
  } else if (freq_dist$dist == "negative binomial") {
    if (is.null(freq_dist$size)) freq_dist$size <- 10
    if (is.null(freq_dist$mu)) freq_dist$mu <- 50
  }
  
  if (sev_dist$dist == "pareto") {
    if (is.null(sev_dist$shape)) sev_dist$shape <- 2.5
    if (is.null(sev_dist$scale)) sev_dist$scale <- 1e4
  } else if (sev_dist$dist == "lognormal") {
    if (is.null(sev_dist$meanlog)) sev_dist$meanlog <- log(1e4)
    if (is.null(sev_dist$sdlog)) sev_dist$sdlog <- 0.5
  } else if (sev_dist$dist == "gamma") {
    if (is.null(sev_dist$shape)) sev_dist$shape <- 2
    if (is.null(sev_dist$rate)) sev_dist$rate <- 1/5000
  }
  
  # Simulazione frequenza
  N <- tryCatch({
    switch(freq_dist$dist,
           "poisson" = rpois(n, freq_dist$lambda),
           "negative binomial" = rnbinom(n, size = freq_dist$size, mu = freq_dist$mu))
  }, error = function(e) {
    log_message(sprintf("Errore simulazione frequenza: %s", e$message), "ERROR")
    stop(e)
  })
  
  # Simulazione gravità
  severity <- tryCatch({
    switch(sev_dist$dist,
           "pareto" = actuar::rpareto(n, sev_dist$shape, sev_dist$scale),
           "lognormal" = rlnorm(n, sev_dist$meanlog, sev_dist$sdlog),
           "gamma" = rgamma(n, shape = sev_dist$shape, rate = sev_dist$rate))
  }, error = function(e) {
    log_message(sprintf("Errore simulazione gravità: %s", e$message), "ERROR")
    stop(e)
  })
  
  # Calcolo perdita aggregata
  aggregate_loss <- N * severity
  if(any(is.na(aggregate_loss))) {
    warning("Rilevati NA nel calcolo delle perdite aggregate")
  }
  if(any(is.infinite(aggregate_loss))) {
    warning("Rilevati valori infiniti nel calcolo delle perdite aggregate")
    aggregate_loss[is.infinite(aggregate_loss)] <- max(aggregate_loss[is.finite(aggregate_loss)]) * 10
  }
  
  # Output strutturato
  result <- structure(
    list(
      n = n,
      claim_counts = N,
      claim_severity = severity,
      aggregate_loss = aggregate_loss,
      frequency_dist = freq_dist,
      severity_dist = sev_dist,
      simulation_time = Sys.time(),
      parameters = list(
        frequency = freq_dist,
        severity = sev_dist
      )
    ),
    class = "actuarial_simulation"
  )
  
  log_message(sprintf("Simulazione completata: %d osservazioni", n))
  return(result)
}

#' Analisi del Rischio
analyze_risk <- function(simulation) {
  tryCatch({
    # Validazione input
    if (!inherits(simulation, "actuarial_simulation")) {
      stop("Input non valido: deve essere un oggetto actuarial_simulation")
    }
    
    # Preparazione dati
    agg_loss <- as.numeric(simulation$aggregate_loss)
    sev <- as.numeric(simulation$claim_severity)
    
    # Pulizia dati
    agg_loss <- agg_loss[is.finite(agg_loss)]
    sev <- sev[is.finite(sev)]
    
    if(length(agg_loss) == 0 || length(sev) == 0) {
      stop("Dati non sufficienti dopo la pulizia")
    }
    
    # Calcolo KRI classici
    kri_classic <- list(
      VaR_995 = quantile(agg_loss, 0.995, na.rm = TRUE),
      CTE_995 = mean(agg_loss[agg_loss > quantile(agg_loss, 0.995, na.rm = TRUE)], na.rm = TRUE),
      SL_ratio = mean(sev > quantile(sev, 0.9, na.rm = TRUE), na.rm = TRUE),
      mean_loss = mean(agg_loss, na.rm = TRUE),
      sd_loss = sd(agg_loss, na.rm = TRUE)
    )
    
    # Calcolo statistiche bayesiane semplificate
    bayes_result <- NULL
    if (length(sev) >= 30) {
      tryCatch({
        log_sev <- log(sev[sev > 0])
        mean_log <- mean(log_sev)
        sd_log <- sd(log_sev)
        ci_95 <- exp(mean_log + c(-1.96, 1.96) * sd_log)
        
        bayes_result <- list(
          posterior_mean = exp(mean_log + (sd_log^2)/2),
          credibility_interval = ci_95,
          model_diagnostics = list(
            n_observations = length(log_sev),
            mean_log = mean_log,
            sd_log = sd_log
          )
        )
      }, error = function(e) {
        log_message(sprintf("Errore nel calcolo bayesiano: %s", e$message), "WARNING")
      })
    }
    
    # Output strutturato
    result <- list(
      classical = kri_classic,
      bayesian = bayes_result,
      analysis_time = Sys.time(),
      data_summary = list(
        n_observations = length(agg_loss),
        missing_rate = mean(is.na(simulation$aggregate_loss)),
        distribution_summary = list(
          mean = mean(agg_loss, na.rm = TRUE),
          median = median(agg_loss, na.rm = TRUE),
          sd = sd(agg_loss, na.rm = TRUE)
        )
      )
    )
    
    log_message("Analisi del rischio completata con successo")
    return(result)
    
  }, error = function(e) {
    log_message(sprintf("Errore simulazione: %s", e$message), "ERROR")
    showNotification(
      sprintf("Errore: %s", e$message),
      duration = 5,
      type = "error"  # Questo è corretto perché "error" è un tipo valido
    )
  })
  
  # Nel server, per gli errori di analisi IA
  tryCatch({
    # ... codice analisi IA ...
  }, error = function(e) {
    log_message(sprintf("Errore analisi IA: %s", e$message), "ERROR")
    showNotification(
      sprintf("Errore: %s", e$message),
      duration = 5,
      type = "error"
    )
  })
}

# INTEGRAZIONE IA E SIMULAZIONE --------------------------------------------

#' Valutazione Scenario di Crisi con AI
#' Valutazione Scenario di Crisi con AI
valuta_scenario_crisi <- function(scenario, 
                                  simulation_data = NULL,
                                  GROQ_API_KEY = Sys.getenv("GROQ_API_KEY"),
                                  model = "llama-3.3-70b-versatile",
                                  temperature = 0.3,
                                  max_retries = 3,
                                  retry_delay = 1) {
  
  # Verifica API key
  if (nchar(GROQ_API_KEY) == 0) {
    log_message("API key non fornita, usando modalità simulazione", "WARNING")
    return(genera_risposta_simulata(scenario, simulation_data))
  }
  
  # Preparazione del contesto con dati simulazione
  simulation_context <- ""
  if (!is.null(simulation_data)) {
    analysis <- analyze_risk(simulation_data)
    # Corretto il problema con l'operatore +
    simulation_context <- sprintf(
      paste0(
        "\nCONTESTO SIMULAZIONE:\n",
        "- Perdita media: %.2f milioni\n",
        "- VaR 99.5%%: %.2f milioni\n",
        "- Frequenza media: %.1f eventi/anno\n",
        "- Deviazione standard: %.2f milioni\n"
      ),
      analysis$classical$mean_loss/1e6,
      analysis$classical$VaR_995/1e6,
      mean(simulation_data$claim_counts),
      analysis$classical$sd_loss/1e6
    )
  }
  
  # Resto della funzione identico...
  # Preparazione del prompt
  prompt <- paste0(
    "ISTRUZIONI PRECISE:\n",
    "1. Sei un attuario esperto. Analizza lo scenario e il contesto fornito.\n",
    "2. Rispondi ESCLUSIVAMENTE con un JSON valido.\n",
    "3. NON aggiungere altro testo prima o dopo il JSON.\n\n",
    "SCENARIO DA ANALIZZARE:\n", 
    scenario,
    simulation_context,
    "\n\nFORMATO JSON RICHIESTO:\n",
    "{\n",
    '  "probabilita_evento": <numero tra 0 e 1>,\n',
    '  "impatti_potenziali": {\n',
    '    "min": <numero intero in milioni>,\n',
    '    "max": <numero intero in milioni>,\n',
    '    "piu_probabile": <numero intero in milioni>\n',
    "  },\n",
    '  "raccomandazioni_mitigazione": [\n',
    '    "<prima raccomandazione>",\n',
    '    "<seconda raccomandazione>",\n',
    '    "<terza raccomandazione>"\n',
    "  ],\n",
    '  "analisi_tecnica": {\n',
    '    "fonte_dati": "combinata",\n',
    '    "affidabilita_stima": <numero tra 0 e 1>,\n',
    '    "note_metodologiche": "<breve descrizione>"\n',
    "  }\n",
    "}"
  )
  
  # Impostazione parametri Groq
  groq_params <- list(
    GROQ_API_KEY = GROQ_API_KEY,
    model = model,
    systemRole = "Sei un attuario esperto. Rispondi solo in formato JSON valido.",
    maxTokens = 1024,
    temperature = temperature,
    top_p = 0.7
  )
  
  # Helper per pulizia e validazione JSON
  clean_and_validate_json <- function(response_text) {
    cleaned <- gsub("```json\\s*|```", "", response_text)
    cleaned <- gsub("^[^{]*|[^}]*$", "", cleaned)
    
    tryCatch({
      result <- jsonlite::fromJSON(cleaned)
      
      if (!is.null(result$probabilita_evento) && 
          is.numeric(result$probabilita_evento) && 
          result$probabilita_evento >= 0 && 
          result$probabilita_evento <= 1 &&
          !is.null(result$impatti_potenziali) &&
          !is.null(result$raccomandazioni_mitigazione)) {
        
        result$metadata <- list(
          timestamp = Sys.time(),
          model = model,
          temperature = temperature,
          scenario_input = scenario,
          has_simulation_data = !is.null(simulation_data)
        )
        return(result)
      }
      return(NULL)
    }, error = function(e) {
      log_message(sprintf("Errore validazione JSON: %s", e$message), "DEBUG")
      return(NULL)
    })
  }
  
  # Tentativo di chiamata API con retry
  for(i in 1:max_retries) {
    tryCatch({
      # Imposta parametri ambiente
      do.call(Sys.setenv, as.list(groq_params))
      
      # Chiamata API
      response <- groqR:::APIcall(
        prompt = prompt,
        GROQ_API_KEY = GROQ_API_KEY,
        model = model,
        systemRole = groq_params$systemRole,
        maxTokens = groq_params$maxTokens,
        temperature = temperature,
        top_p = groq_params$top_p
      )
      
      log_message(sprintf("Risposta API (primi 200 caratteri): %s", 
                          substr(response, 1, 200)), "DEBUG")
      
      result <- clean_and_validate_json(response)
      
      if (!is.null(result)) {
        log_message("Analisi IA completata con successo")
        return(result)
      }
      
      log_message(sprintf("Tentativo %d: JSON non valido o incompleto", i), "WARNING")
      
    }, error = function(e) {
      log_message(sprintf("Tentativo %d fallito: %s", i, e$message), "WARNING")
      if(i < max_retries) {
        log_message(sprintf("Attendo %d secondi prima del retry...", retry_delay))
        Sys.sleep(retry_delay)
      }
    })
  }
  
  # Fallback a simulazione
  log_message("Tutti i tentativi API falliti, usando simulazione", "WARNING")
  return(genera_risposta_simulata(scenario, simulation_data))
}

#' Genera una risposta simulata per scenari catastrofali
#' Genera una risposta simulata per scenari catastrofali
genera_risposta_simulata <- function(scenario, simulation_data = NULL) {
  # Analisi testuale dello scenario
  scenario_lower <- tolower(scenario)
  parole_chiave <- list(
    meteorologico = c("uragano", "ciclone", "tempesta", "tornado", "grandine"),
    sismico = c("terremoto", "sisma", "scossa"),
    idrologico = c("alluvione", "inondazione", "allagamento", "tsunami"),
    incendio = c("incendio", "fuoco", "rogo"),
    tecnologico = c("cyber", "informatico", "tecnologico", "sistema")
  )
  
  # Identificazione tipo di catastrofe
  tipo_catastrofe <- "altro"
  max_matches <- 0
  
  for(tipo in names(parole_chiave)) {
    matches <- sum(sapply(parole_chiave[[tipo]], 
                          function(x) grepl(x, scenario_lower)))
    if(matches > max_matches) {
      tipo_catastrofe <- tipo
      max_matches <- matches
    }
  }
  
  # Parametrizzazione basata sul tipo e dati simulazione
  parametri_base <- list(
    meteorologico = list(prob_base = 0.15, impatto_base = c(20, 100, 45), volatilita = 0.3),
    sismico = list(prob_base = 0.08, impatto_base = c(50, 250, 120), volatilita = 0.4),
    idrologico = list(prob_base = 0.20, impatto_base = c(30, 150, 70), volatilita = 0.35),
    incendio = list(prob_base = 0.12, impatto_base = c(15, 80, 35), volatilita = 0.25),
    tecnologico = list(prob_base = 0.25, impatto_base = c(10, 200, 50), volatilita = 0.45),
    altro = list(prob_base = 0.10, impatto_base = c(25, 125, 60), volatilita = 0.35)
  )
  
  param <- parametri_base[[tipo_catastrofe]]
  
  # Adatta parametri se ci sono dati di simulazione
  if (!is.null(simulation_data)) {
    tryCatch({
      analysis <- analyze_risk(simulation_data)
      
      # Calcola la VaR in maniera più robusta
      agg_loss <- as.numeric(simulation_data$aggregate_loss)
      agg_loss <- agg_loss[is.finite(agg_loss)]
      var_995 <- quantile(agg_loss, 0.995, na.rm = TRUE)
      
      # Utilizziamo i dati di simulazione per calibrare gli impatti
      # Scalati per essere in milioni
      min_impact <- max(1, analysis$classical$mean_loss/2/1e6)
      likely_impact <- max(5, analysis$classical$mean_loss/1e6)
      max_impact <- max(10, var_995/1e6)
      
      # Assicuriamoci che gli impatti siano ordinati correttamente
      param$impatto_base <- sort(c(min_impact, likely_impact, max_impact))
      
      # Calibriamo la probabilità in base alla distribuzione di frequenza
      if (!is.null(simulation_data$claim_counts)) {
        # Se la frequenza media è alta, aumentiamo la probabilità di base
        mean_freq <- mean(simulation_data$claim_counts)
        if (mean_freq > 50) {
          param$prob_base <- min(0.9, param$prob_base * 1.5)
        } else if (mean_freq < 10) {
          param$prob_base <- max(0.01, param$prob_base * 0.7)
        }
      }
      
      # Calibriamo la volatilità in base alla deviazione standard
      param$volatilita <- min(0.5, max(0.1, analysis$classical$sd_loss / analysis$classical$mean_loss))
      
    }, error = function(e) {
      log_message(sprintf("Errore nell'adattamento parametri da simulazione: %s", e$message), "WARNING")
      # Manteniamo i parametri di base in caso di errore
    })
  }
  
  # Generazione valori con rumore casuale
  set.seed(as.numeric(Sys.time()))
  prob <- pmin(1, pmax(0, param$prob_base * (1 + rnorm(1, 0, 0.2))))
  impatti <- param$impatto_base * (1 + rnorm(3, 0, param$volatilita))
  impatti <- round(sort(abs(impatti)))
  
  # Selezione raccomandazioni
  raccomandazioni <- list(
    meteorologico = c(
      "Implementare sistemi di allerta precoce meteo",
      "Rafforzare strutture contro venti forti",
      "Pianificare evacuazioni preventive",
      "Creare reti di supporto post-evento"
    ),
    sismico = c(
      "Valutare vulnerabilità sismica edifici",
      "Implementare sistemi antisismici",
      "Creare piani evacuazione dedicati",
      "Stabilire centri operativi emergenza"
    ),
    idrologico = c(
      "Mappare zone rischio idrogeologico",
      "Installare sistemi pompaggio emergenza",
      "Mantenere reti drenaggio",
      "Predisporre barriere mobili"
    ),
    incendio = c(
      "Installare sistemi rilevamento fumo",
      "Preparare squadre pronto intervento",
      "Mantenere vie fuga sgombre",
      "Creare zone tagliafuoco"
    ),
    tecnologico = c(
      "Implementare backup sistemi critici",
      "Aggiornare protocolli sicurezza",
      "Formare personale specializzato",
      "Testare piani disaster recovery"
    ),
    altro = c(
      "Sviluppare piani emergenza generici",
      "Formare squadre pronto intervento",
      "Stabilire catena comando chiara",
      "Mantenere scorte emergenza"
    )
  )
  
  racc_selezionate <- sample(raccomandazioni[[tipo_catastrofe]], 3)
  
  # Aggiungiamo l'analisi tecnica
  affidabilita_stima <- if (!is.null(simulation_data)) 0.75 else 0.5
  note_metodologiche <- if (!is.null(simulation_data)) 
    "Analisi basata su dati simulati integrati con valutazioni euristiche" 
  else 
    "Analisi basata su valutazioni euristiche di scenario"
  
  # Costruzione risposta
  risultato <- list(
    simulato = TRUE,
    tipo_catastrofe = tipo_catastrofe,
    probabilita_evento = prob,
    impatti_potenziali = list(
      min = impatti[1],
      piu_probabile = impatti[2],
      max = impatti[3]
    ),
    raccomandazioni_mitigazione = racc_selezionate,
    analisi_tecnica = list(
      fonte_dati = if (!is.null(simulation_data)) "combinata" else "euristica",
      affidabilita_stima = affidabilita_stima,
      note_metodologiche = note_metodologiche
    ),
    metadata = list(
      timestamp = Sys.time(),
      scenario_input = scenario,
      has_simulation_data = !is.null(simulation_data),
      confidence_score = runif(1, 0.6, 0.9)
    )
  )
  
  log_message(sprintf("Generata risposta simulata per scenario tipo: %s", tipo_catastrofe))
  return(risultato)
}
# UI DEFINITION -----------------------------------------------------------

create_ui <- function() {
  fluidPage(
    theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .btn-block { width: 100%; }
        .results-container, .comparison-container, .report-preview-container {
          padding: 15px;
          background: #fff;
          border-radius: 5px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12);
          margin-bottom: 20px;
        }
        .shiny-output-error { color: #dc3545; }
        #ia_results { max-height: 400px; overflow-y: auto; }
        .param-group { 
          background: #f8f9fa;
          padding: 15px;
          border-radius: 5px;
          margin-bottom: 15px;
        }
        .nav-tabs { margin-bottom: 20px; }
        .footer {
          margin-top: 50px;
          padding: 20px;
          background-color: #f8f9fa;
          text-align: center;
        }
      "))
    ),
    
    titlePanel("Attuario AI - Dashboard Analisi Rischi"),
    
    navbarPage(
      "Menu",
      id = "mainMenu",
      
      # Tab Simulazione
      tabPanel("Simulazione",
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   div(class = "param-group",
                       numericInput("n_sim", "Numero simulazioni:", 
                                    value = 10000, min = 1000, max = 1e6),
                       
                       selectInput("freq_dist", "Distribuzione Frequenza:",
                                   choices = c("Poisson", "Binomiale Negativa")),
                       
                       numericInput("freq_param", "Lambda/Mu:", 
                                    value = 50, min = 0)
                   ),
                   
                   div(class = "param-group",
                       selectInput("sev_dist", "Distribuzione Gravità:",
                                   choices = c("Pareto", "LogNormale", "Gamma")),
                       
                       numericInput("sev_shape", "Parametro forma:", 
                                    value = 2.5, min = 0),
                       
                       numericInput("sev_scale", "Parametro scala:", 
                                    value = 10000, min = 0)
                   ),
                   
                   div(style = "margin-top: 20px",
                       actionButton("run_sim", "Esegui Simulazione",
                                    class = "btn-primary btn-block"),
                       
                       div(style = "margin-top: 10px",
                           actionButton("generate_scenario", "Genera Scenario per IA",
                                        class = "btn-info btn-block"))
                   )
                 ),
                 
                 mainPanel(
                   width = 9,
                   tabsetPanel(
                     id = "simTabs",
                     tabPanel("Grafici",
                              fluidRow(
                                column(6, 
                                       div(class = "results-container",
                                           h4("Distribuzione Perdite"),
                                           plotOutput("dist_plot"))
                                ),
                                column(6, 
                                       div(class = "results-container",
                                           h4("Distribuzione Frequenze"),
                                           plotOutput("freq_plot"))
                                )
                              )),
                     
                     tabPanel("Statistiche",
                              div(class = "results-container",
                                  h4("Analisi Statistica"),
                                  verbatimTextOutput("sim_summary"))
                     ),
                     
                     tabPanel("Diagnostica",
                              div(class = "results-container",
                                  h4("Diagnostica Simulazione"),
                                  verbatimTextOutput("sim_diagnostics"))
                     )
                   )
                 )
               )),
      
      # Tab Analisi IA
      tabPanel("Analisi IA",
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   div(class = "param-group",
                       textAreaInput("scenario", "Scenario Catastrofale:",
                                     height = "150px",
                                     placeholder = "Descrivi lo scenario da analizzare..."),
                       
                       selectInput("ia_model", "Modello IA:",
                                   choices = c(
                                     "llama-3.3-70b-versatile" = "llama-3.3-70b-versatile",
                                     "mixtral-8x7b-32768" = "mixtral-8x7b-32768",
                                     "gemma-7b-it" = "gemma-7b-it"
                                   )),
                       
                       sliderInput("temperature", "Temperatura:", 
                                   min = 0, max = 1, value = 0.3, step = 0.1)
                   ),
                   
                   div(class = "param-group",
                       checkboxInput("use_sim_data", 
                                     "Utilizza dati simulazione",
                                     value = TRUE),
                       
                       actionButton("analyze", "Analizza Scenario",
                                    class = "btn-success btn-block")
                   )
                 ),
                 
                 mainPanel(
                   width = 9,
                   tabsetPanel(
                     id = "analysisTabset",
                     tabPanel("Risultati",
                              div(class = "results-container",
                                  uiOutput("analysis_status"),
                                  verbatimTextOutput("ia_results"),
                                  plotOutput("impact_plot")
                              )),
                     
                     tabPanel("Confronto",
                              div(class = "comparison-container",
                                  uiOutput("sim_data_status"),
                                  verbatimTextOutput("comparison_results"),
                                  plotOutput("comparison_plot")
                              ))
                   )
                 )
               )),
      
      # Tab Report
      tabPanel("Report",
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   div(class = "param-group",
                       selectInput("report_type", "Tipo Report:",
                                   choices = c(
                                     "Completo" = "full",
                                     "Sintetico" = "summary",
                                     "Tecnico" = "technical"
                                   )),
                       
                       downloadButton("download_report", "Genera Report",
                                      class = "btn-primary btn-block")
                   )
                 ),
                 
                 mainPanel(
                   width = 9,
                   div(class = "report-preview-container",
                       uiOutput("report_preview")
                   )
                 )
               ))
    ),
    
    # Footer
    tags$footer(
      class = "footer",
      div(
        p("Attuario AI Assistant v2.2 | © 2024"),
        p(style = "font-size: 0.8em", "Sviluppato per analisi attuariali avanzate")
      )
    )
  )
}

# SERVER DEFINITION E LAUNCH ----------------------------------------------

#' Crea e avvia dashboard interattiva per analisi attuariale
run_actuarial_dashboard <- function() {
  ui <- create_ui()
  
  server <- function(input, output, session) {
    # Stato reattivo
    sim_results <- reactiveVal(NULL)
    ia_analysis <- reactiveVal(NULL)
    
    # Gestione simulazione
    observeEvent(input$run_sim, {
      withProgress(message = 'Esecuzione simulazione...', {
        tryCatch({
          # Preparazione parametri frequenza
          freq_dist <- if(tolower(input$freq_dist) == "poisson") {
            list(
              dist = "poisson",
              lambda = input$freq_param
            )
          } else {
            list(
              dist = "negative binomial",
              size = 10,
              mu = input$freq_param
            )
          }
          
          # Preparazione parametri gravità
          sev_dist <- switch(tolower(input$sev_dist),
                             "pareto" = list(
                               dist = "pareto",
                               shape = input$sev_shape,
                               scale = input$sev_scale
                             ),
                             "lognormale" = list(
                               dist = "lognormal",
                               meanlog = log(input$sev_scale),
                               sdlog = input$sev_shape
                             ),
                             "gamma" = list(
                               dist = "gamma",
                               shape = input$sev_shape,
                               rate = 1/input$sev_scale
                             ))
          
          # Esecuzione simulazione
          results <- simula_scenario(
            n = input$n_sim,
            freq_dist = freq_dist,
            sev_dist = sev_dist
          )
          
          sim_results(results)
          log_message("Simulazione completata con successo")
          
        }, error = function(e) {
          log_message(sprintf("Errore simulazione: %s", e$message), "ERROR")
          showNotification(
            sprintf("Errore: %s", e$message),
            type = "error"
          )
        })
      })
    })
    
    # Generazione scenario
    # Modifica nel server
    observeEvent(input$generate_scenario, {
      req(sim_results())
      
      results <- sim_results()
      analysis <- analyze_risk(results)
      
      scenario <- tryCatch({
        loss_summary <- analysis$classical
        
        severity_level <- if(loss_summary$mean_loss > loss_summary$VaR_995 * 0.8) {
          "catastrofico"
        } else if(loss_summary$mean_loss > loss_summary$VaR_995 * 0.5) {
          "grave"
        } else {
          "moderato"
        }
        
        event_type <- switch(results$severity_dist$dist,
                             "pareto" = "evento con coda pesante",
                             "lognormal" = "evento naturale",
                             "gamma" = "evento operativo")
        
        sprintf(
          "Scenario %s: %s nella regione con perdita media attesa di %.2f milioni e deviazione standard di %.2f milioni. 
      La frequenza attesa degli eventi è di %d per anno, con potenziale massima perdita di %.2f milioni.",
          severity_level,
          event_type,
          loss_summary$mean_loss/1e6,
          loss_summary$sd_loss/1e6,
          mean(results$claim_counts),
          loss_summary$VaR_995/1e6
        )
      }, error = function(e) {
        "Scenario di rischio basato su simulazione attuariale con potenziali perdite significative"
      })
      
      # Aggiorna il campo scenario nella tab IA
      updateTextAreaInput(session, "scenario", value = scenario)
      
      # Passa automaticamente alla tab IA
      updateTabsetPanel(session, "mainMenu", selected = "Analisi IA")
      
      # Correzione della notifica
      showNotification(
        "Scenario generato dalla simulazione. Ora puoi eseguire l'analisi IA.",
        duration = 5,
        type = "message"  # Usa "message" invece di "info"
      )
    })
    
    # Plot distribuzione perdite
    output$dist_plot <- renderPlot({
      req(sim_results())
      results <- sim_results()
      
      ggplot(data.frame(Loss = results$aggregate_loss), 
             aes(x = Loss)) +
        geom_histogram(aes(y = ..density..), 
                       bins = 50, 
                       fill = "steelblue",
                       alpha = 0.7) +
        geom_density(color = "darkred", size = 1) +
        labs(title = "Distribuzione Perdite Aggregate",
             x = "Perdita (€)",
             y = "Densità") +
        theme_minimal() +
        scale_x_continuous(labels = scales::comma) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        )
    })
    
    # Plot frequenze
    output$freq_plot <- renderPlot({
      req(sim_results())
      results <- sim_results()
      
      ggplot(data.frame(Frequency = results$claim_counts), 
             aes(x = Frequency)) +
        geom_histogram(fill = "lightblue", 
                       color = "black",
                       bins = 30) +
        labs(title = "Distribuzione Frequenze",
             x = "Numero Sinistri",
             y = "Conteggio") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        )
    })
    
    # Summary statistico
    output$sim_summary <- renderPrint({
      req(sim_results())
      results <- sim_results()
      analysis <- analyze_risk(results)
      
      cat("ANALISI STATISTICA\n")
      cat("==================\n")
      cat(sprintf("Numero simulazioni: %d\n", results$n))
      
      if (!is.null(analysis$classical)) {
        cat("\nStatistiche Classiche:\n")
        cat(sprintf("Media perdite: %.2f\n", analysis$classical$mean_loss))
        cat(sprintf("Dev. Standard: %.2f\n", analysis$classical$sd_loss))
        cat(sprintf("VaR 99.5%%: %.2f\n", analysis$classical$VaR_995))
        cat(sprintf("TVaR 99.5%%: %.2f\n", analysis$classical$CTE_995))
      }
      
      if (!is.null(analysis$bayesian)) {
        cat("\nAnalisi Bayesiana:\n")
        cat(sprintf("Media stimata: %.2f\n", analysis$bayesian$posterior_mean))
        cat("Intervallo credibilità 95%:\n")
        cat(sprintf("  Lower: %.2f\n", analysis$bayesian$credibility_interval[1]))
        cat(sprintf("  Upper: %.2f\n", analysis$bayesian$credibility_interval[2]))
      }
    })
    
    # Diagnostica simulazione
    output$sim_diagnostics <- renderPrint({
      req(sim_results())
      results <- sim_results()
      
      cat("DIAGNOSTICA SIMULAZIONE\n")
      cat("======================\n")
      cat(sprintf("Timestamp: %s\n", results$simulation_time))
      cat(sprintf("Distribuzione frequenza: %s\n", results$frequency_dist$dist))
      cat(sprintf("Distribuzione gravità: %s\n", results$severity_dist$dist))
      cat("\nParametri utilizzati:\n")
      cat("Frequenza:\n")
      print(results$parameters$frequency)
      cat("\nGravità:\n")
      print(results$parameters$severity)
    })
    
    # Analisi IA
    # Analisi IA
    # Sezione corretta per l'analisi IA nel server
    # Analisi IA
    observeEvent(input$analyze, {
      req(input$scenario)
      
      withProgress(message = 'Analisi scenario in corso...', {
        # Gestione più robusta dei dati di simulazione
        sim_data <- NULL
        
        if (!is.null(input$use_sim_data) && input$use_sim_data) {
          if (!is.null(sim_results())) {
            sim_data <- sim_results()
            log_message("Utilizzando dati di simulazione per l'analisi")
          } else {
            log_message("Richiesto uso dati simulazione ma non disponibili", "WARNING")
            showNotification(
              "Dati di simulazione richiesti ma non disponibili. Esegui prima una simulazione.",
              duration = 5,
              type = "warning"
            )
          }
        }
        
        tryCatch({
          # Imposta i parametri Groq in modo più sicuro
          groq_env_params <- list(
            GROQ_API_KEY = Sys.getenv("GROQ_API_KEY"),
            GROQ_model = input$ia_model,
            GROQ_systemRole = "Sei un attuario esperto specializzato in analisi dei rischi catastrofali.",
            GROQ_maxTokens = "1024",
            GROQ_temperature = as.character(input$temperature),
            GROQ_top_p = "0.7"
          )
          
          # Applica le impostazioni ambiente
          do.call(Sys.setenv, groq_env_params)
          
          # Esegui l'analisi con logging dettagliato
          log_message(sprintf("Avvio analisi scenario con modello %s", input$ia_model))
          if (!is.null(sim_data)) {
            log_message("Integrazione dati simulazione nell'analisi")
          }
          
          results <- valuta_scenario_crisi(
            scenario = input$scenario,
            simulation_data = sim_data,
            model = input$ia_model,
            temperature = input$temperature
          )
          
          # Verifica che i risultati siano validi prima di salvarli
          if (!is.null(results)) {
            # Log dettagliato del risultato
            if (!is.null(results$simulato) && results$simulato) {
              log_message("Generati risultati in modalità simulazione")
            } else {
              log_message("Analisi IA completata tramite API")
            }
            
            # Controlla la struttura del risultato
            if (is.null(results$impatti_potenziali) || 
                is.null(results$probabilita_evento) || 
                is.null(results$raccomandazioni_mitigazione)) {
              log_message("Risultati mancanti di campi essenziali", "WARNING")
              showNotification(
                "Risultati dell'analisi incompleti, alcuni dati potrebbero non essere visualizzati.",
                duration = 5,
                type = "warning"
              )
            }
            
            # Aggiorna lo stato reattivo con i nuovi risultati
            ia_analysis(results)
            
            # Notifica successo
            showNotification(
              "Analisi completata con successo",
              duration = 5,
              type = "message"
            )
          } else {
            log_message("Risultati analisi non validi", "WARNING")
            showNotification(
              "Analisi non ha prodotto risultati validi",
              duration = 5,
              type = "warning"
            )
          }
          
        }, error = function(e) {
          log_message(sprintf("Errore analisi IA: %s", e$message), "ERROR")
          showNotification(
            sprintf("Errore nell'analisi: %s", e$message),
            duration = 5,
            type = "error"
          )
        })
      })
    })
    
    # Status indicators
    # Status indicators per l'analisi
    output$analysis_status <- renderUI({
      # Verifica iniziale più robusta
      analysis_results <- ia_analysis()
      
      if(is.null(analysis_results)) {
        return(div(class = "alert alert-info", 
                   "In attesa dell'analisi. Inserisci uno scenario e clicca 'Analizza'"))
      }
      
      # Verifica il campo simulato in modo sicuro
      is_simulated <- !is.null(analysis_results$simulato) && analysis_results$simulato
      
      if(is_simulated) {
        return(div(class = "alert alert-warning",
                   "Risultati generati in modalità simulazione"))
      } else {
        return(div(class = "alert alert-success",
                   "Analisi IA completata con successo"))
      }
    })
    
    # Status dei dati di simulazione
    output$sim_data_status <- renderUI({
      # Verifica più robusta
      sim_data_available <- !is.null(sim_results())
      use_sim_data <- !is.null(input$use_sim_data) && input$use_sim_data
      
      if(use_sim_data && !sim_data_available) {
        return(div(class = "alert alert-warning",
                   "Per utilizzare i dati di simulazione, esegui prima una simulazione"))
      } else if(use_sim_data && sim_data_available) {
        return(div(class = "alert alert-success",
                   "Dati di simulazione disponibili per l'analisi"))
      }
      
      # Return NULL se nessuna condizione è soddisfatta
      return(NULL)
    })
    # Aggiorna anche l'observe
    observe({
      analysis_results <- ia_analysis()
      
      if(!is.null(analysis_results)) {
        is_simulated <- !is.null(analysis_results$simulato) && analysis_results$simulato
        
        if(is_simulated) {
          updateTabsetPanel(session, "analysisTabset", selected = "Risultati")
        }
      }
    })
    
    # Risultati IA
    output$ia_results <- renderPrint({
      req(ia_analysis())
      results <- ia_analysis()
      
      cat("ANALISI SCENARIO\n")
      cat("===============\n")
      if (!is.null(results$tipo_catastrofe)) {
        cat(sprintf("Tipo evento: %s\n", 
                    tools::toTitleCase(as.character(results$tipo_catastrofe))))
      }
      
      cat(sprintf("Probabilità: %.1f%%\n", 
                  results$probabilita_evento * 100))
      
      cat("\nIMPATTI POTENZIALI (M€)\n")
      cat("=====================\n")
      cat(sprintf("Minimo: %.1f\n", 
                  results$impatti_potenziali$min))
      cat(sprintf("Più probabile: %.1f\n", 
                  results$impatti_potenziali$piu_probabile))
      cat(sprintf("Massimo: %.1f\n", 
                  results$impatti_potenziali$max))
      
      cat("\nRACCOMANDAZIONI\n")
      cat("==============\n")
      for(i in seq_along(results$raccomandazioni_mitigazione)) {
        cat(sprintf("%d. %s\n", i, 
                    results$raccomandazioni_mitigazione[i]))
      }
      
      if(!is.null(results$simulato) && results$simulato) {
        cat("\nNota: Risultati generati in modalità simulazione\n")
      }
    })
    
    # Plot impatti
    output$impact_plot <- renderPlot({
      req(ia_analysis())
      results <- ia_analysis()
      
      impacts <- data.frame(
        Scenario = factor(c("Minimo", "Più probabile", "Massimo"),
                          levels = c("Minimo", "Più probabile", "Massimo")),
        Impatto = c(
          results$impatti_potenziali$min,
          results$impatti_potenziali$piu_probabile,
          results$impatti_potenziali$max
        )
      )
      
      ggplot(impacts, aes(x = Scenario, y = Impatto, fill = Scenario)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = sprintf("%.1f M€", Impatto)),
                  vjust = -0.5) +
        labs(title = "Impatti Economici Potenziali",
             y = "Milioni €",
             x = "") +
        theme_minimal() +
        scale_fill_brewer(palette = "Blues") +
        theme(
          legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 16),
          axis.text = element_text(size = 12)
        )
    })
    
    # Aggiungi questa parte al server per gestire il report
    
    # Preview del report
    output$report_preview <- renderUI({
      if (is.null(sim_results()) || is.null(ia_analysis())) {
        return(div(class = "alert alert-warning",
                   "Esegui prima una simulazione e un'analisi per visualizzare l'anteprima del report"))
      }
      
      report_content <- generate_report(
        sim_results(), 
        ia_analysis(), 
        input$report_type
      )
      
      # Convertiamo il markdown in HTML per la preview
      html_content <- markdown::markdownToHTML(
        text = report_content,
        fragment.only = TRUE
      )
      
      # Ritorniamo un div con il contenuto HTML
      div(
        h3("Anteprima Report"),
        div(class = "report-content", 
            HTML(html_content))
      )
    })
    
    # Download del report
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("report-attuario-", format(Sys.time(), "%Y%m%d-%H%M"), ".html")
      },
      content = function(file) {
        # Generiamo il contenuto del report
        report_content <- generate_report(
          sim_results(), 
          ia_analysis(), 
          input$report_type
        )
        
        # Temporary file per il markdown
        temp_md <- tempfile(fileext = ".md")
        write(report_content, temp_md)
        
        # Convertiamo in HTML usando rmarkdown
        rmarkdown::render(
          temp_md,
          output_file = file,
          output_format = rmarkdown::html_document(
            theme = "flatly",
            highlight = "tango",
            toc = TRUE
          ),
          quiet = TRUE
        )
      }
    )
    # Confronto risultati
    output$comparison_results <- renderPrint({
      req(ia_analysis(), sim_results())
      
      ia_results <- ia_analysis()
      sim_data <- sim_results()
      sim_analysis <- analyze_risk(sim_data)
      
      cat("CONFRONTO ANALISI\n")
      cat("================\n")
      cat(sprintf("Perdita Media Simulata: %.2f M€\n", 
                  sim_analysis$classical$mean_loss/1e6))
      cat(sprintf("Impatto Probabile IA: %.2f M€\n", 
                  ia_results$impatti_potenziali$piu_probabile))
      cat(sprintf("VaR 99.5%% Simulato: %.2f M€\n", 
                  sim_analysis$classical$VaR_995/1e6))
      cat(sprintf("Impatto Massimo IA: %.2f M€\n", 
                  ia_results$impatti_potenziali$max))
    })
  }
  
  # Launch app
  shinyApp(ui = ui, server = server)
}

# Inizializzazione e avvio
Sys.setenv(GROQ_API_KEY = "gsk_*************")

if(interactive()) {
  initialize_environment()
  log_message("Avvio dashboard")
  run_actuarial_dashboard()
}