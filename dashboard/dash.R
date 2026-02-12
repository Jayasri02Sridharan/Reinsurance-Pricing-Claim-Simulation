library(shiny)
library(ggplot2)
library(dplyr)
library(actuar)
library(fitdistrplus)

#-----------------------------
# Helper Functions
#-----------------------------

apply_layer <- function(losses, retention, limit) {
  pmin(pmax(losses - retention, 0), limit)
}

VaR <- function(x, p = 0.99) quantile(x, p, na.rm = TRUE)
TVaR <- function(x, p = 0.99) {
  v <- VaR(x, p)
  mean(x[x > v], na.rm = TRUE)
}

simulate_frequency <- function(n_years, model, lambda, size) {
  if(model == "Poisson") {
    return(rpois(n_years, lambda))
  } else {
    return(rnbinom(n_years, size = size, mu = lambda))
  }
}

#-----------------------------
# Load Real Dataset (Global)
#-----------------------------

# IMPORTANT:
# This path assumes your app.R is inside dashboard/
# and your data is in ../data/processed/
df_real <- read.csv("D:\\PersProj\\allstate_cleaned_claims.csv")

# Ensure proper column exists
if(!("loss" %in% colnames(df_real))) {
  stop("Dataset must contain column named 'loss'")
}

df_real <- df_real %>% filter(loss > 0)

#-----------------------------
# UI
#-----------------------------

ui <- fluidPage(
  
  titlePanel("Reinsurance Pricing Dashboard (Real Claims Data + Inflation + Simulation)"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Inflation Control"),
      sliderInput("inflation", "Inflation Adjustment (%)",
                  min = 0, max = 30, value = 5, step = 0.5),
      
      hr(),
      h4("Frequency Model"),
      
      selectInput("freq_model", "Frequency Distribution",
                  choices = c("Poisson", "Negative Binomial")),
      
      sliderInput("lambda", "Expected Claims per Year (λ)",
                  min = 10, max = 500, value = 100, step = 10),
      
      sliderInput("nb_size", "NegBin Dispersion (size)",
                  min = 1, max = 50, value = 10, step = 1),
      
      sliderInput("n_years", "Number of Simulated Years",
                  min = 1000, max = 20000, value = 5000, step = 1000),
      
      hr(),
      h4("Severity Model (Fit from Real Data)"),
      
      selectInput("sev_model", "Severity Distribution",
                  choices = c("Lognormal", "Pareto")),
      
      checkboxInput("use_tail_only", "Fit Pareto only on Tail (Top 5%)", value = TRUE),
      
      hr(),
      h4("Reinsurance Layer (Excess of Loss)"),
      
      sliderInput("retention", "Retention (Attachment)",
                  min = 0, max = 200000, value = 20000, step = 5000),
      
      sliderInput("limit", "Limit",
                  min = 10000, max = 500000, value = 100000, step = 10000),
      
      sliderInput("loading", "Risk Loading (%)",
                  min = 0, max = 50, value = 15, step = 1),
      
      hr(),
      h4("Fixed Axis (for visible curve change)"),
      
      sliderInput("xmax", "Max X-axis Loss",
                  min = 50000, max = 500000, value = 200000, step = 10000)
    ),
    
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Plots",
                 br(),
                 fluidRow(
                   column(6, plotOutput("densityPlot", height = "350px")),
                   column(6, plotOutput("aggregatePlot", height = "350px"))
                 ),
                 
                 br(),
                 
                 fluidRow(
                   column(6, plotOutput("cededPlot", height = "350px")),
                   column(6, plotOutput("layerPlot", height = "350px"))
                 )
        ),
        
        tabPanel("Pricing Metrics",
                 br(),
                 h4("Key Pricing Metrics"),
                 tableOutput("metricsTable"),
                 br(),
                 h4("Tail Risk Metrics (Portfolio Level)"),
                 tableOutput("riskTable"),
                 br(),
                 h4("Severity Model Fit Parameters (From Real Data)"),
                 tableOutput("fitTable")
        )
      )
    )
  )
)

#-----------------------------
# Server
#-----------------------------

server <- function(input, output) {
  
  # Inflation-adjusted real loss data
  inflated_real_data <- reactive({
    
    infl <- 1 + input$inflation / 100
    
    df_real %>%
      mutate(loss_inflated = loss * infl)
  })
  
  
  # Fit severity model from real data
  sev_fit <- reactive({
    
    df <- inflated_real_data()
    loss_data <- df$loss_inflated
    
    if(input$sev_model == "Lognormal") {
      
      # Fit lognormal: parameters are meanlog and sdlog
      fit <- fitdist(loss_data, "lnorm")
      return(list(
        model = "Lognormal",
        meanlog = fit$estimate["meanlog"],
        sdlog = fit$estimate["sdlog"]
      ))
      
    } else {
      
      # Pareto fit (tail option)
      if(input$use_tail_only) {
        threshold <- quantile(loss_data, 0.95)
        tail_losses <- loss_data[loss_data >= threshold]
        
        # Fit Pareto to tail
        fit <- fitdist(tail_losses, "pareto")
        
        return(list(
          model = "Pareto (Tail Fit)",
          shape = fit$estimate["shape"],
          scale = fit$estimate["scale"],
          threshold = threshold
        ))
        
      } else {
        fit <- fitdist(loss_data, "pareto")
        
        return(list(
          model = "Pareto (Full Fit)",
          shape = fit$estimate["shape"],
          scale = fit$estimate["scale"],
          threshold = NA
        ))
      }
    }
  })
  
  
  # Generate severity samples from fitted model
  simulate_severity <- function(n, fit_params) {
    
    if(fit_params$model == "Lognormal") {
      return(rlnorm(n, meanlog = fit_params$meanlog, sdlog = fit_params$sdlog))
    }
    
    # Pareto
    return(rpareto(n, shape = fit_params$shape, scale = fit_params$scale))
  }
  
  
  # Simulate annual losses based on frequency + fitted severity
  sim_data <- reactive({
    
    set.seed(123)
    
    n_years <- input$n_years
    
    # simulate frequency
    freq <- simulate_frequency(n_years, input$freq_model, input$lambda, input$nb_size)
    
    fit_params <- sev_fit()
    
    annual_loss <- numeric(n_years)
    annual_ceded <- numeric(n_years)
    
    for(i in 1:n_years) {
      
      n_claims <- freq[i]
      
      if(n_claims > 0) {
        sev <- simulate_severity(n_claims, fit_params)
        
        gross_total <- sum(sev)
        ceded_total <- sum(apply_layer(sev, input$retention, input$limit))
        
      } else {
        gross_total <- 0
        ceded_total <- 0
      }
      
      annual_loss[i] <- gross_total
      annual_ceded[i] <- ceded_total
    }
    
    data.frame(
      year = 1:n_years,
      annual_loss = annual_loss,
      annual_ceded = annual_ceded
    )
  })
  
  
  #-----------------------------
  # Output: Fit parameters table
  #-----------------------------
  
  output$fitTable <- renderTable({
    
    fit <- sev_fit()
    
    if(fit$model == "Lognormal") {
      data.frame(
        Model = fit$model,
        Parameter = c("meanlog", "sdlog"),
        Value = round(c(fit$meanlog, fit$sdlog), 4)
      )
    } else {
      data.frame(
        Model = fit$model,
        Parameter = c("shape (alpha)", "scale (theta)", "threshold (if tail fit)"),
        Value = round(c(fit$shape, fit$scale, fit$threshold), 4)
      )
    }
  })
  
  
  #-----------------------------
  # Output: Pricing metrics
  #-----------------------------
  
  output$metricsTable <- renderTable({
    
    df <- sim_data()
    
    expected_gross <- mean(df$annual_loss)
    expected_ceded <- mean(df$annual_ceded)
    
    loading <- input$loading / 100
    premium <- expected_ceded * (1 + loading)
    
    data.frame(
      Metric = c("Expected Annual Gross Loss",
                 "Expected Annual Ceded Loss",
                 "Risk Loading (%)",
                 "Reinsurance Premium"),
      Value = round(c(expected_gross,
                      expected_ceded,
                      input$loading,
                      premium), 2)
    )
  })
  
  
  #-----------------------------
  # Output: Tail metrics
  #-----------------------------
  
  output$riskTable <- renderTable({
    
    df <- sim_data()
    
    gross_var <- VaR(df$annual_loss, 0.99)
    gross_tvar <- TVaR(df$annual_loss, 0.99)
    
    ceded_var <- VaR(df$annual_ceded, 0.99)
    ceded_tvar <- TVaR(df$annual_ceded, 0.99)
    
    data.frame(
      Metric = c("Gross VaR 99%", "Gross TVaR 99%",
                 "Ceded VaR 99%", "Ceded TVaR 99%"),
      Value = round(c(gross_var, gross_tvar, ceded_var, ceded_tvar), 2)
    )
  })
  
  
  #-----------------------------
  # Plot 1: Real severity density
  #-----------------------------
  
  output$densityPlot <- renderPlot({
    
    df <- inflated_real_data()
    
    ggplot(df, aes(x = loss_inflated)) +
      geom_density(color = "red", linewidth = 1) +
      coord_cartesian(xlim = c(0, input$xmax)) +
      ggtitle("Real Claims Severity Density (Inflation Adjusted)") +
      xlab("Loss Amount") +
      ylab("Density") +
      theme_minimal()
  })
  
  
  #-----------------------------
  # Plot 2: Aggregate annual loss
  #-----------------------------
  
  output$aggregatePlot <- renderPlot({
    
    df <- sim_data()
    
    ggplot(df, aes(x = annual_loss)) +
      geom_histogram(bins = 50, fill = "steelblue", alpha = 0.8) +
      ggtitle("Simulated Annual Aggregate Loss") +
      xlab("Annual Loss") +
      ylab("Count") +
      theme_minimal()
  })
  
  
  #-----------------------------
  # Plot 3: Annual ceded loss
  #-----------------------------
  
  output$cededPlot <- renderPlot({
    
    df <- sim_data()
    
    ggplot(df, aes(x = annual_ceded)) +
      geom_histogram(bins = 50, fill = "darkorange", alpha = 0.8) +
      ggtitle("Simulated Annual Ceded Loss Distribution") +
      xlab("Annual Ceded Loss") +
      ylab("Count") +
      theme_minimal()
  })
  
  
  #-----------------------------
  # Plot 4: Layer function
  #-----------------------------
  
  output$layerPlot <- renderPlot({
    
    retention <- input$retention
    limit <- input$limit
    
    x <- seq(0, retention + limit + retention, length.out = 500)
    ceded <- pmin(pmax(x - retention, 0), limit)
    
    ggplot(data.frame(gross = x, ceded = ceded), aes(x = gross, y = ceded)) +
      geom_line(color = "purple", linewidth = 1.2) +
      ggtitle("Reinsurance Layer Function (Ceded vs Gross Loss)") +
      xlab("Gross Loss (Per Claim)") +
      ylab("Ceded Loss") +
      theme_minimal()
  })
}

#-----------------------------
# Run App
#-----------------------------
shinyApp(ui = ui, server = server)
