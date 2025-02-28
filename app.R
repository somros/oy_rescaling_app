library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(htmltools)
library(rmarkdown)

# The rescaling function is unchanged
rescale_fishing_mortality <- function(abc, cap, w_raw) {
  # Function body remains the same
  # (Function implementation kept from original code)
  
  # 1. Calculate the first reduction factor r1
  r1 <- cap / sum(abc)
  
  # 2. Calculate the first scaling factor s1
  s1 <- r1^(1/w_raw)
  
  # 3. Perform the first catch adjustment
  abc_1 <- abc * s1
  
  # 4. Calculate the second reduction factor r2
  r2 <- cap / sum(abc_1)
  
  # 5. Calculate the second scaling factor s2
  w_scale <- w_raw/max(w_raw)
  s2 <- w_scale + (1 - w_scale) * r2
  
  # 6. Perform the second catch adjustment
  abc_2 <- abc_1 * s2
  
  # 7. Calculate the third reduction factor r_3 and scaling factor s_3
  r3 <- cap/sum(abc_2)
  s3 <- s2 * r3
  
  # 8. Perform the third catch adjustment
  abc_3 <- s3 * abc_1
  
  # 9. Constrain the projected catch to not exceed original ABC
  excess <- 0
  abc_4 <- abc_3
  for(i in 1:length(abc_4)){
    if(abc_4[i] > abc[i]){
      excess <- excess + (abc_4[i] - abc[i])
      abc_4[i] <- abc[i]
    }
  }
  
  # 10. Redistribute excess
  w_resid <- (abc - abc_4)/sum(abc - abc_4)
  w_resid[is.nan(w_resid)] <- 0
  abc_final <- abc_4 + (w_resid * excess)
  
  # 11. Scalar on fishing mortality calculation
  f_rescale <- abc_final/abc
  
  return(list(
    abc = abc,
    weights = w_raw,
    total_abc = sum(abc),
    cap = cap,
    r1 = r1,
    abc_1 = abc_1,
    s1 = s1,
    w_scale = w_scale,
    r2 = r2,
    s2 = s2,
    abc_2 = abc_2,
    r3 = r3,
    s3 = s3,
    abc_3 = abc_3,
    abc_4 = abc_4,
    excess = excess,
    w_resid = w_resid,
    abc_final = abc_final,
    total_abc_final = sum(abc_final),
    f_rescale = f_rescale
  ))
}

# Function to safely read the HTML file and create an iframe with it
read_html_file <- function() {
  html_path <- "www/Method.html"
  
  # Check if file exists
  if (!file.exists(html_path)) {
    return(tags$div(
      class = "alert alert-warning",
      tags$b("Documentation not available:"), 
      "The method documentation file could not be found. Please ensure the HTML file is in the www directory."
    ))
  }
  
  # Create an iframe using srcdoc attribute
  tags$iframe(
    id = "quarto-iframe",
    srcdoc = paste(readLines(html_path, warn = FALSE), collapse = "\n"),
    style = "width: 100%; height: 800px; border: 1px solid #ddd; border-radius: 4px;",
    sandbox = "allow-same-origin allow-scripts",
    frameBorder = 0
  )
}

# Modified UI Definition with sliders that have input boxes
ui <- fluidPage(
  titlePanel("Multi-Species ABC Rescaling Tool"),
  
  sidebarLayout(
    sidebarPanel(
      # Action button
      actionButton("run", "Calculate", class = "btn-primary", style = "margin-bottom: 15px;"),
      
      # Custom CSS for slider-input combinations
      tags$style(HTML("
        .slider-input-container {
          display: flex;
          align-items: center;
          margin-bottom: 15px;
        }
        .slider-container {
          flex: 1;
          padding-right: 10px;
        }
        .numeric-input-container {
          width: 100px;
        }
        .input-label {
          margin-bottom: 5px;
          font-weight: bold;
        }
      ")),
      
      # Cap input with slider and input box
      h4("Optimum Yield Cap"),
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Cap Value:"),
              sliderInput("cap_slider", NULL, 
                          min = 0, max = 500000, value = 200000, 
                          step = 1000, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("cap", NULL, value = 200000, min = 0, max = 500000)
          )
      ),
      
      # ABC inputs with sliders and input boxes
      h4("ABC Values"),
      
      # Pollock
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Pollock:"),
              sliderInput("abc1_slider", NULL, 
                          min = 0, max = 200000, value = 100000, 
                          step = 1000, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("abc1", NULL, value = 100000, min = 0, max = 200000)
          )
      ),
      
      # Cod
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Cod:"),
              sliderInput("abc2_slider", NULL, 
                          min = 0, max = 200000, value = 80000, 
                          step = 1000, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("abc2", NULL, value = 80000, min = 0, max = 200000)
          )
      ),
      
      # Arrowtooth Flounder
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Arrowtooth Flounder:"),
              sliderInput("abc3_slider", NULL, 
                          min = 0, max = 200000, value = 60000, 
                          step = 1000, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("abc3", NULL, value = 60000, min = 0, max = 200000)
          )
      ),
      
      # Pacific Ocean Perch
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Pacific Ocean Perch:"),
              sliderInput("abc4_slider", NULL, 
                          min = 0, max = 200000, value = 40000, 
                          step = 1000, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("abc4", NULL, value = 40000, min = 0, max = 200000)
          )
      ),
      
      # Shallow-water flatfish
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Shallow-water flatfish:"),
              sliderInput("abc5_slider", NULL, 
                          min = 0, max = 200000, value = 40000, 
                          step = 1000, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("abc5", NULL, value = 40000, min = 0, max = 200000)
          )
      ),
      
      # Weight inputs
      h4("Weights (higher = less reduction)"),
      
      # Pollock Weight
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Pollock:"),
              sliderInput("w1_slider", NULL, 
                          min = 0, max = 1000, value = 4, 
                          step = 0.01, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("w1", NULL, value = 4, min = 0, max = 1000, step = 0.01)
          )
      ),
      
      # Cod Weight
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Cod:"),
              sliderInput("w2_slider", NULL, 
                          min = 0, max = 1000, value = 5, 
                          step = 0.01, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("w2", NULL, value = 5, min = 0, max = 1000, step = 0.01)
          )
      ),
      
      # Arrowtooth Flounder Weight
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Arrowtooth Flounder:"),
              sliderInput("w3_slider", NULL, 
                          min = 0, max = 1000, value = 1, 
                          step = 0.01, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("w3", NULL, value = 1, min = 0, max = 1000, step = 0.01)
          )
      ),
      
      # Pacific Ocean Perch Weight
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Pacific Ocean Perch:"),
              sliderInput("w4_slider", NULL, 
                          min = 0, max = 1000, value = 3, 
                          step = 0.01, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("w4", NULL, value = 3, min = 0, max = 1000, step = 0.01)
          )
      ),
      
      # Shallow-water flatfish Weight
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Shallow-water flatfish:"),
              sliderInput("w5_slider", NULL, 
                          min = 0, max = 1000, value = 2, 
                          step = 0.01, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("w5", NULL, value = 2, min = 0, max = 1000, step = 0.01)
          )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        # Changed order - Visualization as first tab
        tabPanel("Visualization",
                 plotOutput("comparison_plot", height = "500px"),
                 # Add reduction factor table below the plot
                 h4("Rescaling by Species"),
                 tableOutput("reduction_table")
        ),
        tabPanel("Summary", 
                 h4("Original Values"),
                 verbatimTextOutput("original_summary"),
                 h4("New Values"),
                 verbatimTextOutput("new_summary")
        ),
        tabPanel("Detailed Results",
                 verbatimTextOutput("detailed_results")
        ),
        tabPanel("Method Documentation",
                 uiOutput("method_doc")
        )
      )
    )
  )
)

# Server logic with additional observers to sync slider and input values
server <- function(input, output, session) {
  # Sync the cap slider and numeric input
  observe({
    updateNumericInput(session, "cap", value = input$cap_slider)
  })
  observeEvent(input$cap, {
    updateSliderInput(session, "cap_slider", value = input$cap)
  }, ignoreInit = TRUE)
  
  # Sync ABC slider and numeric inputs
  # Pollock
  observe({
    updateNumericInput(session, "abc1", value = input$abc1_slider)
  })
  observeEvent(input$abc1, {
    updateSliderInput(session, "abc1_slider", value = input$abc1)
  }, ignoreInit = TRUE)
  
  # Cod
  observe({
    updateNumericInput(session, "abc2", value = input$abc2_slider)
  })
  observeEvent(input$abc2, {
    updateSliderInput(session, "abc2_slider", value = input$abc2)
  }, ignoreInit = TRUE)
  
  # Arrowtooth Flounder
  observe({
    updateNumericInput(session, "abc3", value = input$abc3_slider)
  })
  observeEvent(input$abc3, {
    updateSliderInput(session, "abc3_slider", value = input$abc3)
  }, ignoreInit = TRUE)
  
  # Pacific Ocean Perch
  observe({
    updateNumericInput(session, "abc4", value = input$abc4_slider)
  })
  observeEvent(input$abc4, {
    updateSliderInput(session, "abc4_slider", value = input$abc4)
  }, ignoreInit = TRUE)
  
  # Shallow-water flatfish
  observe({
    updateNumericInput(session, "abc5", value = input$abc5_slider)
  })
  observeEvent(input$abc5, {
    updateSliderInput(session, "abc5_slider", value = input$abc5)
  }, ignoreInit = TRUE)
  
  # Sync weight slider and numeric inputs
  # Pollock Weight
  observe({
    updateNumericInput(session, "w1", value = input$w1_slider)
  })
  observeEvent(input$w1, {
    updateSliderInput(session, "w1_slider", value = input$w1)
  }, ignoreInit = TRUE)
  
  # Cod Weight
  observe({
    updateNumericInput(session, "w2", value = input$w2_slider)
  })
  observeEvent(input$w2, {
    updateSliderInput(session, "w2_slider", value = input$w2)
  }, ignoreInit = TRUE)
  
  # Arrowtooth Flounder Weight
  observe({
    updateNumericInput(session, "w3", value = input$w3_slider)
  })
  observeEvent(input$w3, {
    updateSliderInput(session, "w3_slider", value = input$w3)
  }, ignoreInit = TRUE)
  
  # Pacific Ocean Perch Weight
  observe({
    updateNumericInput(session, "w4", value = input$w4_slider)
  })
  observeEvent(input$w4, {
    updateSliderInput(session, "w4_slider", value = input$w4)
  }, ignoreInit = TRUE)
  
  # Shallow-water flatfish Weight
  observe({
    updateNumericInput(session, "w5", value = input$w5_slider)
  })
  observeEvent(input$w5, {
    updateSliderInput(session, "w5_slider", value = input$w5)
  }, ignoreInit = TRUE)
  
  # Render the method documentation
  output$method_doc <- renderUI({
    read_html_file()
  })
  
  # Reactive calculation
  results <- eventReactive(input$run, {
    abc <- c(input$abc1, input$abc2, input$abc3, input$abc4, input$abc5)
    w_raw <- c(input$w1, input$w2, input$w3, input$w4, input$w5)
    
    rescale_fishing_mortality(abc, input$cap, w_raw)
  })
  
  # Original summary output
  output$original_summary <- renderPrint({
    res <- results()
    stocks <- c("Pollock", "Cod", "Arrowtooth Flounder", "Pacific Ocean Perch", "Shallow-water flatfish")
    cat("Original ABCs:\n")
    for(i in 1:5) {
      cat(stocks[i], ":", res$abc[i], "\n")
    }
    cat("\nTotal ABC:", sum(res$abc), "\n")
    cat("Cap:", res$cap, "\n")
    cat("Initial reduction factor (r1):", res$r1, "\n")
  })
  
  # New summary output
  output$new_summary <- renderPrint({
    res <- results()
    stocks <- c("Pollock", "Cod", "Arrowtooth Flounder", "Pacific Ocean Perch", "Shallow-water flatfish")
    cat("Final ABCs:\n")
    for(i in 1:5) {
      cat(stocks[i], ":", res$abc_final[i], "\n")
    }
    cat("\nTotal New ABC:", res$total_abc_final, "\n")
    cat("\nF Rescaling Factors:\n")
    for(i in 1:5) {
      cat(stocks[i], ":", res$f_rescale[i], "\n")
    }
  })
  
  # Detailed results
  output$detailed_results <- renderPrint({
    res <- results()
    stocks <- c("Pollock", "Cod", "Arrowtooth Flounder", "Pacific Ocean Perch", "Shallow-water flatfish")
    
    cat("Step-by-Step Results:\n\n")
    for(i in 1:5) {
      cat(stocks[i], ":\n")
      cat("Original ABC:", res$abc[i], "\n")
      cat("Weight:", res$weights[i], "\n")
      cat("Scaled Weight:", res$w_scale[i], "\n")
      cat("ABC after first adjustment:", res$abc_1[i], "\n")
      cat("Intermediate scale:", res$s2[i], "\n")
      cat("ABC after intermediate scaling:", res$abc_2[i], "\n")
      cat("Final scale:", res$s3[i], "\n")
      cat("ABC after final scaling:", res$abc_3[i], "\n")
      cat("ABC after HCR check:", res$abc_4[i], "\n")
      cat("Final ABC:", res$abc_final[i], "\n")
      cat("F rescaling factor:", res$f_rescale[i], "\n\n")
    }
    
    cat("Summary Statistics:\n")
    cat("Total excess redistributed:", res$excess, "\n")
    cat("Final r2 adjustment:", res$r2, "\n")
  })
  
  # Reduction factors table
  output$reduction_table <- renderTable({
    res <- results()
    stocks <- c("Pollock", "Cod", "Arrowtooth Flounder", "Pacific Ocean Perch", "Shallow-water flatfish")
    
    data.frame(
      Species = stocks,
      Original_ABC = res$abc,
      Final_ABC = res$abc_final,
      Reduction_Factor = res$f_rescale,  # No rounding here, will control in digits parameter
      Weight = res$weights
    )
  }, align = "lrrrr", digits = c(0, 0, 0, 3, 0), width = "100%", striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Visualization with larger fonts and rescaling factors
  output$comparison_plot <- renderPlot({
    res <- results()
    stocks <- c("Pollock", "Cod", "Arrowtooth Flounder", "Pacific Ocean Perch", "Shallow-water flatfish")
    
    # Create data frame for plotting
    plot_data <- data.frame(
      Species = rep(stocks, 2),
      Type = rep(c("Original", "New"), each = 5),
      ABC = c(res$abc, res$abc_final)
    )
    
    # Make species a factor with specified order
    plot_data$Species <- factor(plot_data$Species, levels = stocks)
    
    # Colorblind-friendly palette (blue and orange)
    cb_palette <- c("#0072B2", "#E69F00")
    
    # Create the plot with larger fonts
    p <- ggplot(plot_data, aes(x = Species, y = ABC, fill = Type)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.7) +
      geom_hline(yintercept = input$cap, linetype = "dashed", color = "red", linewidth = 1.2) +
      labs(
        title = "ABC Comparison",
        subtitle = paste("Cap =", input$cap),
        y = "ABC Value",
        fill = "ABC Type"
      ) +
      scale_fill_manual(values = cb_palette) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 16),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = "top"
      )
    
    p
  })
}

# Run the app
shinyApp(ui = ui, server = server)