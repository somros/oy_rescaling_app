library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(htmltools)
library(rmarkdown)

# Simplified rescaling function using bisection method from app.R
rescale_fishing_mortality <- function(abc, cap, w_raw) {
  # Check if rescaling is needed (only rescale if sum(abc) > cap)
  if (sum(abc) <= cap) {
    # No rescaling needed, return original values
    return(list(
      abc = abc,
      weights = w_raw,
      total_abc = sum(abc),
      cap = cap,
      mult = NA,  # No multiplier needed
      r = 1,      # No reduction needed
      r_scaled = rep(1, length(abc)),
      abc_intermediate = abc,  # Same as original
      abc_final = abc,         # Same as original
      total_abc_final = sum(abc),
      f_rescale = rep(1, length(abc))  # No rescaling
    ))
  }
  
  # Define the prediction function using mult
  ABCPred <- function(mult) {
    r <- cap/sum(abc)
    r_scaled <- r^(1/(w_raw*mult))
    return(abc * r_scaled)
  }
  
  # Define the objective function to minimize (squared difference between cap and sum)
  fn <- function(mult) {
    ABC1 <- ABCPred(mult)
    obj <- (cap - sum(ABC1))^2
    return(obj)
  }
  
  # Simplified bisection method (no visualization components)
  bisection_min <- function(a, b, fn, tol = 1e-7, max_iter = 100) {
    iter <- 0
    
    while ((b - a) > tol && iter < max_iter) {
      c <- (a + b) / 2  # Calculate midpoint
      h <- max(tol * 10, (b - a) / 100)  # Step size for numerical derivative
      
      # Numerical derivative at midpoint using central difference approximation
      deriv_c <- (fn(c + h) - fn(c - h)) / (2 * h)
      
      if (abs(deriv_c) < tol) {
        # Found a stationary point (derivative close to zero)
        return(c)
      } else if (deriv_c > 0) {
        # Function is increasing, move left (minimum is to the left)
        b <- c
      } else {
        # Function is decreasing, move right (minimum is to the right)
        a <- c
      }
      
      iter <- iter + 1
    }
    
    return((a + b) / 2)
  }
  
  # Find the optimal mult value using bisection method
  optimal_mult <- bisection_min(0, 5, fn)
  
  # Calculate the rescaled ABC values using the optimal mult
  r <- cap/sum(abc)
  r_scaled <- r^(1/(w_raw*optimal_mult))
  abc_final <- abc * r_scaled
  
  # Calculate f_rescale as the ratio of final ABC to original ABC
  f_rescale <- abc_final/abc
  
  # Return results with simplified structure
  return(list(
    abc = abc,
    weights = w_raw,
    total_abc = sum(abc),
    cap = cap,
    mult = optimal_mult,
    r = r,
    r_scaled = r_scaled,
    abc_intermediate = abc_final,  # For UI consistency
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

# Modified UI Definition with all 9 species from app_bsai.R
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
      
      # Cap input with slider and input box - set to 2 million
      h4("Optimum Yield Cap"),
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Cap Value:"),
              sliderInput("cap_slider", NULL, 
                          min = 0, max = 3000000, value = 2000000, 
                          step = 10000, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("cap", NULL, value = 2000000, min = 0, max = 3000000)
          )
      ),
      
      # ABC inputs with sliders and input boxes - all 9 species
      h4("ABC Values"),
      
      # Pollock
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Pollock:"),
              sliderInput("abc1_slider", NULL, 
                          min = 0, max = 3000000, value = 2355654, 
                          step = 1000, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("abc1", NULL, value = 2355654, min = 0, max = 3000000)
          )
      ),
      
      # Pacific cod
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Pacific cod:"),
              sliderInput("abc2_slider", NULL, 
                          min = 0, max = 500000, value = 180383, 
                          step = 1000, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("abc2", NULL, value = 180383, min = 0, max = 500000)
          )
      ),
      
      # Arrowtooth Flounder
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Arrowtooth Flounder:"),
              sliderInput("abc3_slider", NULL, 
                          min = 0, max = 200000, value = 87690, 
                          step = 1000, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("abc3", NULL, value = 87690, min = 0, max = 200000)
          )
      ),
      
      # Pacific Ocean Perch
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Pacific Ocean Perch:"),
              sliderInput("abc4_slider", NULL, 
                          min = 0, max = 200000, value = 41096, 
                          step = 1000, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("abc4", NULL, value = 41096, min = 0, max = 200000)
          )
      ),
      
      # Atka Mackerel
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Atka Mackerel:"),
              sliderInput("abc5_slider", NULL, 
                          min = 0, max = 200000, value = 95358, 
                          step = 1000, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("abc5", NULL, value = 95358, min = 0, max = 200000)
          )
      ),
      
      # Flathead Sole
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Flathead Sole:"),
              sliderInput("abc6_slider", NULL, 
                          min = 0, max = 200000, value = 67289, 
                          step = 1000, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("abc6", NULL, value = 67289, min = 0, max = 200000)
          )
      ),
      
      # Rock Sole
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Rock Sole:"),
              sliderInput("abc7_slider", NULL, 
                          min = 0, max = 200000, value = 122091, 
                          step = 1000, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("abc7", NULL, value = 122091, min = 0, max = 200000)
          )
      ),
      
      # Sablefish
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Sablefish:"),
              sliderInput("abc8_slider", NULL, 
                          min = 0, max = 100000, value = 24550, 
                          step = 1000, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("abc8", NULL, value = 24550, min = 0, max = 100000)
          )
      ),
      
      # Yellowfin Sole
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Yellowfin Sole:"),
              sliderInput("abc9_slider", NULL, 
                          min = 0, max = 500000, value = 265913, 
                          step = 1000, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("abc9", NULL, value = 265913, min = 0, max = 500000)
          )
      ),
      
      # Weight inputs - all set to 1
      h4("Weights (higher = less reduction)"),
      
      # Pollock Weight
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Pollock:"),
              sliderInput("w1_slider", NULL, 
                          min = 0, max = 10, value = 1, 
                          step = 0.1, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("w1", NULL, value = 1, min = 0, max = 10, step = 0.1)
          )
      ),
      
      # Pacific cod Weight
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Pacific cod:"),
              sliderInput("w2_slider", NULL, 
                          min = 0, max = 10, value = 1, 
                          step = 0.1, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("w2", NULL, value = 1, min = 0, max = 10, step = 0.1)
          )
      ),
      
      # Arrowtooth Flounder Weight
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Arrowtooth Flounder:"),
              sliderInput("w3_slider", NULL, 
                          min = 0, max = 10, value = 1, 
                          step = 0.1, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("w3", NULL, value = 1, min = 0, max = 10, step = 0.1)
          )
      ),
      
      # Pacific Ocean Perch Weight
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Pacific Ocean Perch:"),
              sliderInput("w4_slider", NULL, 
                          min = 0, max = 10, value = 1, 
                          step = 0.1, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("w4", NULL, value = 1, min = 0, max = 10, step = 0.1)
          )
      ),
      
      # Atka Mackerel Weight
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Atka Mackerel:"),
              sliderInput("w5_slider", NULL, 
                          min = 0, max = 10, value = 1, 
                          step = 0.1, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("w5", NULL, value = 1, min = 0, max = 10, step = 0.1)
          )
      ),
      
      # Flathead Sole Weight
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Flathead Sole:"),
              sliderInput("w6_slider", NULL, 
                          min = 0, max = 10, value = 1, 
                          step = 0.1, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("w6", NULL, value = 1, min = 0, max = 10, step = 0.1)
          )
      ),
      
      # Rock Sole Weight
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Rock Sole:"),
              sliderInput("w7_slider", NULL, 
                          min = 0, max = 10, value = 1, 
                          step = 0.1, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("w7", NULL, value = 1, min = 0, max = 10, step = 0.1)
          )
      ),
      
      # Sablefish Weight
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Sablefish:"),
              sliderInput("w8_slider", NULL, 
                          min = 0, max = 10, value = 1, 
                          step = 0.1, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("w8", NULL, value = 1, min = 0, max = 10, step = 0.1)
          )
      ),
      
      # Yellowfin Sole Weight
      div(class = "slider-input-container",
          div(class = "slider-container",
              div(class = "input-label", "Yellowfin Sole:"),
              sliderInput("w9_slider", NULL, 
                          min = 0, max = 10, value = 1, 
                          step = 0.1, ticks = TRUE, width = "100%")
          ),
          div(class = "numeric-input-container",
              numericInput("w9", NULL, value = 1, min = 0, max = 10, step = 0.1)
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
  
  # Sync ABC slider and numeric inputs for all 9 species
  # Pollock
  observe({
    updateNumericInput(session, "abc1", value = input$abc1_slider)
  })
  observeEvent(input$abc1, {
    updateSliderInput(session, "abc1_slider", value = input$abc1)
  }, ignoreInit = TRUE)
  
  # Pacific cod
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
  
  # Atka Mackerel
  observe({
    updateNumericInput(session, "abc5", value = input$abc5_slider)
  })
  observeEvent(input$abc5, {
    updateSliderInput(session, "abc5_slider", value = input$abc5)
  }, ignoreInit = TRUE)
  
  # Flathead Sole
  observe({
    updateNumericInput(session, "abc6", value = input$abc6_slider)
  })
  observeEvent(input$abc6, {
    updateSliderInput(session, "abc6_slider", value = input$abc6)
  }, ignoreInit = TRUE)
  
  # Rock Sole
  observe({
    updateNumericInput(session, "abc7", value = input$abc7_slider)
  })
  observeEvent(input$abc7, {
    updateSliderInput(session, "abc7_slider", value = input$abc7)
  }, ignoreInit = TRUE)
  
  # Sablefish
  observe({
    updateNumericInput(session, "abc8", value = input$abc8_slider)
  })
  observeEvent(input$abc8, {
    updateSliderInput(session, "abc8_slider", value = input$abc8)
  }, ignoreInit = TRUE)
  
  # Yellowfin Sole
  observe({
    updateNumericInput(session, "abc9", value = input$abc9_slider)
  })
  observeEvent(input$abc9, {
    updateSliderInput(session, "abc9_slider", value = input$abc9)
  }, ignoreInit = TRUE)
  
  # Sync weight slider and numeric inputs for all 9 species
  # Pollock Weight
  observe({
    updateNumericInput(session, "w1", value = input$w1_slider)
  })
  observeEvent(input$w1, {
    updateSliderInput(session, "w1_slider", value = input$w1)
  }, ignoreInit = TRUE)
  
  # Pacific cod Weight
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
  
  # Atka Mackerel Weight
  observe({
    updateNumericInput(session, "w5", value = input$w5_slider)
  })
  observeEvent(input$w5, {
    updateSliderInput(session, "w5_slider", value = input$w5)
  }, ignoreInit = TRUE)
  
  # Flathead Sole Weight
  observe({
    updateNumericInput(session, "w6", value = input$w6_slider)
  })
  observeEvent(input$w6, {
    updateSliderInput(session, "w6_slider", value = input$w6)
  }, ignoreInit = TRUE)
  
  # Rock Sole Weight
  observe({
    updateNumericInput(session, "w7", value = input$w7_slider)
  })
  observeEvent(input$w7, {
    updateSliderInput(session, "w7_slider", value = input$w7)
  }, ignoreInit = TRUE)
  
  # Sablefish Weight
  observe({
    updateNumericInput(session, "w8", value = input$w8_slider)
  })
  observeEvent(input$w8, {
    updateSliderInput(session, "w8_slider", value = input$w8)
  }, ignoreInit = TRUE)
  
  # Yellowfin Sole Weight
  observe({
    updateNumericInput(session, "w9", value = input$w9_slider)
  })
  observeEvent(input$w9, {
    updateSliderInput(session, "w9_slider", value = input$w9)
  }, ignoreInit = TRUE)
  
  # Render the method documentation
  output$method_doc <- renderUI({
    read_html_file()
  })
  
  # Reactive calculation - updated to use the bisection method
  results <- eventReactive(input$run, {
    abc <- c(input$abc1, input$abc2, input$abc3, input$abc4, input$abc5,
             input$abc6, input$abc7, input$abc8, input$abc9)
    w_raw <- c(input$w1, input$w2, input$w3, input$w4, input$w5,
               input$w6, input$w7, input$w8, input$w9)
    
    rescale_fishing_mortality(abc, input$cap, w_raw)
  })
  
  # Original summary output
  output$original_summary <- renderPrint({
    res <- results()
    stocks <- c("Pollock", "Pacific cod", "Arrowtooth Flounder", "Pacific Ocean Perch", "Atka Mackerel",
                "Flathead Sole", "Rock Sole", "Sablefish", "Yellowfin Sole")
    cat("Original ABCs:\n")
    for(i in 1:9) {
      cat(stocks[i], ":", res$abc[i], "\n")
    }
    cat("\nTotal ABC:", sum(res$abc), "\n")
    cat("Cap:", res$cap, "\n")
    cat("Initial reduction factor (r):", res$r, "\n")
  })
  
  # New summary output
  output$new_summary <- renderPrint({
    res <- results()
    stocks <- c("Pollock", "Pacific cod", "Arrowtooth Flounder", "Pacific Ocean Perch", "Atka Mackerel",
                "Flathead Sole", "Rock Sole", "Sablefish", "Yellowfin Sole")
    cat("Final ABCs:\n")
    for(i in 1:9) {
      cat(stocks[i], ":", res$abc_final[i], "\n")
    }
    cat("\nTotal New ABC:", res$total_abc_final, "\n")
    cat("\nF Rescaling Factors:\n")
    for(i in 1:9) {
      cat(stocks[i], ":", res$f_rescale[i], "\n")
    }
  })
  
  # Updated detailed results to match the simplified algorithm
  output$detailed_results <- renderPrint({
    res <- results()
    stocks <- c("Pollock", "Pacific cod", "Arrowtooth Flounder", "Pacific Ocean Perch", "Atka Mackerel",
                "Flathead Sole", "Rock Sole", "Sablefish", "Yellowfin Sole")
    
    cat("Bisection Method Results:\n\n")
    
    if (is.na(res$mult)) {
      cat("No rescaling needed as sum(ABC) <= cap\n\n")
    } else {
      cat("Optimal multiplier value:", res$mult, "\n\n")
    }
    
    cat("Step-by-Step Results:\n\n")
    for(i in 1:9) {
      cat(stocks[i], ":\n")
      cat("Original ABC:", res$abc[i], "\n")
      cat("Weight:", res$weights[i], "\n")
      
      if (!is.na(res$mult)) {
        cat("Reduction factor (r):", res$r, "\n")
        cat("Species-specific scaled reduction factor:", res$r_scaled[i], "\n")
      }
      
      cat("Final ABC:", res$abc_final[i], "\n")
      cat("F rescaling factor:", res$f_rescale[i], "\n\n")
    }
    
    cat("Summary Statistics:\n")
    cat("Total original ABC:", res$total_abc, "\n")
    cat("Total final ABC:", res$total_abc_final, "\n")
    
    if (!is.na(res$mult)) {
      cat("Difference from cap:", res$total_abc_final - res$cap, "\n")
    }
  })
  
  # Reduction factors table
  output$reduction_table <- renderTable({
    res <- results()
    stocks <- c("Pollock", "Pacific cod", "Arrowtooth Flounder", "Pacific Ocean Perch", "Atka Mackerel",
                "Flathead Sole", "Rock Sole", "Sablefish", "Yellowfin Sole")
    
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
    stocks <- c("Pollock", "Pacific cod", "Arrowtooth Flounder", "Pacific Ocean Perch", "Atka Mackerel",
                "Flathead Sole", "Rock Sole", "Sablefish", "Yellowfin Sole")
    
    # Create data frame for plotting
    plot_data <- data.frame(
      Species = rep(stocks, 2),
      Type = rep(c("Original", "New"), each = 9),
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