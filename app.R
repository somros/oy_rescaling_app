library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(htmltools)
library(rmarkdown)

# The rescaling function
rescale_fishing_mortality <- function(abc, cap, w_raw) {
  
  # if(total_abc <= cap) {
  #   return("")
  # }
  
  # 1. Calculate the first reduction factor r1. This calculates the initial reduction factor based on the ratio of the cap to the sum of all projected catches (i.e., how much in excess of the cap are we?).
  r1 <- cap / sum(abc)
  
  # 2. Calculate the first scaling factor s1. This scalar will preserve the catch values for high-value stocks by applying a weighted power transformation.
  
  s1 <- r1^(1/w_raw)
  
  # 3. Perform the first catch adjustment. At this point we may still be in excess of the OY cap.
  
  abc_1 <- abc * s1
  
  # 4.	Calculate the second reduction factor r2. This determines how much further adjustment is still needed after the first adjustment.
  
  r2 <- cap / sum(abc_1)
  
  # 5. 	Calculate the second scaling factor s2. First, scale the weights to 0-1 range:
  
  w_scale <- w_raw/max(w_raw)
  
  # This standardizes the weights relative to the maximum weight value. It is done so that the most valuable stock has a weight of 1. Then compute s_2:
  
  s2 <- w_scale + (1 - w_scale) * r2
  
  # This creates a stock-specific scaling factor as a weighted combination of 1 and the second reduction factor r_2. For the highest-value stock, this ensures minimal rescaling (though we still need to comply with r_2).
  
  # 6.	Perform the second catch adjustment. Note that ABC_2 is an intermediate quantity used to compute the third and final reduction factor.
  
  abc_2 <- abc_1 * s2
  
  # 7. Calculate the third reduction factor r_3 and scaling factor s_3. This will ensure that the total catch remains under the cap after the previous adjustments by performing stock-specific adjustments.
  
  r3 <- cap/sum(abc_2)
  s3 <- s2 * r3
  
  # 8.	Perform the third catch adjustment:
  
  abc_3 <- s3 * abc_1
  
  # 9. 	At this point the aggregate ABC_3 should be at the cap. 
  # However, one remaining issue for some combinations of w (and depending on stock status) is that the previous steps may have caused some upscaling of the original ABC values. This would be a violation of the single-species catch allocation step that precedes the OY rescaling (particularly problematic for stocks that are managed with an HCR). This step constrains the projected catch to not exceed original ABC. We also need to keep track of any leftover unallocated catch E that may results from deducting the ABC in excess in this step:
  
  excess <- 0
  abc_4 <- abc_3
  for(i in 1:length(abc_4)){
    if(abc_4[i] > abc[i]){
      
      excess <- excess + (abc_4[i] - abc[i])
      abc_4[i] <- abc[i]
      
    }
  }
  
  # 10. This last step is only executed if the previous steps have led to E>0. Redistribute E among the other stocks based on their original ABC (i.e., stocks that had larger ABC to absorb more of the excess):
  
  w_resid <- (abc - abc_4)/sum(abc - abc_4)
  abc_final <- abc_4 + (w_resid * excess)
  
  # 11. Scalar on fishing mortality calculation:
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
  # This is the most effective isolation method as it creates a completely separate document context
  tags$iframe(
    id = "quarto-iframe",
    srcdoc = paste(readLines(html_path, warn = FALSE), collapse = "\n"),
    style = "width: 100%; height: 800px; border: 1px solid #ddd; border-radius: 4px;",
    sandbox = "allow-same-origin allow-scripts",
    frameBorder = 0
  )
}

# UI Definition
ui <- fluidPage(
  titlePanel("Multi-Species ABC Rescaling Tool"),
  
  sidebarLayout(
    sidebarPanel(
      # Action button
      actionButton("run", "Calculate"),
      
      # Cap input
      h4("Total ABC Cap"),
      numericInput("cap", "Cap Value:", 200000, min = 0),
      
      # ABC inputs
      h4("ABC Values"),
      numericInput("abc1", "Pollock ABC:", 100000, min = 0),
      numericInput("abc2", "Cod ABC:", 80000, min = 0),
      numericInput("abc3", "Arrowtooth Flounder ABC:", 60000, min = 0),
      numericInput("abc4", "Pacific Ocean Perch ABC:", 40000, min = 0),
      
      # Weight inputs
      h4("Weights (higher = less reduction)"),
      numericInput("w1", "Pollock Weight:", 3, min = 0),
      numericInput("w2", "Cod Weight:", 4, min = 0),
      numericInput("w3", "Arrowtooth Flounder Weight:", 1, min = 0),
      numericInput("w4", "Pacific Ocean Perch Weight:", 2, min = 0)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", 
                 h4("Original Values"),
                 verbatimTextOutput("original_summary"),
                 h4("New Values"),
                 verbatimTextOutput("new_summary")
        ),
        tabPanel("Visualization",
                 plotOutput("comparison_plot")
        ),
        tabPanel("Detailed Results",
                 verbatimTextOutput("detailed_results")
        ),
        tabPanel("Method Documentation",
                 # Using uiOutput to render the iframe on the server side
                 uiOutput("method_doc")
        )
      )
    )
  )
)

# Server logic
server <- function(input, output) {
  # Render the method documentation
  output$method_doc <- renderUI({
    read_html_file()
  })
  
  # Reactive calculation
  results <- eventReactive(input$run, {
    abc <- c(input$abc1, input$abc2, input$abc3, input$abc4)
    w_raw <- c(input$w1, input$w2, input$w3, input$w4)
    
    rescale_fishing_mortality(abc, input$cap, w_raw)
  })
  
  # Original summary output
  output$original_summary <- renderPrint({
    res <- results()
    stocks <- c("Pollock", "Cod", "Arrowtooth Flounder", "Pacific Ocean Perch")
    cat("Original ABCs:\n")
    for(i in 1:4) {
      cat(stocks[i], ":", res$abc[i], "\n")
    }
    cat("\nTotal ABC:", sum(res$abc), "\n")
    cat("Cap:", res$cap, "\n")
    cat("Initial reduction factor (r1):", res$r1, "\n")
  })
  
  # New summary output
  output$new_summary <- renderPrint({
    res <- results()
    stocks <- c("Pollock", "Cod", "Arrowtooth Flounder", "Pacific Ocean Perch")
    cat("Final ABCs:\n")
    for(i in 1:4) {
      cat(stocks[i], ":", res$abc_final[i], "\n")
    }
    cat("\nTotal New ABC:", res$total_abc_final, "\n")
    cat("\nF Rescaling Factors:\n")
    for(i in 1:4) {
      cat(stocks[i], ":", res$f_rescale[i], "\n")
    }
  })
  
  # Detailed results
  output$detailed_results <- renderPrint({
    res <- results()
    stocks <- c("Pollock", "Cod", "Arrowtooth Flounder", "Pacific Ocean Perch")
    
    cat("Step-by-Step Results:\n\n")
    for(i in 1:4) {
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
  
  # Visualization
  output$comparison_plot <- renderPlot({
    res <- results()
    stocks <- c("Pollock", "Cod", "Arrowtooth Flounder", "Pacific Ocean Perch")
    
    # Create data frame for plotting
    plot_data <- data.frame(
      Species = rep(stocks, 2),
      Type = rep(c("Original", "New"), each = 4),
      ABC = c(res$abc, res$abc_final)
    )
    
    # Make species a factor with specified order
    plot_data$Species <- factor(plot_data$Species, levels = stocks)
    
    ggplot(plot_data, aes(x = Species, y = ABC, fill = Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_hline(yintercept = input$cap, linetype = "dashed", color = "red") +
      labs(title = "ABC Comparison",
           subtitle = paste("Cap =", input$cap),
           y = "ABC Value") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the app
shinyApp(ui = ui, server = server)