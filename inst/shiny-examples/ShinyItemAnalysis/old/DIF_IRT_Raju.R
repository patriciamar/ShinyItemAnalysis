# IRT Raju
# Model for plot
model_DIF_IRT_Raju_plot <- reactive({
  group <- DIF_groups()
  data <- correct_answ()
  
  guess <- itemPar3PL(data)[, 3]
  mod <- difRaju(Data = data, group = group, focal.name = 1, 
                 model = "3PL", c = guess,
                 p.adjust.method = "BH")
  mod
})

# Model for print
model_DIF_IRT_Raju_print <- reactive({
  group <- DIF_groups()
  data <- correct_answ()
  
  guess <- itemPar3PL(data)[, 3]
  mod <- difRaju(Data = data, group = group, focal.name = 1, 
                 model = "3PL", c = guess,
                 p.adjust.method = "BH")
  mod
})

# Output print
output$print_DIF_IRT_Raju <- renderPrint({
  print(model_DIF_IRT_Raju_print())
})

# Plot
output$plot_DIF_IRT_Raju <- renderPlot({
  
})

# Table with coefficients
output$tab_coef_DIF_IRT_Raju <- renderTable({
  
  m <- nrow(model_DIF_IRT_Raju_plot()$itemParInit)/2
  
  tab_coef <- c(model_DIF_IRT_Raju_plot()$itemParInit[c(input$inSlider, m + input$inSlider), c(1, 2, 6)])[-6]
  tab_sd <- c(model_DIF_IRT_Raju_plot()$itemParInit[c(input$inSlider, m + input$inSlider), c(3, 4)], NA)
  
  tab <- data.frame(tab_coef, tab_sd)
  
  rownames(tab) <- c('aR', 'aF', 'bR', 'bF', 'c')
  colnames(tab) <- c("Estimate", "SD")
  
  tab
}, 
include.rownames = T,
include.colnames = T)