# Model for plot
model_DIF_logistic_plot <- reactive({
  group <- DIF_groups()
  data <- correct_answ()
  
  mod <- difLogistic(Data = data, group = group, focal.name = 1, 
                     type = input$type_plot_DIF_logistic)
  mod
})

# Model for print
model_DIF_logistic_print <- reactive({
  group <- DIF_groups()
  data <- correct_answ()
  
  mod <- difLogistic(Data = data, group = group, focal.name = 1, 
                     type = input$type_print_DIF_logistic)
  mod
})

# Output print
output$print_DIF_logistic <- renderPrint({
  print(model_DIF_logistic_print())
})

# Plot
output$plot_DIF_logistic <- renderPlot({
  group <- DIF_groups()
  data <- correct_answ()
  
  type <- input$type_plot_DIF_logistic
  plotDIFLogistic(data, group, 
                  type = input$type_plot_DIF_logistic, 
                  item =  input$inSlider, 
                  IRT = F)
})

# Table with coefficients
output$tab_coef_DIF_logistic <- renderTable({

  tab_coef <- model_DIF_logistic_plot()$logitPar[input$inSlider, ]
  tab_sd <- rep(NA, 4) # not yet calculated
  
  tab <- data.frame(tab_coef, tab_sd)
  
  rownames(tab) <- c('b0', 'b1', 'b2', 'b3')
  colnames(tab) <- c("Estimate", "SD")
  
  tab
}, 
include.rownames = T,
include.colnames = T)
