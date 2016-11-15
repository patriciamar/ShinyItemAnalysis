# Model for plot
model_DIF_logistic_IRT_Z_plot <- reactive({
  group <- DIF_groups()
  data <- correct_answ()
  
  mod <- difLogistic(Data = data, group = group, focal.name = 1, 
                     type = input$type_plot_DIF_logistic_IRT_Z,
                     match = scale(scored_test()))
  mod
})

# Model for print
model_DIF_logistic_IRT_Z__print <- reactive({
  group <- DIF_groups()
  data <- correct_answ()
  
  mod <- difLogistic(Data = data, group = group, focal.name = 1, 
                     type = input$type_print_DIF_logistic_IRT_Z,
                     match = scale(scored_test()))
  mod
})

# Output print
output$print_DIF_logistic_IRT_Z <- renderPrint({
  print(model_DIF_logistic_print())
})

# Plot
output$plot_DIF_logistic_IRT_Z <- renderPlot({
  group <- DIF_groups()
  data <- correct_answ()
  
  type <- input$type_plot_DIF_logistic
  plotDIFLogistic(data, group, 
                  type = input$type_plot_DIF_logistic_IRT_Z, 
                  item =  input$inSlider, 
                  IRT = T)
})

output$tab_coef_DIF_logistic_IRT_Z <- renderTable({
  
  tab_coef <- model_DIF_logistic_IRT_Z_plot()$logitPar[input$inSlider, ]
  
  tab_coef_old <- tab_coef
  
  tab_coef[1] <- tab_coef_old[2]
  tab_coef[2] <- -(tab_coef_old[1] / tab_coef_old[2])
  tab_coef[3] <- tab_coef_old[4]
  tab_coef[4] <- -(tab_coef_old[3] + (tab_coef_old[4] * (-tab_coef_old[1] / tab_coef_old[2]))) /
    (tab_coef_old[2] + tab_coef_old[4])
 
  tab_sd <- rep(NA, 4) # not yet calculated
  tab <- data.frame(tab_coef, tab_sd)
  
  
  rownames(tab) <- c('a', 'b', 'aDIF', 'bDIF')
  colnames(tab) <- c("Estimate", "SD")
  
  tab
}, 
include.rownames = T,
include.colnames = T)
