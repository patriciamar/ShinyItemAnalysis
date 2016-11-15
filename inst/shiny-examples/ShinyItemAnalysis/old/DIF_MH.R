model_DIF_MH_print <- reactive({
  group <- DIF_groups()
  data <- correct_answ()
  
  mod <- difMH(Data = data, group = group, focal.name = 1, 
               p.adjust.method = input$correction_method_MZ_print)
  mod
})

model_DIF_MH_tables <- reactive({
  group <- DIF_groups()
  data <- correct_answ()
  
  mod <- difMH(Data = data, group = group, focal.name = 1, 
               p.adjust.method = input$correction_method_MZ_tables)
  mod
})

# ** Output print ####
output$print_DIF_MH <- renderPrint({
  print(model_DIF_MH_print())
})


# ** Contingency tables ####
output$table_DIF_MH <- renderTable({
  
  data <- GMAT[, 1:20]
  group <- GMAT[, 21]
  total <- apply(data, 1, sum)
  
  item <- 7
  
  df <- data.frame(data[, item], group)
  colnames(df) <- c("Answer", "Group")
  df$Answer <- relevel(factor(df$Answer, labels = c("Incorrect", "Correct")), "Correct")
  df$Group <- factor(df$Group, labels = c("Reference Group", "Focal Group"))
  
  df <- df[total == cut, ]

  tab <- dcast(data.frame(xtabs(~ Group + Answer, data = df)), Group ~ Answer,
               value.var = "Freq", margins = T, fun = sum)
  
  colnames(tab)[4] <- tab$Group[3] <- levels(tab$Group)[3]  <- "Total"
  colnames(tab)[1] <- ""
  tab
})


data(GMAT)



