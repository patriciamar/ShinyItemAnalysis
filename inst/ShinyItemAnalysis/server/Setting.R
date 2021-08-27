observeEvent(input$setting_figures_text_size, {
  setting_figures$text_size <- input$setting_figures_text_size
})
observeEvent(input$setting_figures_height, {
  setting_figures$height <- input$setting_figures_height
})
observeEvent(input$setting_figures_width, {
  setting_figures$width <- input$setting_figures_width
})
observeEvent(input$setting_figures_dpi, {
  setting_figures$dpi <- input$setting_figures_dpi
})
