shinyUI(
fluidPage(titlePanel("Delta Method"),
          mainPanel(
            h4("Table of parameters"),
            fluidRow(column(12, align = "center", tableOutput('tab_DM'))),
            br()
          ))
)