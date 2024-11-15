#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(igraph)

##################################
##
## Alternativa con dos paneles
##
##################################

# Define UI for app
ui <- fluidPage(
  titlePanel("Side-by-Side Scatter Plot Visualization by Topics"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("topic1", "Select First Topic", choices = colnames(res.f)),
      selectInput("topic2", "Select Second Topic", choices = colnames(res.f)),
      width = 3
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("scatterPlot1", height = "600px")),
        column(6, plotOutput("scatterPlot2", height = "600px"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Render the first scatter plot
  output$scatterPlot1 <- renderPlot({
    topic_col1 <- res.f[, input$topic1]
    plot(
      V(net)$l1, V(net)$l2, pch = 16,
      col = V(net)$membership,                                 # Color by membership
      cex = log(topic_col1 + 1) / 2,                     # Scale by topic values
      ylim = c(-150, 150), xlim = c(-150, 150),
      xlab = "Dim1", ylab = "", main = input$topic
    )
    title(input$topic1)
  })
  
  # Render the second scatter plot
  output$scatterPlot2 <- renderPlot({
    topic_col2 <- res.f[, input$topic2]
    plot(
      V(net)$l1, V(net)$l2, pch = 16,
      col = V(net)$membership,                                 # Color by membership
      cex = log(topic_col2 + 1) / 2,                     # Scale by topic values
      ylim = c(-150, 150), xlim = c(-150, 150),
      xlab = "Dim1", ylab = "", main = input$topic
    )
    title(input$topic2)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
