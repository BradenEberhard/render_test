# ui.R

fluidPage(
  titlePanel("Basic Shiny App with Docker"),
  sidebarLayout(
    sidebarPanel(
      h4("This is a basic Shiny app")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)
