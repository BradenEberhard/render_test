# server.R

function(input, output) {

  output$plot <- renderPlot({
    ggplot(mtcars, aes(x = mpg, y = disp)) +
      geom_point() +
      theme(text = element_text(family = "Azeret Mono"))
  })

}
