server <- function(input, output, session) {
  aq_plot <- reactive({
    udara_bandung %>%
      filter(station %in% input$station) %>%
      ggplot(aes_string(x = "day", y = input$parameter, colour = "station")) +
      geom_line() +
      scale_x_date(date_breaks = "2 weeks") +
      labs(
        x = "",
        y = ifelse(input$parameter_label == "", input$parameter, input$parameter_label),
        colour = "Station"
      ) +
      theme_light()
  })

  output$aq_plot <- renderPlot({
    aq_plot()
  })
}

