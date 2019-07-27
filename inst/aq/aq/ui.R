ui <- fluidPage(
  headerPanel('Air Quality of Kota Bandung'),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "station",
                  label =  "Station",
                  choices = option_station,
                  multiple = TRUE,
                  selected = option_station[[1]]),
      selectInput(inputId = "parameter",
                  label = "Parameter",
                  choices = option_parameter,
                  selected = "o3"),
      textInput(inputId = "parameter_label",
                label = "Label for parameter",
                value = "")
    ),
    mainPanel(
      plotOutput('aq_plot')
    )
  )
)
