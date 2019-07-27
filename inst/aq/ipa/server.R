server <- function(input, output, session) {
  un_smp_new <- eventReactive(input$predict, {
    data.frame(
      tahun = input$tahun,
      status = input$status,
      jumlah_peserta = input$jumlah_peserta,
      bahasa_indonesia = input$bahasa_indonesia,
      bahasa_inggris = input$bahasa_inggris,
      matematika = input$matematika,
      stringsAsFactors = FALSE
    )
  })
  
  to_predict <- reactive({
    req(un_smp_new())
    un_smp_recipe %>% 
      prep() %>% 
      bake(un_smp_new())
  })
  
  ipa_prediction <- reactive({
    req(to_predict())
    predict(un_smp_rf, new_data = to_predict())
  })
  
  output$ipa_prediction <- renderUI({
    wellPanel(
      h4("Prediksi nilai ujian IPA kamu:"),
      h1(strong(round(ipa_prediction(), digits = 2))),
      h2(ifelse(ipa_prediction() >= 75, 
                "Selamat ya, calon jadi Data Scientist nih! ;)",
                "Tetap semangat ya! Ikut belajar di R Academy Batch selanjutnya yuk ;)")),
      hr(),
      em("Maaf ya saya bukan peramal, hanya seorang data scientist pengguna metode pemodelan Random Forest", icon("smile"))
    )
  })
}