ui <- fluidPage(
  headerPanel("Hmm... berapa ya?"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "tahun",
                  label = "Tahun ujian",
                  choices = as.character(2015:2019)),
      selectInput(inputId = "status",
                  label = "Status sekolah",
                  choices = c("Negeri", "Swasta")),
      numericInput(inputId = "jumlah_peserta",
                   label = "Jumlah peserta ujian",
                   min = 20,
                   max = 500,
                   value = 354),
      numericInput(inputId = "bahasa_indonesia",
                   label = "Nilai ujian Bahasa Indonesia",
                   min = 0,
                   max = 100,
                   value = 78),
      numericInput(inputId = "bahasa_inggris",
                   label = "Nilai ujian Bahasa Inggris",
                   min = 0,
                   max = 100,
                   value = 78),
      numericInput(inputId = "matematika",
                   label = "Nilai ujian matematika",
                   min = 0,
                   max = 100,
                   value = 78),
      actionButton(inputId = "predict",
                   label = "Nilai ujian IPA-ku berapa ya?")
    ),
    mainPanel(
      uiOutput("ipa_prediction")
    )
  )
)