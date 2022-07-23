library(shiny)
library(shinythemes)
library(tidyverse)

# Define UI for application
ui <- fluidPage(
  theme = shinytheme(theme = "superhero"),

  # Application title
  titlePanel(strong("Bayar Cuk!")),
  h4("Sebuah Kalkulator Patungan Promo Go/Grab/Shopee Food"),

  # Sidebar
  sidebarLayout(
    sidebarPanel(
      selectizeInput("list_price", "Harga Pesanan per Pemesan", choices = NULL, multiple = TRUE, options = list(create = TRUE)),
      numericInput("disc_perc", "Diskon (%)", min = 0, max = 100, value = 60),
      numericInput("max_disc", "Maksimal Potongan", min = 0, max = 99999999, value = 20000, step = 500),
      numericInput("deliv_price", "Biaya Antar (Setelah Diskon)", min = 0, max = 9999999, value = 4000, step = 500),
      numericInput("service", "Biaya Layanan", min = 0, max = 99999999, value = 3000, step = 500),
      actionButton("go", "Hitung!")
    ),

  # Show tables of the generated calculation
    mainPanel(
      tableOutput("tabel_rincian"),
      textOutput("designedBy")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  price <- eventReactive(input$go, {
    vec = req(input$list_price)
    as.numeric(vec)
  })
  
  komponen <- eventReactive(input$go, {
    total_disc = sum(price()) * (input$disc_perc/100)
    fix_disc = if_else(total_disc > as.double(input$max_disc), as.double(input$max_disc), as.double(total_disc))
    disc_prop = proportions(price()) * fix_disc
    price_after_disc = price() - disc_prop
    deliv_fee = input$deliv_price/length(price())
    service_fee = input$service/length(price())
    pay_split = price_after_disc + deliv_fee + service_fee
    tibble(
      `Harga Pesanan` = as.integer(price()),
      `Proporsi (%)` = proportions(price()) * 100,
      Potongan = as.integer(disc_prop),
      `Harga Setelah Diskon` = as.integer(price_after_disc),
      Ongkir = as.integer(deliv_fee),
      `Biaya Layanan` = as.integer(service_fee),
      Bayar = as.integer(pay_split)
    )
  })
  
  output$tabel_rincian <- renderTable({
    agregat = komponen() |> 
      summarize_all(sum) |> 
      pivot_longer(1:7, names_to = "Keterangan", values_to = "Nilai") |> 
      select("Total" = Nilai)
    
    komponen() |> 
      mutate(ID = paste("Pemesan", row_number())) |> 
      pivot_longer(-ID, names_to = "Keterangan", values_to = "Nilai") |> 
      pivot_wider(names_from = ID, values_from = "Nilai") |> 
      bind_cols(agregat) |> 
      mutate_if(is.double, as.integer)
  })
  
  output$designedBy <- eventReactive(input$go, {
    paste("by R. Hagatama")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
