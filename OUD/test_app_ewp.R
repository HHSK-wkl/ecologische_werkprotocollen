library(shiny)
library(quarto)

ui <- fluidPage(
  
  titlePanel("Basis opzet uitvoeringsprotocol"),
  
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "project_naam", label = "Projectnaam:"),
      br(),
      downloadButton(outputId = "report", label = "Maak uitvoeringsprotocol:")
    ),
    mainPanel(
      
    )
  )
)


server <- function(input, output) {
  output$report <- downloadHandler(
    filename = paste0("ecologisch_werkprotocol_", Sys.time(), ".docx"),
    content = function(file) {
      
      quarto::quarto_render("ecologische_werkprotocollen.qmd", 
                            execute_params = list(project_naam = input$project_naam))
      
      file.copy("output/ecologische_werkprotocollen.docx", file)
      
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)