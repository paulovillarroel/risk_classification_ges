library(shiny)
library(readxl)
library(tidyverse)
library(janitor)
library(lubridate)



ui <- fluidPage(
  
  fluidPage(
    titlePanel("Categorizador de garantías GES"),
    sidebarLayout(
      sidebarPanel(
        fileInput('file1', "Sube las vigentes",
                  accept= ".xlsx"),
        fileInput('file2', "Sube las vencidas",
                  accept= ".xlsx"),
        
        
        helpText("Esta aplicación te permite subir tus garantías GES y clasificarlas basadas en el modelo de riesgo de vencimiento."),
        br(),
        helpText("Sube los archivos descargados de SIGGES en formato Excel (.xlsx)."),
        br(),
       
        p("Elaborado por Paulo Villarroel"),
        a("Revisa el repositorio de este proyecto en GitHub", href = "https://github.com/paulovillarroel/visor_casos"),
        
        
        tags$hr(),
        
        downloadButton('downloadData', 'Download')
        
      ),
      
      mainPanel(
        
        tableOutput('contents')
        
      )
    )
  )
)


server <- function(input, output) {
  
  
  getData <- reactive({
    
    
    ges_vigentes <- 
      
      read_excel(input$file1$datapath, skip = 7) |>  
      select(1, 4:8, 10) |> 
      clean_names()
    
    ges_vencidas <-
      
      read_excel(input$file2$datapath, skip = 7) |>  
      select(6, 1, 4:5, 8:9, 11) |> 
      clean_names()
    
    
    inFile <-
      
      rbind(ges_vigentes, ges_vencidas) |> 
      na.omit() |>  
      mutate(problema_de_salud = str_remove(problema_de_salud, pattern = "(\\s?[.]?\\s?[{]\\w+\\s\\w+.\\s?\\d+.?\\d+.)"),
             fecha_de_inicio = ymd(fecha_de_inicio),
             fecha_limite = ymd(fecha_limite),
             plazo_ges = as.numeric(fecha_limite - fecha_de_inicio),
             dias_avanzados = as.numeric(today() - fecha_de_inicio),
             dias_retraso = ifelse(fecha_limite >= today(), NA_integer_, as.numeric(today() - fecha_limite)),
             plazo_interno = plazo_ges * 0.7,
             fecha_interna = fecha_de_inicio + plazo_interno,
             indice_resolucion = dias_avanzados / plazo_ges,
             categoria = case_when(
               indice_resolucion > 1 ~ "Vencido",
               indice_resolucion >= 0.7 ~ "Vigente riesgo alto",
               indice_resolucion < 0.7 & indice_resolucion >= 0.35 ~ "Vigente riesgo medio",
               indice_resolucion < 0.35 ~ "Vigente riesgo bajo"
               
             )
             
      )
    
  })
  
  
  output$contents <- renderTable(
    
    if (is.null(input$file1) | is.null(input$file2)) {
      
      return(NULL)
      
    } 
    
    else {
      
      getData() |> 
        select(1:4, 13, 14) |> 
        head()
      
    }
    
  )
  
  output$downloadData <- downloadHandler(
    

    filename = function() {
       paste("categorias_ges_", Sys.Date(), ".csv", sep = "")
     },
    
     content = function(file) {
    
       write.csv(getData(), file, row.names = FALSE, fileEncoding = "latin1")

    })
  
}

shinyApp(ui, server)