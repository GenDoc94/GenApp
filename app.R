library(shiny)
library(dplyr)
library(readxl)

# Cargar script externo para wide -> long (opcional)
source("WtoL.R")

ui <- fluidPage(
        titlePanel("App de Ejemplo"),
        sidebarLayout(
                sidebarPanel(
                        fileInput("file", "Sube un archivo Excel", accept = c(".xlsx")),
                        br(),
                        p("This is a simple application to perform descriptive statistics on NGS data.")
                ),
                mainPanel(
                        tabsetPanel(
                                tabPanel("Data",
                                         tableOutput("table_preview")
                                ),
                                tabPanel("Descriptive",
                                         p("")  # Vacía
                                ),
                                tabPanel("En blanco",
                                         p("")  # Vacía
                                )
                        )
                )
        )
)

server <- function(input, output, session) {
        
        # Reactive para df_wide
        db_wide <- reactive({
                req(input$file)
                read_excel(input$file$datapath) %>%
                        mutate(
                                Id = as.integer(Id),
                                fechanac = format(as.Date(fechanac), "%d/%m/%Y"),
                                fechapet = format(as.Date(fechapet), "%d/%m/%Y")
                        )
        })
        
        # Output Data (df_wide)
        output$table_preview <- renderTable({
                df <- db_wide()  # Llamar al reactivo
                if(is.null(df)) return(NULL)
                
                df %>% select(1:5) %>% head(5)
        })
}

shinyApp(ui, server)
