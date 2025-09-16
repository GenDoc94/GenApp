library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(writexl)
library(ggplot2)


ui <- fluidPage(
        # Definir la clase CSS para tablas pequeñas
        tags$style(HTML("
                .tabla-pequena table { font-size: 8px; }
        ")),
        
        
        
        titlePanel("Prueba"),
        sidebarLayout(
                sidebarPanel(
                        fileInput("archivo", "Sube el Excel", accept = c(".xls", ".xlsx"))
                ),
                mainPanel(
                        tabsetPanel(
                                tabPanel("Wide y Long",
                                         fluidRow(
                                                 column(6,
                                                        uiOutput("tabla_W_ui"),
                                                        uiOutput("button_wide")),
                                                 column(6,
                                                        uiOutput("tabla_L_ui"),
                                                        uiOutput("button_long"))
                                         )),
                                tabPanel("Descriptivo",
                                         uiOutput("graph_mut")),
                                tabPanel("Pestaña 3",
                                         h3("Contenido pestaña 3"))
                        )
                )
        )
        
        
)

server <- function(input, output, session) {
        
        #DATOS WIDE
        datos_wide <- reactive({
                req(input$archivo)
                df <- read_excel(input$archivo$datapath) %>%
                        #Poner la base bonita
                        mutate(
                                Id = as.integer(Id),
                                fechanac = format(as.Date(fechanac), "%d/%m/%Y"),
                                fechapet = format(as.Date(fechapet), "%d/%m/%Y")
                        )
                
                #lo que devuelves
                df
        })
        
        #DATOS LONG
        datos_long <- reactive({
                req(input$archivo)
                df <- datos_wide() %>%
                        pivot_longer(
                                cols = matches("^(gen|g_c|g_nm|g_p|g_imp|g_vaf|g_depth|g_tier)[0-9]+$"),
                                names_to = c(".value", "orden"),
                                names_pattern = "(.+?)([0-9]+)"
                        ) %>%
                        mutate(
                                orden = as.integer(orden),
                                g_depth = as.integer(g_depth),
                                g_vaf = as.numeric(g_vaf)
                        ) %>%
                        filter(!(orden != 1 & (is.na(gen) | gen == "")))
                #lo que devuelves
                df
        })


        #MUTATIONS
        mutations <- reactive({
                req(datos_wide())
                db_wide <- datos_wide()
                cols_gen <- paste0("gen", 1:12)
                genes_unicos <- db_wide %>%
                        select(all_of(cols_gen)) %>%
                        unlist() %>%
                        unique() %>%
                        na.omit() %>%
                        .[. != ""]
                indicadores <- t(apply(db_wide[cols_gen], 1, function(x) as.integer(genes_unicos %in% x)))
                indicadores_df <- as.data.frame(indicadores)
                names(indicadores_df) <- genes_unicos
                mutaciones_df <- bind_cols(Id = db_wide$Id, indicadores_df)
                mut <- mutaciones_df %>%
                        summarise(across(where(is.numeric) & !Id, ~ round(mean(.x, na.rm = TRUE) * 100, 1))) %>%
                        pivot_longer(
                                cols = everything(),
                                names_to = "gen",
                                values_to = "porcentaje"
                        ) %>%
                        arrange(porcentaje) %>%
                        mutate(gen = factor(gen, levels = gen))
                return(mut)
        })
        
        #MUTATIONS 15
        mut15 <- reactive({
                mut <- mutations() %>%
                        slice_max(order_by = porcentaje, n=15)
                return(mut)
        })
        

        
        
        
        #TABLA WIDE (UI)
        output$tabla_W <- renderTable({
                head(datos_wide()[, 1:5], 10)
        })
        output$tabla_W_ui <- renderUI({
                req(datos_wide())
                tagList(
                        h3("Tabla Wide", style = "text-align: center;"),
                        div(style = "width:1px; background-color:#ccc;", class = "tabla-pequena", tableOutput("tabla_W"))       
                )
        })
        
        #TABLA LONG (UI)
        output$tabla_L <- renderTable({
                head(datos_long()[, 1:5], 10)
        })
        output$tabla_L_ui <- renderUI({
                req(datos_long())
                tagList(
                        h3("Tabla Long", style = "text-align: center;"),
                        div(style = "width:1px; background-color:#ccc;", class = "tabla-pequena", tableOutput("tabla_L"))       
                )
        })
        
        #DESCARGAR WIDE
        output$button_wide <- renderUI({
                req(datos_wide())
                downloadButton("download_wide", "Descargar Wide (CSV)", class = "btn-sm")
        })
        output$download_wide <- downloadHandler(
                filename = function() { paste0("datos_wide.csv") },
                content = function(file) {
                        write.csv(datos_wide(), file, row.names = TRUE)
                }
        )
        
        #DESCARGAR LONG
        output$button_long <- renderUI({
                req(datos_long())
                downloadButton("download_long", "Descargar Long (CSV)", class = "btn-sm")
        })
        output$download_long <- downloadHandler(
                filename = function() { paste0("datos_long.csv") },
                content = function(file) {
                        write.csv(datos_long(), file, row.names = TRUE)
                }
        )
        
        
        #GRÁFICA MUTACIONES
        output$graph_mut <- renderUI({
                req(mut15())
                tagList(
                        h3("Frecuencia de mutaciones", style = "text-align: center;"),
                        plotOutput("plot_mut15", height = "400px")
                )
        })
        output$plot_mut15 <- renderPlot({
                req(mut15())
                ggplot(mut15(), aes(x=gen, y=porcentaje)) +
                        geom_bar(stat = "identity") +
                        theme_minimal() +
                        labs(title = paste0("Top 15 genes +fr mutados (n = ", nrow(datos_wide()), ")"),
                             x = "Gen",
                             y = "Frecuencia (%)") +
                        theme(axis.text.y = element_text(hjust = 1, size = 8)) +
                        geom_text(aes(label = paste0(round(porcentaje,1), "%")), 
                                  hjust = -0.1, size = 3) +  # coloca texto a la derecha de la barra
                        coord_flip()
                
        })
        
        
        
        
        
}

shinyApp(ui, server)
