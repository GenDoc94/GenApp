library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(writexl)
library(ggplot2)
library(DT)


ui <- fluidPage(
        # Definir la clase CSS para tablas pequeñas
        tags$style(HTML("
                .tabla-pequena table { font-size: 8px; }
        ")),
        
        
        
        titlePanel("GenApp"),
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
                                         uiOutput("descriptive"),
                                         uiOutput("graph_mut"),
                                         uiOutput("graph_mutfreq")),
                                tabPanel("Analítico",
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
                                fechanac = as.Date(fechanac),
                                fechapet = as.Date(fechapet),
                                edad = as.numeric(difftime(fechapet, fechanac, units = "days")) / 365.25,
                                Dx = factor(Dx),
                                sexo = factor(sexo)
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
                                g_vaf = as.numeric(g_vaf),
                                g_imp = factor(g_imp)
                        ) %>%
                        filter(!(orden != 1 & (is.na(gen) | gen == "")))
                #para el cálculo de mutaciones por paciente
                df <- df %>%
                        mutate(contaje = ifelse(orden == 1 & is.na(gen), 0, orden))
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

        
        #MUTATIONSTYPE
        dbgen <- reactive({
                req(datos_long())
                db_long <- datos_long()
                dbgen <- as.data.frame(table(db_long$gen, db_long$g_imp))
                colnames(dbgen) <- c("Gen", "MutationType", "Frequency")
                gene_freq <- dbgen %>%
                        group_by(Gen) %>%
                        summarise(TotalFrequency = sum(Frequency)) %>%
                        arrange(desc(TotalFrequency))  # Ordenar de mayor a menor
                dbgen$Gen <- factor(dbgen$Gen, levels = gene_freq$Gen)
                return(dbgen)
        })

        
        #TABLA WIDE (UI)
        output$tabla_W_ui <- renderUI({
                req(datos_wide())
                tagList(
                        h3("Tabla Wide", style = "text-align: center;"),
                        div(style = "width:1px; background-color:#ccc;", class = "tabla-pequena", tableOutput("tabla_W"))       
                )
        })
        output$tabla_W <- renderTable({
                head(datos_wide()[, 1:5] %>%
                             mutate(fechanac = format(fechanac, "%d/%m/%Y"),
                                    fechapet = format(fechapet, "%d/%m/%Y")), 10)
        })
        
        #TABLA LONG (UI)
        output$tabla_L_ui <- renderUI({
                req(datos_long())
                tagList(
                        h3("Tabla Long", style = "text-align: center;"),
                        div(style = "width:1px; background-color:#ccc;", class = "tabla-pequena", tableOutput("tabla_L"))       
                )
        })
        output$tabla_L <- renderTable({
                head(datos_long()[, 1:5] %>%
                             mutate(fechanac = format(fechanac, "%d/%m/%Y"),
                                    fechapet = format(fechapet, "%d/%m/%Y")), 10)
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
        
        #DESCRIPTIVE
        output$descriptive <- renderUI({
                req(datos_wide())
                tagList(
                        h3("Tabla Resumen", style = "text-align: center;"),
                        DT::DTOutput("resumen_dt")
                )
                
        })
        output$resumen_dt <- DT::renderDT({
                req(datos_wide())
                df <- datos_wide()
                df2 <- datos_long()
                dx_texto <- df %>%
                        count(Dx, sort = TRUE) %>%
                        mutate(
                                texto = paste0(Dx, ": ", n, " (", round(100 * n / n_total, 1), "%)")
                        ) %>%
                        pull(texto) %>%
                        paste(collapse = "<br>")  # saltos de línea
                sexo_texto <- df %>%
                        count(sexo, sort = TRUE) %>%
                        mutate(
                                texto = paste0(sexo, ": ", n, " (", round(100 * n / n_total, 1), "%)")
                        ) %>%
                        pull(texto) %>%
                        paste(collapse = "; ")
                
                
                resumen <- tibble(
                        Descriptivo = c("Periodo de estudio",
                                        "Tamaño muestral",
                                        "Sexo",
                                        "Edad, media (SD)",
                                        "Diagnósticos",
                                        "Media de mutaciones por paciente"),
                        Valor = c(paste0("Desde ", format(min(df$fechapet, na.rm = TRUE), "%d/%m/%Y"),", hasta ",format(max(df$fechapet, na.rm = TRUE), "%d/%m/%Y")),
                                nrow(df),
                                sexo_texto,
                                paste0(round(mean(df$edad, na.rm = TRUE), 1)," (",round(sd(df$edad, na.rm = TRUE), 1),")"),
                                dx_texto,
                                paste0(round(mean(df2$contaje, na.rm = TRUE), 1),". (min: ",min(df2$contaje, na.rm = TRUE),", máx: ",max(df2$contaje, na.rm = TRUE),")")
                                  )
                )
                DT::datatable(resumen,
                              escape = FALSE,
                              options = list(dom = 't',
                                             ordering = FALSE),
                              rownames = FALSE)
        })
        

        #GRÁFICA PACIENTES CON MUTACIONES
        output$graph_mut <- renderUI({
                req(mut15())
                tagList(
                        h3("Porcentaje de pacientes con las mutaciones más frecuentes", style = "text-align: center;"),
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
        
        
        
        #GRÁFICA PACIENTES CON MUTACIONES
        output$graph_mutfreq <- renderUI({
                req(dbgen())
                tagList(
                        h3("Tipo de mutaciones de todos los genes mutados", style = "text-align: center;"),
                        plotOutput("plot_mutfreq", height = "400px")
                )
        })
        output$plot_mutfreq <- renderPlot({
                req(dbgen())
                ggplot(dbgen(), aes(x = Gen, y = Frequency, fill = MutationType)) +
                        geom_bar(stat = "identity") +
                        geom_text(aes(label = ifelse(Frequency > 0, as.character(Frequency), "")), 
                                  position = position_stack(vjust = 0.5),  # Posicionar el texto en el medio de cada barra apilada
                                  size = 3, color = "white") +  # Color blanco para el texto
                        theme_minimal() +
                        labs(
                                title = "Frecuencia de Mutaciones por Gen",
                                x = "Gen",
                                y = "Frecuencia",
                                fill = "Tipo de Mutación"
                        ) +
                        theme(
                                axis.text.x = element_text(angle = 90, hjust = 1),
                                axis.title.x = element_text(size = 12),
                                axis.title.y = element_text(size = 12),
                                legend.title = element_text(size = 12),
                                legend.text = element_text(size = 10)
                        )
        })        
        
        
        
        
}

shinyApp(ui, server)
