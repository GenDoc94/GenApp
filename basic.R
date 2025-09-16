library(tidyverse)
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(ggplot2)

db_wide <- read_excel("GenDB.xlsx") %>%
        mutate(
                Id = as.integer(Id),
                fechanac = format(as.Date(fechanac), "%d/%m/%Y"),
                fechapet = format(as.Date(fechapet), "%d/%m/%Y")
        )

db_long <- db_wide %>%
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


# Columnas de genes en tu base wide
cols_gen <- paste0("gen", 1:12)

# Obtener genes únicos
genes_unicos <- db_wide %>%
        select(all_of(cols_gen)) %>%
        unlist() %>%
        unique() %>%
        na.omit() %>%
        .[. != ""]

# Crear columnas indicadoras usando apply
indicadores <- t(apply(db_wide[cols_gen], 1, function(x) as.integer(genes_unicos %in% x)))

# Convertir a data frame y poner nombres
indicadores_df <- as.data.frame(indicadores)
names(indicadores_df) <- genes_unicos

# Unir con la base original
mutaciones <- bind_cols(Id = db_wide$Id, indicadores_df)

#eliminar innecesarios
rm(cols_gen, genes_unicos, indicadores, indicadores_df)




#Calcula los % de cada mutación
mut15 <- mutaciones %>%
        summarise(across(where(is.numeric) & !Id, ~ round(mean(.x, na.rm = TRUE) * 100, 1))) %>%
        pivot_longer(
                cols = everything(),
                names_to = "gen",
                values_to = "porcentaje"
        ) %>%
        slice_max(order_by = porcentaje, n = 15) %>%
        arrange(porcentaje) %>%
        mutate(gen = factor(gen, levels = gen)) #mantiene orden en ggplot2

n_total <-nrow(mutaciones)

#Gráfica
ggplot(mut15, aes(x=gen, y=porcentaje)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = paste0("Top 15 genes +fr mutados (n = ", n_total, ")"),
             x = "Gen",
             y = "Frecuencia (%)") +
        theme(axis.text.y = element_text(hjust = 1, size = 6)) +
        geom_text(aes(label = paste0(round(porcentaje,1), "%")), 
                  hjust = -0.1, size = 3) +  # coloca texto a la derecha de la barra
        coord_flip()

