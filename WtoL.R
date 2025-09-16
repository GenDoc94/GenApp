library(tidyverse)
library(readxl)
library(writexl)

#Pasar de wide a long

wide_to_long <- function(file_path) {
        
        # Leer Excel
        db_wide <- read_excel(file_path) %>%
                mutate(
                        Id = as.integer(Id),
                        fechanac = format(as.Date(fechanac), "%d/%m/%Y"),
                        fechapet = format(as.Date(fechapet), "%d/%m/%Y")
                )
        
        # Transformar a long
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
                )
        
        # Limpiar base
        db_long_clean <- db_long %>%
                filter(!(orden != 1 & (is.na(gen) | gen == "")))
        
        # Opcional: guardar en disco
        #write_xlsx(db_long, "GenDB_long.xlsx")
        #write_xlsx(db_long_clean, "GenDB_long_clean.xlsx")
        
        return(db_long_clean)
}
