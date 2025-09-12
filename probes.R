library(tidyverse)
library(readxl)


datos <- read_excel("GenDB.xlsx") %>%
        mutate(
                Id = as.integer(Id),
                fechanac = format(as.Date(fechanac), "%d/%m/%Y"),
                fechapet = format(as.Date(fechapet), "%d/%m/%Y"),
        )

datos_long <- datos %>%
        pivot_longer(
                cols = matches("^(gen|g_c|g_nm|g_p|g_imp|g_vaf|g_depth|g_tier)[0-9]+$"),
                names_to = c(".value", "orden"),
                names_pattern = "(.+?)([0-9]+)"
        ) %>%
        mutate(
                orden = as.integer(orden),
                g_depth = as.integer(g_depth),
                g_vaf   = as.numeric(g_vaf)
        )
