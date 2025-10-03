library(tidyverse)
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(ComplexHeatmap)
library(circlize)
library(grid)

#SV
library(knitr)
library(survival)
library(tibble)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(condsurv)
library(prodlim)
library(survminer)

db_wide <- read_excel("GenDB.xlsx") %>%
        mutate(
                Id = as.integer(Id),
                fechanac = as.Date(fechanac),
                fechapet = as.Date(fechapet),
                #edad = as.numeric(difftime(fechapet, fechanac, units = "days")) / 365.25,
                Dx = factor(Dx)
        )

db_wide$edad <- (db_wide$fechapet - db_wide$fechanac)/365.25

#db_wide <- db_wide %>% filter(db_wide$Dx == "LMA")

db_long <- db_wide %>%
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

#db_op <- db_long %>% select(Id, gen, g_imp)


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
rm(cols_gen, indicadores, indicadores_df)

#contaje mutaciones
db_long <- db_long %>%
        mutate(contaje = ifelse(orden == 1 & is.na(gen), 0, orden))


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

#mutaciones frecuencia
dbgen <- as.data.frame(table(db_long$gen, db_long$g_imp))
colnames(dbgen) <- c("Gen", "MutationType", "Frequency")

gene_freq <- dbgen %>%
        group_by(Gen) %>%
        summarise(TotalFrequency = sum(Frequency)) %>%
        arrange(desc(TotalFrequency))  # Ordenar de mayor a menor

# Reordenar los factores de 'Gen' según la frecuencia total
dbgen$Gen <- factor(dbgen$Gen, levels = gene_freq$Gen)