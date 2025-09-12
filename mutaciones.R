library(dplyr)

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