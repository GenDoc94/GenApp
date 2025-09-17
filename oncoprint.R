library(tidyverse)
library(ComplexHeatmap)
library(circlize)

# Seleccionar columnas y asegurarse de que sean character
db_op <- db_long %>% 
        select(Id, gen, g_imp) %>%
        mutate(
                Id = as.character(Id),
                gen = as.character(gen),
                g_imp = as.character(g_imp)
        )

# Reemplazar gen vacío por NA y g_imp vacío por ""
db_op <- db_op %>%
        mutate(
                gen = ifelse(gen == "", NA, gen),
                g_imp = ifelse(g_imp == "", "", g_imp)
        )

# Todos los pacientes y genes
all_ids <- sort(unique(db_op$Id))
all_genes <- sort(unique(db_op$gen[!is.na(db_op$gen)]))

# Crear matriz vacía de caracteres
mat <- matrix("", nrow = length(all_genes), ncol = length(all_ids),
              dimnames = list(all_genes, all_ids))

# Rellenar la matriz con las mutaciones
for(i in seq_len(nrow(db_op))){
        pid <- db_op$Id[i]
        gene <- db_op$gen[i]
        mut <- db_op$g_imp[i]
        if(!is.na(gene) & mut != ""){
                if(mat[gene, pid] == ""){
                        mat[gene, pid] <- mut
                } else {
                        mat[gene, pid] <- paste(mat[gene, pid], mut, sep=";")
                }
        }
}

# Definir funciones de color
alter_fun = list(
        background = function(x, y, w, h) grid::grid.rect(x, y, w, h,
                                                          gp = grid::gpar(fill = "white", col = "lightgrey")),
        Missense  = function(x, y, w, h) grid::grid.rect(x, y, w*0.9, h*0.9,
                                                         gp = grid::gpar(fill = "#1f77b4", col = NA)),
        Nonsense  = function(x, y, w, h) grid::grid.rect(x, y, w*0.9, h*0.9,
                                                         gp = grid::gpar(fill = "#ff7f0e", col = NA)),
        Frameshift = function(x, y, w, h) grid::grid.rect(x, y, w*0.9, h*0.9,
                                                          gp = grid::gpar(fill = "#2ca02c", col = NA)),
        Splicing  = function(x, y, w, h) grid::grid.rect(x, y, w*0.9, h*0.9,
                                                         gp = grid::gpar(fill = "#d62728", col = NA)),
        Inframe   = function(x, y, w, h) grid::grid.rect(x, y, w*0.9, h*0.9,
                                                         gp = grid::gpar(fill = "#9467bd", col = NA))
)

col = c(
        Missense = "#1f77b4",
        Nonsense = "#ff7f0e",
        Frameshift = "#2ca02c",
        Splicing = "#d62728",
        Inframe = "#9467bd"
)

# Dibujar OncoPrint
oncoPrint(
        mat,
        alter_fun = alter_fun,
        col = col,
        remove_empty_columns = FALSE,
        remove_empty_rows = FALSE,
        column_title = "Pacientes",
        row_title = "Genes",
        heatmap_legend_param = list(title = "Tipo de mutación")
)
