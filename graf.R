source("basic.R")

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

#Gráfica mutaciones y tipos
ggplot(dbgen, aes(x = Gen, y = Frequency, fill = MutationType)) +
        geom_bar(stat = "identity") +
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