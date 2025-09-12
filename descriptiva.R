library(dplyr)
library(tidyr)

#Calcula los % de cada mutación
mutaciones %>%
        summarise(across(where(is.numeric) & !Id, ~ round(mean(.x, na.rm = TRUE) * 100, 1))) %>%
        pivot_longer(
                cols = everything(),
                names_to = "gen",
                values_to = "porcentaje"
        ) %>%
        arrange(desc(porcentaje))