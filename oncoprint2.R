library(tidyverse)
db_op <- as.data.frame(db_op)

all_ids <- unique(db_op$Id)

# Eliminamos sólo filas con gen vacío, pero mantenemos los Ids
db_op_clean <- db_op %>% filter(!is.na(gen) & gen != "")