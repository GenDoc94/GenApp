source("basic.R")

#EDAD
#PARA 1 GEN
#testJAK2 <- t.test(edad ~ JAK2, data = dbsvmut)
#edad_media_JAK2 <- as.list(tapply(dbsvmut$edad, dbsvmut$JAK2, function(x) round(mean(x, na.rm = TRUE), 1)))
#p_valor_testJAK2 <- round(as.numeric(testJAK2[["p.value"]]), 3)
#paste0(edad_media_JAK2[1], " vs. ", edad_media_JAK2[2]," (",p_valor_testJAK2,")")

resultados_edad <- list()
for (i in genes_unicos) {
        #counting observation per group
        n_por_grupo <- table(dbsvmut[[i]])
        edad_media_gen <- as.list(tapply(dbsvmut$edad, dbsvmut[[i]], function(x) round(mean(x, na.rm = TRUE), 1)))
        #only if at least 2 per group
        if (all(n_por_grupo > 1)) {
                testgen <- t.test(edad ~ dbsvmut[[i]], data = dbsvmut)
                p_valor_testgen <- round(as.numeric(testgen[["p.value"]]), 3)
                p_valor_testgen <- ifelse(
                        p_valor_testgen < 0.001,
                        "<0.001",
                        sprintf("%.3f", p_valor_testgen)
                )
        } else {
                p_valor_testgen <- "NA"
        }
        if (testgen[["p.value"]] < 0.05){
        resultados_edad[i] <- paste0(edad_media_gen[[1]], " vs. ", edad_media_gen[[2]], " (", p_valor_testgen, "*)")
        } else {
        resultados_edad[i] <- paste0(edad_media_gen[[1]], " vs. ", edad_media_gen[[2]], " (", p_valor_testgen, ")")
        }
}

#SEXO
#PARA 1 GEN
#tabJAK2 <- table(dbsvmut$sexo, dbsvmut$JAK2)
#prop_mujeres <- round((prop.table(tabJAK2, margin = 2)["Mujer", ])*100, 2)
#if (any(tabJAK2 < 5)) {
#        test <- fisher.test(tabJAK2)
#} else {
#        test <- chisq.test(tabJAK2)
#}
#p_valor <- round(test[["p.value"]],3)
#paste0("JAK2",": ",prop_mujeres[[1]], "% vs. ", prop_mujeres[[2]], "% (", p_valor, ")")

resultados_sexo <- list()
for (i in genes_unicos) {
        tabgen <- table(dbsvmut$sexo, dbsvmut[[i]])
        prop_mujeres <- round((prop.table(tabgen, margin = 2)["Mujer", ])*100, 1)
        prop_mujeres <- sprintf("%.1f", prop_mujeres)
        if (any(tabgen < 5)) {
                test <- fisher.test(tabgen)
                p_valor <- round(as.numeric(test[["p.value"]]), 3)
                p_valor<- ifelse(
                        p_valor < 0.001,
                        "<0.001F",
                        paste0(sprintf("%.3f", p_valor),"F")
                )
        } else {
                test <- chisq.test(tabgen)
                p_valor <- round(as.numeric(test[["p.value"]]), 3)
                p_valor <- ifelse(
                        p_valor < 0.001,
                        "<0.001C",
                        paste0(sprintf("%.3f", p_valor),"C")
                )
        }
        if (test[["p.value"]] < 0.05){
                resultados_sexo[i] <- paste0(prop_mujeres[[1]], "% vs. ", prop_mujeres[[2]], "% (", p_valor, "*)")
        } else {
                resultados_sexo[i] <- paste0(prop_mujeres[[1]], "% vs. ", prop_mujeres[[2]], "% (", p_valor, ")")
        }
        
}


#SUPERVIVENCIA
#PARA 1 GEN
#smut <- survfit(Surv(time, statusn) ~ JAK2, data = dbsvmut) #-> ESTA SOBRA
#logrank <- survdiff(Surv(time, statusn) ~ JAK2, data = dbsvmut)
#pval <- 1 - pchisq(logrank$chisq, df = length(logrank$n) - 1)
#pval_text <- paste0("p = ", signif(pval, 3))
#print(paste0("JAK2: ", pval_text))

resultados_sv <- list()
for (i in genes_unicos) {
        logrank <- survdiff(Surv(time, statusn) ~ dbsvmut[[i]], data = dbsvmut)
        pval <- 1 - pchisq(logrank$chisq, df = length(logrank$n) - 1)
        pval <- signif(pval, 3)
        if (pval < 0.05){
                resultados_sv[i] <- paste0(i,": ", sprintf("%.3f", pval),"*")
        } else {
                resultados_sv[i] <- paste0(i,": ", sprintf("%.3f", pval))
        }
}




















#MONTAR DATOS
tabla_resumen <- data.frame(
        genes = genes_unicos,
        edad = unlist(resultados_edad[genes_unicos]),
        sexo = unlist(resultados_sexo[genes_unicos]),
        sv = unlist(resultados_sv[genes_unicos]),
        stringsAsFactors = FALSE
)

# Renombrar columnas si quieres
colnames(tabla_resumen) <- c("Genes", 
                             "Edad media: wt vs. mut (p-valor)", 
                             "Mujeres: %wt vs. %mut (p-valor)", 
                             "SV (p-valor)")
rownames(tabla_resumen) <- NULL