source("basic.R")

#SUPERVIVENCIA
dbsv <- db_wide %>% select(Id, fechapet, fechaus, status, edad, sexo) %>% 
        mutate(
                fechapet = as.Date(fechapet),
                fechaus = as.Date(fechaus),
                status = factor(status),
                statusn = ifelse(status == "exitus", 1, 0))
dbsv$time <- dbsv$fechaus - dbsv$fechapet
s <- survfit(Surv(time, statusn) ~ 1, data = dbsv) #simple
s2 <- survfit2(Surv(time, status) ~ 1, data = dbsv) #riesgos competitivos

#1y-OS
super <- summary(s, times = 365.25)
sup365 <- super$surv*100
paste0("Supervivencia al año: ", round(sup365, 2), "%")

#Median Survival Time
quantile(prodlim(Surv(time, statusn) ~ 1, dbsv, reverse = FALSE))

#Median Follow Up Time
quantile(prodlim(Surv(time, statusn) ~ 1, dbsv, reverse = TRUE))


ggsurvfit(s) + 
        labs(x = "Time", y = "OS probability") + 
        add_confidence_interval() +
        add_risktable() +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
        #+ geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
        #geom_vline(xintercept = 365, linetype = "dashed", color = "blue") +
        #annotate("text", 
        #         x = 365, 
        #         y = super$surv, 
        #         label = paste0("OS = ", round(sup365, 1), "%"), 
        #         vjust = -0.5, hjust = - 0.1 , size = 3.5, color = "blue")

ggcuminc(s2) +
        labs(x = "Time", y = "Cumulative incidence") + 
        add_risktable()


#SV con GENES
# Si quieres conservar todos los registros de db1 y traer las variables de db2
dbsvmut <- left_join(dbsv, mutaciones, by = "Id")

for (g in genes_unicos) {
        dbsvmut[[g]] <- factor(dbsvmut[[g]], 
                               levels = c(0,1), 
                               labels = c(paste0(g, "wt"), paste0(g, "mut")))
}


smut <- survfit(Surv(time, statusn) ~ TP53, data = dbsvmut)

logrank <- survdiff(Surv(time, statusn) ~ TP53, data = dbsvmut)
pval <- 1 - pchisq(logrank$chisq, df = length(logrank$n) - 1)
pval_text <- paste0("p = ", sprintf("%.3f", signif(pval, 3)))

names(smut$strata) <- levels(dbsvmut$TP53)
ggsurvfit(smut) +
        labs(x = "Time", y = "OS probability") +
        add_confidence_interval() +
        add_risktable() +
        add_censor_mark() +
        annotate("text", x = 0, y = 0.05, label = pval_text, hjust = 0, vjust = 0, size = 4, color = "black")
