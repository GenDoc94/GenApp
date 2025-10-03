source("basic.R")

#Read and create the matrix
mat <- as.matrix(mutaciones[,-1])
rownames(mat) <- mutaciones[[1]]
genes <- colnames(mat)

#Fisher
results <- data.frame()
for (i in 1:(length(genes) - 1)) {
        for (j in (i + 1):length(genes)) {
                g1 <- genes[i]
                g2 <- genes[j]
                x <- mat[, g1]
                y <- mat[, g2]
                tbl <- table(x, y)
                if (all(dim(tbl) == 2)) {
                        fisher <- fisher.test(tbl)
                        sign <- ifelse(fisher$estimate > 1, 1, -1)
                        score <- -log10(fisher$p.value) * sign
                        results <- rbind(results, data.frame(g1, g2, score, pval = fisher$p.value))
                }
        }
}

#Triangle in the matrix
M <- matrix(NA, nrow = length(genes), ncol = length(genes),
            dimnames = list(genes, genes))
stars <- matrix("", nrow = length(genes), ncol = length(genes),
                dimnames = list(genes, genes))

#p-value */**
for (k in 1:nrow(results)) {
        g1 <- results$g1[k]
        g2 <- results$g2[k]
        M[g2, g1] <- results$score[k]
        if (results$pval[k] < 0.01) {
                stars[g2, g1] <- "**"
        } else if (results$pval[k] < 0.05) {
                stars[g2, g1] <- "*"
        }
}

#Colors
col_fun <- colorRamp2(c(min(M, na.rm = TRUE), 0, max(M, na.rm = TRUE)),
                      c("blue", "white", "red"))

#Skip first/last gene
rownames(M)[1] <- ""
colnames(M)[ncol(M)] <- ""

#HEATMAP
Heatmap(M,
        name = "-log10(p-value)",
        col = col_fun,
        cluster_rows = FALSE, cluster_columns = FALSE, # sin dendrogramas
        rect_gp = gpar(col = NA),
        na_col = "white",
        row_names_side = "left",
        column_names_side = "bottom",
        column_names_rot = 45,
        row_names_gp = gpar(fontsize = 8),
        column_names_gp = gpar(fontsize = 8),
        cell_fun = function(j, i, x, y, width, height, fill) {
                if (!is.na(M[i, j])) {
                        grid.rect(x, y, width, height,
                                  gp = gpar(col = "black", fill = NA, lwd = 0.5)) # borde negro
                        if (stars[i, j] != "") {
                                grid.text(stars[i, j], x, y,
                                          gp = gpar(col = "black", fontsize = 8)) # asterisco
                        }
                }
        })

#Add more legend to the heatmap
grid.text("* p < 0.05", x = unit(0.5, "npc"), y = unit(0.95, "npc"), gp = gpar(fontsize = 10, col = "black"))
grid.text("** p < 0.01", x = unit(0.5, "npc"), y = unit(0.90, "npc"), gp = gpar(fontsize = 10, col = "black"))
