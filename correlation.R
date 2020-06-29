library(Hmisc)
library(tidyverse)
library(corrplot)

# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software#compute-correlation-matrix

wd <- "/Users/jonathanleape/Dropbox (MIT)/PMCMV"

covars <- file.path(wd, "data", "output", "panels", "panel2.Rds") %>%
  readRDS() %>% 
  dplyr::select(old_emp, old_r_inc, old_hrs_wk,
                favela,
                old_analfabeto, old_fundamental_completo,
                old_medio_completo, old_superior_completo,
                feminino, idade, cor_negra, cor_branca, cor_outras,
                murder_rate, dist_center, 
                acc, displacement,
                emp, r_inc, hrs_wk)

colnames(covars) <- c("Orig. Employment Status", "Orig. Income (MW)", "Orig. Weekly Hours", 
                      "Orig. Favela", "Orig. Illiterate", "Orig. Primary School", 
                      "Orig. Secondary School", "Orig. Higher Education",
                      "Female", "Age", "Race: Black", "Race: White", "Race: Other", 
                      "Murder Rate", "Proximity to Downtown", 
                      "Job Accessibility", "Moving Distance",
                      "Employment Status", "Income (MW)", "Weekly Hours")

corr_m <- rcorr(as.matrix(covars), type = "pearson")

saveRDS(corr_m, file.path(wd, "data", "output", "correlation", "corr_m.Rds"))

# convert to df
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

corr_df <- flattenCorrMatrix(corr_m$r, corr_m$P)

saveRDS(corr_df, file.path(wd, "data", "output", "correlation", "corr_df.Rds"))

# plot and save png 
png(file.path(wd, "data", "output", "correlation", "corr_matrix.png"),
    width = 800, height = 800)
corrplot(corr_m$r, type="upper", order="original", 
         p.mat = corr_m$P, sig.level = 0.01, insig = "blank", 
         diag = FALSE, tl.col = "black", tl.cex = 1.5, cl.cex = 1.5)
dev.off()

