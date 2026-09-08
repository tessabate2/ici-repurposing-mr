### ici manuscript multiple testing correction

library(tidyverse)
library(stats)
ici_res <- read.csv('PD1_PDL1_maininst_surv_res_240925.csv',
                    sep = ",", header = T) %>%
  dplyr::filter(method == "IVW accounting for LD using UKB")

ici_res$`FDR p-value` <- stats::p.adjust(p = ici_res$pval, method = c("fdr"), n = length(ici_res$pval))

ici_res <- dplyr::select(ici_res, Protein, Survival, b, se, pval, `FDR p-value`) %>%
  dplyr::mutate(Protein = case_when(Protein == "PD1" ~ "PD-1",
                                    Protein == "PDL1" ~ "PD-L1")) %>%
  dplyr::rename(Beta = b, SE = se, `Unadjusted p-value` = pval)

ici_res$Survival <- str_to_title(ici_res$Survival)
ici_res$Beta <- round(ici_res$Beta, 3)
ici_res$SE <- round(ici_res$SE, 3)
ici_res$`Unadjusted p-value` <- round(ici_res$`Unadjusted p-value`, 3)
ici_res$`FDR p-value` <- round(ici_res$`FDR p-value`, 3)

write.table(ici_res, "PD1_PDL1_maininst_surv_res_240925_wFDRp_120826.csv", sep = ",", 
            quote = F, row.names = F)