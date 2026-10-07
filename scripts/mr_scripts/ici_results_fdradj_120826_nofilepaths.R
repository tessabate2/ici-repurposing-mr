### ici manuscript multiple testing correction - liberal and conservative

library(tidyverse)
library(stats)
ici_res <- read.csv('PD1_PDL1_maininst_surv_res_240925.csv', sep = ",", header = T) %>%
  dplyr::filter(method == "IVW accounting for LD using UKB") %>%
  dplyr::filter(Survival %in% c("ovarian", "prostate"))

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

ici_res_no_fdr <- read.csv('PD1_PDL1_maininst_surv_res_240925.csv', sep = ",", header = T) %>%
  dplyr::filter(method == "IVW accounting for LD using UKB") %>%
  dplyr::filter(!Survival %in% c("ovarian", "prostate")) %>%
  dplyr::select(Protein, Survival, b, se, pval) %>%
  dplyr::mutate(Protein = case_when(Protein == "PD1" ~ "PD-1",
                                    Protein == "PDL1" ~ "PD-L1")) %>%
  dplyr::rename(Beta = b, SE = se, `Unadjusted p-value` = pval)

ici_res_no_fdr$Survival <- str_to_title(ici_res_no_fdr$Survival)
ici_res_no_fdr$Beta <- round(ici_res_no_fdr$Beta, 3)
ici_res_no_fdr$SE <- round(ici_res_no_fdr$SE, 3)
ici_res_no_fdr$`Unadjusted p-value` <- round(ici_res_no_fdr$`Unadjusted p-value`, 3)
ici_res_no_fdr <- dplyr::mutate(ici_res_no_fdr, `FDR p-value` = NA)

ici_res <- dplyr::full_join(ici_res, ici_res_no_fdr) %>%
  dplyr::rename(`FDR p-value (liberal)` = `FDR p-value`)

cons_fdr <- read.table("PD1_PDL1_maininst_surv_res_240925_wFDRp_120826.csv",
                       sep = ",", header = T) %>%
  dplyr::rename(`FDR p-value (conservative)` = `FDR.p.value`, `Unadjusted p-value` = `Unadjusted.p.value`)

ici_res <- dplyr::inner_join(ici_res, cons_fdr)

write.table(ici_res, "PD1_PDL1_maininst_surv_res_240925_wFDRp_140926.csv", sep = ",", quote = F, row.names = F)
