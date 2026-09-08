## updated 170826 to separate sites w different mortality outcomes

# cancer-specific: breast, colorectal, melanoma, prostate
# all-cause: lung, ovarian

library(meta)
library(tidyverse)

pd1_pdl1_res <- read.csv('PD1_PDL1_maininst_surv_res_240925.csv') %>%
  dplyr::filter(method == "IVW accounting for LD using UKB")


## cancer-specific sites (irrespective of current approvals)
pd1_pdl1_res_CS <- dplyr::filter(pd1_pdl1_res, Survival %in% c("breast", "colorectal",
                                                               "melanoma", "prostate"))

### PD-1:
pd1_res_CS <- metagen(TE = dplyr::filter(pd1_pdl1_res_CS, Protein == "PD1")$b, 
                   seTE = dplyr::filter(pd1_pdl1_res_CS, Protein == "PD1")$se)
summary(pd1_res_CS)

pd1_meta_res_CS <- data.frame(Protein = "PD1",
  Survival = "random effects meta-analysis (cancer-specific mortality*)", 
  est = pd1_res_CS$TE.random,
est_95ci_low = pd1_res_CS$lower.random, 
est_95ci_upp = pd1_res_CS$upper.random,
est_p = pd1_res_CS$pval.random,
q = pd1_res_CS$Q,
q_df = pd1_res_CS$df.Q,
q_pval = pd1_res_CS$pval.Q) %>%
dplyr::mutate(se = (est-est_95ci_low)/1.96)


### PD-L1:
pdl1_res_CS <- metagen(TE = dplyr::filter(pd1_pdl1_res_CS, Protein == "PDL1")$b, 
                   seTE = dplyr::filter(pd1_pdl1_res_CS, Protein == "PDL1")$se)

pdl1_meta_res_CS <- data.frame(Protein = "PDL1",
                            Survival = "random effects meta-analysis (cancer-specific mortality*)",
                            est = pdl1_res_CS$TE.random,
                           est_95ci_low = pdl1_res_CS$lower.random, 
                           est_95ci_upp = pdl1_res_CS$upper.random,
                           est_p = pdl1_res_CS$pval.random,
                           q = pdl1_res_CS$Q,
                           q_df = pdl1_res_CS$df.Q,
                           q_pval = pdl1_res_CS$pval.Q) %>%
  dplyr::mutate(se = (est-est_95ci_low)/1.96)


## all-cause mortality sites
pd1_pdl1_res_AS <- dplyr::filter(pd1_pdl1_res, Survival %in% c("lung", "ovarian"))

### PD-1:
pd1_res_AS <- metagen(TE = dplyr::filter(pd1_pdl1_res_AS, Protein == "PD1")$b, 
                      seTE = dplyr::filter(pd1_pdl1_res_AS, Protein == "PD1")$se)
summary(pd1_res_AS)

pd1_meta_res_AS <- data.frame(Protein = "PD1",
                              Survival = "random effects meta-analysis (all-cause mortality**)", 
                              est = pd1_res_AS$TE.random,
                              est_95ci_low = pd1_res_AS$lower.random, 
                              est_95ci_upp = pd1_res_AS$upper.random,
                              est_p = pd1_res_AS$pval.random,
                              q = pd1_res_AS$Q,
                              q_df = pd1_res_AS$df.Q,
                              q_pval = pd1_res_AS$pval.Q) %>%
  dplyr::mutate(se = (est-est_95ci_low)/1.96)

### PD-L1:
pdl1_res_AS <- metagen(TE = dplyr::filter(pd1_pdl1_res_AS, Protein == "PDL1")$b, 
                       seTE = dplyr::filter(pd1_pdl1_res_AS, Protein == "PDL1")$se)

pdl1_meta_res_AS <- data.frame(Protein = "PDL1",
                               Survival = "random effects meta-analysis (all-cause mortality**)",
                               est = pdl1_res_AS$TE.random,
                               est_95ci_low = pdl1_res_AS$lower.random, 
                               est_95ci_upp = pdl1_res_AS$upper.random,
                               est_p = pdl1_res_AS$pval.random,
                               q = pdl1_res_AS$Q,
                               q_df = pdl1_res_AS$df.Q,
                               q_pval = pdl1_res_AS$pval.Q) %>%
  dplyr::mutate(se = (est-est_95ci_low)/1.96)


## cancer-specific positive control sites
pd1_pdl1_res_CS_pos <- dplyr::filter(pd1_pdl1_res, Survival %in% c("breast", "colorectal",
                                                                   "melanoma"))

### PD-1:
pd1_res_CS_pos <- metagen(TE = dplyr::filter(pd1_pdl1_res_CS_pos, Protein == "PD1")$b, 
                      seTE = dplyr::filter(pd1_pdl1_res_CS_pos, Protein == "PD1")$se)
summary(pd1_res_CS_pos)

pd1_meta_res_CS_pos <- data.frame(Protein = "PD1",
                              Survival = "random effects meta-analysis (cancer-specific mortality events, positive control sites)", 
                              est = pd1_res_CS_pos$TE.random,
                              est_95ci_low = pd1_res_CS_pos$lower.random, 
                              est_95ci_upp = pd1_res_CS_pos$upper.random,
                              est_p = pd1_res_CS_pos$pval.random,
                              q = pd1_res_CS_pos$Q,
                              q_df = pd1_res_CS_pos$df.Q,
                              q_pval = pd1_res_CS_pos$pval.Q) %>%
  dplyr::mutate(se = (est-est_95ci_low)/1.96)

### PD-L1:
pdl1_res_CS_pos <- metagen(TE = dplyr::filter(pd1_pdl1_res_CS_pos, Protein == "PDL1")$b, 
                       seTE = dplyr::filter(pd1_pdl1_res_CS_pos, Protein == "PDL1")$se)

pdl1_meta_res_CS_pos <- data.frame(Protein = "PDL1",
                               Survival = "random effects meta-analysis (cancer-specific mortality events, positive control sites)",
                               est = pdl1_res_CS_pos$TE.random,
                               est_95ci_low = pdl1_res_CS_pos$lower.random, 
                               est_95ci_upp = pdl1_res_CS_pos$upper.random,
                               est_p = pdl1_res_CS_pos$pval.random,
                               q = pdl1_res_CS_pos$Q,
                               q_df = pdl1_res_CS_pos$df.Q,
                               q_pval = pdl1_res_CS_pos$pval.Q) %>%
  dplyr::mutate(se = (est-est_95ci_low)/1.96)


## join results together
meta_res <- dplyr::full_join(pd1_meta_res_CS, pdl1_meta_res_CS) %>%
  dplyr::full_join(pd1_meta_res_AS) %>%
  dplyr::full_join(pdl1_meta_res_AS) %>%
  dplyr::full_join(pd1_pdl1_res, by = c("Protein" = "Protein",
                   "Survival" = "Survival",
                   "est" = "b",
                   "est_95ci_low" = "CI.lower",
                   "est_95ci_upp" = "CI.upper",
                   "est_p" = "pval",
                   "se" = "se"))

## forest plot results
meta_res$site_name <- gsub("\\b([a-z])", "\\U\\1", meta_res$Survival, perl=TRUE)
meta_res <- dplyr::mutate(meta_res, Protein = case_when(Protein == "PD1" ~ "PD-1",
                                                Protein == "PDL1" ~ "PD-L1"))
meta_res$Protein <- factor(meta_res$Protein, levels = c("PD-L1", "PD-1"))
meta_res <- dplyr::arrange(meta_res, factor(site_name, 
                                  levels = c("Breast", "Colorectal", "Melanoma", "Prostate",
                                             "Random Effects Meta-Analysis (Cancer-Specific Mortality*)",
                                             "Lung", "Ovarian",
                                             "Random Effects Meta-Analysis (All-Cause Mortality**)")))

pdf('PD1_PDL1_maininst_surv_res_forplot_170826.pdf',
    width=14, height=8)
pd1_pdl1_res_wmeta_forplot <- ggforestplot::forestplot(df = meta_res,
                                                       estimate = est, pvalue = est_p, se = se,
                                                       logodds = TRUE, psignif = 0.05, 
                                                       name = site_name,
                                                       ci = 0.95,
                                                       colour = Protein,
                                                       xlab = "HR (95% CI)",
                                                       ylab = "Cancer site", size=0.1) +
  geom_point(data = meta_res, aes(x = exp(est), size = 1/se^2, colour = Protein, fill = Protein),
             shape = 22L, show.legend = FALSE, position = ggstance::position_dodgev(height = 0.5)) +
  scale_size(range = c(2, 10)) +
  guides(shape = "none")
print(pd1_pdl1_res_wmeta_forplot)
dev.off()
