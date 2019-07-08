library(blackboxstudyR)
library(ggplot2)
library(RColorBrewer)
library(ggforce)
library(tidyverse)
my_theme <-  theme_bw() + 
  theme(text = element_text(family="serif"))

### CTS DATA

diff = c(-4.45, -4.90, -4.60, -4.75, -4.21, -2.43, -4.72, -4.22, -4.09, -4.26, -4.70, -4.57)
theta_cts = seq(-10, 10, by = .1)


### FBI DATA
im_scored <- score_bb_data(TestResponses, "inconclusive_mcar")
im_data <- irt_data_bb(TestResponses, im_scored)
#im_model <- fit_rasch(im_data, iterations = 600, n_chains = 4)
obs_p_score <- bb_person_score(TestResponses, im_scored)
q_diff <- apply(im_samples, 3, median)[grep("b\\[", names(apply(im_samples, 3, median)))]
ex_error_rates <- error_rate_analysis(TestResponses, q_diff)
person_mcmc_intervals(im_samples) %>% 
  right_join(., obs_p_score, by = "exID") %>% 
  full_join(., ex_error_rates, by = "exID") %>%
  dplyr::select(., score, m, ll, hh, exID, avg_diff, fpr, fnr) %>%
  ggplot(., aes(
    x = fpr,
    y = m,
    ymin = ll,
    ymax = hh
  )) +
  geom_pointrange(size = .3, col = 'gray30') +
  labs(x = "False Positive Rate", y = "Proficiency Estimate") +
  my_theme -> p1

person_mcmc_intervals(im_samples) %>%
  right_join(., obs_p_score, by = "exID") %>% 
  full_join(., ex_error_rates, by = "exID") %>%
  dplyr::select(., score, m, ll, hh, exID, avg_diff, fpr, fnr) %>%
  ggplot(., aes(
    x = fnr,
    y = m,
    ymin = ll,
    ymax = hh,
    color = fpr > 0
  )) +
  geom_pointrange(size = .3) +
  labs(x = "False Negative Rate", y = "Proficiency Estimate") +
  scale_colour_manual(values = c("gray30", "darkturquoise")) +
  my_theme + 
  theme(legend.position = "none")  -> p2

pdf(file="../thesis-doc/figures/pdf/prof-error-rate.pdf",width=6,height=3)
ggpubr::ggarrange(p1, p2, ncol = 2)
dev.off()

person_mcmc_intervals(im_samples) %>%
  right_join(obs_p_score, by = "exID") %>% 
  ggplot(aes(
    x = score,
    y = m,
    ymin = ll,
    ymax = hh
  )) +
  geom_pointrange(size = .3, col = 'gray30') +
  gghighlight::gghighlight(score < .96 & score > .94) +
  labs(x = "Observed Score", y = "Proficiency Estimate") +
  my_theme -> p1

person_mcmc_intervals(im_samples) %>%
  right_join(obs_p_score, by = "exID") %>% 
  full_join(ex_error_rates, by = "exID") %>%
  dplyr::select(score, m, ll, hh, exID, avg_diff, pct_skipped) %>%
  filter(score < .96 & score > .94) %>%
  ggplot(aes(
    x = avg_diff,
    y = m,
    ymin = ll,
    ymax = hh,
    col = 1 - pct_skipped
  )) +
  scale_color_viridis_c(end = .75) +
  geom_pointrange(size = .3) +
  labs(x = "Avg Question Difficulty", y = "Proficiency Estimate", color = "% Conclusive") +
  my_theme -> p2

pdf(file="../thesis-doc/figures/pdf/prof-observed.pdf",width=6,height=3)
ggpubr::ggarrange(p1, p2, ncol = 2, widths = c(1,1.4))
dev.off()


nci_scored <- score_bb_data(TestResponses, "no_consensus_incorrect")
nci_data <- irt_data_bb(TestResponses, nci_scored)
pc_scored <- score_bb_data(TestResponses, "partial_credit")
pc_data <- irt_data_bb(TestResponses, pc_scored)

p_score_im <- bb_person_score(TestResponses, im_scored)
person_mcmc_intervals(blackboxstudyR::im_samples) %>%
  right_join(p_score_im, by = "exID") %>%
  mutate(scoring = rep("im", nrow(p_score_im))) -> p_score_im

p_score_nci <- bb_person_score(TestResponses, nci_scored)
person_mcmc_intervals(blackboxstudyR::nci_samples) %>% 
  right_join(p_score_nci, by = "exID") %>%
  mutate(scoring = rep("nci", nrow(p_score_nci))) -> p_score_nci

p_score_pc <- bb_person_score(TestResponses, pc_scored)
person_mcmc_intervals(blackboxstudyR::pc_samples) %>%
  right_join(p_score_pc, by = "exID") %>%
  mutate(scoring = rep("pc", nrow(p_score_pc))) -> p_score_pc

p_score_im %>%
  bind_rows(p_score_nci) %>%
  bind_rows(p_score_pc) %>%
  mutate(Scoring = ifelse(scoring == "im", "Inconclusive MCAR", 
                          ifelse(scoring == "pc", "Partial Credit", "Consensus-Based"))) %>%
  ggplot(aes(x = score, y = m, ymin = ll, ymax = hh, col = Scoring)) + 
  geom_linerange(alpha = .5) +
  geom_point(size = .5, alpha = .5) +
  scale_color_viridis_d(end = .75) +
  labs(x = "Observed Score",
       y = "Estimated Proficiency") +
  my_theme -> p1

p_score_im %>%
  bind_rows(p_score_nci) %>%
  bind_rows(p_score_pc) %>%
  group_by(exID) %>% 
  mutate(Scoring = ifelse(scoring == "im", "Inconclusive MCAR", 
                          ifelse(scoring == "pc", "Partial Credit", "Consensus-Based"))) %>%
  ggplot(aes(x = score, y = m, ymin = ll, ymax = hh, col = Scoring, group = exID)) + 
  #geom_linerange(alpha = .5) +
  geom_pointrange(size = .5, alpha = .5, fatten = 1) +
  gghighlight::gghighlight(hh < -0.5, use_group_by = FALSE) + 
  scale_color_viridis_d(end = .75) +
  geom_line(col = "black", linetype = "dotted") +
  labs(x = "Observed Score",
       y = "Estimated Proficiency") +
  geom_hline(yintercept = - 0.5, col = "darkred", linetype = "dashed") +
  my_theme -> p2

pdf(file="../thesis-doc/figures/pdf/prof-three-scores.pdf",width=6,height=3)
ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend="bottom")
dev.off()

p_score_im %>%
  bind_rows(p_score_nci) %>%
  bind_rows(p_score_pc) %>%
  arrange(parameter) %>%
  mutate(id = rep(1:169,each = 3)) %>%
  dplyr::select(id, m, scoring) %>%
  tidyr::spread(scoring, m) %>%
  mutate(max.diff = apply( cbind(abs(im - nci), abs(nci - pc), abs(im - pc)), 1, max)) %>%
  tidyr::gather("scoring", "median", -c(id, max.diff)) %>%
  mutate(Scoring = ifelse(scoring == "im", "Inconclusive\nMCAR", 
                          ifelse(scoring == "pc", "Partial\nCredit", 
                                 "Consensus\nBased"))) %>%
  ggplot(aes(x = Scoring, y = median, group = id, col = id)) + 
  geom_point() +
  geom_line() +
  scale_color_viridis_c(end = .75) +
  labs(x = "Scoring Method",
       y = "Estimated Proficiency") +
  my_theme -> p1

p_score_im %>%
  bind_rows(p_score_nci) %>%
  bind_rows(p_score_pc) %>%
  arrange(parameter) %>%
  mutate(id = rep(1:169,each = 3)) %>%
  dplyr::select(id, m, scoring) %>%
  tidyr::spread(scoring, m) %>%
  mutate(max.diff = apply( cbind(abs(im - nci), abs(nci - pc), abs(im - pc)), 1, max)) %>%
  tidyr::gather("scoring", "median", -c(id, max.diff)) %>%
  mutate(Scoring = ifelse(scoring == "im", "Inconclusive\nMCAR", 
                          ifelse(scoring == "pc", "Partial\nCredit", 
                                 "Consensus\nBased"))) %>%
  ggplot(aes(x = Scoring, y = median, group = id, col = id)) + 
  geom_point() +
  geom_line() +
  scale_color_viridis_c(end = .75) +
  labs(x = "Scoring Method",
       y = "Estimated Proficiency") +
  gghighlight::gghighlight( max.diff > 1.95, label_key = id, 
                            use_group_by = FALSE,
                            label_params = list(
                              label.size = 0,
                              nudge_x = .9,
                              fill = NA
                            )) +
  my_theme -> p2

pdf(file="../thesis-doc/figures/pdf/prof-by-id.pdf",width=6,height=3)
ggpubr::ggarrange(p1, p2, ncol=2, common.legend = TRUE, legend="bottom")
dev.off()


library(RColorBrewer)
theta = seq(-8, 8, by = .01)

brewer.colors = c(1, brewer.pal(4, "Paired"))
line.rasch = 1/(1+exp(-(theta - 0)))
line.2pl.sm = 1/(1+exp(-.5*(theta - 0)))
line.2pl.big = 1/(1+exp(-2*(theta - 0)))
line.3pl.sm = .3 + (1-.3)/(1+exp(-.5*(theta - 0)))
line.3pl.big = .3 + (1-.3)/(1+exp(-2*(theta - 0)))

pdf(file="../thesis-doc/figures/pdf/rasch2pl3plICCs.pdf",width=6,height=3.5)
plot(line.rasch~theta, type = 'l', lwd = 2,col = brewer.colors[1], ylab = "Probability", 
     xlab = expression(theta), xlim = c(-6,6), ylim = c(0,1), main = bquote(P(Y["pi"]==1)), family = 'serif')
points(line.2pl.sm~theta, type = 'l', lwd = 2,col = brewer.colors[2])
points(line.2pl.big~theta, type = 'l', lwd = 2,col = brewer.colors[3])
points(line.3pl.sm~theta, type = 'l', lwd = 2,col = brewer.colors[4])
points(line.3pl.big~theta, type = 'l', lwd = 2,col = brewer.colors[5])
legend("bottomright", legend = 
         c('Rasch, b=0', '2PL, a=0.5', '2PL, a=2', '3PL, a=0.5, c=0.3', '3PL, a=2, c=0.3'),  lty = rep(1,5),
       bty = 'n', lwd = rep(1.5,5), col = brewer.colors, cex = .8)
dev.off()


brewer.colors = brewer.pal(4, "Set1")[2:4]
theta = seq(-8, 8, by = .01)
beta1=-3
beta2=.2
line.cat0 = 1/(1+exp(theta-beta1) + exp(2*theta - beta1 - beta2))
line.cat1 = exp(theta-beta1)/(1+exp(theta-beta1) + exp(2*theta - beta1 - beta2))
line.cat2 = exp(2*theta-beta1-beta2)/(1+exp(theta-beta1) + exp(2*theta - beta1 - beta2))

line2.cat0 = 1 - (exp(theta - beta1)/(1+exp(theta - beta1)))
line2.cat1 = (exp(theta - beta1)/(1+exp(theta - beta1))) - (exp(theta - beta2)/(1+exp(theta - beta2))) 
line2.cat2 = (exp(theta - beta2)/(1+exp(theta - beta2))) 


pdf(file="../thesis-doc/figures/pdf/pcmICCs.pdf",width=6,height=3.5)
plot(line.cat0~theta, type = 'l', lwd = 2,col = brewer.colors[1], ylab = "Probability", 
     xlab = expression(theta), xlim = c(-6,6), ylim = c(0,1), main = bquote(P(Y["pi"]==0,1,2)), family = 'serif')
points(line.cat1~theta, type = 'l', lwd = 2,col = brewer.colors[2])
points(line.cat2~theta, type = 'l', lwd = 2,col = brewer.colors[3])
points(line2.cat0~theta, type = 'l', lwd = 2,col = brewer.colors[1], lty =2)
points(line2.cat1~theta, type = 'l', lwd = 2,col = brewer.colors[2], lty=2)
points(line2.cat2~theta, type = 'l', lwd = 2,col = brewer.colors[3], lty =2)
abline(v=c(beta1, beta2), lty = 2)
text(beta1, .9, bquote(beta[1] == .(beta1)), adj = c(0,0))
text(beta2, .9, bquote(beta[2] == .(beta2)), adj = c(0,0))
legend("right", legend = 
         c("Incorrect", "Partial Credit", "Correct", "PCM", "GRM"),  lty = c(rep(1,3), 1, 2),
       bty = 'n', lwd = rep(1.5,5), col = c(brewer.colors, 1, 1), cex = .8)
dev.off()

fbi <- TestResponses

fbi %>%
  group_by(Pair_ID) %>%
  summarize(
    pct_nv = mean(Latent_Value == 'NV', na.rm = T),
    pct_inc = mean(Compare_Value == 'Inconclusive', na.rm = T)
  ) %>%
  filter(.,pct_nv > 0 & pct_inc > 0)

## Non-consensus question example
pdf(file="../thesis-doc/figures/pdf/consensus-examples.pdf",width=6,height=3)
fbi %>%
  dplyr::filter(.,Pair_ID  %in% c("M003064", "N057413") )%>%
  mutate(
    Latent = ifelse(Latent_Value == "NV", "NV", "Has Value"),
    Source = fct_explicit_na(case_when(
      Compare_Value == "Exclusion" ~ 'Exc.', 
      Compare_Value == "Individualization" ~ 'ID',
      Compare_Value == "Inconclusive" ~ 'Inc.'
    ), na_level = "NV"),
    Inconclusive_Reason = fct_explicit_na(Inconclusive_Reason),
    Difficulty = fct_explicit_na(substr(Difficulty, 1, 1), na_level = "NV")
  ) %>%
  group_by(Latent, Source, Difficulty, Pair_ID) %>%
  tally() %>%
  ggforce::gather_set_data(., 1:3) %>%
  mutate(
    x = factor(x, levels = c("Latent", "Source", "Difficulty"))
  ) %>%
  ggplot(., aes(x, id = id, split = y, value = n)) +
  ggforce::geom_parallel_sets(aes(fill = Source), alpha = .5, axis.width = 0.15) + 
  ggforce::geom_parallel_sets_axes(axis.width = 0.2) + 
  ggforce::geom_parallel_sets_labels(colour = 'white', size = 2.8) +
  facet_grid(cols = vars(Pair_ID)) + 
  scale_fill_viridis_d(end = .75) +
  labs(x = '') +
  my_theme +
  theme(legend.position = 'none',
        strip.background = element_blank(),
        strip.text = element_text(size = 12)) +
  labs(x = "Decision",
       y = "N Respondents")
dev.off()
