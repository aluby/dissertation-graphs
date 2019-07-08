library(tidyr)
library(dplyr)
library(ggplot2)
library(kableExtra)
library(viridis)
library(gridExtra)
library(ggpubr)
library(dplyr)
library(bayesplot)
library(blackboxstudyR)

doc_theme <-  theme_bw() + 
  theme(text = element_text(family="serif"))

load("data/parameter-ests-from-3-stans.RData")

pdf(file="../thesis-doc/figures/pdf/inc-nv-hists.pdf",width=6,height=3)
fbi %>%
  select(exID, Compare_Value, Latent_Value) %>%
  group_by(exID) %>%
  summarize(., n_nv = sum(Latent_Value == 'NV'), 
            n_inc = sum(Compare_Value == 'Inconclusive', na.rm = TRUE)) %>%
  select(exID, n_nv, n_inc) %>%
  gather(., key = "resp", value = "obs", -exID) %>%
  mutate(., resp2 = ifelse(resp == 'n_nv', 'No Value', 'Inconclusive')) %>%
  ggplot(., aes(x = obs, fill = resp2)) +
  geom_histogram(bins = 25) +
  facet_grid(cols = vars(resp2)) +
  scale_fill_viridis_d(end = .5) +
  doc_theme +
  theme(legend.position = "none") +
  labs(
    x = 'Number Reported',
    y = 'N Examiners')
dev.off()

## HISTOGRAMS OF POINT ESTIMATES FOR EACH TREE SPLIT
pdf(file="../thesis-doc/figures/pdf/or-item-params.pdf",width=4,height=6)
select(items, contains("or")) %>% 
  gather(name, value) %>%
  tidyr::extract(name, c('model', 'parameter'), "(..)(.*)") %>%
  mutate(., split = ifelse(parameter == "b22",
                           "b21",
                           ifelse(parameter == "b32", 
                                  "b31",
                                  parameter))) %>%
  mutate(., param_name = factor(case_when(
    parameter == "b11" ~ "Value", 
    parameter == "b21" ~ "Ind-Inc", 
    parameter == "b22" ~ "Inc-Exc", 
    parameter == "b31" ~ "Clo-Ins", 
    parameter == "b32" ~ "Ins-NoO"
  ), levels = c("Value", "Ind-Inc", "Inc-Exc","Clo-Ins", "Ins-NoO"),
  labels = c(expression(paste(b[1], " (Value)")),
             expression(paste(b[21], " (Ind-Inc) ")),
             expression(paste(b[22], " (Inc-Exc)")),
             expression(paste(b[31], " (Clo-Ins)")),
             expression(paste(b[32], " (Ins-NoO)"))))) %>%
  ggplot(., aes(x=value, col = split, fill = split)) +
  geom_histogram(bins = 100, alpha = .3) +
  scale_color_viridis(name = "", discrete=TRUE) +
  scale_fill_viridis(name = "", discrete=TRUE) +
  labs(
    title = 'Ordered Response Tree Item Parameters',
    x = "") +
  doc_theme +
  facet_grid(rows = vars(param_name), labeller=label_parsed) +
  theme(legend.position = "none") +
  guides(color = FALSE)
dev.off()

pdf(file="../thesis-doc/figures/pdf/or-person-params.pdf",width=4,height=6)
select(examiners, contains("or")) %>% 
  gather(name, value) %>%
  tidyr::extract(name, c('model', 'parameter'), "(..)(.*)") %>%
  mutate(., param_name = factor(case_when(
    parameter == "theta1" ~ "No Val Tend",
    parameter == "theta2" ~ "Exc Tend",
    parameter == "theta3" ~ "No Ov Tend"
  ), levels = c("No Val Tend", "Exc Tend", "No Ov Tend"),
  labels = c(expression(paste(theta[1], " (No Value Tend)")),
             expression(paste(theta[2], " (Exclusion Tend) ")),
             expression(paste(theta[3], " (No Overlap Tend)"))
             ))) %>%
  ggplot(., aes(x = value, col = parameter, fill = parameter)) +
  geom_histogram(bins = 50, alpha = .3) +
  doc_theme + 
  scale_color_viridis(name = "", discrete=TRUE) +
  scale_fill_viridis(name = "", discrete=TRUE) +
  labs(
    title = 'Ordered Response Tree Person Parameters',
    x = "") +
  facet_grid(rows = vars(param_name), labeller = label_parsed) +
  theme(legend.position = "none")
dev.off()

pdf(file="../thesis-doc/figures/pdf/hp-item-params.pdf",width=4,height=6)
select(items, contains("hp")) %>% 
  gather(name, value) %>%
  tidyr::extract(name, c('model', 'parameter'), "(..)(.*)") %>%
  mutate(., split = ifelse(parameter == "b22",
                           "b21",
                           ifelse(parameter == "b32", 
                                  "b31",
                                  parameter))) %>%
  mutate(., param_name = factor(case_when(
    parameter == "b1" ~ "Value Tend",
    parameter == "b2" ~ "Suff Tend",
    parameter == "b3" ~ "Non-Match Tend", 
    parameter == "b4" ~ "Close Tend",
    parameter == "b5" ~ "No Ov Tend"
  ), levels = c("Value Tend", "Suff Tend", "Non-Match Tend", "Close Tend", "No Ov Tend"),
  labels = c(expression(paste(b[1], " (Value Tend)")),
             expression(paste(b[2], " (Suff Tend) ")),
             expression(paste(b[3], " (Non-Match)")),
             expression(paste(b[4], " (Close Tend)")),
             expression(paste(b[5], " (No Ov Tend)"))))) %>%
  ggplot(., aes(x=value, col = split, fill = split)) +
  geom_histogram(bins = 100, alpha = .3) +
  scale_color_viridis(name = "", discrete=TRUE) +
  scale_fill_viridis(name = "", discrete=TRUE) +
  labs(
    title = 'Binary Process Tree Item Parameters',
    x = "") +
  doc_theme +
  facet_grid(rows = vars(param_name), labeller = label_parsed) +
  theme(legend.position = "none") +
  guides(color = FALSE)
dev.off()

pdf(file="../thesis-doc/figures/pdf/hp-person-params.pdf",width=4,height=6)
select(examiners, contains("hp")) %>% 
  gather(name, value) %>%
  tidyr::extract(name, c('model', 'parameter'), "(..)(.*)") %>%
  mutate(., param_name = factor(case_when(
    parameter == "theta1" ~ "No Val Tend",
    parameter == "theta2" ~ "Ins Tend",
    parameter == "theta3" ~ "Match Tend", 
    parameter == "theta4" ~ "Ind Tend",
    parameter == "theta5" ~ "Excl Tend"
  ), levels = c("No Val Tend", "Ins Tend", "Match Tend", "Ind Tend", "Excl Tend"),
  labels = c(expression(paste(theta[1], " (No Value Tend)")),
             expression(paste(theta[2], " (Insuff Tend) ")),
             expression(paste(theta[3], " (Match Tend)")),
             expression(paste(theta[4], " (Individ Tend)")),
             expression(paste(theta[5], " (Excl Tend)"))
  ))) %>%
  ggplot(., aes(x = value, col = parameter, fill = parameter)) +
  geom_histogram(bins = 50, alpha = .3) +
  doc_theme + 
  scale_color_viridis(name = "", discrete=TRUE) +
  scale_fill_viridis(name = "", discrete=TRUE) +
  labs(
    title = 'Binary Process Tree Person Parameters',
    x = "") +
  facet_grid(rows = vars(param_name), labeller = label_parsed) +
  theme(legend.position = "none")
dev.off()

pdf(file="../thesis-doc/figures/pdf/ss-item-params.pdf",width=4,height=4)
select(items, contains("ss")) %>% 
  gather(name, value) %>%
  tidyr::extract(name, c('model', 'parameter'), "(..)(.*)") %>%
  mutate(., param_name = factor(case_when(
    parameter == "b1" ~ "Value Tend",
    parameter == "b2" ~ "Difficulty"
  ), levels = c("Value Tend", "Difficulty"),
  labels = c(expression(paste(b[1], " (Value Tend)")),
             expression(paste(b[2], " (Difficulty) "))))) %>%
  ggplot(., aes(x=value, col = param_name, fill = param_name)) +
  geom_histogram(bins = 50, alpha = .3) +
  scale_color_viridis(name = "", discrete=TRUE, end = .5) +
  scale_fill_viridis(name = "", discrete=TRUE, end = .5) +
  labs(
    title = 'Simplified-Scored Tree Item Parameters',
    x = "") +
  doc_theme +
  facet_grid(rows = vars(param_name), labeller = label_parsed) +
  theme(legend.position = "none") +
  guides(color = FALSE)
dev.off()

pdf(file="../thesis-doc/figures/pdf/ss-person-params.pdf",width=4,height=4)
select(examiners, contains("ss")) %>% 
  gather(name, value) %>%
  tidyr::extract(name, c('model', 'parameter'), "(..)(.*)") %>%
  mutate(., param_name = factor(case_when(
    parameter == "theta1" ~ "No Val Tend",
    parameter == "theta2" ~ "Proficiency"
  ), levels = c("No Val Tend", "Proficiency"),
  labels = c(expression(paste(theta[1], " (No Value Tend)")),
             expression(paste(theta[2], " (Proficiency) "))
  ))) %>%
  ggplot(., aes(x = value, col = parameter, fill = parameter)) +
  geom_histogram(bins = 50, alpha = .3) +
  doc_theme + 
  scale_color_viridis(name = "", discrete=TRUE, end = .5) +
  scale_fill_viridis(name = "", discrete=TRUE, end = .5) +
  labs(
    title = 'Simplified-Scored Tree Person Parameters',
    x = "") +
  facet_grid(rows = vars(param_name), labeller = label_parsed) +
  theme(legend.position = "none")
dev.off()

### COMPARISON OF NO VALUE SPLITS 

nv_e = dplyr::select(examiners, dplyr::contains("1")) %>%
  gather(model, estimate, -c(hptheta1)) %>%
  tidyr::extract(model, c('model', 'parameter'), "(..)(.*)") %>%
  mutate(model_name = case_when(
    model == "or" ~ "Ordered Tree",
    model == "ss" ~ "Simp-Scored Tree",
    model == "hp" ~ "Binary Process Tree"
  )) %>%
  ggplot(., aes(x=hptheta1, y = estimate, col = model)) + 
  geom_point(alpha = .6) + 
  geom_abline(intercept = 0, slope = 1, col = "gray") + 
  doc_theme + 
  scale_color_viridis(discrete=TRUE,
                      name = '',
                      breaks = c('or', 'ss'),
                      labels = c('Observed Response', 'Simplified Scored'),
                      end = .75) +
  labs(x = expression(theta[1]~' from Binary Process Tree'),
       y = expression(theta[1]~' Estimates')) +
  facet_grid(rows = vars(model_name)) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 25, hjust = 1))

nv_i = dplyr::select(items, contains("1")) %>%
  dplyr::select(., -c(orb21, orb31)) %>%
  gather(model, estimate, -c(hpb1)) %>%
  tidyr::extract(model, c('model', 'parameter'), "(..)(.*)") %>%
  mutate(model_name = case_when(
    model == "or" ~ "Ordered Tree",
    model == "ss" ~ "Simp-Scored Tree",
    model == "hp" ~ "Binary Process Tree"
  )) %>%
  ggplot(., aes(x=hpb1, y = estimate, col = model)) + 
  geom_point(alpha = .6) + 
  geom_abline(intercept = 0, slope = 1, col = "gray") + 
  doc_theme + 
  scale_color_viridis(discrete=TRUE,
                      name = '',
                      breaks = c('or', 'ss'),
                      labels = c('Observed Response', 'Simplified Scored'),
                      end = .75) +
  labs(x = expression(b[1]~' from Binary Process Tree'),
       y = expression(b[1]~' Estimates')) +
  facet_grid(rows = vars(model_name)) +
  theme(legend.position = "none",  axis.text.x = element_text(angle = 25, hjust = 1))

pdf(file="../thesis-doc/figures/pdf/node1-params.pdf",width=6,height=3, onefile = FALSE)
ggpubr::ggarrange( nv_i, nv_e, nrow=1, ncol=2)
dev.off()

### COMPARISON OF THETA PARAMETERS ACROSS MODELS 
pdf(file="../thesis-doc/figures/pdf/sstheta2-otherthetas.pdf",width=6,height=3, onefile = FALSE)
dplyr::select(examiners, -dplyr::contains("theta1")) %>%
  gather(model, estimate, -c(sstheta2)) %>%
  tidyr::extract(model, c('model', 'parameter'), "(..)(.*)") %>%
  mutate(., model_name = ifelse(model == 'hp', "Binary Process", "Ordered Response"),
         param_name = case_when(
           model == "hp" & parameter == "theta2" ~ "Insuf Tend",
           model == "hp" & parameter == "theta3" ~ "Match Tend",
           model == "hp" & parameter == "theta4" ~ "Ind Tend",
           model == "hp" & parameter == "theta5" ~ "Excl Tend",
           model == "or" & parameter == "theta2" ~ "Excl Tend",
           model == "or" & parameter == "theta3" ~ "No Ov Tend"
         )) %>%
  ggplot(., aes(x=sstheta2, y = estimate, col = model)) + 
  facet_grid(cols = vars(param_name), rows = vars(model_name)) +
  geom_point() + 
  doc_theme + 
  scale_color_viridis(discrete=TRUE, name = '', end = .75) +
  labs(x = expression(theta[2]~'Estimate in SS model'),
       y = expression(theta[k]~'Estimate')) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 25, hjust = 1))
dev.off()

### INCONCLUSIVE ANALYSIS 


fbi <- TestResponses
fbi$exID = as.integer(fbi$Examiner_ID)
fbi$qID = as.integer(fbi$Pair_ID)
mixtree = readRDS("data/hp-irtree-results")
#load("data/fbi-withpreds.RData")
load("data/parameter-ests-from-3-stans.RData")

logistic = function(theta, b){
  return(exp(theta-b)/(1+exp(theta-b)))
}

fbi$tree_response = case_when(
  fbi$Compare_Value == 'Individualization' ~ 1,
  fbi$Inconclusive_Reason  == 'Close' ~ 2,
  fbi$Inconclusive_Reason == 'Insufficient' ~ 3,
  fbi$Inconclusive_Reason == 'Overlap' ~ 4,
  fbi$Compare_Value == 'Exclusion' ~ 5)

incs = fbi[which(fbi$tree_response < 5 & fbi$tree_response > 1),]
theta.0.m = examiners$hptheta1
theta.1.m = examiners$hptheta2
theta.2.m = examiners$hptheta3
theta.3.m = examiners$hptheta4
theta.4.m = examiners$hptheta5

item.0.m = items$hpb1
item.1.m = items$hpb2
item.2.m = items$hpb3
item.3.m = items$hpb4
item.4.m = items$hpb5

fbi$p.1 = rep(NA, nrow(fbi))
fbi$p.2 = rep(NA, nrow(fbi))
fbi$p.3 = rep(NA, nrow(fbi))
fbi$p.4 = rep(NA, nrow(fbi))
fbi$p.5 = rep(NA, nrow(fbi))


for(obs in 1:nrow(fbi)){
  ex = fbi$exID[obs]
  it = fbi$qID[obs]
  fbi$p.1[obs] = (1-logistic(theta.0.m[ex], item.0.m[it]))*(1-logistic(theta.1.m[ex], item.1.m[it]))*logistic(theta.2.m[ex], item.2.m[it])*logistic(theta.3.m[ex], item.3.m[it])
  fbi$p.2[obs] = (1-logistic(theta.0.m[ex], item.0.m[it]))*(1-logistic(theta.1.m[ex], item.1.m[it]))*logistic(theta.2.m[ex], item.2.m[it])*(1-logistic(theta.3.m[ex], item.3.m[it])) 
  fbi$p.3[obs] = (1-logistic(theta.0.m[ex], item.0.m[it]))*(logistic(theta.1.m[ex], item.1.m[it]))
  fbi$p.4[obs] = (1-logistic(theta.0.m[ex], item.0.m[it]))*(1-logistic(theta.1.m[ex], item.1.m[it]))*(1-logistic(theta.2.m[ex], item.2.m[it]))*(1-logistic(theta.4.m[ex], item.4.m[it]))
  fbi$p.5[obs] = (1-logistic(theta.0.m[ex], item.0.m[it]))*(1-logistic(theta.1.m[ex], item.1.m[it]))*(1-logistic(theta.2.m[ex], item.2.m[it]))*logistic(theta.4.m[ex], item.4.m[it])
}

fbi$conc = (fbi$tree_response == 1) | (fbi$tree_response == 5)
fbi$concl = fbi$conc 
fbi$concl = c('Inconclusive', 'Conclusive')[fbi$conc + 1]
fbi$p.conc = fbi$p.1 + fbi$p.5

pdf(file="../thesis-doc/figures/pdf/pconcdensity.pdf",width=6,height=3)
fbi %>%
  filter(!is.na(concl)) %>%
  ggplot(., aes(p.conc, fill = concl, col = concl)) +
  geom_histogram(position = 'identity', alpha = .7, bins = 50) +
  scale_fill_viridis_d(name = 'Observed Response', end = .75) +
  scale_color_viridis_d(name = 'Observed Response', end = .75) +
  xlab('P(Conclusive) from model') + 
  ylab('Examiner x Item Observations') + 
  doc_theme +
  theme(legend.position = "bottom") + 
  geom_vline(xintercept = .5, col = 'gray', linetype = "dashed") + 
  annotate("text", x = .75, y = 1500, label = "Conclusive\nExpected", family = "serif") +
  annotate("text", x = .25, y = 1500, label = "Inconclusive\nExpected", family = "serif")
dev.off()

inc.m = fbi %>%
  filter(., Compare_Value == "Inconclusive") %>%
  filter(., Mating == "Mates") %>%
  ggplot(., aes(p.1, fill = Mating, col = Mating)) +
  geom_histogram(position = 'identity', 
                 alpha = .7, 
                 color = viridis(4)[3], 
                 fill = viridis(4)[3],
                 bins = 35) +
  doc_theme +
  xlab('P(Individualization) from model') + 
  ylab('Examiner x Item Observations') + 
  theme(legend.position = "none") + 
  ggtitle('True Matches') +
  geom_vline(xintercept = .5, col = 'gray', linetype = "dashed") + 
  annotate("text", x = .75, y = 1500, label = "Individualization\nExpected", family = "serif") +
  annotate("text", x = .25, y = 1500, label = "Inconclusive\nExpected", family = "serif")

inc.nm = fbi %>%
  filter(., Compare_Value == "Inconclusive") %>%
  filter(., Mating == "Non-mates") %>%
  ggplot(., aes(p.5, fill = Mating, col = Mating)) +
  geom_histogram(position = 'identity', 
                 alpha = .7, 
                 color = viridis(4)[3], 
                 fill = viridis(4)[3],
                 bins = 40) +
  doc_theme +
  xlab('P(Exclusion) from model') + 
  ylab('Examiner x Item Observations') + 
  theme(legend.position = "none") + 
  ggtitle('True Non-Matches') +
  geom_vline(xintercept = .5, col = 'gray', linetype = "dashed") + 
  annotate("text", x = .75, y = 80, label = "Exclusion\nExpected", family = "serif") +
  annotate("text", x = .25, y = 80, label = "Inconclusive\nExpected", family = "serif")

pdf(file="../thesis-doc/figures/pdf/pconcsplit.pdf",width=6,height=3)
ggpubr::ggarrange(inc.m, inc.nm, nrow=1, ncol=2)
dev.off()

# Spits out cutoff where only 5% of conclusives are less than p (.38)
cutoff = fbi %>%
  filter(., conc == TRUE) %>%
  mutate(cumdist = cume_dist(p.conc)) %>%
  filter(cumdist < .05) %>%
  summarise(., max(p.conc))

fbi$logodds = log(fbi$p.conc/(1-fbi$p.conc))

wrongincs = fbi %>%
  filter(., conc == FALSE & (p.1>.5 | p.5 > .5) )

length(unique(wrongincs$exID))
length(unique(wrongincs$qID))

table(wrongincs$exID, wrongincs$qID)

table(wrongincs$tree_response, wrongincs$hrPred, wrongincs$Mating)


table(fbi$tree_response, fbi$mixPred, fbi$match)

### Single Example

predict_hptree_scalar = function(item.params, exam.params){
  return(list(
    p.nv = logistic(exam.params[1], item.params[1]),
    p.ind = (1-logistic(exam.params[1], item.params[1]))*(1-logistic(exam.params[2], item.params[2]))*logistic(exam.params[3], item.params[3])*logistic(exam.params[4], item.params[4]),
    p.close = (1-logistic(exam.params[1], item.params[1]))*(1-logistic(exam.params[2], item.params[2]))*logistic(exam.params[3], item.params[3])*(1-logistic(exam.params[4], item.params[4])), 
    p.ins = (1-logistic(exam.params[1], item.params[1]))*(logistic(exam.params[2], item.params[2])),
    p.noov = (1-logistic(exam.params[1], item.params[1]))*(1-logistic(exam.params[2], item.params[2]))*(1-logistic(exam.params[3], item.params[3]))*(1-logistic(exam.params[5], item.params[5])),
    p.exc = (1-logistic(exam.params[1], item.params[1]))*(1-logistic(exam.params[2], item.params[2]))*(1-logistic(exam.params[3], item.params[3]))*logistic(exam.params[5], item.params[5])
  ))
}

predict_hptree_matrix = function(item.params, exam.params){
  return(list(
    p.nv = logistic(exam.params[,1], item.params[,1]),
    p.ind = (1-logistic(exam.params[,1], item.params[,1]))*(1-logistic(exam.params[,2], item.params[,2]))*logistic(exam.params[,3], item.params[,3])*logistic(exam.params[,4], item.params[,4]),
    p.close = (1-logistic(exam.params[,1], item.params[,1]))*(1-logistic(exam.params[,2], item.params[,2]))*logistic(exam.params[,3], item.params[,3])*(1-logistic(exam.params[,4], item.params[,4])), 
    p.ins = (1-logistic(exam.params[,1], item.params[,1]))*(logistic(exam.params[,2], item.params[,2])),
    p.noov = (1-logistic(exam.params[,1], item.params[,1]))*(1-logistic(exam.params[,2], item.params[,2]))*(1-logistic(exam.params[,3], item.params[,3]))*(1-logistic(exam.params[,5], item.params[,5])),
    p.exc = (1-logistic(exam.params[,1], item.params[,1]))*(1-logistic(exam.params[,2], item.params[,2]))*(1-logistic(exam.params[,3], item.params[,3]))*logistic(exam.params[,5], item.params[,5])
  ))
}

predict_hptree_scalar(items[556,1:5], examiners[55,1:5])

posteriors = as.data.frame(predict_hptree_matrix(
  rstan::extract(mixtree, pars = 'b')$b[,556,], 
  rstan::extract(mixtree, pars = 'theta')$theta[,55,])
) 

quantile.90 = function(x){
  return(quantile(x, probs = c(.90)))
}

quantile.25 = function(x){
  return(quantile(x, probs = c(.25)))
}
quantile.10 = function(x){
  return(quantile(x, probs = c(.10)))
}

quantiles = posteriors %>%
  summarise_all(., funs(quantile.10)) %>%
  gather(., value = "grp.quantile")



# Make ROC - ish graph
fbi.inconc = fbi[which(fbi$Compare_Value == 'Inconclusive'),]
preds = predict_hptree_scalar(items[fbi.inconc$qID,1:5], examiners[fbi.inconc$exID, 1:5])

preds.df = data.frame(
  p.nv = preds$p.nv,
  p.ind = preds$p.ind,
  p.close = preds$p.close,
  p.ins = preds$p.ins,
  p.noov = preds$p.noov, 
  p.ex = preds$p.exc,
  row.names = NULL
)
colnames(preds.df) = c('p.nv', 'p.ind', 'p.cl', 'p.ins', 'p.noov', 'p.ex')

df = preds.df %>%
  mutate(predicted = apply(., 1, which.max)) %>%
  select(p.ind, p.ex, predicted) %>%
  mutate(truth = fbi.inconc$Mating)

classify_incs = function(ind.prob, ex.prob, predicted.outcome, truth, cutoff, use.conc = FALSE){
  m.err = sum((ind.prob > cutoff) & (predicted.outcome != 2) & (truth == "Mates")) / 
    sum((ind.prob > cutoff) & (truth == "Mates"))
  nm.err = sum((ex.prob > cutoff) & (predicted.outcome != 6) & (truth == "Non-mates")) / 
    sum((ex.prob > cutoff) & (truth == "Non-mates"))
  n.flagged = sum(((ex.prob > cutoff) | (ind.prob > cutoff)))
  if(use.conc){
    m.err = sum(((ind.prob + ex.prob)> cutoff) & (predicted.outcome != 2) & (truth == "Mates")) / 
      sum(((ind.prob + ex.prob)> cutoff) & (truth == "Mates"))
    nm.err = sum(((ex.prob + ind.prob) > cutoff) & (predicted.outcome != 6) & (truth == "Non-mates")) / 
      sum(((ind.prob + ex.prob)> cutoff) & (truth == "Non-mates"))
    n.flagged = sum((((ex.prob + ind.prob) > cutoff)))
  }
  return(c(m.err, nm.err, n.flagged))
}

roc_df = data.frame(m.err = rep(NA, 2*101), 
                    nm.err = rep(NA, 2*101), 
                    n.flagged = rep(NA, 2*101),
                    type = c(rep('ind_or_ex', 101), rep('conclusive', 101)))
for(ii in 1:101){
  cutoffs = seq(0,1,.01)
  roc_df[ii,1:3] = classify_incs(df$p.ind, df$p.ex, df$predicted, df$truth, cutoffs[ii])
  roc_df[ii+101,1:3] = classify_incs(df$p.ind, df$p.ex, df$predicted, df$truth, cutoffs[ii], use.conc = TRUE)
}

roc_df$cutoffs = rep(cutoffs, 2)
pdf(file="../thesis-doc/figures/pdf/inconc-analysis-roc.pdf",width=6,height=3)
roc_df %>%
  filter(type == 'conclusive') %>%
  ggplot(., aes(x= (1-nm.err), y = (1-m.err), group = type, linetype = type, label = cutoffs)) +
  geom_line(col = 'darkgray') +
  geom_point(aes(col = n.flagged/4907)) +
  ggrepel::geom_text_repel(nudge_x = .01, 
                  nudge_y = -.025,
                  aes(label = ifelse(cutoffs %in% c(.01, .05, .1, .2, .3, .5), 
                                     paste0("c=",cutoffs), ''),
                      col = n.flagged/4907)) + 
  doc_theme +
  scale_color_viridis(name = "Inconclusives classified as errors ", end = .75) + 
  theme(legend.position = 'bottom',
        legend.title = ) +
  xlim(0,1) +
  ylim(0,1) +
  scale_linetype(guide = 'none') +
  labs(x = 'Correct Exclusion Prediction Rate', 
       y = 'Correct ID Prediction Rate')
dev.off()


pdf(file="../thesis-doc/figures/pdf/inconc-exind-roc.pdf",width=6,height=3)
roc_df %>%
  filter(type != 'conclusive') %>%
  ggplot(., aes(x= (1-nm.err), y = (1-m.err), group = type, linetype = type, label = cutoffs)) +
  geom_line(col = 'darkgray') +
  geom_point(aes(col = n.flagged/4907)) +
  ggrepel::geom_text_repel(nudge_x = .01, 
                           nudge_y = -.025,
                           aes(label = ifelse(cutoffs %in% c(.01, .05, .1, .2, .3, .5), 
                                              paste0("c=",cutoffs), ''),
                               col = n.flagged/4907)) + 
  doc_theme +
  scale_color_viridis(name = "Inconclusives classified as errors ", end = .75) + 
  theme(legend.position = 'bottom',
        legend.title = ) +
  xlim(0,1) +
  ylim(0,1) +
  scale_linetype(guide = 'none') +
  labs(x = 'Correct Exclusion Prediction Rate', 
       y = 'Correct ID Prediction Rate')
dev.off()

### POSTERIOR PREDICTIVE CHECK
logistic = function(x){
  return(1/(1+exp(-x)))
}
fbi <- TestResponses
exID = as.integer(fbi$Examiner_ID)
qID = as.integer(fbi$Pair_ID)

mt = readRDS("data/hp-irtree-results")
pct = readRDS("data/or-irtree-results")
st = readRDS("data/ss-irtree-2d-results")

p.val = 1 - logistic(rstan::extract(pct, pars = 'theta')$theta[,exID,1] - rstan::extract(pct, pars = 'b')$b[,qID,1])
split2.den = 1 + exp(rstan::extract(pct, pars = 'theta')$theta[,exID,2] - rstan::extract(pct, pars = 'b')$b[,qID,2]) + 
  exp(2*rstan::extract(pct, pars = 'theta')$theta[,exID,2] - 
        rstan::extract(pct, pars = 'b')$b[,qID,2]  - 
        rstan::extract(pct, pars = 'b')$b[,qID,3])
split3.den = 1 + exp(rstan::extract(pct, pars = 'theta')$theta[,exID,3] - rstan::extract(pct, pars = 'b')$b[,qID,4]) + 
  exp(2*rstan::extract(pct, pars = 'theta')$theta[,exID,3] - 
        rstan::extract(pct, pars = 'b')$b[,qID,4]  - 
        rstan::extract(pct, pars = 'b')$b[,qID,5])

pct.pp = array(dim = c(4000, 17121, 6))
pct.pp[,,1] = 1-p.val
pct.pp[,,2] = p.val * 1/split2.den
pct.pp[,,3] = p.val * 
    exp(rstan::extract(pct, pars = 'theta')$theta[,exID,2] - rstan::extract(pct, pars = 'b')$b[,qID,2])/split2.den *
    1/split3.den
pct.pp[,,4] = p.val * 
    exp(rstan::extract(pct, pars = 'theta')$theta[,exID,2] - rstan::extract(pct, pars = 'b')$b[,qID,2])/split2.den *
    exp(rstan::extract(pct, pars = 'theta')$theta[,exID,3] - rstan::extract(pct, pars = 'b')$b[,qID,4])/split3.den
pct.pp[,,5] = 
  p.val * 
    exp(rstan::extract(pct, pars = 'theta')$theta[,exID,2] - rstan::extract(pct, pars = 'b')$b[,qID,2])/split2.den *
    exp(2*rstan::extract(pct, pars = 'theta')$theta[,exID,3] - 
          rstan::extract(pct, pars = 'b')$b[,qID,4]  - 
          rstan::extract(pct, pars = 'b')$b[,qID,5])/split3.den
pct.pp[,,6] = p.val * exp(2*rstan::extract(pct, pars = 'theta')$theta[,exID,2] - 
                        rstan::extract(pct, pars = 'b')$b[,qID,2] - 
                        rstan::extract(pct, pars = 'b')$b[,qID,3])/split2.den


##pct.pp = array(unlist(pct.pp.list), dim = c(2000, 17121, 6))
## CHECK: are they all = 1? 
# table(pct.pp$p.nv + pct.pp$p.ind + pct.pp$p.close + pct.pp$p.ins + pct.pp$p.noov + pct.pp$p.exc)
# table(apply(pct.pp, c(1,2), sum))

which.max = function(x){
  return(which(x == max(x)))
}

## CHECK: are they all = 1?
## pct.sums = apply(pct.pp[1:2000, 100:200, 1:6], c(1,2), sum)
## apply(pct.pp[1, 1:100 , 1:6], c(1,2), which.max)
pct.predictions = apply(pct.pp, c(1,2), which.max)

mt.pp = array(dim = c(4000, 17121, 6))
mt.pp[,,1] = logistic(rstan::extract(mt, pars = 'theta')$theta[,exID,1] - rstan::extract(mt, pars = 'b')$b[,qID,1])
mt.pp[,,2] = (1-logistic(rstan::extract(mt, pars = 'theta')$theta[,exID,1] - rstan::extract(mt, pars = 'b')$b[,qID,1]))*
    (1-logistic(rstan::extract(mt, pars = 'theta')$theta[,exID,2] - rstan::extract(mt, pars = 'b')$b[,qID,2]))*
    logistic(rstan::extract(mt, pars = 'theta')$theta[,exID,3] - rstan::extract(mt, pars = 'b')$b[,qID,3])*
    logistic(rstan::extract(mt, pars = 'theta')$theta[,exID,4] - rstan::extract(mt, pars = 'b')$b[,qID,4])
mt.pp[,,3] = (1-logistic(rstan::extract(mt, pars = 'theta')$theta[,exID,1] - rstan::extract(mt, pars = 'b')$b[,qID,1]))*
    (1-logistic(rstan::extract(mt, pars = 'theta')$theta[,exID,2] - rstan::extract(mt, pars = 'b')$b[,qID,2]))*
    logistic(rstan::extract(mt, pars = 'theta')$theta[,exID,3] - rstan::extract(mt, pars = 'b')$b[,qID,3])*
    (1-logistic(rstan::extract(mt, pars = 'theta')$theta[,exID,4] - rstan::extract(mt, pars = 'b')$b[,qID,4]))
mt.pp[,,4] = (1-logistic(rstan::extract(mt, pars = 'theta')$theta[,exID,1] - rstan::extract(mt, pars = 'b')$b[,qID,1]))*
    (logistic(rstan::extract(mt, pars = 'theta')$theta[,exID,2] - rstan::extract(mt, pars = 'b')$b[,qID,2]))
mt.pp[,,5] = (1-logistic(rstan::extract(mt, pars = 'theta')$theta[,exID,1] - rstan::extract(mt, pars = 'b')$b[,qID,1]))*
    (1-logistic(rstan::extract(mt, pars = 'theta')$theta[,exID,2] - rstan::extract(mt, pars = 'b')$b[,qID,2]))*
    (1-logistic(rstan::extract(mt, pars = 'theta')$theta[,exID,3] - rstan::extract(mt, pars = 'b')$b[,qID,3]))*
    (1-logistic(rstan::extract(mt, pars = 'theta')$theta[,exID,5] - rstan::extract(mt, pars = 'b')$b[,qID,5]))
mt.pp[,,6] = (1-logistic(rstan::extract(mt, pars = 'theta')$theta[,exID,1] - rstan::extract(mt, pars = 'b')$b[,qID,1]))*
    (1-logistic(rstan::extract(mt, pars = 'theta')$theta[,exID,2] - rstan::extract(mt, pars = 'b')$b[,qID,2]))*
    (1-logistic(rstan::extract(mt, pars = 'theta')$theta[,exID,3] - rstan::extract(mt, pars = 'b')$b[,qID,3]))*
    logistic(rstan::extract(mt, pars = 'theta')$theta[,exID,5] - rstan::extract(mt, pars = 'b')$b[,qID,5])

#mt.pp = array(unlist(mt.pp.list), dim = c(nrow(mt.pp.list$p.nv), ncol(mt.pp.list$p.nv), 6))
#table(apply(mt.pp[1:100, 1:100, 1:6], c(1,2), sum))

mt.predictions = apply(mt.pp, c(1,2), which.max)
beepr::beep(3)
# count the number of errors for each observation
# plot the distribution of errors for each _observed_ response type (1-6) 

#load('fbi-withpreds.RData')
obs_resp = fbi$tree_response
obs_resp[which(is.na(obs_resp))] = 0
obs_resp = obs_resp + 1

cbind(errors = c(colMeans(mt.predictions != matrix(rep(obs_resp, nrow(mt.predictions)), 
                                                   byrow = TRUE, ncol = length(obs_resp))), 
                 colMeans(pct.predictions != matrix(rep(obs_resp, nrow(pct.predictions)), 
                                                    byrow = TRUE, ncol = length(obs_resp)))),
      obs_resp = rep(obs_resp,2)) %>%
  as.data.frame(.) %>%
  mutate(.,obs = as.factor(obs_resp)) %>%
  mutate(., model = c(rep("HP", length(obs_resp)/2), rep("OR", length(obs_resp)/2))) %>%
  ggplot(., aes(x=errors, col = model, fill = model)) +
  geom_histogram(alpha = 0.1, bins = 20, position = "identity", aes(y=log(..count..))) +
  facet_grid(cols = vars(obs)) 


cbind(errors = colMeans(pct.predictions != matrix(rep(obs_resp, nrow(pct.predictions)), 
                                                  byrow = TRUE, ncol = length(obs_resp))), 
      obs_resp) %>%
  as.data.frame(.) %>%
  mutate(.,obs = as.factor(obs_resp)) %>%
  ggplot(., aes(x=errors, col = obs, fill = obs, grp = obs)) +
  geom_freqpoly(aes(y=..count..)) +
  coord_cartesian(xlim = c(0,1)) +
  theme_minimal() 


cbind(errors = c(colMeans(mt.predictions != matrix(rep(obs_resp, nrow(mt.predictions)), 
                                                   byrow = TRUE, ncol = length(obs_resp))), 
                 colMeans(pct.predictions != matrix(rep(obs_resp, nrow(pct.predictions)), 
                                                    byrow = TRUE, ncol = length(obs_resp)))),
      obs_resp = rep(obs_resp,2)) %>%
  as.data.frame(.) %>%
  mutate(.,obs = as.factor(obs_resp)) %>%
  mutate(., model = c(rep("HP", length(obs_resp)/2), rep("OR", length(obs_resp)/2))) %>%
  ggplot(., aes(x=errors, col = model, fill = model)) +
  geom_histogram(alpha = 0.3, bins = 50, position = "identity", aes(y=log(..count..))) 


preds.dist.func = function(sample.preds){
  return(tabulate(sample.preds))
}

preds.dist.mt = t(apply(mt.predictions, 1, preds.dist.func))
preds.dist.pct = t(apply(pct.predictions, 1, preds.dist.func))

quantile.025 = function(x){
  return(quantile(x, probs = c(.025)))
}
quantile.975 = function(x){
  return(quantile(x, probs = c(.975)))
}

nr = length(unique(exID)) * 6
person.pp = data.frame(id = rep(1:169, each = 6), 
                       outcome = rep(1:6, length(unique(exID))),
                       mt.low = rep(NA, nr), 
                       mt.high = rep(NA, nr), 
                       pct.low = rep(NA, nr),
                       pct.high = rep(NA, nr),
                       obs = rep(NA, nr))

for(ii in 1:length(unique(exID))){
  id = unique(exID)[ii]
  mt.preds = t(apply(mt.predictions[,which(exID ==id)], 1, tabulate))
  low_ind = id*6-5
  high_ind = low_ind + 5
  person.pp$mt.low[low_ind:high_ind] = apply(mt.preds, 2, quantile.025)
  person.pp$mt.high[low_ind:high_ind] = apply(mt.preds, 2, quantile.975)
  pct.preds = t(apply(pct.predictions[,which(exID ==id)], 1, tabulate))
  person.pp$pct.low[low_ind:high_ind] = apply(pct.preds, 2, quantile.025)
  person.pp$pct.high[low_ind:high_ind] = apply(pct.preds, 2, quantile.975)
  person.pp$obs[low_ind:high_ind] = tabulate(obs_resp[which(exID == id)])
}

#save(person.pp, file = "data/person_pp_df.RData")
load("data/person_pp_df.RData")
pdf(file="../thesis-doc/figures/pdf/pp-check-person.pdf",width=6,height=3)
person.pp %>%
  gather(., key = "parameter", value = value, -c(id, outcome, obs)) %>%
  separate(., parameter, c("model", "bound")) %>%
  spread(., bound, value) %>%
  mutate(model_name = ifelse(model == "mt", "Hypothesized Tree", "Ordered Tree"),
         outcome_name = factor(case_when(
           outcome == 1 ~ "No Value",
           outcome == 2 ~ "Individ.",
           outcome == 3 ~ "Close",
           outcome == 4 ~ "Insuff.",
           outcome == 5 ~ "No Ov.",
           outcome == 6 ~ "Exclusion"),
           levels = c("No Value", "Individ.", "Close","Insuff.", "No Ov.", "Exclusion")
         )) %>%
  ggplot(., aes(x=obs, ymin = low , ymax = high , fill = model, col = model)) +
  geom_linerange(alpha = .7) +
  facet_grid(cols = vars(outcome_name), rows = vars(model_name)) +
  geom_abline(slope = 1, intercept = 0, col = 'gray', linetype = "dashed") +
  scale_color_viridis_d(
    name = "Model:",
    breaks = c("mt", "pct"),
    labels = c("Binary Tree", "Ordered Tree"),
    end = .75) +
  doc_theme +
  theme(legend.position = 'none')+
  labs(
    x = "Observed Score",
    y = "Predicted Score",
    title = "95% Posterior Predictive Intervals"
  )
dev.off()