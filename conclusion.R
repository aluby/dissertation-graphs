
load("data/parameter-ests-from-3-stans.RData")
lqm <- read.table("data/BlackBox_LQMetricValues.txt", header = T, sep = '\t')
fbi_irtree_val <- mutate(fbi, 
              qID = as.integer(Pair_ID)) %>%
  mutate(irtree_b1 = items$ssb1[qID]) %>%
  group_by(Pair_ID) %>%
  summarize(
    irtree_b1 = mean(irtree_b1),
    pct_nv = mean(Latent_Value == "NV")
  )

lqm %>% 
  left_join(., fbi_irtree_val, by = "Pair_ID") %>%
  tbl_df() %>%
  select(LQMetric_EstVCMP, LQMetric_EstVID, irtree_b1, pct_nv) %>%
  pivot_longer(., -pct_nv, names_to = "Quantity", values_to = "Value") %>%
  mutate(Quantity = ifelse(
    Quantity == "irtree_b1", "b[1]~(IRTree)", 
    ifelse(Quantity == "LQMetric_EstVCMP", "Est~VCMP", "Est~VID"))
  ) %>%
  ggplot(., aes(x = Value, y = 1-pct_nv, col = Quantity)) + 
  geom_point(alpha = .4, size = .5) +
  facet_grid(cols = vars(Quantity), scales = "free_x", 
             labeller = label_parsed) +
  doc_theme +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = 12)) + 
  scale_color_viridis_d(end = .75) +
  labs(x = "",
       y = "% Has Value (Observed)") -> lqm_b1_compare

ggsave("../thesis-doc/figures/pdf/lqm-b1-comparison.pdf",
       lqm_b1_compare,
       width = 6, height = 3)
