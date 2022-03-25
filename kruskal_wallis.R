library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)
library(broom)

PlantGrowth <- read.csv('C:/Users/Administrator/Ai/R/inferential/KRUSKAL_WALLIS/kruskal_Wallis.csv')
PlantGrowth

############################### Summary statistics
PlantGrowth %>% 
  group_by(group) %>%
  get_summary_stats(weight, type = "common")

############################### Visualization
ggboxplot(PlantGrowth, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "weight", xlab = "group")


################################# Computation
res.kruskal <- PlantGrowth %>% kruskal_test(weight ~ group)
res.kruskal

################################# Post-hoc tests
# Pairwise comparisons
pwc <- PlantGrowth %>% tukey_hsd(weight ~ group)
pwc

################################# Effect size
PlantGrowth %>% kruskal_effsize(weight ~ group)

################################# Multiple pairwise-comparisons
# Pairwise comparisons using Dunn's test:
pwc <- PlantGrowth %>% 
  dunn_test(weight ~ group, p.adjust.method = "bonferroni") 
pwc

# Pairwise comparisons using Wilcoxon's test:
pwc2 <- PlantGrowth %>% 
  wilcox_test(weight ~ group, p.adjust.method = "bonferroni")
pwc2

################################# Report
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "group")
ggboxplot(PlantGrowth, x = "group", y = "weight",
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07")) +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.kruskal, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

