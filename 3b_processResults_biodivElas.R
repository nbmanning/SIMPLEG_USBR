# Title: 3b_processResults_biodivElas.R
# Purpose: Plot bar plots to visualize the "highest-high" and "lowest-low" values for each taxa across each 
# demand elasticity scenario from SIMPLE-G (low (l), medium (m), or high (h))

# Author: Nick Manning 
# Created on: April 2024
# Last edited: July 2025

# REQUIRES:
## Regional and Global dataframes calculated (manually) across each scenario from '3a_processResults_biodiv.R'

# NEXT:
## Retrieve LL and HH values for each taxa from df

# # # # # # # # # # # # # # # # # # # # # # # # 

rm(list = ls())

library(ggplot2)

# 1) Import Results ------
load("../Results/SIMPLEG-2024-11-15/l/df_reg_sum_l.Rdata")
df_l_reg <- df_reg_sum %>% mutate(Elas = "Low")
load("../Results/SIMPLEG-2024-11-15/l/df_global_sum_l.Rdata")
df_l_global <- df_global_sum %>% mutate(Elas = "Low")


load("../Results/SIMPLEG-2024-11-15/m/df_reg_sum_m.Rdata")
df_m_reg <- df_reg_sum %>% mutate(Elas = "Med")
load("../Results/SIMPLEG-2024-11-15/m/df_global_sum_m.Rdata")
df_m_global <- df_global_sum %>% mutate(Elas = "Med")


load("../Results/SIMPLEG-2024-11-15/h/df_reg_sum_h.Rdata")
df_h_reg <- df_reg_sum %>% mutate(Elas = "High")
load("../Results/SIMPLEG-2024-11-15/h/df_global_sum_h.Rdata")
df_h_global <- df_global_sum %>% mutate(Elas = "High")

# combine lmh scenarios into one df each
df_reg <- rbind(df_l_reg, df_m_reg, df_h_reg)
df_reg$Elas <- factor(df_reg$Elas, levels=c("High", "Med", "Low"))

df_global <- rbind(df_l_global, df_m_global, df_h_global)
df_global$Elas <- factor(df_global$Elas, levels=c("High", "Med", "Low"))

# 2) Plot -------
# plot with wider spaces

## regional ##
ggplot(df_reg, aes(x=Taxa, y=Median, fill=Elas)) +
  geom_bar(stat="identity", position=position_dodge(width=0.5), width=0.4) +
  geom_errorbar(aes(ymin=error_low, ymax=error_high), width=0.2, position=position_dodge(width=0.5)) +
  labs(title="Regional Extinctions Across Elasticity Scenarios",
       subtitle = "Bars are Error Low/HIgh",
       x = "",#x="Taxa",
       y = ""#y="Median Value"
       ) +
  theme_minimal() +
  scale_fill_brewer(palette="Set1", direction = -1)+
  coord_flip()

ggsave("../Figures/2024-11-15/biodiversity/bar_reg_elas.png")

## global ##
ggplot(df_global, aes(x=Taxa, y=Median, fill=Elas)) +
  geom_bar(stat="identity", position=position_dodge(width=0.5), width=0.4) +
  geom_errorbar(aes(ymin=error_low, ymax=error_high), width=0.2, position=position_dodge(width=0.5)) +
  labs(title="Global Extinctions Across Elasticity Scenarios",
       subtitle = "Bars are 95% Confidence Intervals",
       x="Group",
       y="Mean Value") +
  theme_minimal() +
  scale_fill_brewer(palette="Set1", direction = -1)+
  coord_flip()

ggsave("../Figures/2024-11-15/biodiversity/bar_global_elas.png")