# Title: processResults_SIMPLEG_3b_biodiv_elas.R
# Purpose: Get impact sto biodiversity at the WWF ecoregion level

# Initial SIMPLE-G script by: Iman Haqiqi
# Initial date: Aug 2019

# Edited by: Nick Manning 
# Initial edit date: May 2023
# Last edited: April 2024


###

### Test Code ###
# Example data
groups <- rep(c("Group 1", "Group 2", "Group 3", "Group 4"), each=3)
scenarios <- rep(c("Low", "Medium", "High"), times=4)
means <- c(10, 15, 20, 12, 18, 24, 14, 21, 28, 16, 24, 32)
lower_ci <- means - c(2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4)  # Lower bounds of 95% confidence intervals
upper_ci <- means + c(2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4)  # Upper bounds of 95% confidence intervals

data <- data.frame(Group=groups, Scenario=scenarios, Mean=means, Lower=lower_ci, Upper=upper_ci)

# plot 
ggplot(data, aes(x=Group, y=Mean, fill=Scenario)) +
  geom_bar(stat="identity", position=position_dodge(0.9), width=0.7) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0.2, position=position_dodge(0.9)) +
  labs(title="Grouped Barplot with 95% Confidence Intervals",
       x="Group",
       y="Mean Value") +
  theme_minimal() +
  scale_fill_brewer(palette="Set1")

# plot with wider spaces
ggplot(data, aes(x=Group, y=Mean, fill=Scenario)) +
  geom_bar(stat="identity", position=position_dodge(width=0.5), width=0.4) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0.2, position=position_dodge(width=0.5)) +
  labs(title="Grouped Barplot with 95% Confidence Intervals",
       x="Group",
       y="Mean Value") +
  theme_minimal() +
  scale_fill_brewer(palette="Set1")


###



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

df_reg <- rbind(df_l_reg, df_m_reg, df_h_reg)
df_reg$Elas <- factor(df_reg$Elas, levels=c("High", "Med", "Low"))

df_global <- rbind(df_l_global, df_m_global, df_h_global)
df_global$Elas <- factor(df_global$Elas, levels=c("High", "Med", "Low"))


# 2) Group Results -------


# 3) Plot -------
# plot with wider spaces
ggplot(df_reg, aes(x=Taxa, y=Median, fill=Elas)) +
  geom_bar(stat="identity", position=position_dodge(width=0.5), width=0.4) +
  geom_errorbar(aes(ymin=error_low, ymax=error_high), width=0.2, position=position_dodge(width=0.5)) +
  labs(title="Grouped Barplot with 95% Confidence Intervals",
       x="Group",
       y="Mean Value") +
  theme_minimal() +
  scale_fill_brewer(palette="Set1", direction = -1)+
  coord_flip()

ggsave("../Figures/2024-11-15/biodiversity/")

ggplot(df_global, aes(x=Taxa, y=Median, fill=Elas)) +
  geom_bar(stat="identity", position=position_dodge(width=0.5), width=0.4) +
  geom_errorbar(aes(ymin=error_low, ymax=error_high), width=0.2, position=position_dodge(width=0.5)) +
  labs(title="Grouped Barplot with 95% Confidence Intervals",
       x="Group",
       y="Mean Value") +
  theme_minimal() +
  scale_fill_brewer(palette="Set1", direction = -1)+
  coord_flip()
