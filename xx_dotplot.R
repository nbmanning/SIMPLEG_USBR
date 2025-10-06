

rm(list = ls())
library(readxl)
library(dplyr)
library(ggplot2)


# Define file paths
file_l <- "../Results/SIMPLEG-2024-11-15/l/_regional_aggregate_l.xlsx"
file_m <- "../Results/SIMPLEG-2024-11-15/m/_regional_aggregate_m.xlsx"
file_h <- "../Results/SIMPLEG-2024-11-15/h/_regional_aggregate_h.xlsx"

# Read in the data
df_l <- read_excel(file_l) 
df_m <- read_excel(file_m) #%>% mutate(modeltype = "m")
df_h <- read_excel(file_h) #%>% mutate(modeltype = "h")

# Combine all scenarios
df_all <- bind_rows(df_l, df_m, df_h)
df <- df_all %>% 
  filter(
    variable == "Soy Area" & region_abv == "US" |
      variable == "Soy Production" & region_abv == "US" |
      variable == "Soy Exp Price index" & region_abv == "US" |
      variable == "Soy Exp" & region_abv == "US" |
      variable == "Soy Exp Price index" & region_abv == "Total" |
      variable == "Soy Production" & region_abv == "Brazil" |
      variable == "Soy Area" & region_abv == "Brazil" |
      variable == "Soy Area" & region_abv == "Cerrado" 
  )

# Pivot to wide format for plotting
df_wide <- df %>%
  select(region_abv, variable, modeltype, pct_chg) %>%
  tidyr::pivot_wider(names_from = modeltype, values_from = pct_chg)

# Create a label for y-axis
df_wide <- df_wide %>%
  mutate(label = paste(region_abv, variable, sep = " - ")) 

df_wide <- df_wide %>% 
  mutate(label = case_when(
    label == "US - Soy Area" ~ "US Soy Area",
    label == "US - Soy Production" ~ "US Soy Production",
    label == "US - Soy Exp" ~ "US Soy Exports",
    label == "US - Soy Exp Price index" ~ "US Soy Prices",
    label == "Total - Soy Exp Price index" ~ "Global Soy Prices",
    label == "Brazil - Soy Area" ~ "Brazil Soy Area",
    label == "Brazil - Soy Production" ~ "Brazil Soy Production",
    label == "Cerrado - Soy Area" ~ "Cerrado Soy Area",
    TRUE ~ label
  ))

df_wide <- df_wide %>% 
  mutate(label = factor(label, levels =
                          c("US Soy Area", "US Soy Production",
                            "US Soy Exports", "US Soy Prices",
                            "Global Soy Prices",
                            "Brazil Soy Area", "Brazil Soy Production",
                            "Cerrado Soy Area")))

# Plot
ggplot(df_wide, aes(y = label)) +
  #geom_errorbar(aes(xmax = h, xmin = l))+
  #geom_segment(aes(x = l, xend = m, yend = label), color = "#C198E0", linetype = "dotted") +
  geom_segment(aes(x = l, xend = m, yend = label), color = "darkorchid2", linetype = "dotted", linewidth = 1) +
  geom_segment(aes(x = h, xend = m, yend = label), color = "chocolate2", linetype = "dashed", linewidth = 0.8) +
  geom_point(aes(x = m), color = "black", size = 2) +
  scale_y_discrete(limits = rev(levels(df_wide$label)))+
  labs(
    #x = "Percent Change",
    x = "",
    #y = "Region - Variable",
    y = "",
    title = "Percent Change across Low, Medium, and High Elasticity Scenarios"
  ) +
  theme_minimal()+
  theme(legend.position = "bottom")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = "../Figures/2024-11-15/_dotplot2_scenario.png", dpi = 300,
       height = 3.5, width = 7)
