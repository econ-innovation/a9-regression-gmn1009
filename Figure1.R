# Load necessary libraries
library(tidyverse)
library(haven)

# Read the dataset
data <- read_dta("aer_2016-1574_replication_set.dta")

# Keep relevant columns
data <- data %>%
  select(id, xid, star_id, srce_pmid, srce_pubyear, year, treat, death_year, after_death, nb_pmra_tcoauth, nb_pmra_ycoauth, nb_pmra_ncoauth)

# Calculate stk_nbpmra_tcoauth, stk_nbpmra_ncoauth, and stk_nbpmra_ycoauth
data <- data %>%
  arrange(id, year) %>%
  group_by(id) %>%
  mutate(
    stk_nbpmra_tcoauth = cumsum(nb_pmra_tcoauth),
    stk_nbpmra_ncoauth = cumsum(nb_pmra_ncoauth),
    stk_nbpmra_ycoauth = cumsum(nb_pmra_ycoauth)
  ) %>%
  ungroup() %>%
  filter(year == death_year)

# Reorder treat as factor
data$treat <- factor(data$treat, levels = c(0, 1), labels = c("Control", "Treated"))

# Calculate weight
data <- data %>%
  group_by(xid, treat) %>%
  mutate(weight = 1 / n_distinct(srce_pmid)) %>%
  ungroup()

# Calculate stk_nbpmra_tcoauth sum by treat
stk_nbpmra_tcoauth_summary <- data %>%
  group_by(treat) %>%
  summarise(stk_nbpmra_tcoauth_sum = sum(stk_nbpmra_tcoauth * weight))

# Plot histogram
ggplot(data, aes(x = stk_nbpmra_tcoauth, weight = weight, fill = treat)) +
  geom_histogram(binwidth = 30, position = "identity", alpha = 0.7) +
  facet_wrap(~treat) +
  labs(x = "Baseline Stock of Related Articles in the Field",
       y = "Fraction of Subfields",
       title = "Baseline Stock of Related Articles in a Subfield") +
  scale_x_continuous(breaks = seq(0, 400, by = 100)) +
  scale_y_continuous(breaks = seq(0, 0.15, by = 0.05)) +
  theme_minimal() +
  theme(legend.position = "bottom")
