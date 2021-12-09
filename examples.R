# load packages
library(combinat)
library(tidyverse)


# source function from other script
source("model.R")


#------------------
# base example from section 2.3: The function, what it does and how to run it
find_matches(specie = "ATCGGTAT", stress = "heat")


#------------------
# example 1 from section 3.1: Specific DNAs with specific stresses

# vector of DNA species of different lengths
species <- c(
  "ATGCTTAGC",
  "GTACCCAAT",
  "GTACGTAAT",
  "GTATAATA",
  "GTACC",
  "TACGGTA",
  "GATCC",
  "ATCG",
  "ATTTGCA",
  "ATCG"
)

# vector of stresses to applied to each dna
stresses <- c(
  "heat",
  "light",
  "light",
  "food",
  "heat",
  "light",
  "food",
  NA,
  "light",
  "heat"
)

# example 1: apply function to each row of data
df1 <- map2_dfr(species, stresses, find_matches)
df1

# summary table (for internal calculations)
summary1 <- df1 %>%
  group_by(specie, stress) %>%
  filter(!is.na(stress)) %>% 
  summarise(count_changes = n()) %>%
  mutate(
    total_nucleotides = nchar(specie),
    prob_changes = (total_nucleotides - count_changes) / total_nucleotides
  ) %>%
  arrange(desc(prob_changes))

# boxplot1
ggplot(summary1, aes(x = prob_changes)) +
  geom_boxplot() +
  facet_wrap(~stress) +
  labs(
    x = "Proportion of the DNA string that mutates",
    title = "What proportion of the DNA string mutates by stresss?",
  ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


#------------------
# example 2 from section 3.2: Many DNAs with all 3 stresses

# test all species in vector with all stresses
# initiate empty tibble
df2 <- tibble(
  specie = character(),
  new = character(),
  stress = character(),
  position = integer()
)

# loop and map find_matches()
for (specie in species) {
  df2 <- map2_dfr(specie, c("food", "heat", "light"), find_matches) %>%
    bind_rows(df2)
}

# summary table (for internal calculations)
summary2 <- df2 %>%
  group_by(specie, stress) %>%
  summarise(count_changes = n()) %>%
  mutate(
    total_nucleotides = nchar(specie),
    prob_changes = (total_nucleotides - count_changes) / total_nucleotides
  ) %>%
  arrange(desc(prob_changes))

# boxplot2
ggplot(summary2, aes(x = prob_changes)) +
  geom_boxplot() +
  facet_wrap(~stress) +
  labs(
    x = "Proportion of the DNA string that mutates",
    title = "What proportion of the DNA string mutates by stresss?",
  ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# summary table - avg percentage change by stress ~ about the same!
summary2 %>%
  select(stress, prob_changes) %>%
  group_by(stress) %>%
  summarise(mean = mean(prob_changes))
