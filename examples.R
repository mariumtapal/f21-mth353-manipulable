# load packages
library(combinat)
library(tidyverse)


# source function from other script
source("model.R")


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

# merge vectors in a dataset
data <- tibble(
  specie = species,
  stress = stresses
)


# example 1: apply function to each row of data
df1 <- map2_dfr(data$specie, data$stress, find_matches)

# summary table
summary1 <- df1 %>%
  group_by(specie, stress) %>%
  summarise(count_changes = n()) %>%
  mutate(
    total_nucleotides = nchar(specie),
    prob_changes = count_changes / total_nucleotides
  ) %>%
  arrange(desc(prob_changes))

# boxplot
ggplot(summary1, aes(x = prob_changes)) +
  geom_boxplot() +
  facet_wrap(~stress) +
  labs(
    x = "Probability of Nuceleotide Change",
    title = "Distribution of Nucleotide Changes by Stress"
  )


# try one specie with different stresses
df2 <- map2_dfr("GTACGTAATA", c("food", "heat", "light"), find_matches)
summary2 <- df2 %>%
  group_by(specie, stress) %>%
  summarise(count_changes = n()) %>%
  mutate(
    total_nucleotides = nchar(specie),
    prob_changes = count_changes / total_nucleotides
  ) %>%
  arrange(desc(prob_changes))

ggplot(summary, aes(x = prob_changes)) +
  geom_boxplot()

ggplot(summary2, aes(x = stress, y = prob_changes, fill = stress)) +
  geom_col()


# test all species in vector with all stresses
# initiate empty tibble
df3 <- tibble(
  specie = character(),
  new = character(),
  stress = character(),
  i = integer()
)

for (specie in species) {
  df3 <- map2_dfr(specie, c("food", "heat", "light"), find_matches) %>%
    bind_rows(df3)
}

summary3 <- df3 %>%
  group_by(specie, stress) %>%
  summarise(count_changes = n()) %>%
  mutate(
    total_nucleotides = nchar(specie),
    prob_changes = count_changes / total_nucleotides
  ) %>%
  arrange(desc(prob_changes))

ggplot(summary3, aes(x = prob_changes)) +
  geom_boxplot() +
  facet_wrap(~stress)

ggplot(summary3, aes(x = specie, y = prob_changes, fill = stress)) +
  geom_col(position = "dodge") +
  coord_flip()

# avg percentage change by stress ~ about the same!
summary3 %>%
  select(stress, prob_changes) %>%
  group_by(stress) %>%
  summarise(mean = mean(prob_changes))

# in the real world this number would be different because we would use real DNA
# and mutated DNA but this is a way to visualize the changes by stress