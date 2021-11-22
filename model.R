# stresses (what does each do?): heat, light, food given, -- maybe a slider of how much of each is given?

# create a string DNA of the anemones -- we have six of them? or sth

# right before death - change
# some string didn't change -- resistant?

# string length?

# new string after stress

# what's the difference between the strings, how would you go back?

# how many different ways can it be changed?

# permutations of the string?

# how many of certain substrings are in the string

# probability

# maybe visualize it?

# shiny app?
# with buttons for selecting species,

# combinat::permn

library(combinat)
library(tidyverse)

# species <- c(
#   "ATGCTTAGC",
#   "GTACCCAAT",
#   "GTACGTAAT",
#   "GTACGTAATA"
# )
# 
# stresses <- c(
#   "heat",
#   "light",
#   "light",
#   "food"
# )

find_matches <- function(specie, stress = NA) {
  
  #for ()
  if (!is.na(stress)) {
    
    # set seed for sampling - do we even need this?
    if (stress == "heat") {
      set.seed(34524)
    }

    if (stress == "light") {
      set.seed(5432)
    }
    
    if (stress == "food") {
      set.seed(65312)
    }

    # try getting a new permutation
    #specie_split <- strsplit(specie, "")[[1]]
    
    # for (i in specie_split) {
    #  new <- str_replace(specie_split, specie_split, sample(c("A", "T", "G", "C"), 1))
    # }
    
    # we are assuming each nucleotide represents a substring, so really, it is moving around substrings
    # we can see how many remain in the same position?
    old_split <- strsplit(specie, "")[[1]]
    new <- sample(permn(old_split), 1) %>%
      unlist() %>%
      paste(collapse = "")
  }
  
  # if na, no change
  if (is.na(stress)) {new <- specie}
  

  # split into nucleotides  
  new_split <- strsplit(new, "")[[1]]
  
  
  # find the position of the matches
  i <- which(old_split == new_split)
  if (length(i) == 0L) {i <- NA }
  
  # form output df
  out <- data.frame(specie, new, i)
  
  return(out)
}

# # creates a bound df together
# 
# find_matches(species[[1]], stresses[[1]])
# find_matches(species[[2]], stresses[[2]])
# find_matches(species[[3]], stresses[[3]])
# find_matches(species[[4]], stresses[[4]])
# 
# find_matches("GTACGTAATA", "food")

# max length is 10

# stress = "light"
# df <- map2_dfr(species, stress, find_matches)
# # 
# # 
# summary <- df %>% group_by(specie, stress) %>%
#   summarise(count_changes = n()) %>%
#   mutate(total_nucleotides = nchar(specie),
#          prob_changes = count_changes/total_nucleotides) %>%
#   arrange(desc(prob_changes)) 
#   
  #df %>% pivot_wider(values_from = i, names_repair = "unique")
# 
# # bargraph
# ggplot(summary, aes(x=prob_changes)) + 
#   geom_boxplot()
# 
# # box plot
# ggplot(summary, aes(x=specie, y=prob_changes)) + 
#   geom_col() + coord_flip()
# 
# 
# # maybe print both out and visualize it?
# 
# # calculate the probability of which stresses, and to which anemone change the the string the most
# # if 
# 
# 
# # ideally would take in and randomly replace string
# 
# # so func 1, is a simulation of what the lab would give dna trackers
# 
# # func 2 finds match indices
# 
# # func 3 calculates probability of some stresses affecting a set of anemones and 
# # seeing how many changes occur - a bar graph perhaps
# 
# prob <- function(df) {
#   # df has columns
# }