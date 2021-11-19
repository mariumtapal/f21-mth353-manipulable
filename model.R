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

species <- c(
  "ATGCTTAGC",
  "GTACCCAAT"
)

stresses <- c(
  "heat",
  "light",
  "food"
)

new_dna <- function(specie, stress = NA) {
  if (!is.na(stress)) {
    
    # set seed for sampling
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
    specie_split <- strsplit(specie, "")[[1]]
    
    # for (i in specie_split) {
    #  new <- str_replace(specie_split, specie_split, sample(c("A", "T", "G", "C"), 1))
    # }
    
    # we are assuming each nucleotide represents a substring, so really, it is moving around substrings
    # we can see how many remain in the same position?
    new <- sample(permn(specie_split), 1) %>%
      unlist() %>%
      paste(collapse = "")
  }
  
  # if na, no change
  if (is.na(stress)) {new <- specie}
  
  return(new)
}

change_in_dna <- function(specie, stress = NA) {
  new <- new_dna(specie = specie, stress = "light")
  old_split <- strsplit(specie, "")[[1]]
  new_split <- strsplit(new, "")[[1]]

  # the position of the matches
  i <- which(old_split == new_split)
  return(i)
  
  
}
