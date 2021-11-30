find_matches <- function(specie, stress = NA) {

  # for ()
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

    # we are assuming each nucleotide represents a substring, so really, it is moving around substrings
    # we can see how many remain in the same position?
    old_split <- strsplit(specie, "")[[1]]
    new <- sample(permn(old_split), 1) %>%
      unlist() %>%
      paste(collapse = "")
    # split into nucleotides
    new_split <- strsplit(new, "")[[1]]
    # find the position of the matches
    i <- which(old_split == new_split)
    if (length(i) == 0L) {
      i <- NA
    }
  }

  # if na, no change
  if (is.na(stress)) {
    new <- specie
    i <- NA
  }


  # form output df
  out <- data.frame(specie, new, stress, i)

  return(out)
}
