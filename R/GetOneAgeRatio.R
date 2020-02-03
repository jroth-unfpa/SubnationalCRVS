GetOneAgeRatio <- function(vec_ages,
                         vec_counts) {
  n_age_groups <- length(vec_ages)
  vec_age_ratios <- rep(NA, n_age_groups)
  for (v in 1:n_age_groups) {
    one_age <- vec_ages[v]
    one_idx <- which(vec_ages == one_age)
    if (length(one_idx) != 0) {
      if (one_idx != 1 & one_idx != n_age_groups) {
        vec_age_ratios[one_idx] <- 2 * vec_counts[one_idx] / 
          (vec_counts[(one_idx - 1)] + vec_counts[(one_idx + 1)])
      } else {
        vec_age_ratios[one_idx] <- NA
      }
    } else {
      vec_age_ratios[one_idx] <- NA
    }
  }
  return(100*vec_age_ratios)
}
