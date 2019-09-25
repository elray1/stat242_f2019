library(gmodels)
library(dplyr)

pop_means <- 1:5
pop_sd <- 1

n_sims <- 1000
n <- 100

results <- data.frame(
  ci_lower_12 = vector("numeric", n_sims),
  ci_lower_13 = vector("numeric", n_sims),
  ci_lower_14 = vector("numeric", n_sims),
  ci_lower_15 = vector("numeric", n_sims),
  ci_lower_23 = vector("numeric", n_sims),
  ci_lower_24 = vector("numeric", n_sims),
  ci_lower_25 = vector("numeric", n_sims),
  ci_lower_34 = vector("numeric", n_sims),
  ci_lower_35 = vector("numeric", n_sims),
  ci_lower_45 = vector("numeric", n_sims),
  ci_upper_12 = vector("numeric", n_sims),
  ci_upper_13 = vector("numeric", n_sims),
  ci_upper_14 = vector("numeric", n_sims),
  ci_upper_15 = vector("numeric", n_sims),
  ci_upper_23 = vector("numeric", n_sims),
  ci_upper_24 = vector("numeric", n_sims),
  ci_upper_25 = vector("numeric", n_sims),
  ci_upper_34 = vector("numeric", n_sims),
  ci_upper_35 = vector("numeric", n_sims),
  ci_upper_45 = vector("numeric", n_sims),
  contains_diff_12 = vector("numeric", n_sims),
  contains_diff_13 = vector("numeric", n_sims),
  contains_diff_14 = vector("numeric", n_sims),
  contains_diff_15 = vector("numeric", n_sims),
  contains_diff_23 = vector("numeric", n_sims),
  contains_diff_24 = vector("numeric", n_sims),
  contains_diff_25 = vector("numeric", n_sims),
  contains_diff_34 = vector("numeric", n_sims),
  contains_diff_35 = vector("numeric", n_sims),
  contains_diff_45 = vector("numeric", n_sims)
)

set.seed(42)

for(i in 1:n_sims) {
  sim_dat <- rbind(
    data.frame(
      group = "1",
      y = rnorm(n, mean = pop_means[1], sd = pop_sd)
    ),
    data.frame(
      group = "2",
      y = rnorm(n, mean = pop_means[2], sd = pop_sd)
    ),
    data.frame(
      group = "3",
      y = rnorm(n, mean = pop_means[3], sd = pop_sd)
    ),
    data.frame(
      group = "4",
      y = rnorm(n, mean = pop_means[4], sd = pop_sd)
    ),
    data.frame(
      group = "5",
      y = rnorm(n, mean = pop_means[5], sd = pop_sd)
    )
  )
  
  lm_fit <- lm(y ~ group, data = sim_dat)
  
  for(ind1 in 1:4) {
    for(ind2 in (ind1+1):5) {
      c_vec <- rep(0, 5)
      c_vec[ind1] <- 1
      c_vec[ind2] <- -1
      
      ci <- fit.contrast(lm_fit, "group", c_vec, conf.int = 0.95)
      ci_lower <- ci[1, 5]
      ci_upper <- ci[1, 6]
      
      true_diff <- pop_means[ind1] - pop_means[ind2]
      results[i, paste0("ci_lower_", ind1, ind2)] <- ci_lower
      results[i, paste0("ci_upper_", ind1, ind2)] <- ci_upper
      results[i, paste0("contains_diff_", ind1, ind2)] <- (ci_lower <= true_diff) && (ci_upper >= true_diff)
    }
  }
}

results <- results %>% mutate(
  all_cis_contain_diff = (contains_diff_12 + contains_diff_13 + contains_diff_14 + contains_diff_15 + contains_diff_23 + contains_diff_24 + contains_diff_25 + contains_diff_34 + contains_diff_35 + contains_diff_45) == 10
)

results %>%
  select(contains_diff_12:all_cis_contain_diff) %>%
  summarize_all(mean)
