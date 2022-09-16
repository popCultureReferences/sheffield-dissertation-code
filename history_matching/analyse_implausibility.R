library(magrittr)
imp <- readr::read_rds("wave2_implausibility_measures_part2.rds")
required <- readr::read_rds(
  "~/Maths/Sheffield/dissertation/data/required_names.rds")

impl2_df <- purrr::map_dfr(imp, ~{
  vars <- unlist(.x[1])
  names(vars) <- required$predictors
  measures <- .x[2] %>%
    unlist() %>% sort()
  I2 <- measures[length(measures)-1]
  cbind(t(vars),I2) %>% tibble::as_tibble()
})

df <- purrr::map_dfr(imp, ~{
  vars <- unlist(.x[1])
  names(vars) <- required$predictors
  i2 <-( .x[2] %>%
           unlist()  )^2 %>% sum()
  cbind(t(vars),i2) %>% tibble::as_tibble()
})

# this is I2 measure
implausibility_measures <- purrr::map(imp, ~{
  measures <- .x[2] %>%
    unlist() %>% sort()
  measures[length(measures)-1]
  #  if (i<3) {i}
  #  else {NULL}
}) %>% unlist()

chi_squared <- purrr::map(imp, ~{
  (   .x[2] %>%
        unlist()  )^2 %>% sum()
}) %>% unlist()

inputs <- purrr::map(imp,~{
  .x[1] %>% unlist()
})

plausible_inputs <- inputs[chi_squared < qchisq(.99, 464) &
                             implausibility_measures < 3 ]

plausible_df <- purrr::map_dfr(plausible_inputs, ~{
  t(.x) %>% tibble::as_tibble()
})
names(plausible_df) <- required$predictors
readr::write_rds(plausible_df,"plausibles_wave2.rds")
