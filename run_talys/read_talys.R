library(magrittr)

# iterate over run directories
run_data <- purrr::map(list.dirs(".")[-1],function(run_dir){
  # iterate over xs files
  results <- purrr::map_dfr( list.files(run_dir, pattern = "^xs"), ~{
    # Get rxn type
    talys_out <- file.path(run_dir, .x)
    rxn_string <- readr::read_lines(talys_out, n_max = 1) %>%
      strex::str_after_first(":") %>% 
      stringr::str_split(" ")
    rxn <- rxn_string[[1]][2]
    # Get output
    talys <- readr::read_table(talys_out, skip=4) %>%
      dplyr::select(c("#","E")) %>%
      dplyr::rename(E = "#", xs = "E") %>%
      dplyr::mutate(Reaction = rxn) %>%
      tidyr::pivot_wider(names_from=E, values_from = xs) 
    # get parameter settings
    talys_in <- file.path(run_dir, "talys.in")
    param_vals <- readr::read_table(talys_in, skip = 30, 
                                    n_max = 28, col_names = FALSE) %>%
      dplyr::select(-X2) %>%
      tidyr::pivot_wider(names_from = X1, values_from = X3)
    dplyr::bind_cols(param_vals, talys)
  }   )
}  )

# total xs are different
totals <- purrr::map_dfr( list.dirs(".")[-1], ~{
  path <- file.path(.x, "totalxs.tot")
  logging::loginfo("Reading %s", path)
  total_run <- readr::read_table(path ,
                     skip = 4) %>%
    dplyr::select(c("#", "E")) %>%
    tidyr::pivot_wider(names_from = "#", values_from = "E" ) %>%
    dplyr::mutate(Reaction = "(n,tot)")
  # get parameter settings
  talys_in <- file.path(.x, "talys.in")
  param_vals <- readr::read_table(talys_in, skip = 30, 
                                  n_max = 28, col_names = FALSE) %>%
    dplyr::select(-X2) %>%
    tidyr::pivot_wider(names_from = X1, values_from = X3)
  dplyr::bind_cols(param_vals, total_run)
}   )

# consolidate and save
reaction_data <- dplyr::bind_rows(run_data, totals)
readr::write_csv(reaction_data, "~/Maths/Sheffield/dissertation/data/test_data_1_logscale.csv")
# for convenience, save the predictor, predictant and reaction names
#predictors <- names(param_vals)
#reactions <- unique(reaction_data$Reaction)
#predictants <- talys %>% dplyr::select(-Reaction) %>% names()
#required_names <- list(predictors = predictors,
                    #   predictants = predictants,
                     #  reactions = reactions)
#saveRDS(required_names, "../../data/required_names.rds")
