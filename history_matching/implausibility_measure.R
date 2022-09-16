library(magrittr)
valid_models <- readr::read_rds("valid_models_wave2_part2.rds")
models <- readr::read_rds("dice_models_wave2.rds")
observations <- readr::read_rds(
  "~/Maths/Sheffield/dissertation/data/expDt.rda") %>%
  dplyr::filter(L1 <=10 & L1 >=5)
required_names <- readr::read_rds(
  "~/Maths/Sheffield/dissertation/data/required_names.rds")
# Name the observations as the models
observations$REAC <- dplyr::recode_factor(
  observations$REAC,
  "(26-FE-56(N,A)24-CR-53,,SIG)" = "(n,a)",
  "(26-FE-56(N,INL)26-FE-56,,SIG)" = "(n,n')",
  "(26-FE-56(N,P)25-MN-56,,SIG)" = "(n,p)",
  "(26-FE-56(N,TOT),,SIG)" = "(n,tot)" )
# Gener
observations <- observations %>%
  dplyr::filter(REAC %in% c("(n,a)", "(n,n)", "(n,p)", "(n,tot)")) %>%
  dplyr::select(c("REAC", "L1", "DATA")) %>%
  dplyr::rename("Reaction" = REAC,
                "Energy" = L1,
                "xs" = DATA) %>%
  dplyr::mutate(ident = paste(Reaction, Energy, sep = " "),
             #   xs = log(xs),
                obs_uncertainty = .05*xs,
                model_inad = .05*xs)
# named list of models with names in the style "(n,g) 5.0958"
# This is a shit show
valid <- purrr::pmap(valid_models %>% dplyr::select(reaction, energy), ~{
  n <- paste(.x,.y, sep = " ")
  m <-   list(purrr::chuck(models, .x, .y) )
  names(m) <- n
  m }) %>% purrr::flatten()
# pull out the subset of observations for which we have a model
obs_subset <- observations %>% dplyr::filter(ident %in% names(valid)   )
valid_subset <- valid[names(valid) %in% obs_subset$ident]
# drop some of the big, unneeded objects from memory
rm(models, valid_models, valid)
# generate a massive grid on which to predict
# each column is a 28-vector
logging::loginfo("Start: %s", Sys.time() )
samples <- matrix(runif(1e+6, min=-1, max=1),nrow=28)
# compute implausibility measures
impls <- parallel::mclapply( asplit(samples,2), function(sample){
 i <- purrr::imap(valid_subset, ~{
    logging::loginfo("Computing plausibility measure for %s", .y)
    p <- predict(.x, t(sample), type = "UK", cov.compute=F)
    obs <- obs_subset %>% dplyr::filter(ident == .y) %>%
      dplyr::slice_head(n=1)
    obs_unc <- obs %>% dplyr::pull(obs_uncertainty)
    m_inad <- obs %>% dplyr::pull(model_inad)
    z <- obs %>% dplyr::pull(xs)
    abs(z - p$mean) / sqrt(p$sd^2 + obs_unc^2 + m_inad^2   )
  }) %>% unlist() %>% sort()
 list(sample,i)
},mc.cores=8) 
readr::write_rds(impls, "wave2_implausibility_measures_part2.rds")
logging::loginfo("End: %s", Sys.time() )




