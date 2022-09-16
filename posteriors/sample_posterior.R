library(magrittr)
valid_model_idents <- readr::read_rds("valid_models_wave3.rds")
models <- readr::read_rds("~/talys/wave3/named_models.rds")
idents <- valid_model_idents$ident
# pull out the validated models
valid_models  <- models[idents]
# Read in data and add idnetifier column that maps to model names
data <- readr::read_rds("~/talys/wave3/combined_wave3_data.rds") %>%
  dplyr::select(Reaction,Energy,obs_uncertainty, model_inad,xs) %>%
  dplyr::mutate(ident = paste0(Reaction, "-", Energy) ) %>%
  dplyr::distinct(ident, .keep_all = T)
# Change this to plausibles wave 3
plausibles <- readr::read_rds("plausibles_wave3.rds")
# Compute sample mean and covariance matrix of plausibles
proposal_mean <- purrr::map(plausibles, mean) %>% unlist()
proposal_sigma <- cov(plausibles)
# generate a big list of proposal samples
samples <- MASS::mvrnorm(n=5, mu = proposal_mean, Sigma = proposal_sigma)
# Compute the probabilities of the samples wrt the proposal density
probabilities <- mvtnorm::dmvnorm(
  x = samples,mean = proposal_mean, sigma = proposal_sigma,
  log=T)


preds <- purrr::map(asplit(samples,1), function(sample){
# Generate a mean pred and variance for each model
  purrr::imap_dfr(valid_models, function(model,ident){
  logging::loginfo("Computing  likelihood for %s", ident)
  # use model name to identify relevant
  # simulator inadequacy/ observation uncertainty and
  # experimental observation
  relevant_data <- data %>%
    dplyr::filter(ident == !!ident)
  obs_unc <- relevant_data %>% dplyr::pull(obs_uncertainty)
  model_inad <- relevant_data %>% dplyr::pull(model_inad)
  observed <- relevant_data %>% dplyr::pull(xs)
  # generate predictions based on proposal sample input
  logging::loginfo("Predicting with %s model",ident)
  p <- predict(model, t(sample),type="UK", cov.compute=F)
  tibble::tibble(mean = p$mean, var = p$sd^2 + obs_unc^2 + model_inad^2,
                 observed = observed)
}) })

likelihoods <- purrr::map(asplit(samples,1), function(sample){
  # Compute likelihood of the data given the proposal sample
  likl_mean <- preds$mean
  likl_cov <- diag(preds$var)
  obs <- preds$observed
    prob <-  mvtnorm::dmvnorm( x = obs ,mean = likl_mean,
                               sigma = likl_cov, log=T)
  }   ) %>% unlist()

# probs are logged weights are L(x)/p(x)
weights <- exp(likelihoods - probabilities)

posterior_samples <- sample(samples, size=10, prob=weights  )
readr::write_rds(posterior_samples, "posterior_samples.rds")
