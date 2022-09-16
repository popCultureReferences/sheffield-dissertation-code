library(magrittr)
valid_model_idents <- readr::read_rds("~/talys/wave3/wave3testvalidate/valid_models_wave3.rds")
data <- readr::read_rds("~/talys/wave3/combined_wave3_data.rds") %>%
  tidyr::drop_na() %>%
  dplyr::mutate(ident = paste0(Reaction, "-",Energy)) %>%
  dplyr::select(ident,xs,obs_uncertainty, model_inad)
models <- readr::read_rds("~/talys/wave3/named_models.rds")
idents <- valid_model_idents$ident
# pull out the validated models
valid_models  <- models[idents]

logging::loginfo("Start of implasibility calcs")
# Generate 1e5/28 proposals
samples <- matrix(runif(1e+5, min=-1,max=1),nrow=28)
# Iterate over all the proposals
imp_measures <- parallel::mclapply(asplit(samples,2), function(sample){
  # For each proposal iterate every model and compute implausibility measures
  i <- purrr::imap(valid_models, function(model,ident){
   # logging::loginfo("Computing implausibiltiy for %s",ident)
    tryCatch(
    {p <- predict(model, t(sample), type="UK", cov.compute=F)
    obs <- data %>% dplyr::filter(ident == !!ident) 
    obs_unc <- obs$obs_uncertainty
    m_inad <- obs$model_inad
    z <- obs$xs
    # univariate implausibility
    abs(z-p$mean)/ sqrt(p$sd^2 +obs_unc^2 + m_inad^2)},
  error=function(e){NA} )
  }) %>% unlist() %>% sort()
  list(samples,i)
},mc.cores=16 )
readr::write_rds(imp_measures, "wave3_impl_measures.rds")
logging::loginfo("Implausibiltiy analysis complete")

library(magrittr)
imp <- readr::read_rds("~/talys/wave3/wave3testvalidate/wave3_impl.rds")
measures <- purrr::map(unlist(imp), ~{

  imp[[1]][2] %>% unlist()

})

samps <- matrix(unlist(imp[[1]][1]),nrow=28)
samp_split <- asplit(samps,2)
tagged_list <- purrr::map(1:3572, ~{

  list(unlist(samp_split[.x]), unlist(measures[.x]))
})

readr::write_rds(tagged_list,"wave3_impl.rds")

