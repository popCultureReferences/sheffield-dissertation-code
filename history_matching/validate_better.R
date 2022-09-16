`%>%` <- magrittr::`%>%`
models <- readr::read_rds("~/talys/wave3/named_models.rds")
test_runs <- readr::read_csv(
  "wave3test_1.csv") %>%
  dplyr::bind_rows(readr::read_csv("wave3test_2.csv") )
required_names <- readr::read_rds(
  "~/talys/wave1_export/required_names.rds")
# Create a df with a row for every prediction
test_long <- test_runs %>% tidyr::pivot_longer(
  required_names$predictants,
  names_to="Energy", values_to = "sim_xs") %>%
  dplyr::mutate(Energy = as.numeric(Energy)) %>%
  dplyr::mutate(ident = paste(Reaction, Energy, sep="-") ) %>%
  tidyr::drop_na()


predictions <- purrr::imap(models, function(model,ident){
  logging::loginfo("Computing Mahalanobis distance for %s", ident)
  rxn <- strsplit(ident,split="-")[[1]][1]
  energy <- strsplit(ident,split="-")[[1]][2]
  # pick out the relevant entries from the data
  # get the points in X at which to predict
  test_predictors <- test_long %>%
    dplyr::filter(Reaction == rxn & Energy == energy) %>%
    dplyr::select(required_names$predictors)
  test_predictors <- log(test_predictors, base=10)
  logging::loginfo("Predicting for %s",ident)
  p <-  predict(model, test_predictors, type = "UK", cov.compute=T)
  logging::loginfo("Succes, gathering data for %s",ident)
  list( mean = p$mean, cov = p$cov,
        n = model@n, m = nrow(test_predictors),
        q = model@p )
})
readr::write_rds(predictions, "test_predictions.rds")

mahal_distances <- purrr::imap_dfr(predictions, function(p, ident){
  rxn <- strsplit(ident,split="-")[[1]][1]
  energy <- strsplit(ident,split="-")[[1]][2]
  logging::loginfo("Computing Mahalanobis distances for %s",ident)
  y_star <- test_long %>%
    dplyr::filter(Reaction == rxn & Energy == energy) %>%
    dplyr::pull(sim_xs)
  tryCatch(
    {mahal = stats::mahalanobis(t(y_star), p$mean, p$cov)
    scaled = (p$n  - p$q)/(p$m*(p$n - p$q - 2))*mahal
    prob = pf(scaled, p$m, (p$n - p$q), lower.tail = T   )
    logging::loginfo("Prob = %s", prob)
    dplyr::bind_cols(ident = ident,
                     mahal = mahal, scaled = scaled, prob = prob)},
    error = function(e){
      logging::loginfo("Model failure: %s ", ident)
      dplyr::bind_cols(ident =NA, mahal=NA, scaled=NA, prob=NA) } )

} )

valid_models <- mahal_distances %>%tidyr::drop_na()%>%
  dplyr::filter(prob >.001 & prob<.999)

valid_models %>% dplyr::pull(scaled) %>%
  hist(freq=F, ylim=c(0,5.1))
x<- seq(0,3,length.out=100)
m <- predictions$`(n,a)-5.5`$m
n <- predictions$`(n,a)-5.5`$n
q <- predictions$`(n,a)-5.5`$q
y <- df(x,df1=m,df2 = (n-q) )
lines(x,y)
plot(x,y)

readr::write_rds(valid_models, "valid_model_idents_wave3.rds")

