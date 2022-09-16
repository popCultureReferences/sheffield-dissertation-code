X <- readr::read_rds("plausibles_wave2.rds")
# Se the initial distance as a very small number
max_dist <- 1e-16
winner <- NULL
for (i in 1:1e5){
  logging::loginfo("Iteration %s, min dist = %s", i, min_dist)
  samp <- dplyr::sample_n(X, size=600)
  dist <- min(dist(samp))
  if (dist > max_dist ){
    max_dist <- dist
    winner <- samp
    }
}
samp <- winner %>%
  dplyr::mutate(dplyr::across(everything(), ~10^.x))

leftovers <- dplyr::anti_join(X,samp)
training <- dplyr::sample_n(leftovers,size=60) %>%
  dplyr::mutate(dplyr::across(everything(), ~10^.x))

readr::write_csv(training, "wave3_training.csv")


