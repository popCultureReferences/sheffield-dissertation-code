library(magrittr)
na_posteriors <- readr::read_rds("posteriors/na_posterior.rds")
nn_posteriors <- readr::read_rds("posteriors/nn_posterior.rds")
np_posteriors <- readr::read_rds("posteriors/np_posterior.rds")

na_flat <- purrr::map_dfr(na_posteriors, ~{
  .x
  }) %>%
  tidyr::pivot_longer(cols=dplyr::everything(), names_to = "Parameter",
                      values_to = "value")

nn_flat <- purrr::map_dfr(nn_posteriors, ~{
  .x
  })%>%
  tidyr::pivot_longer(cols=dplyr::everything(), names_to = "Parameter",
                      values_to = "value")

np_flat <- purrr::map_dfr(np_posteriors, ~{
  .x
  }) %>%
  tidyr::pivot_longer(cols=dplyr::everything(), names_to = "Parameter",
                      values_to = "value")


na_flat %>%
  dplyr::filter(abs(value)<1) %>%ggplot2::ggplot() +
  ggplot2::geom_histogram(ggplot2::aes(x=value,fill=Parameter)) +
  ggplot2::facet_wrap(~Parameter,ncol=4) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "None", axis.text.y= ggplot2::element_blank() ) +
  ggplot2::labs(x= "Parameter value (log-scaled)", y= "Probability density")


nn_flat %>% dplyr::filter(abs(value)<1) %>%
  ggplot2::ggplot() +
  ggplot2::geom_histogram(ggplot2::aes(x=value,fill=Parameter)) +
  ggplot2::facet_wrap(~Parameter,ncol=4) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "None")

na_flat %>% dplyr::filter(abs(value)<1) %>%
  ggplot2::ggplot() +
  ggplot2::geom_histogram(ggplot2::aes(x=value,fill=Parameter)) +
  ggplot2::facet_wrap(~Parameter,ncol=4) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "None", axis.text.y= ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(face="bold", size=16),
                 axis.title.x = ggplot2::element_text(face="bold", size=18),
                 axis.title.y = ggplot2::element_text(face="bold", size=18),
                 strip.text.x =  ggplot2::element_text(face="bold", size=18),
                 plot.title = ggplot2::element_text(face="bold", size=20,hjust=.5)
                 ) +
  ggplot2::labs(x= "Parameter value (log-scaled)", y= "Probability density")
