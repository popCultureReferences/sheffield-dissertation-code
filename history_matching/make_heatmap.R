library(magrittr)
# load in chi squared stats by variable values
df1 <- readr::read_rds("max_i2_impl_wave1.rds")%>%
  dplyr::mutate(plausible = ifelse(I2<3,1,0 ))
df2 <- readr::read_rds("wave2plausibles/max_i2_impl_wave2.rds")%>%
  dplyr::mutate(plausible = ifelse(I2<3,1,0 ))
df3 <- readr::read_rds("plausiblewave3/max_i2_impl_wave3.rds")%>%
  dplyr::mutate(plausible = ifelse(I2<3,1,0 ))

# Pull out the minimum maximum implausibilities
get_implausibilites <- function(v1,v2,df){
  v1 = rlang::sym(v1)
  v2 = rlang::sym(v2)
  x <- seq(-1,1,length.out = 20)
  gap <- (x[2] - x[1])/2
  midpoint_1 <- x[1] + gap
  y <- seq(midpoint_1,-midpoint_1,length.out=19)
  gr <- expand.grid(y,y)
  # Slice up the parameter space and find the minimum
  # implausibility in each interval
  interval_mins <- purrr::pmap_dfr(gr, function(Var1,Var2){
    l1 <- Var1 - gap
    l2 <- Var2 - gap
    u1 <- Var1 + gap
    u2 <- Var2 + gap
    min_i <- df %>% dplyr::filter(!!v1 < u1 & !!v1 > l1 &
                                    !!v2 < u2 & !!v2 > l2) %>%
      dplyr::pull(I2) %>% min()
    tibble::as_tibble(t(c(Var1,Var2,min_i))) %>%
      dplyr::rename(v1 = V1, v2 = V2, "Implausibility"=V3)
  } )
  interval_mins %>%
    ggplot2::ggplot(ggplot2::aes(x=v1,y=v2)) +
    ggplot2::geom_tile(ggplot2::aes(fill=Implausibility ) ) +
    ggplot2::theme_minimal() + ggplot2::scale_fill_viridis_c(direction=-1) +
    ggplot2::labs(fill= "Minimal\nImplausibility",
                  x = v1, y=v2)
}


# Compute optical depths in each interval and plot heatmap
optical_depth_plot <- function(v1,v2,df){
  v1 = rlang::sym(v1)
  v2 = rlang::sym(v2)
  x <- seq(-1,1,length.out = 20)
  gap <- (x[2] - x[1])/2
  midpoint_1 <- x[1] + gap
  y <- seq(midpoint_1,-midpoint_1,length.out=19)
  gr <- expand.grid(y,y)
  optical_depths <- purrr::pmap_dfr(gr, function(Var1,Var2){
    l1 <- Var1 - gap
    l2 <- Var2 - gap
    u1 <- Var1 + gap
    u2 <- Var2 + gap
    depth <- df %>% dplyr::filter(!!v1 < u1 & !!v1 > l1 &
                                    !!v2 < u2 & !!v2 > l2) %>%
      dplyr::summarise(count = dplyr::n(), plausible = sum(plausible)) %>%
      dplyr::mutate(o_depth = plausible/count) %>% dplyr::pull(o_depth)
    tibble::as_tibble(t(c(Var1,Var2,depth))) %>%
      dplyr::rename(v1 = V1, v2 = V2, "Depth"=V3)
  } )
  optical_depths %>%
    ggplot2::ggplot(ggplot2::aes(x=v1,y=v2)) +
    ggplot2::geom_tile(ggplot2::aes(fill=Depth  ) )+
    ggplot2::theme_minimal() + ggplot2::scale_fill_viridis_c(direction=-1,
                                                             option="C") +
    ggplot2::labs(fill= "Optical\nDepth",
                  x = v1, y=v2)

}

waves <- list(`Wave 1` = df1, `Wave 2` = df2, `Wave 3` = df3)
get_implausibilites("rvadjust", "d3adjust", waves)

impl <- purrr::imap_dfr(waves, ~{
  get_implausibilites("rvadjust", "d1adjust", .x,.y)
})
plot_interval_maxes(impl,"rvadjust","d1adjust")

wave1 <- get_implausibilites("rvadjust", "rwadjust",df2, "Wave 2")
plot_interval_maxes(wave1,"rvadjust", "rwadjust")

p1 <- min_impl_plot("rwadjust","rvadjust",df1)
p2 <- optical_depth_plot("rwadjust","rvadjust",df1)
p3 <- min_impl_plot("rwadjust","rvadjust",df2)
p4 <- optical_depth_plot("rwadjust","rvadjust",df2)
p5 <- min_impl_plot("rwadjust","rvadjust",df3)
p6 <- optical_depth_plot("rwadjust","rvadjust",df3)
gridExtra::grid.arrange(p1,p3,p5,p2,p4,p6,ncol=3)
