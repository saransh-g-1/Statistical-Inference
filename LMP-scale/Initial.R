library(tidyverse)
library(purrr)


find_test_stat <- function(m, n, theta_vec,
                                 theta_names, density_fun, ...){
  
  expectations <- rnorm((m+n)*90000, mean = 0, sd = 1) %>% 
    matrix(ncol = (m+n)) %>%
    apply(MARGIN = 1, FUN = sort) %>% 
    t() %>%
    apply(MARGIN = 1, FUN = function(x) x^2) %>% 
    t() %>%
    apply(MARGIN = 2, FUN = mean)
  
  data_x <- lapply(theta_vec, 
                          FUN = function(x) density_fun(n, scale_param = x, ...))
  names(data_x) <- theta_names
  
  
  data_y <- density_fun(m, scale_param = 1, ...)
  
  x_ranks_vec <- lapply(data_x, 
                        FUN = function(x) {
                          tibble(obs = c(x, data_y)) %>%
                            mutate(ranks = rank(obs, ties.method = "random")) %>%
                            head(length(x)) %>%
                            pull(ranks) 
                        })
  
  capon_test_stat_value <- sapply(x_ranks_vec, 
                            FUN = function(x){
                              expectations[x] %>% sum()
                            })
  
  
  klotz_test_stat_value <- lapply(x_ranks_vec, 
                                  function(x){ 
                                    sapply(1:length(x), function(y) dnorm(x[y]/(m+n+1)))
                                    }) %>%
    lapply(FUN = function(x) x^2)%>%
    sapply(FUN = sum)
  return(list(capon_test_stat_value,
              klotz_test_stat_value))

}

theta_vec <- sapply(seq(1,3, by = 0.1), function(x) 1/x)
theta_names <- as.character(seq(1, 3, by = 0.1))



distr_exp <- replicate(100, find_test_stat(n = 5, m = 8,
                                           theta_vec = 1,
                                           theta_names = "1",
                                           density_fun = function(n, scale_param, ...){
                                             rexp(n, rate = scale_param, ...)
                                           })[[1]],
                   simplify = T)




norm_dist <- function(n, scale_param, ...){
  rnorm(n, sd = scale_param, ...)}
  
exp_dist <- function(n, scale_param, ...){
  rexp(n, rate = 1/scale_param, ...)}
  
cauchy_dist <- function(n, scale_param, ...){
  rcauchy(n, scale = scale_param, ...)
  }

logistic_dist <- function(n, scale_param, ...){
  rlogis(n, scale = scale_param, ...)
}

gamma_dist <- function(n, scale_param, ...){
  rgamma(n, scale = scale_param, ...)
}



df <- map2(
  .x = list(c(6, 8),
            c(15, 13),
            c(25, 30),
            c(81, 70),
            c(100, 100)),
  .y = list(norm_dist,
            exp_dist,
            cauchy_dist,
            logistic_dist,
            gamma_dist),
  ~ replicate(150, find_test_stat(n = .x[1], m = .x[2],
              theta_vec = seq(1, 2.1, by = 0.05),
              theta_names = as.character(seq(1, 2.1, by = 0.05)),
              density_fun = .y))
  )


df_6_8 <- mapply(
  FUN = replicate,
  list(norm_dist,
       exp_dist,
       cauchy_dist,
       logistic_dist,
       gamma_dist),
  MoreArgs = list(150, n = 6, m = 8,
                  theta_vec = seq(1, 2.1, by = 0.05),
                  theta_names = as.character(seq(1, 2.1, by = 0.05)))
  
)
  
  
  
  
  
  
  


