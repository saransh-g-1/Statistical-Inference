library(tidyverse)




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



distr <- replicate(500, find_test_stat(n = 10, m = 6,
                                           theta_vec = theta_vec,
                                           theta_names = theta_names,
                                           density_fun = function(n, scale_param, ...){
                                             rnorm(n, sd = scale_param, ...)
                                           },
                                           mean = 3))


