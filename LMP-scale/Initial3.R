library(tidyverse)
library(purrr)
library(patchwork)


num_replications <- 200


find_test_stat <- function(m, n, theta_vec,
                           theta_names, density_fun, ...){
  
  expectations <- readRDS("D:\\Rohan\\Maths\\MStat\\Semester 2\\Non parametric\\Project\\squared_expectations.RData")[[(m+n-1)]]
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
  
  mood_test_stat_value <- sapply(x_ranks_vec,
                                 function(x){
                                   sum((x - 0.5 * (n+m+1))^2)
                                 })
  
  savage_test_stat_value <- sapply(x_ranks_vec, 
                                   function(x){
                                     sapply(1:length(x), 
                                            function(y){
                                              sapply(1:x[y], function(z) (1/(m+n-z+1))) %>%
                                                sum()
                                            }) %>%
                                       sum()
                                   })
  
  return(list(capon_test_stat_value,
              klotz_test_stat_value,
              mood_test_stat_value,
              savage_test_stat_value))
  
}

norm_dist <- function(n, scale_param, ...){
  rnorm(n, sd = scale_param, ...)
}

exp_dist <- function(n, scale_param, ...){
  rexp(n, rate = 1/scale_param, ...)
}

cauchy_dist <- function(n, scale_param, ...){
  rcauchy(n, scale = scale_param, ...)
}

logistic_dist <- function(n, scale_param, ...){
  rlogis(n, scale = scale_param, ...)
}

gamma_dist <- function(n, scale_param, ...){
  rgamma(n, scale = scale_param, shape = 2, ...)
}


# Create a list of the distribution functions

distributions <- list(norm_dist,
                      exp_dist,
                      cauchy_dist,
                      logistic_dist,
                      gamma_dist)

names(distributions) <- c("norm_dist", "exp_dist", "cauchy_dist", "logistic_dist", "gamma_dist")


inputs <- expand_grid(
  nm_pair = tibble(n = c(6,15,25,83,100), m = c(8,13,30,70,100)),
  dist = c("norm_dist", "exp_dist", "cauchy_dist", "logistic_dist", "gamma_dist")
)

inputs <- as.matrix(inputs)
colnames(inputs)<- c("n", "m", "dist")
inputs <- as_tibble(inputs) %>%
  mutate(n = str_squish(n) %>% as.double(),
         m = str_squish(m) %>% as.double())

inputs <- as.data.frame(inputs)
rownames(inputs) <- str_c(rep("n=", 25), 
                          as.character(inputs$n),
                          rep("m=", 25), 
                          as.character(inputs$m),
                          inputs$dist,
                          sep = " ")


run_simulation <- function(n, m, dist) {
  density_fun <- distributions[[dist]]
  theta_vec <- seq(1, 2.1, by = 0.05)
  theta_names <- as.character(theta_vec)
  result <- replicate(num_replications, find_test_stat(n, m, theta_vec,
                                                       theta_names, density_fun),
                      simplify = F)
  return(result)
}

# run the simulations
results <- pmap(inputs, run_simulation)


saveRDS(results,
        file = "D:\\Rohan\\Maths\\MStat\\Semester 2\\Non parametric\\Project\\results.RData")


# Can be read using readRDS() function in R


###################################

null_dist_capon_df <- sapply(1:(num_replications), FUN = function(x){
  sapply(1:25, function(y) results[[y]][[x]][[1]][1])}) %>%
  as_tibble() %>%
  mutate(nm = rep(c("n = 6, m = 8", "n = 15, m = 13", 
                    "n = 25, m = 30", "n = 83, m = 70", "n = 100, m = 100"), each = 5),
         distr = rep(c("norm_dist", "exp_dist", "cauchy_dist", "logistic_dist", 
                       "gamma_dist"), times = 5)) %>%
  select(nm, distr, everything()) %>%
  pivot_longer(cols = starts_with("V"),
               names_to = "colname",
               values_to = "test_stat_value") %>%
  select(-colname)

dist_free_plots <- lapply(unique(null_dist_capon_df$nm), function(name){
  null_dist_capon_df %>%
    filter(nm == name) %>%
    ggplot(aes(x = test_stat_value, color = distr))+
    geom_density(show.legend = F)+
    stat_density(geom = "line", position = "identity")+
    labs(x  = "Capon's Test Statistic Value",
         y = "Density",
         title = paste(name),
         color = "Distribution"
    )+
    scale_color_manual(values = c("black", "green", "steelblue", "yellow", "red")) +
    #  facet_wrap(~nm, ncol = 2 )+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
})  



capon_dist_free <- (dist_free_plots[[1]] + dist_free_plots[[2]])/(dist_free_plots[[3]] + dist_free_plots[[4]])/dist_free_plots[[5]]

ggsave(capon_dist_free, 
       filename = "D:\\Rohan\\Maths\\MStat\\Semester 2\\Non parametric\\Project\\capon_dist_free.jpg",
       units = "cm",
       height = 16, width = 20)




null_dist_klotz_df <- sapply(1:(num_replications), FUN = function(x){
  sapply(1:25, function(y) results[[y]][[x]][[2]][1])}) %>%
  as_tibble() %>%
  mutate(nm = rep(c("n = 6, m = 8", "n = 15, m = 13", 
                    "n = 25, m = 30", "n = 83, m = 70", "n = 100, m = 100"), each = 5),
         distr = rep(c("norm_dist", "exp_dist", "cauchy_dist", "logistic_dist", 
                       "gamma_dist"), times = 5)) %>%
  select(nm, distr, everything()) %>%
  pivot_longer(cols = starts_with("V"),
               names_to = "colname",
               values_to = "test_stat_value") %>%
  select(-colname)

dist_free_klotz_plots <- lapply(unique(null_dist_klotz_df$nm), function(name){
  null_dist_klotz_df %>%
    filter(nm == name) %>%
    ggplot(aes(x = test_stat_value, color = distr))+
    geom_density(show.legend = F)+
    stat_density(geom = "line", position = "identity")+
    labs(x  = "Klotz's Test Statistic Value",
         y = "Density",
         title = paste(name),
         color = "Distribution"
    )+
    scale_color_manual(values = c("black", "green", "steelblue", "yellow", "red")) +
    #  facet_wrap(~nm, ncol = 2 )+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
})  



klotz_dist_free <- (dist_free_klotz_plots[[1]] + dist_free_klotz_plots[[2]])/(dist_free_klotz_plots[[3]] + dist_free_klotz_plots[[4]])/dist_free_klotz_plots[[5]]

ggsave(klotz_dist_free, 
       filename = "D:\\Rohan\\Maths\\MStat\\Semester 2\\Non parametric\\Project\\klotz_dist_free.jpg",
       units = "cm",
       height = 16, width = 20)






