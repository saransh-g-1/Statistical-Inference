library(tidyverse)
library(patchwork)
library(magick)

num_replications <- 250






find_test_stat <- function(m, n, theta_vec,
                           theta_names, density_fun, ...){
  
  expectations <- readRDS("D:\\Rohan\\Maths\\MStat\\Semester 2\\Non parametric\\Project\\squared_expectations.RData")[[(m+n-1)]]
  data_x <- lapply(theta_vec, 
                   FUN = function(x) density_fun(n, scale_param = x, ...))
  names(data_x) <- theta_names
  
  
  data_y <- density_fun(m, scale_param = 1, ...)
  
  x_ranks_vec <- lapply(data_x, 
                        FUN = function(x) {
                          rank(c(x, data_y), ties.method = "random")[1:n] 
                        })
  
  capon_test_stat_value <- map_dbl(x_ranks_vec, ~ sum(expectations[.]))
  
  klotz_test_stat_value <- map_dbl(x_ranks_vec,~ sum((qnorm(./(m+n+1)))^2))
  
  mood_test_stat_value <- map_dbl(x_ranks_vec, ~ sum((. - 0.5 * (n+m+1))^2))
  
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

gamma_dist <- function(n, scale_param, shape = 2, ...){
  rgamma(n, scale = scale_param, shape = shape, ...)
}


# Create a list of the distribution functions

distributions <- list(norm_dist,
                      exp_dist,
                      cauchy_dist,
                      logistic_dist,
                      gamma_dist)

names(distributions) <- c("norm_dist", "exp_dist", "cauchy_dist", "logistic_dist", "gamma_dist")


inputs <- expand_grid(
  nm_pair = tibble(n = c(5,9,19,26, 70, 150), m = c(6,8,12,31, 40, 190)),
  dist = c("norm_dist", "exp_dist", "cauchy_dist", "logistic_dist", "gamma_dist")
)

inputs <- as.matrix(inputs)
colnames(inputs)<- c("n", "m", "dist")
inputs <- as_tibble(inputs) %>%
  mutate(n = str_squish(n) %>% as.double(),
         m = str_squish(m) %>% as.double())

inputs <- as.data.frame(inputs)
rownames(inputs) <- str_c(rep("n=", 30), 
                          as.character(inputs$n),
                          rep("m=", 30), 
                          as.character(inputs$m),
                          inputs$dist,
                          sep = " ")


run_simulation <- function(n, m, dist) {
  density_fun <- distributions[[dist]]
  theta_vec <- seq(1, 2.5, length.out = 50)
  theta_names <- as.character(theta_vec)
  result <- replicate(num_replications, find_test_stat(n, m, theta_vec,
                                                       theta_names, density_fun),
                      simplify = F)
  print(paste(n, ",", m, ",", dist))
  return(result)
}

# run the simulations
results <- pmap(inputs, run_simulation)


saveRDS(results,
        file = "D:\\Rohan\\Maths\\MStat\\Semester 2\\Non parametric\\Project\\results2.RData")


#############################################################################


null_dist_capon_df <- sapply(1:(num_replications), FUN = function(x){
  sapply(1:30, function(y) results[[y]][[x]][[1]][1])}) %>%
  as_tibble() %>%
  mutate(nm = rep(c("n = 5, m = 6", "n = 9, m = 8", 
                    "n = 19, m = 12", "n = 26, m = 31", "n = 70, m = 40", 
                    "n = 150, m = 190"), each = 5),
         distr = rep(c("norm_dist", "exp_dist", "cauchy_dist", "logistic_dist", 
                       "gamma_dist"), times = 6)) %>%
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



capon_dist_free <- (dist_free_plots[[1]] + dist_free_plots[[2]])/(dist_free_plots[[3]] + dist_free_plots[[4]])/(dist_free_plots[[5]] + dist_free_plots[[6]])

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


############################

library(magick)

dir_out <- file.path(tempdir(), "capon_theta_vary")
dir.create(dir_out, recursive = TRUE)

for(j in 1:50){
  
  null_dist_capon_df <- sapply(1:(num_replications), FUN = function(x){
    sapply(1:30, function(y) results[[y]][[x]][[1]][j])}) %>%
    as_tibble() %>%
    mutate(nm = rep(c("n = 5, m = 6", "n = 9, m = 8", 
                      "n = 19, m = 12", "n = 26, m = 31", "n = 70, m = 40", 
                      "n = 150, m = 190"), each = 5),
           distr = rep(c("norm_dist", "exp_dist", "cauchy_dist", "logistic_dist", 
                         "gamma_dist"), times = 6)) %>%
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
  
  
  
  capon_dist_free <- (dist_free_plots[[1]] + dist_free_plots[[2]])/(dist_free_plots[[3]] + dist_free_plots[[4]])/(dist_free_plots[[5]]+dist_free_plots[[6]])
  capon_dist_free <- capon_dist_free +
    plot_annotation(title = paste("theta =", 1 + j/20))
  
  
  
  fp <- file.path(dir_out, paste0(letters[j], ".png"))
  
  ggsave(plot = capon_dist_free, 
         filename = fp, 
         width = 18.5, height = 18, units = "cm",
         device = "png")
}


list.files(dir_out, full.names = TRUE) %>%
  lapply(image_read) %>%
  image_join() %>%
  image_animate(fps = 2.5) %>%
  image_write(image = .,
              path = "D:\\Rohan\\Maths\\MStat\\Semester 2\\Non parametric\\Project\\capon_varying_theta.gif")

unlink(dir_out, recursive = T)



#######################



dir_out <- file.path(tempdir(), "klotz_theta_vary")
dir.create(dir_out, recursive = TRUE)

for(j in 1:50){
  
  null_dist_klotz_df <- sapply(1:(num_replications), FUN = function(x){
    sapply(1:30, function(y) results[[y]][[x]][[2]][j])}) %>%
    as_tibble() %>%
    mutate(nm = rep(c("n = 5, m = 6", "n = 9, m = 8", 
                      "n = 19, m = 12", "n = 26, m = 31", "n = 70, m = 40", 
                      "n = 150, m = 190"), each = 5),
           distr = rep(c("norm_dist", "exp_dist", "cauchy_dist", "logistic_dist", 
                         "gamma_dist"), times = 6)) %>%
    select(nm, distr, everything()) %>%
    pivot_longer(cols = starts_with("V"),
                 names_to = "colname",
                 values_to = "test_stat_value") %>%
    select(-colname)
  
  dist_free_plots <- lapply(unique(null_dist_klotz_df$nm), function(name){
    null_dist_klotz_df %>%
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
  
  
  
  klotz_dist_free <- (dist_free_plots[[1]] + dist_free_plots[[2]])/(dist_free_plots[[3]] + dist_free_plots[[4]])/(dist_free_plots[[5]]+dist_free_plots[[6]])
  klotz_dist_free <- klotz_dist_free +
    plot_annotation(title = paste("theta =", 1 + j/20))
  
  
  
  fp <- file.path(dir_out, paste0(letters[j], ".png"))
  
  ggsave(plot = klotz_dist_free, 
         filename = fp, 
         width = 18.5, height = 18, units = "cm",
         device = "png")
}


list.files(dir_out, full.names = TRUE) %>%
  lapply(image_read) %>%
  image_join() %>%
  image_animate(fps = 2) %>%
  image_write(image = .,
              path = "D:\\Rohan\\Maths\\MStat\\Semester 2\\Non parametric\\Project\\klotz_varying_theta.gif")

unlink(dir_out, recursive = T)



#####################


dir_out <- file.path(tempdir(), "mood_theta_vary")
dir.create(dir_out, recursive = TRUE)

for(j in 1:50){
  
  null_dist_mood_df <- sapply(1:(num_replications), FUN = function(x){
    sapply(1:30, function(y) results[[y]][[x]][[3]][j])}) %>%
    as_tibble() %>%
    mutate(nm = rep(c("n = 5, m = 6", "n = 9, m = 8", 
                      "n = 19, m = 12", "n = 26, m = 31", "n = 70, m = 40", 
                      "n = 150, m = 190"), each = 5),
           distr = rep(c("norm_dist", "exp_dist", "cauchy_dist", "logistic_dist", 
                         "gamma_dist"), times = 6)) %>%
    select(nm, distr, everything()) %>%
    pivot_longer(cols = starts_with("V"),
                 names_to = "colname",
                 values_to = "test_stat_value") %>%
    select(-colname)
  
  dist_free_plots <- lapply(unique(null_dist_mood_df$nm), function(name){
    null_dist_mood_df %>%
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
  
  
  
  mood_dist_free <- (dist_free_plots[[1]] + dist_free_plots[[2]])/(dist_free_plots[[3]] + dist_free_plots[[4]])/(dist_free_plots[[5]]+dist_free_plots[[6]])
  mood_dist_free <- mood_dist_free +
    plot_annotation(title = paste("theta =", 1 + j/20))
  
  
  
  fp <- file.path(dir_out, paste0(letters[j], ".png"))
  
  ggsave(plot = mood_dist_free, 
         filename = fp, 
         width = 18.5, height = 18, units = "cm",
         device = "png")
}


list.files(dir_out, full.names = TRUE) %>%
  lapply(image_read) %>%
  image_join() %>%
  image_animate(fps = 2) %>%
  image_write(image = .,
              path = "D:\\Rohan\\Maths\\MStat\\Semester 2\\Non parametric\\Project\\mood_varying_theta.gif")

unlink(dir_out, recursive = T)



###############



dir_out <- file.path(tempdir(), "savage_theta_vary")
dir.create(dir_out, recursive = TRUE)

for(j in 1:50){
  
  null_dist_savage_df <- sapply(1:(num_replications), FUN = function(x){
    sapply(1:30, function(y) results[[y]][[x]][[4]][j])}) %>%
    as_tibble() %>%
    mutate(nm = rep(c("n = 5, m = 6", "n = 9, m = 8", 
                      "n = 19, m = 12", "n = 26, m = 31", "n = 70, m = 40", 
                      "n = 150, m = 190"), each = 5),
           distr = rep(c("norm_dist", "exp_dist", "cauchy_dist", "logistic_dist", 
                         "gamma_dist"), times = 6)) %>%
    select(nm, distr, everything()) %>%
    pivot_longer(cols = starts_with("V"),
                 names_to = "colname",
                 values_to = "test_stat_value") %>%
    select(-colname)
  
  dist_free_plots <- lapply(unique(null_dist_savage_df$nm), function(name){
    null_dist_savage_df %>%
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
  
  
  
  savage_dist_free <- (dist_free_plots[[1]] + dist_free_plots[[2]])/(dist_free_plots[[3]] + dist_free_plots[[4]])/(dist_free_plots[[5]]+dist_free_plots[[6]])
  savage_dist_free <- savage_dist_free +
    plot_annotation(title = paste("theta =", 1 + j/20))
  
  
  
  fp <- file.path(dir_out, paste0(letters[j], ".png"))
  
  ggsave(plot = savage_dist_free, 
         filename = fp, 
         width = 18.5, height = 18, units = "cm",
         device = "png")
}


list.files(dir_out, full.names = TRUE) %>%
  lapply(image_read) %>%
  image_join() %>%
  image_animate(fps = 2) %>%
  image_write(image = .,
              path = "D:\\Rohan\\Maths\\MStat\\Semester 2\\Non parametric\\Project\\savage_varying_theta.gif")

unlink(dir_out, recursive = T)
































############################################################################

theta_vec <- seq(1, 2.5, length.out = 50)
theta_names <- str_c("theta = ", as.character(theta_vec), sep = "")


power_list <- lapply(c(1,6,11,16,21,26)+1, FUN = function(x){
  test_values_null <- sapply(1:(num_replications), FUN = function(y){
    c(results[[x]][[y]][[1]][1],
      results[[x]][[y]][[2]][1],
      results[[x]][[y]][[3]][1],
      results[[x]][[y]][[4]][1])
  })%>%
    t() %>%
    as_tibble() %>%
    rename("capon_test_stat" = `1`,
           "klotz_test_stat" = V2,
           "mood_test_stat" = V3,
           "savage_test_stat" = V4)
  
  cutoffs <- apply(test_values_null, MARGIN = 2, FUN = function(val){
    quantile(val, probs = 0.95)})
  
  power_theta <- sapply(seq_along(theta_vec), FUN = function(z){
      test_values_alternative <- sapply(1:(num_replications), FUN = function(y){
        c(results[[x]][[y]][[1]][z],
          results[[x]][[y]][[2]][z],
          results[[x]][[y]][[3]][z],
          results[[x]][[y]][[4]][z])
      })%>%
        t() 
        colnames(test_values_alternative) <- c("capon_test_stat",
               "klotz_test_stat" ,
               "mood_test_stat" ,
               "savage_test_stat")
      sapply(1:4, FUN = function(alt){
        sum(test_values_alternative[,alt] >= cutoffs[alt])/(num_replications)
      })
      
  })
  
  colnames(power_theta) <- theta_names
  rownames(power_theta) <-c("capon_test_stat",
                             "klotz_test_stat" ,
                             "mood_test_stat" ,
                             "savage_test_stat") 
  return(power_theta %>% t())
  
})



power_curves <- lapply(1:6, FUN = function(x){
  power_list[[x]] %>%
    as_tibble() %>%
    mutate(theta = theta_vec) %>%
    pivot_longer(cols = ends_with("stat"),
                 names_to = "Distribution",
                 values_to = "power") %>%
#    filter(Distribution == "capon_test_stat") %>%
    ggplot(aes(x = theta, y = power, color = Distribution))+
    geom_smooth(se = F, method = "loess")+
    geom_hline(aes(yintercept = 0.05))+
#    xlim(1, 1.5)+
    theme_bw()
    
})
























