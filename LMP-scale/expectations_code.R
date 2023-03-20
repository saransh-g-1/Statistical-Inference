library(dplyr)

expectation_square_fun <- function(N){
  rnorm(N*6000, mean = 0, sd = 1) %>%
    matrix(ncol = N) %>%
    apply(MARGIN = 1, FUN = function(x) (sort(x))^2) %>% 
    t() %>%
    apply(MARGIN = 2, FUN = mean)
}


expectations <- purrr::map(1:1000, ~ expectation_square_fun(.))
expectations2 <- expectations[-1]
saveRDS(expectations2,
        file = "D:\\Rohan\\Maths\\MStat\\Semester 2\\Non parametric\\Project\\squared_expectations.RData")
