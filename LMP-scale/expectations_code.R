library(dplyr)

#Enter all required filepaths HERE
working_directory = str_replace_all(getwd(), "[/]", "\\\\")
file_path_squared_expectation = str_c(working_directory, "\\LMP-scale\\squared_expectations.RData")

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
        file = file_path_squared_expectation)
