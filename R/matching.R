library(magrittr)
N <- 4
sum(sample(N) == 1:N)
replicate(1000, sum(sample(N) == 1:N)) %>% 
  table %>%
  proportions * 24
#> 9/24, 8/24, 6/24, 1/24 에 가까워짐을 관찰
N <- 5
sum(sample(N) == 1:N)
replicate(1000, sum(sample(N) == 1:N)) %>% 
  table %>%
  proportions * 120
