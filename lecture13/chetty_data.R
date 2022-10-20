
library(readxl)
library(tidyverse)

chetty <- read_xls("online_data_tables.xls", sheet = 2, skip = 8) %>%
  rename(child_q = `...1`) %>%
  pivot_longer(-child_q, names_to = "parent_q", values_to = "pct") %>%
  mutate(n = round(pct * 5000))

child_rank <- c()
parent_rank <- c()

for (i in 1:nrow(chetty)) {
  child_rank <- c(child_rank, rep(chetty$child_q[i], chetty$n[i]))
  parent_rank <- c(parent_rank, rep(chetty$parent_q[i], chetty$n[i]))
}

write.csv(tibble(child_rank = as.numeric(child_rank), 
                 parent_rank = as.numeric(parent_rank)),
          "chetty_data.csv")
