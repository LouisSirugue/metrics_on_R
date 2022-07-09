

write.csv(read.csv('pppub20.csv') %>% 
            filter(A_LFSR == 1 & (ERN_YN == 1 | WAGEOTR == 1) & WSAL_VAL > 0) %>% 
            mutate(Sex = ifelse(A_SEX == 1, "Male", "Female")) %>%
            rename(Earnings = WSAL_VAL) %>%
            select(Sex, Earnings),
          "asec_2020.csv", row.names = F)

asec_2020 <- read.csv('D:/Postgrad/Thèse/asecpub20csv/pppub20.csv') %>% 
  filter(A_LFSR == 1 & (ERN_YN == 1 | WAGEOTR == 1) & WSAL_VAL > 0) %>% 
  mutate(Sex = ifelse(A_SEX == 1, "Male", "Female"),
         Race = case_when(PRDTRACE == 1 ~ "White",
                          PRDTRACE == 2 ~ "Black",
                          PRDTRACE == 4 ~ "Asian",
                          PRDTRACE > 4 | PRDTRACE == 3 ~ "Other")) %>%
  rename(Earnings = WSAL_VAL) %>%
  select(Sex, Earnings, Race)

write.csv(asec_2020, "D:/Postgrad/Thèse/data-analysis-course-gh-pages/data-analysis-course-gh-pages/lecture8/asec_2020.csv", row.names = F)