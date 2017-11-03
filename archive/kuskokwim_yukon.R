library(dplyr)
library(ggplot2)

cfec_all = readRDS("data-generated/salmon_data_for_paper.rds")

f = filter(cfec_all, p_fshy %in% c("S 04W", "S 04P", "S 04Y", "S 04T", "S 03T"))

people = group_by(f, p_fshy, year) %>%
  summarize(n = length(unique(p_holder))) %>%
  ggplot(aes(year,n)) + geom_point() + geom_line() +
  facet_wrap(~p_fshy, scale="free_y")

