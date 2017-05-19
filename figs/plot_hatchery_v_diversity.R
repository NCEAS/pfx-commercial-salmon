library(ggplot2)
library(ggsidekick)
library(viridis)

d = readRDS("data-generated/hatchery_diversity.rds")
d$p  <- d$p*0.453592 / 1e6

ggplot(d, aes(p, tot_div, col=year)) +
  geom_point(size=2) +
  xlab("Hatchery harvest (million kg)") +
  ylab("Catch diversity") +
  facet_wrap(~p_fshy, scale="free_x") +
  scale_color_viridis() + 
  theme_sleek() + labs(colour = "Year")
