library(ggplot2)
library(ggsidekick)
library(viridis)

d = readRDS("data-generated/hatchery_diversity.rds")

ggplot(d, aes(p/1000000, tot_div, col=year)) +
  geom_point(size=2) + xlab("Hatchery harvest (million pounds)") + ylab("Catch diversity") + facet_wrap(~p_fshy, scale="free_x") +
  scale_color_viridis() + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme_sleek()
# +  geom_segment(aes(x=p_1, y=div_1, xend=p, yend=tot_div),
#    arrow = arrow(length = unit(0.1, "inches")))
