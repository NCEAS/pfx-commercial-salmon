# process hatchery releases from ADFG

d = read.csv("data/HR097216.csv")

library(dplyr)
library(ggplot2)

d = filter(d, Species %in% c("CHINOOK","COHO","PINK","CHUM","SOCKEYE"))

d = d[-which(is.na(d$Release.Total)),]
d = d[-which(is.na(d$Site.Region)),]

s = group_by(d, Species, Year.Released, Region.Released) %>%
  summarize(tot_by_spec = sum(Total.Released)) %>%
  group_by(Species, Year.Released) %>%
  mutate(tot = sum(tot_by_spec), p_by_spec = tot_by_spec/tot) %>%
  filter(!is.na(Region.Released)) %>%
  filter(Year.Released >= 1975)

s$Region = as.character(s$Region.Released)
s$Region[which(s$Region=="1")] = "Southeast"
s$Region[which(s$Region=="2")] = "PWS and Cook Inlet"
s$Region[which(s$Region=="3")] = "Arctic/Yukon/Kuskokwim"
s$Region[which(s$Region=="4")] = "Kodiak"

div = group_by(s, Region, Year.Released) %>%
  summarize(div = sum(p_by_spec^2)) %>%
  ggplot(aes(Year.Released, div)) + facet_wrap(~Region) + geom_line() + xlab("Year released") + ylab("Diversity")


