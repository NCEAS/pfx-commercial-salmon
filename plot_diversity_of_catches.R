# process hatchery releases from ADFG

d = read.csv("data-generated/commonPropertyCommercialCatch.csv")

library(dplyr)
library(ggplot2)

d$tot_harvest = d$Chinook_harvest+d$Sockeye_harvest+d$Pink_harvest+d$Chum_harvest+d$Coho_harvest
d$tot_enhanced = d$Chinook_enhanced+d$Sockeye_enhanced+d$Pink_enhanced+d$Chum_enhanced+d$Coho_enhanced
d$tot_wild = d$tot_harvest - d$tot_enhanced
d$p = d$tot_enhanced/d$tot_harvest

d$Chinook_wild = d$Chinook_harvest-d$Chinook_enhanced
d$Coho_wild = d$Coho_harvest-d$Coho_enhanced
d$Chum_wild = d$Chum_harvest-d$Chum_enhanced
d$Sockeye_wild = d$Sockeye_harvest-d$Sockeye_enhanced
d$Pink_wild = d$Pink_harvest-d$Pink_enhanced

d$div = (d$Chinook_harvest/d$tot_harvest)^2 +
  (d$Coho_harvest/d$tot_harvest)^2 +
  (d$Sockeye_harvest/d$tot_harvest)^2 +
  (d$Chum_harvest/d$tot_harvest)^2 +
  (d$Pink_harvest/d$tot_harvest)^2

d$div_enhanced = (d$Chinook_enhanced/d$tot_enhanced)^2 +
  (d$Coho_enhanced/d$tot_enhanced)^2 +
  (d$Sockeye_enhanced/d$tot_enhanced)^2 +
  (d$Chum_enhanced/d$tot_enhanced)^2 +
  (d$Pink_enhanced/d$tot_enhanced)^2

ggplot(d, aes(Year, 1/div_enhanced)) +
  geom_point(color="red", alpha=d$p) +
  facet_wrap(~Region, scale="free_y") +
  geom_point(aes(Year, 1/div), color="grey")

cfec = readRDS("data-generated/salmon_data_for_paper.rds")

# only use data from these 6 fisheries
cfec = cfec[cfec$p_fshy%in%c("S 01A", "S 01E", "S 01K", "S 03A", "S 03E", "S 01M"),]
cfec$year = as.numeric(substr(cfec$landdate, 1, 4))

cfec$region=NA
cfec$region[which(cfec$p_fshy %in% c("S 01A", "S 03A"))] = "Southeast"
cfec$region[which(cfec$p_fshy %in% c("S 01E", "S 03E"))] = "PWS"
cfec$region[which(cfec$p_fshy %in% c("S 01K"))] = "Kodiak"
cfec$region[which(cfec$p_fshy %in% c("S 01M"))] = "AP"

# calculate revenue by species-region
s = group_by(cfec, region, year, spec) %>%
  summarize(g = sum(g_earn)) %>%
  group_by(region, year)

# merge in the hatchery fraction above
hatch = d[,c("Year","Region")]
names(hatch) = tolower(names(hatch))

hatch$CHNK = d$Chinook_enhanced / d$Chinook_harvest
hatch$CHUM = d$Chum_enhanced / d$Chum_harvest
hatch$COHO = d$Coho_enhanced / d$Coho_harvest
hatch$SOCK = d$Sockeye_enhanced / d$Sockeye_harvest
hatch$PINK = d$Pink_enhanced / d$Pink_harvest

library(reshape2)
aql <- melt(hatch, id.vars = c("year", "region"))
aql = rename(aql, spec = variable)
s = left_join(s, aql)

# now we can calculate expected diversity of total,
# and expected diversity of hatchery catches

simp.div = function(x) {
  1/sum((x/sum(x))^2)
}

s$value[which(is.na(s$value))] = 0

# calculate hatchery and total diversity
div = group_by(s[which(s$year > 0),], year, region) %>%
  summarize(tot_div = simp.div(g),
    hatch_div = simp.div(g*value),
    wild_div = simp.div(g*(1-value)),
    p = sum(g*value) / sum(g))

ggplot(div) +
  facet_wrap(~region) +
  geom_line(aes(year, hatch_div), color="red") +
  geom_line(aes(year, wild_div), color="blue") +
  geom_line(aes(year, tot_div), color="purple") +
  ylab("Diversity")

ggplot(div, aes(year, p)) +
  facet_wrap(~region) +
  geom_point(color="red")
