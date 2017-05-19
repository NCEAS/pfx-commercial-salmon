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

# calculate revenue by species-region
s2 = group_by(cfec, p_fshy, year, spec) %>%
  summarize(g = sum(g_earn), region=region[1])
s2 = filter(s2, spec %in% c("COHO", "CHUM", "SOCK", "PINK"))
s = s2

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

div = group_by(s[which(s$year > 0),], year, p_fshy) %>%
  summarize(tot_div = simp.div(g),
    hatch_div = simp.div(g*value),
    wild_div = simp.div(g*(1-value)),
    p = sum(g*value) / sum(g),
    p_pink = g[which(spec=="PINK")]*value[which(spec=="PINK")],
    p_sock = g[which(spec=="SOCK")]*value[which(spec=="PINK")],
    p_chum = g[which(spec=="CHUM")]*value[which(spec=="CHUM")])

library(viridis)

d1 = div[which(div$p_fshy == "S 03E"),]
d1 = select(d1, year, p_fshy, tot_div, p_sock)
d1 = rename(d1, p = p_sock)

d2 = div[which(div$p_fshy == "S 01E"),]
d2 = select(d2, year, p_fshy, tot_div, p_pink)
d2 = rename(d2, p = p_pink)

d3 = div[which(div$p_fshy == "S 01A"),]
d3 = select(d3, year, p_fshy, tot_div, p_chum)
d3 = rename(d3, p = p_chum)

d4 = div[which(div$p_fshy == "S 03A"),]
d4 = select(d4, year, p_fshy, tot_div, p_chum)
d4 = rename(d4, p = p_chum)

d = rbind(d1, d2, d3, d4)
d$species = NA
d$species[which(d$p_fshy=="S 01A")] = "chum"
d$species[which(d$p_fshy=="S 01E")] = "pink"
d$species[which(d$p_fshy=="S 03A")] = "chum"
d$species[which(d$p_fshy=="S 03E")] = "sockeye"

d$p_fshy[which(d$p_fshy=="S 01A")] = "SE - seine"
d$p_fshy[which(d$p_fshy=="S 01E")] = "PWS - seine"
d$p_fshy[which(d$p_fshy=="S 03A")] = "SE - gillnet"
d$p_fshy[which(d$p_fshy=="S 03E")] = "PWS - gillnet"

# add previous year's data for connecting the dots / arrows
d$year_1 = d$year - 1
d$div_1 = d$tot_div[match(paste(d$year_1, d$p_fshy), paste(d$year, d$p_fshy))]
d$p_1 = d$p[match(paste(d$year_1, d$p_fshy), paste(d$year, d$p_fshy))]



saveRDS(d, "data-generated/hatchery_diversity.rds")
