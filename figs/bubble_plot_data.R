library(dplyr)
dat.annual = readRDS(file="data-generated/salmon-annual-for-modeling.rds")

cfec_all = readRDS("data-generated/salmon_data_for_paper.rds")
people = group_by(cfec_all, p_fshy, year) %>%
  summarize(n = length(unique(p_holder))) %>%
  group_by(p_fshy) %>% summarize(n = mean(n,na.rm=T))
names(people)[which(names(people)=="p_fshy")] = "strategy_permit"
people$strategy_permit = paste0(substr(people$strategy_permit,1,1), substr(people$strategy_permit,3,5))

dat.annual = filter(dat.annual, npermit==1 & substr(strategy_permit,1,1)=="S") %>%
  group_by(strategy_permit) %>%
  mutate(nPeople = length(unique(p_holder))) %>%
  filter(nPeople > 200) %>%
  dplyr::select(-nPeople) %>%
  group_by(strategy_permit, year) %>%
  summarize(meanrev = mean(revenue), meandiv = mean(specDiv),
    meandays = mean(days)) %>%
  arrange(strategy_permit, year) %>%
  group_by(strategy_permit) %>%
  summarize(mean_rev = mean(meanrev,na.rm=T), cv_rev = sd(diff(log(meanrev)),na.rm=T),
    diversity = mean(meandiv,na.rm=T), days = mean(meandays, na.rm=T)) %>% as.data.frame

dat.annual = left_join(dat.annual, people)
dat.annual$gear = substr(dat.annual$strategy_permit,2,3)
dat.annual$gear[which(dat.annual$gear=="01")]="Purse seine"
dat.annual$gear[which(dat.annual$gear=="03")]="Drift gillnet"
dat.annual$gear[which(dat.annual$gear=="04")]="Set gillnet"
dat.annual$gear[which(dat.annual$gear=="05")]="Hand troll"
dat.annual$gear[which(dat.annual$gear=="15")]="Power troll"

dat.annual$price = NA
dat.annual$price[dat.annual$strategy_permit=="S01A"]=250000
dat.annual$price[dat.annual$strategy_permit=="S01E"]=186700
dat.annual$price[dat.annual$strategy_permit=="S01H"]=84800
dat.annual$price[dat.annual$strategy_permit=="S01K"]=40000
dat.annual$price[dat.annual$strategy_permit=="S01L"]=227500
dat.annual$price[dat.annual$strategy_permit=="S01M"]=56900

dat.annual$price[dat.annual$strategy_permit=="S03A"]=88900
dat.annual$price[dat.annual$strategy_permit=="S03E"]=224200
dat.annual$price[dat.annual$strategy_permit=="S03H"]=63500
dat.annual$price[dat.annual$strategy_permit=="S03M"]=119500
dat.annual$price[dat.annual$strategy_permit=="S03T"]=148200
dat.annual$price[dat.annual$strategy_permit=="S03W"]=NA

dat.annual$price[dat.annual$strategy_permit=="S04D"]=88900
dat.annual$price[dat.annual$strategy_permit=="S04H"]=15300
dat.annual$price[dat.annual$strategy_permit=="S04K"]=77000
dat.annual$price[dat.annual$strategy_permit=="S04M"]=55900
dat.annual$price[dat.annual$strategy_permit=="S04T"]=38500
dat.annual$price[dat.annual$strategy_permit=="S04W"]=7300
dat.annual$price[dat.annual$strategy_permit=="S04X"]=5400
dat.annual$price[dat.annual$strategy_permit=="S04Y"]=9900
dat.annual$price[dat.annual$strategy_permit=="S04Z"]=11100

dat.annual$price[dat.annual$strategy_permit=="S05B"]=11000
dat.annual$price[dat.annual$strategy_permit=="S15B"]=38300
