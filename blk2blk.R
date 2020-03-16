#######################
##COREY RUNKEL THESIS##
###CENSUS BLOCK CONV###
######2020-03-09#######
#######################

#libraries
library(tidyverse)
library(vroom)

#download files from census bureau
##get state codes needed
statecodes <- unique(vroom("data/voting/tracts.csv", col_select = "STATE"))
for (i in statecodes[24,1]) {
  url <- paste0("https://www2.census.gov/geo/docs/maps-data/data/rel/t00t10/TAB2000_TAB2010_ST_", i, "_v2.zip")
  temp <- tempfile()
  download.file(url, temp)
  unzip(temp, exdir = "data/voting/foreclosures")
  unlink(temp)
}

##import data
blk2blk <- vroom("data/voting/foreclosures/TAB2000_TAB2010_ST_04_v2.txt", col_types = "cccccddfcccccddfdd") %>%
  filter(STATE_2000 == "00")
for (i in statecodes$STATE) {
  blk2blk <- vroom(paste0("data/voting/foreclosures/TAB2000_TAB2010_ST_", i, "_v2.txt"), col_types = "cccccddfcccccddfdd") %>%
    bind_rows(blk2blk)
}
blk2blk <- mutate(blk2blk, BGO = substring(BLK_2000, 1, 1), BGR = substring(BLK_2010, 1, 1)) #make block groups

#re-aggregate 2010 block groups
blk0 <- select(blk2blk, STATE_2000, COUNTY_2000, TRACT_2000, BLK_2000, BGO, AREALAND_2000) %>%
  unique() %>%
  group_by(STATE_2000, COUNTY_2000, TRACT_2000, BGO) %>%
  summarize(AREALAND_2000 = sum(AREALAND_2000))
tract0 <- group_by(blk0, STATE_2000, COUNTY_2000, TRACT_2000) %>%
  summarize(AREALAND_2000 = sum(AREALAND_2000))

blk1 <- select(blk2blk, STATE_2010, COUNTY_2010, TRACT_2010, BLK_2010, BGR, AREALAND_2010) %>%
  unique() %>%
  group_by(STATE_2010, COUNTY_2010, TRACT_2010, BGR) %>%
  summarize(AREALAND_2010 = sum(AREALAND_2010))
tract1 <- group_by(blk1, STATE_2010, COUNTY_2010, TRACT_2010) %>%
  summarize(AREALAND_2010 = sum(AREALAND_2010))


blk2blk <- group_by(blk2blk, STATE_2000, COUNTY_2000, TRACT_2000, BGO, STATE_2010, COUNTY_2010, TRACT_2010, BGR) %>%
  summarize(AREALAND_INT = sum(AREALAND_INT))
tract2tract <- group_by(blk2blk, STATE_2000, COUNTY_2000, TRACT_2000, STATE_2010, COUNTY_2010, TRACT_2010) %>%
  summarize(AREALAND_INT = sum(AREALAND_INT))

blk2blk <- select(blk2blk, STATE_2000, COUNTY_2000, TRACT_2000, BGO, STATE_2010, COUNTY_2010, TRACT_2010, BGR, AREALAND_INT) %>%
  unique() %>%
  inner_join(blk0) %>%
  inner_join(blk1) %>%
  mutate(OPCT = AREALAND_INT/AREALAND_2000, RPCT = AREALAND_INT/AREALAND_2010)

rm(blk0,blk1)

tract2tract <- unique(tract2tract) %>%
  inner_join(tract0) %>%
  inner_join(tract1) %>%
  mutate(OPCT = AREALAND_INT/AREALAND_2000, RPCT = AREALAND_INT/AREALAND_2010)

rm(tract0,tract1)

#convert NSP files
##import, standardize variables
nsp3 <- read_csv("data/voting/NSP/nsp3.csv") %>%
  separate(SUM090, c("STATE", "COUNTY", NA, "TRACT", NA, "BGO"), sep = c(2, 5, 15, 21, 22), convert = F) %>%
  mutate(HICOST = HC_RATE*(HMDA/100), SDQ = SDQ_RATE*(HMDA/100), FORQ3 = STARTS, MORT3 = HMDA, UNEM05 = UNEM_05, UNEM10 = UNEM_10) %>%
  select(STATE, COUNTY, TRACT, BGO, FORQ3, MORT3, HICOST, SDQ, PRICECHG, UNEM05, UNEM10) %>%
  group_by(STATE, COUNTY, TRACT, BGO) %>%
  mutate_at(vars(PRICECHG, UNEM05, UNEM10), mean) %>%
  group_by(STATE, COUNTY, TRACT, BGO, PRICECHG, UNEM05, UNEM10) %>%
  summarize_all(sum, na.rm = T) %>%
  ungroup() %>%
  rename(STATE_2000 = STATE, COUNTY_2000 = COUNTY, TRACT_2000 = TRACT) %>%
  inner_join(blk2blk) %>%
  mutate_at(vars(FORQ3, MORT3, HICOST, SDQ), funs(./RPCT)) %>%
  group_by(STATE_2010, COUNTY_2010, TRACT_2010) %>%
  mutate_at(vars(UNEM05, UNEM10, PRICECHG), mean, na.rm = T) %>%
  group_by(STATE_2010, COUNTY_2010, TRACT_2010, UNEM05, UNEM10, PRICECHG) %>%
  summarize_at(vars(FORQ3, MORT3, HICOST, SDQ), sum, na.rm = T) %>%
  ungroup() %>%
  mutate_at(vars(FORQ3, HICOST, SDQ), funs(./MORT3)) %>%
  rename(STATE = STATE_2010, COUNTY = COUNTY_2010, TRACT = TRACT_2010) %>%
  select(STATE, COUNTY, TRACT, FORQ3, HICOST, SDQ, UNEM05, UNEM10, SDQ, PRICECHG)
nsp1 <- read_csv("data/voting/NSP/nsp1.csv") %>%
  mutate(FORQ1 = estimated_number_foreclosures, MORT1 = estimated_number_mortgages) %>%
  rename_all(toupper) %>%
  select(STATE, COUNTY, TRACT, FORQ1, MORT1)
nsp <- read_csv("data/voting/NSP/nsp2.csv") %>%
  separate(geoid, c("STATE", "COUNTY", "TRACT"), sep = c(2, 5), convert = F) %>%
  mutate(FORQ2 = fordq_num, MORT2 = num_mort_tract) %>%
  select(STATE, COUNTY, TRACT, FORQ2, MORT2) %>%
  full_join(nsp1) %>%
  rename(STATE_2000 = STATE, COUNTY_2000 = COUNTY, TRACT_2000 = TRACT) %>%
  inner_join(tract2tract) %>%
  mutate(FORQ1 = FORQ1*RPCT, MORT1 = MORT1*RPCT, FORQ2 = FORQ2*RPCT, MORT2 = MORT2*RPCT) %>%
  group_by(STATE_2010, COUNTY_2010, TRACT_2010) %>%
  summarize_at(vars(FORQ1, FORQ2, MORT1, MORT2), sum, na.rm = T) %>%
  ungroup() %>%
  mutate(FORQ1 = FORQ1/MORT1, FORQ2 = FORQ2/MORT2) %>%
  rename(STATE = STATE_2010, COUNTY = COUNTY_2010, TRACT = TRACT_2010) %>%
  select(STATE, COUNTY, TRACT, FORQ1, FORQ2) %>%
  right_join(nsp3)