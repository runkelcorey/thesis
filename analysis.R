#######################
##COREY RUNKEL THESIS##
########ANALYSIS#######
######2020-03-16#######
#######################

#libraries
library(sf)
library(tigris)
library(spdep)
library(spatialreg)
set.seed(22903)


#naïve regressions: residuals decidedly out-of-whack
r <- lm(RPCT ~ FORQ3*DENSITY + PRICECHG + UNEM10 + VAP_B + VAP_NA + EVANRATE + LDSRATE + VAP_H + VAP_H2 + MEDHHI + OWNEROCC + OLD + WHITESOMECOL + NSP, wide)

#spatial dependence check
spwide <- st_read("data/voting/display/wide.gpkg") %>%
  na.omit() %>%
  st_as_sf() %>%
  as_Spatial(IDs = GEOID10) #return to sp object

##regression
s <- lm(RPCT ~ FORQ3*DENSITY + PRICECHG + UNEM10 + VAP_B + VAP_NA + EVANRATE + LDSRATE + VAP_H + VAP_H2 + MEDHHI + OWNEROCC + OLD + WHITESOMECOL + NUMROUNDS, spwide@data)

##create neighbors list
nbwide <- poly2nb(spwide, row.names = spwide$GEOID10)

##create weights lists
standardized <- nb2listw(nbwide, style = "W", zero.policy = TRUE)
binary <- nb2listw(nbwide, style = "B", zero.policy = TRUE)

##spaital dependence tests on PCTR: all three reject conclusively the idea of no spatial dependence and indicate positive spatial correlation of RPCT
lm.morantest(s, standardized, alternative = "two.sided", zero.policy = T)
geary.test(spwide@data$RPCT, binary, zero.policy = T, alternative = "two.sided")

#spatial regression models
##lag
slag <- lagsarlm(RPCT ~ FORQ3*DENSITY + PRICECHG + UNEM10 + VAP_B + VAP_NA + EVANRATE + LDSRATE + VAP_H + VAP_H2 + MEDHHI + OWNEROCC + OLD + WHITESOMECOL + NUMROUNDS, spwide@data, standardized, type = "lag", zero.policy = TRUE, method = "MC")

#models
mod1 <- lm(RPCT ~ NSP + FORQ3*DENSITY + PRICECHG + VAP_B + EVANRATE + LDSRATE + VAP_H + VAP_H2 + MEDHHI + OWNEROCC + UNEM10 + OLD + WHITESOMECOL*VAP_W + STATE, wide)
mod2 <- lm(RPCT ~ NSP + FORQ3*DENSITY + PRICECHG + VAP_B + EVANRATE + LDSRATE + VAP_H + VAP_H2 + MEDHHI + OWNEROCC + UNEM10 + OLD + WHITESOMECOL*VAP_W, spwide@data)

mod3 <- lm(RPCT ~ NSP + FORQ3*DENSITY + PRICECHG + VAP_B + EVANRATE + LDSRATE + VAP_H + VAP_H2 + MEDHHI + OWNEROCC + UNEM10 + OLD + WHITESOMECOL*VAP_W + STATE, filter(spwide@data, RESULT != "EXTRA-TP"))
mod4 <- lm(RPCT ~ NSP + FORQ3*DENSITY + PRICECHG + VAP_B + EVANRATE + LDSRATE + VAP_H + VAP_H2 + MEDHHI + OWNEROCC + UNEM10 + OLD + WHITESOMECOL*VAP_W, filter(spwide@data, RESULT != "EXTRA-TP"))
mod5 <- lm(RPCT ~ NSP + FORQ3*DENSITY + PRICECHG + VAP_B + EVANRATE + LDSRATE + VAP_H + VAP_H2 + MEDHHI + OWNEROCC + UNEM10 + OLD + WHITESOMECOL*VAP_W, filter(spwide@data, RESULT == "LOSE"))
mod6 <- lm(RPCT ~ NSP + FORQ3*DENSITY + PRICECHG + VAP_B + EVANRATE + LDSRATE + VAP_H + VAP_H2 + MEDHHI + OWNEROCC + UNEM10 + OLD + WHITESOMECOL*VAP_W, filter(spwide@data, RESULT == "WIN"))

spwide@data <- mutate(spwide@data, RPCT = ifelse(RPCT < .05, RPCT + .05, ifelse(RPCT > .95, RPCT - .05, RPCT))/100)
mod7 <- betareg(RPCT ~ NSP + FORQ3*DENSITY + PRICECHG + VAP_B + EVANRATE + LDSRATE + VAP_H + VAP_H2 + MEDHHI + OWNEROCC + UNEM10 + OLD + WHITESOMECOL*VAP_W, spwide@data)
mod8 <- betareg(RPCT ~ NSP + FORQ3*DENSITY + PRICECHG + VAP_B + EVANRATE + LDSRATE + VAP_H + VAP_H2 + MEDHHI + OWNEROCC + UNEM10 + OLD + WHITESOMECOL*VAP_W, dplyr::filter(spwide@data, RESULT != "EXTRA-TP"))
mod9 <- betareg(RPCT ~ NSP + FORQ3*DENSITY + PRICECHG + VAP_B + EVANRATE + LDSRATE + VAP_H + VAP_H2 + MEDHHI + OWNEROCC + UNEM10 + OLD + WHITESOMECOL*VAP_W, filter(spwide@data, RESULT == "LOSE"))
mod10 <- betareg(RPCT ~ NSP + FORQ3*DENSITY + PRICECHG + VAP_B + EVANRATE + LDSRATE + VAP_H + VAP_H2 + MEDHHI + OWNEROCC + UNEM10 + OLD + WHITESOMECOL*VAP_W, filter(spwide@data, RESULT == "WIN"))
