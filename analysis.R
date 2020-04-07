#######################
##COREY RUNKEL THESIS##
########ANALYSIS#######
######2020-03-16#######
#######################

#libraries
library(sf)
library(tigris)
library(betareg)
library(spdep)
library(spatialreg)
set.seed(22903)


#linear regressions: residuals decidedly out-of-whack
##full-sample
mod1 <- lm(RPCT ~ NSP1 + DISTNSP1 + OWNEROCC*FORQ3 + UNEMCHG + MEDHHI + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, wide)
mod2 <- lm(RPCT ~ NSP1 + DISTNSP1 + PRICECHG0810*OWNEROCC + OWNEROCC*FORQ3 + UNEMCHG + MEDHHI + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, wide)

##justp
mod3 <- lm(RPCT ~ NSP1 + DISTNSP1 + OWNEROCC*FORQ3 + UNEMCHG + MEDHHI + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, filter(wide, RESULT != "EXTRA-TP"))
mod4 <- lm(RPCT ~ NSP1 + DISTNSP1 + PRICECHG0810*OWNEROCC + OWNEROCC*FORQ3 + UNEMCHG + MEDHHI + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, filter(wide, RESULT != "EXTRA-TP"))

  mod5 <- lm(RPCT ~ NSP1 + DISTNSP1 + OWNEROCC*FORQ3 + UNEMCHG + MEDHHI + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, filter(wide, RESULT == "WIN"))
  mod6 <- lm(RPCT ~ NSP1 + DISTNSP1 + PRICECHG0810*OWNEROCC + OWNEROCC*FORQ3 + UNEMCHG + MEDHHI + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, filter(wide, RESULT == "WIN"))

  mod7 <- lm(RPCT ~ NSP1 + DISTNSP1 + OWNEROCC*FORQ3 + UNEMCHG + MEDHHI + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, filter(wide, RESULT == "LOSE"))
  mod8 <- lm(RPCT ~ NSP1 + DISTNSP1 + PRICECHG0810*OWNEROCC + OWNEROCC*FORQ3 + UNEMCHG + MEDHHI + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, filter(wide, RESULT == "LOSE"))

#beta regressions
wide <- mutate(wide, RPCT = ifelse(RPCT > 99.9, RPCT - .1, ifelse(RPCT < .1, RPCT + .1, RPCT))/100)

mod9 <- betareg(RPCT ~ NSP1 + DISTNSP1 + OWNEROCC*FORQ3 + UNEMCHG + MEDHHI + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, wide)
mod10 <- betareg(RPCT ~ NSP1 + DISTNSP1 + PRICECHG0810*OWNEROCC + OWNEROCC*FORQ3 + UNEMCHG + MEDHHI + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, wide)

mod11 <- betareg(RPCT ~ NSP1 + DISTNSP1 + OWNEROCC*FORQ3 + UNEMCHG + MEDHHI + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, filter(wide, RESULT != "EXTRA-TP"))
mod12 <- betareg(RPCT ~ NSP1 + DISTNSP1 + PRICECHG0810*OWNEROCC + OWNEROCC*FORQ3 + UNEMCHG + MEDHHI + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, filter(wide, RESULT != "EXTRA-TP"))

  mod13 <- betareg(RPCT ~ NSP1 + DISTNSP1 + OWNEROCC*FORQ3 + UNEMCHG + MEDHHI + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, filter(wide, RESULT == "LOSE"))
  mod14 <- betareg(RPCT ~ NSP1 + DISTNSP1 + PRICECHG0810*OWNEROCC + OWNEROCC*FORQ3 + UNEMCHG + MEDHHI + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, filter(wide, RESULT == "LOSE"))

  mod15 <- betareg(RPCT ~ NSP1 + DISTNSP1 + OWNEROCC*FORQ3 + UNEMCHG + MEDHHI + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, filter(wide, RESULT == "WIN"))
  mod16 <- betareg(RPCT ~ NSP1 + DISTNSP1 + PRICECHG0810*OWNEROCC + OWNEROCC*FORQ3 + UNEMCHG + MEDHHI + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, filter(wide, RESULT == "WIN"))


#spatial dependence check
spwide <- as_Spatial(st_as_sf(wide, coords = c("LONGITUDE", "LATITUDE"), crs = 4269), IDs = GEOID10) #return to sp object

##create neighbors list
nbwide <- tri2nb(spwide, row.names = spwide$GEOID10)

##create weights lists
standardized <- nb2listw(nbwide, style = "W", zero.policy = TRUE)
binary <- nb2listw(nbwide, style = "B", zero.policy = TRUE)

##spaital dependence tests on PCTR: all three reject conclusively the idea of no spatial dependence and indicate positive spatial correlation of RPCT
lm.morantest(mod1, standardized, alternative = "two.sided", zero.policy = T)
geary.test(mod1$residuals, binary, zero.policy = T, alternative = "two.sided")