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
mod1 <- lm(RPCT ~ NSP1 + ADJ_PRICE08*ANXIETY + PRICERISK + PRICEQUITY + UNEMCHG + MEDHHI + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, wide)

##justp
mod2 <- lm(RPCT ~ NSP1 + ADJ_PRICE08*ANXIETY + PRICERISK + PRICEQUITY + UNEMCHG + MEDHHI + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, filter(wide, RESULT != "EXTRA-TP"))

#beta regressions
##full-sample
mod3 <- betareg(RPCT ~ NSP1 + ADJ_PRICE08*ANXIETY + PRICERISK + PRICEQUITY + UNEMCHG + MEDHHI + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, wide)

##justp
mod4 <- betareg(RPCT ~ NSP1 + ADJ_PRICE08*ANXIETY + PRICERISK + PRICEQUITY + UNEMCHG + MEDHHI + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, filter(wide, RESULT != "EXTRA-TP"))

#spatial dependence check
spwide <- as_Spatial(st_as_sf(wide, coords = c("LONGITUDE", "LATITUDE"), crs = 4269), IDs = GEOID10) #return to sp object

##create neighbors list
nbwide <- tri2nb(spwide)

##create weights lists
standardized <- nb2listw(nbwide, style = "W", zero.policy = TRUE)
binary <- nb2listw(nbwide, style = "B", zero.policy = TRUE)

##spaital dependence tests on PCTR: all three reject conclusively the idea of no spatial dependence and indicate positive spatial correlation of RPCT
lm.morantest(mod1, standardized, alternative = "two.sided", zero.policy = T)
geary.test(mod1$residuals, binary, zero.policy = T, alternative = "two.sided")

moran.test(mod3$residuals, standardized, alternative = "greater", zero.policy = T)
geary.test(mod3$residuals, binary, alternative = "greater", zero.policy = T)