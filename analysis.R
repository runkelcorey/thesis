#######################
##COREY RUNKEL THESIS##
########ANALYSIS#######
######2020-03-16#######
#######################

#libraries
library(sf)
library(betareg)
library(spdep)
library(spatialreg)
set.seed(22903)


#linear regressions: residuals decidedly out-of-whack
##full-sample
mod4 <- lm(RPCT ~ NSP1 + ADJ_FORQ*EMPLOYRISK + UNEMCHG + MEDHHI + ADJ_FORQ*DENSITY + TENURE + TENURE2 + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + PVI08, wide)
mod5 <- lm(RPCT ~ NSP1 + ADJ_FORQ*EMPLOYRISK*ADJ_PRICE07 + UNEMCHG + MEDHHI + ADJ_FORQ*DENSITY + TENURE + TENURE2 + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + PVI08, wide)
##justp
mod6 <- lm(RPCT ~ NSP1 + ADJ_FORQ*EMPLOYRISK + UNEMCHG + MEDHHI + ADJ_FORQ*DENSITY + TENURE + TENURE2 + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + PVI08, filter(wide, RESULT != "EXTRA-TP"))
mod7 <- lm(RPCT ~ NSP1 + ADJ_FORQ*EMPLOYRISK*ADJ_PRICE07 + UNEMCHG + MEDHHI + ADJ_FORQ*DENSITY + TENURE + TENURE2 + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + PVI08, filter(wide, RESULT != "EXTRA-TP"))

#beta regressions
mod8 <- betareg(RPCT ~ NSP1 + ADJ_FORQ*EMPLOYRISK + UNEMCHG + MEDHHI + ADJ_FORQ*DENSITY + TENURE + TENURE2 + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + PVI08, wide)
mod9 <- betareg(RPCT ~ NSP1 + ADJ_FORQ*EMPLOYRISK*ADJ_PRICE07 + UNEMCHG + MEDHHI + ADJ_FORQ*DENSITY + TENURE + TENURE2 + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + PVI08, wide)

#spatial dependence check
spwide <- as_Spatial(st_as_sf(wide, coords = c("LONGITUDE", "LATITUDE"), crs = 4269), IDs = GEOID10) #return to sp object
tpwide <- as_Spatial(st_as_sf(filter(wide, RESULT != "EXTRA-TP"), coords = c("LONGITUDE", "LATITUDE"), crs = 4269), IDs = GEOID10)

##create neighbors list
nbwide <- tri2nb(spwide)
nbwidetp <- tri2nb(tpwide)

##create weights lists
standardized <- nb2listw(nbwide, style = "W", zero.policy = TRUE)
binary <- nb2listw(nbwide, style = "B", zero.policy = TRUE)

standardizedtp <- nb2listw(nbwidetp, style = "W", zero.policy = TRUE)
binarytp <- nb2listw(nbwidetp, style = "B", zero.policy = TRUE)

##spaital dependence tests on PCTR: all three reject conclusively the idea of no spatial dependence and indicate positive spatial correlation of RPCT
lm.morantest(mod7, standardizedtp, alternative = "greater", zero.policy = T)
geary.test(mod7$residuals, binarytp, zero.policy = T, alternative = "greater")

moran.test(mod9$residuals, standardizedtp, alternative = "greater", zero.policy = T)
geary.test(mod9$residuals, binarytp, alternative = "greater", zero.policy = T)