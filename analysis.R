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
mod4 <- lm(RPCT ~ NSP1 + ADJ_FORQ*EMPLOYRISK + UNEM10 + MEDHHI + ADJ_FORQ*DENSITY + TENURE + TENURE2 + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, wide)
mod5 <- lm(RPCT ~ NSP1 + ADJ_PRICE07*ANXIETY + ADJ_PRICE07*ADJ_FORQ + ADJ_PRICE07*EMPLOYRISK + UNEM10 + MEDHHI + ADJ_FORQ*DENSITY + TENURE + TENURE2 + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, wide)

#justp
mod6 <- lm(RPCT ~ NSP1 + ADJ_FORQ*EMPLOYRISK + UNEM10 + MEDHHI + ADJ_FORQ*DENSITY + TENURE + TENURE2 + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE, filter(wide, RESULT != "EXTRA-TP"))
mod7 <- lm(RPCT ~ NSP1 + ADJ_PRICE07*ANXIETY + ADJ_PRICE07*ADJ_FORQ + ADJ_PRICE07*EMPLOYRISK + UNEM10 + MEDHHI + ADJ_FORQ*DENSITY + TENURE + TENURE2 + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE, filter(wide, RESULT != "EXTRA-TP"))

#beta regressions
wide <- mutate(wide, ADJ_RPCT = ifelse(RPCT > 99.9, RPCT - .1, ifelse(RPCT < .1, RPCT + .1, RPCT))/100)
mod8 <- betareg(ADJ_RPCT ~ NSP1 + ADJ_PRICE07*ANXIETY + ADJ_PRICE07*ADJ_FORQ + ADJ_PRICE07*EMPLOYRISK + UNEM10 + MEDHHI + ADJ_FORQ*DENSITY + TENURE + TENURE2 + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, wide)
mod9 <- betareg(ADJ_RPCT ~ NSP1 + ADJ_PRICE07*ANXIETY + ADJ_PRICE07*ADJ_FORQ + ADJ_PRICE07*EMPLOYRISK + UNEM10 + MEDHHI + ADJ_FORQ*DENSITY + TENURE + TENURE2 + OLD + VAP_B + VAP_H + VAP_H2 + VAP_W*WHITESOMECOL + EVANRATE + LDSRATE + STATE, filter(wide, RESULT != "EXTRA-TP"))

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