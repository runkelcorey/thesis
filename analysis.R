#######################
##COREY RUNKEL THESIS##
########ANALYSIS#######
######2020-03-16#######
#######################

r <- lm(RPCT ~ FORQ3*DENSITY + PRICECHG + UNEM10 + VAP_B + VAP_NA + EVANRATE + LDSRATE + VAP_H*VAP_H + MEDHHI + OWNEROCC + OLD + WHITESOMECOL, wide)