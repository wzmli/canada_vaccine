library(shellpipes)
library(tidyverse);theme_set(theme_bw())
library(directlabels)
startGraphics()

commandEnvironments()

cutDays <- 7
futureSteps <- 101
satScale <- 10
hesitancy = c(0.1, 0.2)

## Simple saturating model

start <- (dd
	%>% select(-province)
	%>% tail(cutDays)
	%>% mutate(across(.fns=mean))
	%>% tail(1)
)

print(start)

vfun <- function(vpop, steps, start, tpop, scale=1){
	stopifnot(tpop > vpop + start*scale)
	maxvacc = (tpop-vpop)*start/(tpop-vpop-start*scale) 
	v <- numeric(steps)
	v[[1]] <- vpop
	for(i in 2:steps){
		pool <- tpop - vpop
		vacc <- pool*maxvacc/(pool+maxvacc*scale)
		v[[i]] <- vpop <- vpop + vacc
	}
	return(v)
}

## The most naive saturating approach
vacc_project <- tibble(NULL
	, date = seq(start$date, length.out=futureSteps, by=1)
	, vaxPop=vfun(
		start$vaxPop, futureSteps, start$dailyJabs - start$dailySecond
		, ((1-hesitancy[[1]])*on_eli_pop)
		, satScale
	)
	, secondVaxPop=vfun(
		start$secondVaxPop, futureSteps, start$dailySecond
		, ((1-hesitancy[[2]])*on_eli_pop)
		, satScale
	)
	, firstJabs = diff(c(NA, vaxPop))
	, secondJabs = diff(c(NA, secondVaxPop))
	, jabs = firstJabs + secondJabs
	, on_eli_pop = on_eli_pop
)

vaccM <- vacc_project %>% select(-date) 
vaccM <- vaccM/1e3
print(vaccM)

rdsSave(vacc_project)

