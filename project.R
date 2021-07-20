library(shellpipes)
library(tidyverse);theme_set(theme_bw())
library(directlabels)
startGraphics()

commandEnvironments()

cutDays <- 7
futureSteps <- 150
satScale <- 20
hesitancy = c(0.1, 0.15)

## Simple saturating model

popdat <- read_csv("pop.csv")

startdat <- (dd
	%>% group_by(province)
	%>% filter(date >= (max(date)-7))
	%>% mutate(across(.fns=mean)
		, dailySecondProp = ifelse(is.na(dailySecondProp),dailySecond/dailyJabs,dailySecondProp)
#		, date = max(dd$date)
		)
	%>% distinct()
	%>% left_join(popdat)
)

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


provinces <- c("bc","ab","sk","mb","on","qc","nb","ns","pe","nl","yt","nt","nu")
provinces <- provinces[1:10]

vacproject <- function(pp){

start <- startdat %>% filter(province == pp) 

vacc_project <- tibble(NULL
	, date = seq(start$date, length.out=futureSteps, by=1)
	, province = pp
	, vaxPop=vfun(
		start$vaxPop, futureSteps, start$dailyJabs - start$dailySecond
		, ((1-hesitancy[[1]])*start$eli_pop)
		, satScale
	)
	, secondVaxPop=vfun(
		start$secondVaxPop, futureSteps, start$dailySecond
		, ((1-hesitancy[[2]])*start$eli_pop)
		, satScale
	)
	, firstJabs = diff(c(NA, vaxPop))
	, secondJabs = diff(c(NA, secondVaxPop))
	, jabs = firstJabs + secondJabs
	, eli_pop = start$eli_pop
	, pop = start$pop

)

return(vacc_project)

}

projectList <- lapply(provinces,vacproject)
projectdat <- (bind_rows(projectList)
)

rdsSave(projectdat)



