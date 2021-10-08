library(shellpipes)
library(tidyverse);theme_set(theme_bw())
library(directlabels)
startGraphics()

loadEnvironments()

cutDays <- 7
futureSteps <- 100
satScale <- 1 ## Look into this; should it be different for doses or provinces?

## Specify hesitancy as cumulative and then convert to per-step
hesitancy = c(0.05, 0.07)
hesitancy[[2]] <- 1 - (1-hesitancy[[2]])/(1-hesitancy[[1]])
print(hesitancy)

## Simple saturating model

popdat <- csvRead()

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



print(startdat)



canstart <- (startdat
	%>% group_by(date)
	%>% summarise(secondVaxPop = sum(secondVaxPop,na.rm=TRUE)
		, vaxPop = sum(vaxPop, na.rm=TRUE)
		, dailyJabs = sum(dailyJabs,na.rm=TRUE)
		, dailySecond = sum(dailySecond,na.rm=TRUE)
		, dailySecondProp = mean(dailySecondProp,na.rm=TRUE)
		, pop = sum(pop)
		, eli_pop = sum(eli_pop)
		, province = "Canada"
		, province2 = "Canada"
	)
)

startdat <- rbind(startdat,canstart)

## Return vaccine projections
## vpop, how many people have been vaccinated
## start is current rate of vaccination
## tpop is vaccinatable pool
## Number vaccinated is a softmax-like function of maxvacc and pool
## maxvacc chosen so that we start at start
## scale adjusts how quickly we can use up the last bunch of people;
## scale = 1 should be very fast
vfun <- function(vpop, steps, start, tpop, scale=1){
	print(c(vpop=vpop, start=start, tpop=tpop))
	if(length(tpop)==1){
		tpop = rep(tpop, steps)
	}
	stopifnot(length(tpop)==steps)
	pool1 = tpop[[1]]-vpop
	stopifnot(pool1 > start*scale)
	maxvacc = pool1*start/(pool1-start*scale) 
	v <- numeric(steps)
	v[[1]] <- vpop
	for(i in 2:steps){
		pool <- tpop[[i]] - vpop
		vacc <- pool*maxvacc/(pool+maxvacc*scale)
		v[[i]] <- vpop <- vpop + vacc
	}
	return(v)
}

## The most naive saturating approach

provinces <- c("Canada","bc","ab","sk","mb","on","qc","nb","ns","pe","nl","nt","nu","yt")
# provinces <- provinces[1:7]
provinces <- provinces[5]


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
			, ((1-hesitancy[[2]])*vaxPop)
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
projectdat <- (bind_rows(projectList))


print(projectdat)


rdsSave(projectdat)



