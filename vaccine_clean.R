library(shellpipes)
library(dplyr)

start_date <- as.Date("2020-12-01")
on_pop <- 14.57e6
eli_pop <- 12.93e6

## NAs in cumulative columns should all be zeroes
dd <- (bind_rows(rdsRead())
	%>% mutate(NULL
		, total_vaccinated = ifelse(is.na(total_vaccinated), 0, total_vaccinated)
		, total_vaccinations = ifelse(is.na(total_vaccinations), 0, total_vaccinations)
	)
)

## Sensible names and simple calcs
dd <- (dd
	%>% group_by(province)
	%>% transmute(date = as.Date(date)
		, secondVaxPop = total_vaccinated
		, vaxPop = total_vaccinations - secondVaxPop
		, dailyJabs = diff(c(0,total_vaccinations))
		, dailySecond = diff(c(0,total_vaccinated))
		, dailySecondProp = dailySecond/dailyJabs
	)
	%>% filter(date > start_date)
	%>% ungroup()
)

## Looking at the second dose prop for the last couple of days and the average vaccine administered

summary(dd)
rdsSave(dd)
