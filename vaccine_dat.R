library(jsonlite)
library(tidyverse)
library(shellpipes)
library(lubridate)

get_data <- function(province){
	url <- paste0('https://api.covid19tracker.ca/reports/province/'
					, province
					, '?fill_dates=true'
			)
	upd <- fromJSON(url)
	dat <- (upd$data
			%>% mutate(province = province)
			%>% select(province, date,total_vaccinations,total_vaccinated)
		)
	return(dat)
}

provinces <- c("bc","ab","sk","mb","on","qc","nb","ns","pe","nl","yt","nt","nu")

listdat <- lapply(provinces,get_data)

print(listdat)

rdsSave(listdat)

