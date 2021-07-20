library(shellpipes)
library(tidyverse);theme_set(theme_bw())

startGraphics(width=15,height=10)

commandEnvironments()

projectdat <- rdsRead()

## print(projectdat)

gg_proj <- (gg_pop
	+ geom_line(data=projectdat, aes(x=date,y=vaxPop), color="black", lty="dashed")
	+ geom_line(data=projectdat, aes(x=date,y=secondVaxPop), color="blue", lty="dashed")
	+ geom_vline(aes(xintercept=as.Date("2021-07-18")))
	+ scale_x_date(date_labels="%b", date_breaks="1 month")
	+ facet_wrap(~province, scale="free",ncol=3)
)

print(gg_proj)

spreadDat <- (cum_dat
	%>% spread(key=population,value=count)
)

print(spreadDat)

print(projectdat)

outputdat <- (projectdat
	%>% select(-c(firstJabs, secondJabs, jabs))
	%>% bind_rows(spreadDat)
	%>% arrange(province,date)
	%>% rename(at_least_1_dose = vaxPop
		, double_dose = secondVaxPop
		, population = pop
		, eligible_population = eli_pop 
	)
	%>% mutate(type = ifelse(date <= as.Date("2021-07-18"), "data", "projection")
		, unvaccinated_population = population - at_least_1_dose
		, partially_dose = at_least_1_dose - double_dose
	)
)


write.csv(outputdat,"vaccine_projection.csv")

