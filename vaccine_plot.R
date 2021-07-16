library(shellpipes)
library(tidyverse);theme_set(theme_bw())
startGraphics()

start_date <- as.Date("2020-12-01")

dd <- (bind_rows(rdsRead())
	%>% group_by(province)
	%>% mutate(second_dose_prop = total_vaccinated/total_vaccinations
		, daily_vaccination = diff(c(0,total_vaccinations))
		, daily_fully_vacc = diff(c(0,total_vaccinated))
		, daily_second_dose_prop = daily_fully_vacc/daily_vaccination
		, date = as.Date(date)
	)
	%>% filter(date > start_date)
	%>% filter(between(daily_second_dose_prop,0,1))
)

print(dd)

gg <- (ggplot(dd, aes(x=date,y=daily_second_dose_prop))
	+ geom_point()
	+ facet_wrap(~province,nrow=6)
	+ scale_x_date(date_breaks="1 month", date_labels="%b")
)

print(gg)


gg_provinces <- (gg %+% filter(dd, !(province %in% c("yt","nu","nt"))))

print(gg_provinces)

