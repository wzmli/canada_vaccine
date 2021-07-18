library(shellpipes)
library(tidyverse);theme_set(theme_bw())
library(directlabels)
startGraphics()

start_date <- as.Date("2020-12-01")
on_pop <- 14.57e6
eli_pop <- 12.93e6


## Calculating daily dose prop and first dose

dd <- (bind_rows(rdsRead())
#	%>% group_by(province)
	%>% filter(province == "on")
	%>% mutate(second_dose_prop = total_vaccinated/total_vaccinations
		, cum_first_dose = total_vaccinations - total_vaccinated
		, daily_vaccination = diff(c(0,total_vaccinations))
		, daily_fully_vacc = diff(c(0,total_vaccinated))
		, daily_second_dose_prop = daily_fully_vacc/daily_vaccination
		, date = as.Date(date)
	)
	%>% rename(cum_second_dose = total_vaccinated)
	%>% filter(date > start_date)
)

## Looking at the second dose prop for the last couple of days and the average vaccine administered

print(tail(dd$daily_second_dose_prop))
print(avg_vac <- tail(dd$daily_vaccination) %>% mean %>% round)

## daily second dose proportions

ggprop <- (ggplot(dd, aes(date, daily_second_dose_prop))
	+ geom_point()
)

print(ggprop)


cum_dat <- (dd
	%>% select(date, cum_first_dose, cum_second_dose)
	%>% group_by(date)
	%>% pivot_longer(names_to = "dose", values_to = "count",-date)
)


## Cumulative first and second dose with population and eligible population

gg_cum <- (ggplot(cum_dat,aes(date,y=count,color=dose))
	+ geom_line()
	+ scale_color_manual(values=c("blue","black"))
	+ ylab("Cumulative count")
	+ geom_hline(aes(yintercept = on_pop))
	+ geom_hline(aes(yintercept = eli_pop),color="red")
	+ theme(legend.position="bottom")
	+ ggtitle("Black = ON_pop; red = eligible pop")
)

print(gg_cum)

dd2 <- dd %>% select(date,cum_first_dose,cum_second_dose)

vac_by_prop <- tail(dd2,1)[rep(1,301),]

rownames(vac_by_prop) <- NULL

for(i in 1:300){
vac_by_prop[i+1,"date"] <- vac_by_prop[i,"date"] + 1
vac_by_prop[i+1,"cum_first_dose"] <- round((1 - vac_by_prop[i,"cum_first_dose"]/on_pop)*avg_vac) + vac_by_prop[i,"cum_first_dose"]
vac_by_prop[i+1,"cum_second_dose"] <- round((1 - vac_by_prop[i,"cum_second_dose"]/on_pop)*avg_vac) + vac_by_prop[i,"cum_second_dose"]
}

print(gg_cum
	+ geom_line(data=vac_by_prop, aes(date,y=cum_first_dose),color="blue",lty="dashed")
	+ geom_line(data=vac_by_prop, aes(date,y=cum_second_dose),color="black",lty="dashed")
	+ ggtitle("Vaccinate by proportion")
)


vac_by_current <- tail(dd2,1)[rep(1,301),]
rownames(vac_by_current) <- NULL

for(i in 1:300){
vac_by_current[i+1,"date"] <- vac_by_current[i,"date"] + 1
vac_by_current[i+1,"cum_first_dose"] <- round(avg_vac*(1-0.85)) + vac_by_current[i,"cum_first_dose"]
vac_by_current[i+1,"cum_second_dose"] <- round(avg_vac*0.85) + vac_by_current[i,"cum_second_dose"]
}


print(gg_cum
   + geom_line(data=vac_by_current, aes(date,y=cum_first_dose),color="blue",lty="dashed")
   + geom_line(data=vac_by_current, aes(date,y=cum_second_dose),color="black",lty="dashed")
   + ggtitle("Vaccinate by 85% second dose")
	+ ylim(c(NA,15e6))
)

