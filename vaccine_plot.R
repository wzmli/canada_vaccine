library(shellpipes)
library(tidyverse);theme_set(theme_bw())
startGraphics()

## Start with just ON
start_date <- as.Date("2020-12-01")
on_pop <- 14.57e6
on_eli_pop <- 12.93e6

## Calculating daily dose prop and first dose

dd <- (rdsRead()
	%>% filter(province == "on")
)

## Looking at the second dose prop for the last couple of days and the average vaccine administered

print(tail(dd$dailySecondProp))
print(avg_vac <- tail(dd$dailyJabs) %>% mean %>% round)

## daily second dose proportions

print(ggplot(dd, aes(date, dailySecondProp))
	+ geom_point()
)

cum_dat <- (dd
	%>% select(date, vaxPop, secondVaxPop)
	%>% group_by(date)
	%>% pivot_longer(names_to = "pop", values_to = "count",-date)
)

## Cumulative first and second dose with population and eligible population

gg_pop <- (ggplot(cum_dat,aes(date,y=count,color=pop))
	+ geom_line()
	+ scale_color_manual(values=c("blue","black"))
	+ ylab("Cumulative count")
	+ geom_hline(aes(yintercept = on_pop))
	+ geom_hline(aes(yintercept = on_eli_pop),color="red")
	+ theme(legend.position="bottom")
	+ ggtitle("Black = ON_pop; red = eligible pop")
)

print(gg_pop)

saveEnvironment()
