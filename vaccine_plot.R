library(shellpipes)
library(tidyverse);theme_set(theme_bw())
startGraphics()

popdat <- read_csv("pop.csv")

## Start with just ON
start_date <- as.Date("2020-12-01")

## Calculating daily dose prop and first dose

## Looking at the second dose prop for the last couple of days and the average vaccine administered

dd <- rdsRead()

print(ggplot(dd, aes(date, dailySecondProp))
	+ geom_point()
	+ facet_wrap(~province,scale="free")
)

cum_dat <- (dd
	%>% select(date, vaxPop, secondVaxPop,province)
	%>% group_by(date,province)
	%>% pivot_longer(names_to = "population", values_to = "count",-c(date,province))
	%>% left_join(.,popdat)
)

canada_cum <- (cum_dat
	%>% group_by(date, population)
   %>% summarise(count = sum(count,na.rm=TRUE)     
		, pop = sum(pop,na.rm=TRUE)
		, eli_pop = sum(eli_pop,na.rm=TRUE)
		, province = "Canada")
)

print(cum_dat)
print(canada_cum)

cum_dat <- (rbind(cum_dat, canada_cum)
	%>% mutate(province2 = ifelse(province == "Canada","Canada",province2))
	%>% mutate(province2 = factor(province2))
)
# cum_dat <- filter(cum_dat, province %in% c("bc","ab","sk","mb","on","qc"))

print(tail(cum_dat))

## Cumulative first and second dose with population and eligible population

print(cum_dat)

gg_pop <- (ggplot(cum_dat,aes(date,y=count,color=population))
	+ geom_line()
	+ facet_wrap(~province2, scale="free")
	+ scale_color_manual(values=c("blue","black"))
	+ ylab("Cumulative count")
	+ geom_hline(aes(yintercept = pop))
	+ geom_hline(aes(yintercept = eli_pop),color="red")
	+ theme(legend.position="bottom")
	+ ggtitle("Black = pop; red = eligible pop")
)

print(gg_pop)


gg_pop2 <- (ggplot(cum_dat,aes(date,y=count/pop,color=population))
   + geom_line()
   + facet_wrap(~province2, scale="free")
   + scale_color_manual(values=c("blue","black"))
   + ylab("Cumulative count")
   + geom_hline(aes(yintercept = pop/pop))
   + geom_hline(aes(yintercept = eli_pop/pop),color="red")
   + theme(legend.position="bottom")
   + ggtitle("Black = pop; red = eligible pop")
)

print(gg_pop2)


saveEnvironment()
