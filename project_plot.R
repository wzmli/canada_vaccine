library(shellpipes)
library(tidyverse);theme_set(theme_bw())

startGraphics(width=15,height=12)

commandEnvironments()

projectdat <- rdsRead()

print(projectdat)

print(gg_pop
	+ geom_line(data=projectdat, aes(x=date,y=vaxPop), color="black", lty="dashed")
	+ geom_line(data=projectdat, aes(x=date,y=secondVaxPop), color="blue", lty="dashed")
	+ geom_vline(aes(xintercept=as.Date("2021-07-18")))
	+ scale_x_date(date_labels="%b", date_breaks="1 month")
	+ facet_wrap(~province, scale="free",ncol=2)
)

