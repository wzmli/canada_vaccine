library(shellpipes)
library(tidyverse);theme_set(theme_bw())

commandEnvironments()

projectdat <- rdsRead()

print(projectdat)

print(gg_pop
	+ geom_line(data=projectdat, aes(x=date,y=vaxPop), color="black", lty="dashed")
	+ geom_line(data=projectdat, aes(x=date,y=secondVaxPop), color="blue", lty="dashed")
)

