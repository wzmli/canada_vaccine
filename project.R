library(shellpipes)
library(tidyverse);theme_set(theme_bw())
library(directlabels)
startGraphics()

commandEnvironments()

dd2 <- dd %>% select(date,vaxPop,secondVaxPop)

vac_by_prop <- tail(dd2,1)[rep(1,301),]

rownames(vac_by_prop) <- NULL

for(i in 1:100){
vac_by_prop[i+1,"date"] <- vac_by_prop[i,"date"] + 1
vac_by_prop[i+1,"vaxPop"] <- round((1 - vac_by_prop[i,"vaxPop"]/on_pop)*avg_vac) + vac_by_prop[i,"vaxPop"]
vac_by_prop[i+1,"secondVaxPop"] <- round((1 - vac_by_prop[i,"secondVaxPop"]/on_pop)*avg_vac) + vac_by_prop[i,"secondVaxPop"]
}

print(gg_pop
	+ geom_line(data=vac_by_prop, aes(date,y=vaxPop),color="blue",lty="dashed")
	+ geom_line(data=vac_by_prop, aes(date,y=secondVaxPop),color="black",lty="dashed")
	+ ggtitle("Vaccinate by proportion")
)

vac_by_current <- tail(dd2,1)[rep(1,301),]
rownames(vac_by_current) <- NULL

for(i in 1:100){
vac_by_current[i+1,"date"] <- vac_by_current[i,"date"] + 1
vac_by_current[i+1,"vaxPop"] <- round(avg_vac*(1-0.85)) + vac_by_current[i,"vaxPop"]
vac_by_current[i+1,"secondVaxPop"] <- round(avg_vac*0.85) + vac_by_current[i,"secondVaxPop"]
}


print(gg_pop
   + geom_line(data=vac_by_current, aes(date,y=vaxPop),color="blue",lty="dashed")
   + geom_line(data=vac_by_current, aes(date,y=secondVaxPop),color="black",lty="dashed")
   + ggtitle("Vaccinate by 85% second dose")
	+ ylim(c(NA,15e6))
)


