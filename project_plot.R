rownames(vacc_project) <- NULL

for(i in 1:futureSteps){
vac_by_prop[i+1,"date"] <- vac_by_prop[i,"date"] + 1
vac_by_prop[i+1,"vaxPop"] <- round((1 - vac_by_prop[i,"vaxPop"]/on_pop)*avg_vac) + vac_by_prop[i,"vaxPop"]
vac_by_prop[i+1,"secondVaxPop"] <- round((vac_by_prop[i,"vaxPop"]/on_pop)*avg_vac) + vac_by_prop[i,"secondVaxPop"]
}

print(gg_pop
	+ geom_line(data=vac_by_prop, aes(date,y=vaxPop),color="black",lty="dashed")
	+ geom_line(data=vac_by_prop, aes(date,y=secondVaxPop),color="blue",lty="dashed")
	+ ggtitle("Vaccinate by proportion")
)

vac_by_current <- tail(dd,1)[rep(1,futureSteps),]
rownames(vac_by_current) <- NULL

for(i in 1:futureSteps){
vac_by_current[i+1,"date"] <- vac_by_current[i,"date"] + 1
vac_by_current[i+1,"vaxPop"] <- round(avg_vac*(1-second_dose_prop)) + vac_by_current[i,"vaxPop"]
vac_by_current[i+1,"secondVaxPop"] <- round(avg_vac*second_dose_prop) + vac_by_current[i,"secondVaxPop"]
}


quit()
print(gg_pop
   + geom_line(data=vac_by_current, aes(date,y=vaxPop),color="black",lty="dashed")
   + geom_line(data=vac_by_current, aes(date,y=secondVaxPop),color="blue",lty="dashed")
   + ggtitle(paste0("Vaccinate by ",second_dose_prop,"% second dose"))
	+ ylim(c(NA,15e6))
)



