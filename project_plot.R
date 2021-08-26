library(shellpipes)
library(tidyverse);theme_set(theme_bw())

startGraphics(width=10,height=8)

commandEnvironments()

projectdat <- rdsRead()


projectdat <- (projectdat
	%>% filter(province %in% c("bc","ab","sk","mb","on","qc"))
	%>% mutate( province2 = province
		, province2 = ifelse(province2 == "bc", "BC", province2)
		, province2 = ifelse(province2 == "ab", "AB", province2)
		, province2 = ifelse(province2 == "sk", "SK", province2)
		, province2 = ifelse(province2 == "mb", "MB", province2)
		, province2 = ifelse(province2 == "on", "ON", province2)
		, province2 = ifelse(province2 == "qc", "QC", province2)
		, province2 = factor(province2,level=c("BC","AB","SK","MB","ON","QC"))
		)

)

gg_proj <- (gg_pop
	+ geom_line(data=projectdat, aes(x=date,y=vaxPop), color="black", lty="dashed")
	+ geom_line(data=projectdat, aes(x=date,y=secondVaxPop), color="blue", lty="dashed")
	+ geom_vline(aes(xintercept=as.Date("2021-08-25")))
	+ scale_x_date(date_labels="%b", date_breaks="1 month")
	+ facet_wrap(~province2, scale="free",ncol=2)
	+ ggtitle("")
)

print(gg_proj)
ggsave("vac_proj.png",width=10,height=8)


print(gg_proj2 <- (gg_pop2
   + geom_line(data=projectdat, aes(x=date,y=vaxPop/pop), color="black", lty="dashed")
   + geom_line(data=projectdat, aes(x=date,y=secondVaxPop/pop), color="blue", lty="dashed")
   + geom_vline(aes(xintercept=as.Date("2021-08-25")))
   + scale_x_date(date_labels="%b", date_breaks="1 month")
   + facet_wrap(~province2, scale="free",ncol=2)
	+ ggtitle("")
))

ggsave("vac_proj_prop.png",width=10,height=8)

spreadDat <- (cum_dat
	%>% spread(key=population,value=count)
)

print(spreadDat)

print(projectdat)

outputdat <- (projectdat
#	%>% select(-c(firstJabs, secondJabs, jabs))
	%>% bind_rows(spreadDat)
	%>% arrange(province,date)
	%>% rename(at_least_1_dose = vaxPop
		, double_dose = secondVaxPop
		, population = pop
		, eligible_population = eli_pop 
	)
	%>% mutate(type = ifelse(date <= as.Date("2021-08-16"), "data", "projection")
		, unvaccinated_population = population - at_least_1_dose
		, partially_dose = at_least_1_dose - double_dose
	)
)
print(outputdat)

write.csv(outputdat,"vaccine_projection.csv")
write.csv(projectdat,"vaccine_projection_only.csv")
