current: target
-include target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

Sources += $(wildcard *.R)

## Get the latest data (needs to be refreshed sometimes)
vaccine_dat.Rout: vaccine_dat.R
	$(pipeR)

## Clean
vaccine_clean.Rout: vaccine_clean.R vaccine_dat.rds
	$(pipeR)

## Plot raw, cleaned data. Also lazily do a calc in the wrong place
vaccine_plot.Rout: vaccine_plot.R vaccine_clean.rds pop.csv
	$(pipeR)

## Provincial populations
Sources += pop.csv

## Vaccine projections with saturation
project.Rout: project.R pop.csv vaccine_plot.rda
	$(pipeR)

project_plot.Rout: project_plot.R vaccine_plot.rda project.rds
	$(pipeR)

######################################################################

### Makestuff

Sources += Makefile

## Sources += content.mk
## include content.mk

Ignore += makestuff
msrepo = https://github.com/dushoff

Makefile: makestuff/Makefile
makestuff/Makefile:
	git clone $(msrepo)/makestuff
	ls makestuff/Makefile

-include makestuff/os.mk

-include makestuff/pipeR.mk

-include makestuff/git.mk
-include makestuff/visual.mk
