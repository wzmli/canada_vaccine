current: target
-include target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

Sources += $(wildcard *.R)

vaccine_dat.Rout: vaccine_dat.R
	$(pipeR)

vaccine_clean.Rout: vaccine_clean.R vaccine_dat.rds
	$(pipeR)

vaccine_plot.Rout: vaccine_plot.R vaccine_clean.rds
	$(pipeR)

project.Rout: project.R vaccine_plot.rda
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
