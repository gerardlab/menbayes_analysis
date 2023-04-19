nc = 6
rexec = R CMD BATCH --no-save --no-restore

.PHONY : all
all : blue

# Blueberry analysis ----
.PHONY : blue
blue : ./output/blue/bluefits.RDS

./output/blue/bluefits.RDS : ./code/blue_up.R ./data/updog_input_240ind_Sweet_Indi.Rdata
	mkdir -p ./output/rout
	mkdir -p ./output/blue
	$(rexec) '--args nc=$(nc)' $< ./output/rout/$(basename $(notdir $<)).Rout
