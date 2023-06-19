nc = 6
rexec = R CMD BATCH --no-save --no-restore

simsout = ./output/sims/null_sims_g.csv \
          ./output/sims/null_sims_gl.csv \
          ./output/sims/alt_sims_g.RDS \
          ./output/sims/alt_sims_gl.RDS

.PHONY : all
all : blue sims

# Blueberry analysis ----
.PHONY : blue
blue : ./output/blue/blue_df.csv

./output/blue/bluefits.RDS : ./code/blue_up.R ./data/updog_input_240ind_Sweet_Indi.Rdata
	mkdir -p ./output/rout
	mkdir -p ./output/blue
	$(rexec) '--args nc=$(nc)' $< ./output/rout/$(basename $(notdir $<)).Rout

./output/blue/blue_df.csv : ./code/blue_test.R ./output/blue/bluefits.RDS
	mkdir -p ./output/rout
	mkdir -p ./output/blue
	$(rexec) '--args nc=$(nc)' $< ./output/rout/$(basename $(notdir $<)).Rout

# Sims ----
.PHONY : sims
sims : $(simsout)

./output/sims/null_sims_g.csv : ./code/null_sims_g.R
	mkdir -p ./output/rout
	mkdir -p ./output/sims
	$(rexec) $< ./output/rout/$(basename $(notdir $<)).Rout

./output/sims/null_sims_gl.csv : ./code/null_sims_gl.R
	mkdir -p ./output/rout
	mkdir -p ./output/sims
	$(rexec) '--args nc=$(nc)' $< ./output/rout/$(basename $(notdir $<)).Rout

./output/sims/alt_sims_g.RDS : ./code/alt_sims_g.R
	mkdir -p ./output/rout
	mkdir -p ./output/sims
	$(rexec) $< ./output/rout/$(basename $(notdir $<)).Rout

./output/sims/alt_sims_gl.RDS : ./code/alt_sims_gl.R
	mkdir -p ./output/rout
	mkdir -p ./output/sims
	$(rexec) $< ./output/rout/$(basename $(notdir $<)).Rout
