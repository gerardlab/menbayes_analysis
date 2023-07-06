nc = 6
rexec = R CMD BATCH --no-save --no-restore

simsout = ./output/sims/null_sims_g.csv \
          ./output/sims/null_sims_gl.csv \
          ./output/sims/alt_sims_g.RDS \
          ./output/sims/alt_sims_gl.RDS

chisq_plots = ./output/sims/chisq//ucsq_g_01.pdf \
              ./output/sims/chisq//ucsq_g_02.pdf \
              ./output/sims/chisq//ucsq_g_11.pdf \
              ./output/sims/chisq//ucsq_g_12.pdf \
              ./output/sims/chisq//ucsq_gl_rd10_01.pdf \
              ./output/sims/chisq//ucsq_gl_rd10_02.pdf \
              ./output/sims/chisq//ucsq_gl_rd10_11.pdf \
              ./output/sims/chisq//ucsq_gl_rd10_12.pdf \
              ./output/sims/chisq//ucsq_gl_rd10_22.pdf \
              ./output/sims/chisq//ucsq_gl_rd100_01.pdf \
              ./output/sims/chisq//ucsq_gl_rd100_02.pdf \
              ./output/sims/chisq//ucsq_gl_rd100_11.pdf \
              ./output/sims/chisq//ucsq_gl_rd100_12.pdf \
              ./output/sims/chisq//ucsq_gl_rd100_22.pdf

pm_plots = ./output/sims/null_g_pmalpha_box.pdf \
           ./output/sims/null_gl_pmalpha_box.pdf

blueplots = ./output/blue/bad_diff.pdf \
            ./output/blue/bad_snps.pdf \
            ./output/blue/blue_logbf_hist.pdf \
            ./output/blue/blue_qq.pdf \
            ./output/blue/chr_dr_scatter.pdf

.PHONY : all
all : blue sims

# Blueberry analysis ----
.PHONY : blue
blue : $(blueplots)

./output/blue/bluefits.RDS : ./code/blue_up.R ./data/updog_input_240ind_Sweet_Indi.Rdata
	mkdir -p ./output/rout
	mkdir -p ./output/blue
	$(rexec) '--args nc=$(nc)' $< ./output/rout/$(basename $(notdir $<)).Rout

./output/blue/blue_df.csv : ./code/blue_test.R ./output/blue/bluefits.RDS
	mkdir -p ./output/rout
	mkdir -p ./output/blue
	$(rexec) '--args nc=$(nc)' $< ./output/rout/$(basename $(notdir $<)).Rout

$(blueplots) : ./code/blue_plots.R ./output/blue/blue_df.csv
	mkdir -p ./output/rout
	mkdir -p ./output/sims
	$(rexec) $< ./output/rout/$(basename $(notdir $<)).Rout

# Sims ----
.PHONY : sims
sims : $(chisq_plots) $(pm_plots)

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

$(chisq_plots) : ./code/chisq_plots.R $(simsout)
	mkdir -p ./output/rout
	mkdir -p ./output/sims
	mkdir -p ./output/sims/chisq
	$(rexec) $< ./output/rout/$(basename $(notdir $<)).Rout

$(pm_plots) : ./code/pm_plots.R $(simsout)
	mkdir -p ./output/rout
	mkdir -p ./output/sims
	$(rexec) $< ./output/rout/$(basename $(notdir $<)).Rout
