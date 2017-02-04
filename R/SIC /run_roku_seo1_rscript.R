cat('This is my R script executing\n', format(Sys.Date()))
sessionInfo()
# Setup a log for input/output messages
zz <- file("run_roku_seo_rscript_output_v1.1.log")
sink(zz, append=TRUE)
#sink(zz, append=TRUE, type="output")
sink(zz, append=TRUE, type="message")

source("D:\\imm_automation\\R_Scripts\\roku_seo_rscript_working_v1.1.R")
setwd("D:\\imm_automation\\R_Scripts")
rseo.pull()


# reset sink, restore output to console
sink()
#sink(type="output")
sink(type="message")

#return exit code
#stop("R script finished: rseo.pull")
quit(save = "no", status = 0, runLast = FALSE)