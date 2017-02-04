cat('This is my R script executing\n', format(Sys.Date()))
sessionInfo()
# Setup a log for input/output messages
zz <- file("run_R_GA_output.log")
sink(zz, append=TRUE)
#sink(zz, append=TRUE, type="output")
sink(zz, append=TRUE, type="message")

source("D:\\imm_automation\\R_Scripts\\Drew-RGoogleAnalyticsNZR.R")
rga.pull()

# reset sink, restore output to console
sink()
#sink(type="output")
sink(type="message")

#return exit code
#stop("R script finished: rga.pull")
quit(save = "no", status = 0, runLast = FALSE)

