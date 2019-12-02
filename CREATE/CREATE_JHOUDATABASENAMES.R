experiments <- list("runway" = names(runway),
                    "locomotor" = names(rawfiles_locomotor_wide),
                    "progressivepunishment" = names(rawfiles_pp), 
                    "progressiveratio" = names(rawfiles_pr),
                    "delayedpunishment" = names(rawfiles_dpresses_test),
                    "master_table" = names(WFU_Jhou_test[[2]])) # lapply(WFU_Jhou_test, names) shows that all but the first one are consistent bc columns 16-18 should be removed
experiments <- mapply(append, experiments, "comment")
experiments <- mapply(append, experiments, "resolution")
experiments$runway <- experiments$runway[!grepl("^.+time",experiments$runway)] # remove the time values for which diff was calculated 
experiments$runway <- gsub("diff", "elapsedtime", experiments$runway) # rename diff var
experiments$locomotor <- append(experiments$locomotor, c("session", "bodyweightperc", "cohort", "date", "time")) 
experiments$progressivepunishment <- append(experiments$progressivepunishment, c("session", "bodyweightperc", "session", "box", "lastblocknumtrials", "cohort", "rfid"))
experiments$progressivepunishment <- gsub("shocks", "shock", experiments$progressivepunishment)
experiments$progressiveratio <- append(experiments$progressiveratio, c("session", "bodyweightperc", "session", "box", "cohort", "rfid"))
experiments$delayedpunishment <- append(experiments$delayedpunishment, c("shockcompleted", "shockattempted", "delay", "box", "cohort", "rfid"))
experiments$delayedpunishment <- mgsub::mgsub(experiments$delayedpunishment,
             c("L", "R"),
             c("active", "inactive"))
# experiments <- mapply(append, experiments, "master_table") # by apurva's request

# in runway, add (nothing)
# in locomotor, add cohort, date, and time, 
# in propun, add box number, number of trials at last shock intensity,
# in prorat, add cohort, rfid, session, and bodyweightpercent
# in delpun, add (see abov)
# in all, add the notes column 

save(experiments, file = "jhouexpnames.RData")
