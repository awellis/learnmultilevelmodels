library(designr)

design1 <-
    fixed.factor("Speed", levels=c("slow", "medium", "fast")) +
    fixed.factor("Load",  levels=c("yes", "no")) +
    random.factor("Subj", instances=6) +
    random.factor("Item", instances=6)
design1


codes1 <- arrange(design.codes(design1), Subj, Item)[c(3, 4, 2, 1)]
codes1
tail(codes1, 10)
xtabs(~ Load + Speed, codes1)



design <-
    fixed.factor("X", levels=c("X1", "X2")) + # fixed effect
    random.factor("Subj", instances=5)        # random effect

dat <- design.codes(design) # create data frame (tibble) from experimental design
length(unique(dat$Subj)) # number of subjets
data.frame(dat) # look at data
