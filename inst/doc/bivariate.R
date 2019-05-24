### R code from vignette source 'bivariate.Rnw'

###################################################
### code chunk number 1: bivariate.Rnw:34-39
###################################################
options(continue=" ")
options(SweaveHooks=list(fig=function()
par(mar=c(4.1, 4.1, 2.6, 1.6), cex=0.7, cex.main=1)))

set.seed (1)


###################################################
### code chunk number 2: bivariate.Rnw:97-100
###################################################
library (intoo)
library (bivariate)
library (MASS)


###################################################
### code chunk number 3: bivariate.Rnw:111-112
###################################################
f = dubvpmf (1, 4, 1, 4)


###################################################
### code chunk number 4: bivariate.Rnw:120-121
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE)


###################################################
### code chunk number 5: bivariate.Rnw:128-130
###################################################
getOption("SweaveHooks")[["fig"]]()
F = dubvcdf (1, 4, 1, 4)
plot (F, TRUE)


###################################################
### code chunk number 6: bivariate.Rnw:136-137
###################################################
f (1, 1)


###################################################
### code chunk number 7: bivariate.Rnw:159-161
###################################################
getOption("SweaveHooks")[["fig"]]()
f = bnbvpmf (0.5, 0.5, 20)
plot (f)


###################################################
### code chunk number 8: bivariate.Rnw:163-164
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE)


###################################################
### code chunk number 9: bivariate.Rnw:171-173
###################################################
getOption("SweaveHooks")[["fig"]]()
F = bnbvcdf (0.5, 0.5, 20)
plot (F, TRUE)


###################################################
### code chunk number 10: bivariate.Rnw:202-204
###################################################
getOption("SweaveHooks")[["fig"]]()
f = pbvpmf.2 (8, 8, 2)
plot (f)


###################################################
### code chunk number 11: bivariate.Rnw:206-207
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE)


###################################################
### code chunk number 12: bivariate.Rnw:214-216
###################################################
getOption("SweaveHooks")[["fig"]]()
F = pbvcdf.2 (8, 8, 2)
plot (F, TRUE)


###################################################
### code chunk number 13: bivariate.Rnw:226-229
###################################################
z = matrix (sample (1:16), 4, 4) / 136
rownames (z) = colnames (z) = c ("A", "B", "C", "D")
z


###################################################
### code chunk number 14: bivariate.Rnw:233-235
###################################################
getOption("SweaveHooks")[["fig"]]()
f = cbvpmf (z)
plot (f, TRUE)


###################################################
### code chunk number 15: bivariate.Rnw:240-242
###################################################
f (1, 2)
f ("A", "B")


###################################################
### code chunk number 16: bivariate.Rnw:253-255
###################################################
getOption("SweaveHooks")[["fig"]]()
f = cubvpdf (0, 2, 0, 2)
plot (f, TRUE)


###################################################
### code chunk number 17: bivariate.Rnw:262-263
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, TRUE)


###################################################
### code chunk number 18: bivariate.Rnw:274-275
###################################################
f = nbvpdf (0, 0, 1, 1, 0)


###################################################
### code chunk number 19: bivariate.Rnw:279-280
###################################################
f


###################################################
### code chunk number 20: bivariate.Rnw:286-287
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f)


###################################################
### code chunk number 21: bivariate.Rnw:290-291
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE)


###################################################
### code chunk number 22: bivariate.Rnw:296-297
###################################################
F = nbvcdf (0, 0, 1, 1, 0)


###################################################
### code chunk number 23: bivariate.Rnw:303-304
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F)


###################################################
### code chunk number 24: bivariate.Rnw:307-308
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, TRUE)


###################################################
### code chunk number 25: bivariate.Rnw:326-328
###################################################
getOption("SweaveHooks")[["fig"]]()
f = bmbvpdf (3.5, 0, 1, 1, 6.5, 0, 1, 1)
plot (f, npoints=40)


###################################################
### code chunk number 26: bivariate.Rnw:331-332
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE, npoints=40)


###################################################
### code chunk number 27: bivariate.Rnw:339-341
###################################################
getOption("SweaveHooks")[["fig"]]()
F = bmbvcdf (3.5, 0, 1, 1, 6.5, 0, 1, 1)
plot (F, npoints=40)


###################################################
### code chunk number 28: bivariate.Rnw:344-345
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, TRUE, npoints=40)


###################################################
### code chunk number 29: bivariate.Rnw:359-361
###################################################
getOption("SweaveHooks")[["fig"]]()
f = dtvpdf (2, 4, 6)
plot (f)


###################################################
### code chunk number 30: bivariate.Rnw:364-365
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE)


###################################################
### code chunk number 31: bivariate.Rnw:372-373
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE, log=TRUE)


###################################################
### code chunk number 32: bivariate.Rnw:387-389
###################################################
data ("geyser")
attach (geyser)


###################################################
### code chunk number 33: bivariate.Rnw:392-393
###################################################
f = kbvpdf (duration, waiting, 0.7, 7)


###################################################
### code chunk number 34: bivariate.Rnw:398-399
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, xlab="duration", ylab="waiting")


###################################################
### code chunk number 35: bivariate.Rnw:402-403
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE, xlab="duration", ylab="waiting")


###################################################
### code chunk number 36: bivariate.Rnw:407-408
###################################################
detach (geyser)


###################################################
### code chunk number 37: bivariate.Rnw:416-418
###################################################
x = rnorm (20)
y = rnorm (20)


###################################################
### code chunk number 38: bivariate.Rnw:422-424
###################################################
getOption("SweaveHooks")[["fig"]]()
F = ebvcdf (x, y)
plot (F)


###################################################
### code chunk number 39: bivariate.Rnw:427-428
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, TRUE)


###################################################
### code chunk number 40: bivariate.Rnw:465-467
###################################################
f1 = nbvpdf (0, 0, 1, 1, 0)
f1 %$% matrix.variances


###################################################
### code chunk number 41: bivariate.Rnw:470-471
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f1, all=TRUE)


###################################################
### code chunk number 42: bivariate.Rnw:478-480
###################################################
f2 = nbvpdf (0, 0, 1, 1, 0.75)
f2 %$% matrix.variances


###################################################
### code chunk number 43: bivariate.Rnw:483-484
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f2, all=TRUE)


###################################################
### code chunk number 44: bivariate.Rnw:491-493
###################################################
f3 = nbvpdf (0, 0, 1, 1, -0.75)
f3 %$% matrix.variances


###################################################
### code chunk number 45: bivariate.Rnw:496-497
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f3, all=TRUE)


###################################################
### code chunk number 46: bivariate.Rnw:510-511
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (dtvpdf (1, 1, 1), TRUE)


###################################################
### code chunk number 47: bivariate.Rnw:518-519
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (dtvpdf (2, 2, 2), TRUE)


###################################################
### code chunk number 48: bivariate.Rnw:526-527
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (dtvpdf (0.5, 0.5, 0.5), TRUE)


