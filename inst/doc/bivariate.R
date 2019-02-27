### R code from vignette source 'bivariate.Rnw'

###################################################
### code chunk number 1: bivariate.Rnw:34-37
###################################################
options(continue=" ")
options(SweaveHooks=list(fig=function()
par(mar=c(4.1, 4.1, 2.6, 1.6), cex=0.7, cex.main=1)))


###################################################
### code chunk number 2: bivariate.Rnw:85-87
###################################################
library (bivariate)
library (moments)


###################################################
### code chunk number 3: bivariate.Rnw:96-97
###################################################
f = dubvpmf (1, 10, 1, 10)


###################################################
### code chunk number 4: bivariate.Rnw:103-104
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f)


###################################################
### code chunk number 5: bivariate.Rnw:111-113
###################################################
getOption("SweaveHooks")[["fig"]]()
F = dubvcdf (1, 10, 1, 10)
plot (F)


###################################################
### code chunk number 6: bivariate.Rnw:119-120
###################################################
f (1, 1)


###################################################
### code chunk number 7: bivariate.Rnw:141-143
###################################################
getOption("SweaveHooks")[["fig"]]()
f = bnbvpmf (20, 0.5, 0.5)
plot (f)


###################################################
### code chunk number 8: bivariate.Rnw:150-152
###################################################
getOption("SweaveHooks")[["fig"]]()
F = bnbvcdf (20, 0.5, 0.5)
plot (F)


###################################################
### code chunk number 9: bivariate.Rnw:174-176
###################################################
getOption("SweaveHooks")[["fig"]]()
f = pbvpmf (8, 8, 2)
plot (f)


###################################################
### code chunk number 10: bivariate.Rnw:185-187
###################################################
getOption("SweaveHooks")[["fig"]]()
F = pbvcdf (8, 8, 2)
plot (F)


###################################################
### code chunk number 11: bivariate.Rnw:199-201
###################################################
getOption("SweaveHooks")[["fig"]]()
f = cubvpdf (0, 1, 0, 1)
plot (f)


###################################################
### code chunk number 12: bivariate.Rnw:204-205
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE)


###################################################
### code chunk number 13: bivariate.Rnw:212-214
###################################################
getOption("SweaveHooks")[["fig"]]()
F = cubvcdf (0, 1, 0, 1)
plot (F)


###################################################
### code chunk number 14: bivariate.Rnw:217-218
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, TRUE)


###################################################
### code chunk number 15: bivariate.Rnw:229-230
###################################################
f = nbvpdf (0, 0, 1, 1, 0)


###################################################
### code chunk number 16: bivariate.Rnw:235-236
###################################################
#f = nbvpdf (mean.x, mean,y, sd.x ^ 2, sd.y ^ 2, sd.x * sd.y * cor.xy)


###################################################
### code chunk number 17: bivariate.Rnw:242-243
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f)


###################################################
### code chunk number 18: bivariate.Rnw:246-247
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE)


###################################################
### code chunk number 19: bivariate.Rnw:253-254
###################################################
F = nbvcdf (0, 0, 1, 1, 0)


###################################################
### code chunk number 20: bivariate.Rnw:258-259
###################################################
#F = nbvcdf (mean.x, mean,y, sd.x ^ 2, sd.y ^ 2, sd.x * sd.y * cor.xy)


###################################################
### code chunk number 21: bivariate.Rnw:265-266
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F)


###################################################
### code chunk number 22: bivariate.Rnw:269-270
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, TRUE)


###################################################
### code chunk number 23: bivariate.Rnw:282-283
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, all=TRUE)


###################################################
### code chunk number 24: bivariate.Rnw:292-294
###################################################
getOption("SweaveHooks")[["fig"]]()
f2 = nbvpdf (0, 0, 1, 1, 0.75)
plot (f2, all=TRUE)


###################################################
### code chunk number 25: bivariate.Rnw:301-303
###################################################
getOption("SweaveHooks")[["fig"]]()
f3 = nbvpdf (0, 0, 1, 1, -0.75)
plot (f3, all=TRUE)


###################################################
### code chunk number 26: bivariate.Rnw:315-317
###################################################
getOption("SweaveHooks")[["fig"]]()
f = bmbvpdf (3.5, 0, 1, 1, 6.5, 0, 1, 1)
plot (f)


###################################################
### code chunk number 27: bivariate.Rnw:320-321
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE, npoints=40)


###################################################
### code chunk number 28: bivariate.Rnw:328-330
###################################################
getOption("SweaveHooks")[["fig"]]()
F = bmbvcdf (3.5, 0, 1, 1, 6.5, 0, 1, 1)
plot (F)


###################################################
### code chunk number 29: bivariate.Rnw:333-334
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, TRUE, npoints=40)


###################################################
### code chunk number 30: bivariate.Rnw:346-347
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, npoints=20, all=TRUE)


###################################################
### code chunk number 31: bivariate.Rnw:355-357
###################################################
getOption("SweaveHooks")[["fig"]]()
f2 = bmbvpdf (3.5, 3.5, 1, 1, 6.5, 6.5, 1, 1)
plot (f2, npoints=20, all=TRUE)


###################################################
### code chunk number 32: bivariate.Rnw:363-365
###################################################
getOption("SweaveHooks")[["fig"]]()
f3 = bmbvpdf (3.5, -3.5, 1, 1, 6.5, -6.5, 1, 1)
plot (f3, npoints=20, all=TRUE)


###################################################
### code chunk number 33: bivariate.Rnw:378-380
###################################################
data ("geyser", package="MASS")
attach (geyser)


###################################################
### code chunk number 34: bivariate.Rnw:383-384
###################################################
f = kbvpdf (duration, waiting, 0.7, 7)


###################################################
### code chunk number 35: bivariate.Rnw:389-391
###################################################
getOption("SweaveHooks")[["fig"]]()

plot (f, xlab="duration", ylab="waiting")


###################################################
### code chunk number 36: bivariate.Rnw:394-395
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE, xlab="duration", ylab="waiting")


###################################################
### code chunk number 37: bivariate.Rnw:402-404
###################################################
getOption("SweaveHooks")[["fig"]]()
F = kbvcdf (duration, waiting, 0.7, 7)
plot (F, xlab="duration", ylab="waiting")


###################################################
### code chunk number 38: bivariate.Rnw:407-408
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, TRUE, xlab="duration", ylab="waiting")


###################################################
### code chunk number 39: bivariate.Rnw:452-458
###################################################
getOption("SweaveHooks")[["fig"]]()
head (geyser, 3)
tail (geyser, 3)
summary (geyser)
skewness (geyser)
cov (geyser)
plot (duration, waiting)


