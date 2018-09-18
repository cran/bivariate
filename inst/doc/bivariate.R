### R code from vignette source 'bivariate.Rnw'

###################################################
### code chunk number 1: bivariate.Rnw:34-37
###################################################
options(continue=" ")
options(SweaveHooks=list(fig=function()
par(mar=c(4.1, 4.1, 2.6, 1.6), cex=0.7, cex.main=1)))


###################################################
### code chunk number 2: bivariate.Rnw:73-74
###################################################
library (bivariate)


###################################################
### code chunk number 3: bivariate.Rnw:82-83
###################################################
getOption("SweaveHooks")[["fig"]]()
plot3d.empty ()


###################################################
### code chunk number 4: bivariate.Rnw:97-99
###################################################
getOption("SweaveHooks")[["fig"]]()
f = dubvpmf (0, 1, 0, 1)
plot (f)


###################################################
### code chunk number 5: bivariate.Rnw:106-108
###################################################
getOption("SweaveHooks")[["fig"]]()
F = dubvcdf (0, 1, 0, 1)
plot (F)


###################################################
### code chunk number 6: bivariate.Rnw:114-115
###################################################
f


###################################################
### code chunk number 7: bivariate.Rnw:118-119
###################################################
f (0.5, 0.5)


###################################################
### code chunk number 8: bivariate.Rnw:134-136
###################################################
getOption("SweaveHooks")[["fig"]]()
f = bnbvpmf (20, 0.5, 0.5)
plot (f)


###################################################
### code chunk number 9: bivariate.Rnw:143-145
###################################################
getOption("SweaveHooks")[["fig"]]()
F = bnbvcdf (20, 0.5, 0.5)
plot (F)


###################################################
### code chunk number 10: bivariate.Rnw:165-167
###################################################
getOption("SweaveHooks")[["fig"]]()
f = pbvpmf (10, 10, 2)
plot (f)


###################################################
### code chunk number 11: bivariate.Rnw:174-176
###################################################
getOption("SweaveHooks")[["fig"]]()
F = pbvcdf (10, 10, 2)
plot (F)


###################################################
### code chunk number 12: bivariate.Rnw:188-190
###################################################
getOption("SweaveHooks")[["fig"]]()
f = cubvpdf (0, 1, 0, 1)
plot (f)


###################################################
### code chunk number 13: bivariate.Rnw:193-194
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE)


###################################################
### code chunk number 14: bivariate.Rnw:201-203
###################################################
getOption("SweaveHooks")[["fig"]]()
F = cubvcdf (0, 1, 0, 1)
plot (F)


###################################################
### code chunk number 15: bivariate.Rnw:206-207
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, TRUE)


###################################################
### code chunk number 16: bivariate.Rnw:218-219
###################################################
f = nbvpdf (0, 0, 1, 1, 0)


###################################################
### code chunk number 17: bivariate.Rnw:224-225
###################################################
#f = nbvpdf (mean.x, mean,y, sd.x ^ 2, sd.y ^ 2, sd.x * sd.y * cor.xy)


###################################################
### code chunk number 18: bivariate.Rnw:231-232
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f)


###################################################
### code chunk number 19: bivariate.Rnw:235-236
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE)


###################################################
### code chunk number 20: bivariate.Rnw:242-243
###################################################
F = nbvcdf (0, 0, 1, 1, 0)


###################################################
### code chunk number 21: bivariate.Rnw:247-248
###################################################
#F = nbvcdf (mean.x, mean,y, sd.x ^ 2, sd.y ^ 2, sd.x * sd.y * cor.xy)


###################################################
### code chunk number 22: bivariate.Rnw:254-255
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F)


###################################################
### code chunk number 23: bivariate.Rnw:258-259
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, TRUE)


###################################################
### code chunk number 24: bivariate.Rnw:270-271
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, all=TRUE)


###################################################
### code chunk number 25: bivariate.Rnw:279-281
###################################################
getOption("SweaveHooks")[["fig"]]()
f2 = nbvpdf (0, 0, 1, 1, 0.75)
plot (f2, all=TRUE)


###################################################
### code chunk number 26: bivariate.Rnw:287-289
###################################################
getOption("SweaveHooks")[["fig"]]()
f3 = nbvpdf (0, 0, 1, 1, -0.75)
plot (f3, all=TRUE)


###################################################
### code chunk number 27: bivariate.Rnw:301-303
###################################################
getOption("SweaveHooks")[["fig"]]()
f = bmbvpdf (3.5, 0, 1, 1, 6.5, 0, 1, 1)
plot (f)


###################################################
### code chunk number 28: bivariate.Rnw:306-307
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE, 40)


###################################################
### code chunk number 29: bivariate.Rnw:316-318
###################################################
getOption("SweaveHooks")[["fig"]]()
F = bmbvcdf (3.5, 0, 1, 1, 6.5, 0, 1, 1)
plot (F)


###################################################
### code chunk number 30: bivariate.Rnw:321-322
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, TRUE, 40)


###################################################
### code chunk number 31: bivariate.Rnw:334-335
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, all=TRUE)


###################################################
### code chunk number 32: bivariate.Rnw:343-345
###################################################
getOption("SweaveHooks")[["fig"]]()
f2 = bmbvpdf (3.5, 3.5, 1, 1, 6.5, 6.5, 1, 1)
plot (f2, all=TRUE)


###################################################
### code chunk number 33: bivariate.Rnw:351-353
###################################################
getOption("SweaveHooks")[["fig"]]()
f3 = bmbvpdf (3.5, -3.5, 1, 1, 6.5, -6.5, 1, 1)
plot (f3, all=TRUE)


###################################################
### code chunk number 34: bivariate.Rnw:365-369
###################################################
getOption("SweaveHooks")[["fig"]]()
xy = generate.bivariate.sample (200)
x = xy [,1]
y = xy [,2]
plot (x, y)


###################################################
### code chunk number 35: bivariate.Rnw:381-383
###################################################
getOption("SweaveHooks")[["fig"]]()
f = kbvpdf (x, y, 20, 20)
plot (f)


###################################################
### code chunk number 36: bivariate.Rnw:386-387
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE)


###################################################
### code chunk number 37: bivariate.Rnw:394-396
###################################################
getOption("SweaveHooks")[["fig"]]()
F = kbvcdf (x, y, 20, 20)
plot (F)


###################################################
### code chunk number 38: bivariate.Rnw:399-400
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, TRUE)


