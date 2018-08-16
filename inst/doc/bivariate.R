### R code from vignette source 'bivariate.Rnw'

###################################################
### code chunk number 1: bivariate.Rnw:34-37
###################################################
options(continue=" ")
options(SweaveHooks=list(fig=function()
par(mar=c(4.1, 4.1, 2.6, 1.6), cex=0.7, cex.main=1)))


###################################################
### code chunk number 2: bivariate.Rnw:57-59
###################################################
library (mvtnorm)
library (bivariate)


###################################################
### code chunk number 3: bivariate.Rnw:67-68
###################################################
getOption("SweaveHooks")[["fig"]]()
plot3d.empty ()


###################################################
### code chunk number 4: bivariate.Rnw:81-85
###################################################
getOption("SweaveHooks")[["fig"]]()
x = y = 1:4
f = function (x, y) 2 * x + y ^ 2
z = outer (x, y, f)
plot3d.step.regular (z)


###################################################
### code chunk number 5: bivariate.Rnw:92-93 (eval = FALSE)
###################################################
## ?plot3d.step


###################################################
### code chunk number 6: bivariate.Rnw:103-107
###################################################
getOption("SweaveHooks")[["fig"]]()
x = y = 1:4
f = function (x, y) 2 * x + y ^ 2
z = outer (x, y, f)
plot3d.continuous.regular (z)


###################################################
### code chunk number 7: bivariate.Rnw:114-115 (eval = FALSE)
###################################################
## ?plot3d.continuous


###################################################
### code chunk number 8: bivariate.Rnw:122-123
###################################################
f = nbpdf (0, 0, 1, 1, 0)


###################################################
### code chunk number 9: bivariate.Rnw:127-128
###################################################
#f = nbpdf (mean.x, mean,y, sd.x ^ 2, sd.y ^ 2, sd.x * sd.y * cor.xy)


###################################################
### code chunk number 10: bivariate.Rnw:132-133
###################################################
f


###################################################
### code chunk number 11: bivariate.Rnw:138-139
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f)


###################################################
### code chunk number 12: bivariate.Rnw:142-143
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE)


###################################################
### code chunk number 13: bivariate.Rnw:148-149 (eval = FALSE)
###################################################
## ?nbpdf


###################################################
### code chunk number 14: bivariate.Rnw:153-154
###################################################
f (0, 0)


###################################################
### code chunk number 15: bivariate.Rnw:163-164
###################################################
F = nbcdf (0, 0, 1, 1, 0)


###################################################
### code chunk number 16: bivariate.Rnw:168-169
###################################################
#F = nbcdf (mean.x, mean,y, sd.x ^ 2, sd.y ^ 2, sd.x * sd.y * cor.xy)


###################################################
### code chunk number 17: bivariate.Rnw:173-174
###################################################
F


###################################################
### code chunk number 18: bivariate.Rnw:179-180
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F)


###################################################
### code chunk number 19: bivariate.Rnw:183-184
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, TRUE)


###################################################
### code chunk number 20: bivariate.Rnw:189-190 (eval = FALSE)
###################################################
## ?nbcdf


###################################################
### code chunk number 21: bivariate.Rnw:194-195
###################################################
F (0, 0)


###################################################
### code chunk number 22: bivariate.Rnw:207-208
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, all=TRUE)


###################################################
### code chunk number 23: bivariate.Rnw:216-218
###################################################
getOption("SweaveHooks")[["fig"]]()
f2 = nbpdf (0, 0, 1, 1, 0.75)
plot (f2, all=TRUE)


###################################################
### code chunk number 24: bivariate.Rnw:224-226
###################################################
getOption("SweaveHooks")[["fig"]]()
f3 = nbpdf (0, 0, 1, 1, -0.75)
plot (f3, all=TRUE)


###################################################
### code chunk number 25: bivariate.Rnw:249-254
###################################################
x1 = y1 = -1
x2 = y2 = 1
compute.bivariate.probability = function (F, x1, x2, y1, y2)
    F (x1, y1) + F (x2, y2) - F (x1, y2) - F (x2, y1)
compute.bivariate.probability (F, x1, x2, y1, y2)


###################################################
### code chunk number 26: bivariate.Rnw:259-263
###################################################
x1 = y1 = -1:-3
x2 = y2 = 1:3
p = compute.bivariate.probability (F, x1, x2, y1, y2)
cbind (x1, x2, y1, y2, p)


