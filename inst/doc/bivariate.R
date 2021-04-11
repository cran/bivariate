### R code from vignette source 'bivariate.Rnw'

###################################################
### code chunk number 1: bivariate.Rnw:41-46
###################################################
options(continue="   ")
options(SweaveHooks=list(fig=function()
par(mar=c(4.1, 4.1, 1.35, 1.6), cex=0.7, cex.main=1)))

set.seed (1)


###################################################
### code chunk number 2: bivariate.Rnw:101-103
###################################################
library (barsurf)
library (bivariate)


###################################################
### code chunk number 3: bivariate.Rnw:113-114
###################################################
set.bs.options (nhl=0, rendering.style="pdf", theme="gold", ref.arrows=FALSE)


###################################################
### code chunk number 4: bivariate.Rnw:139-145
###################################################
f <- dubvpmf (
    1, 6, #first variable
    1, 6) #second variable
F <- dubvcdf (
    1, 6,
    1, 6)


###################################################
### code chunk number 5: bivariate.Rnw:151-152
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f)


###################################################
### code chunk number 6: bivariate.Rnw:154-155
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F)


###################################################
### code chunk number 7: bivariate.Rnw:163-165
###################################################
f (2, 4)
F (2, 4)


###################################################
### code chunk number 8: bivariate.Rnw:173-177
###################################################
d.unif.pmf.eval <- function (x, a, b)    #x ignored
    1 / (b - a + 1)
d.unif.cdf.eval <- function (x, a, b)    #x used
    (x - a + 1) / (b - a + 1)


###################################################
### code chunk number 9: bivariate.Rnw:180-182
###################################################
d.unif.pmf.eval (2, 1, 6) * d.unif.pmf.eval (4, 1, 6)
d.unif.cdf.eval (2, 1, 6) * d.unif.cdf.eval (4, 1, 6)


###################################################
### code chunk number 10: bivariate.Rnw:206-208
###################################################
f <- bnbvpmf (0.5, 0.5, 10)
F <- bnbvcdf (0.5, 0.5, 10)


###################################################
### code chunk number 11: bivariate.Rnw:214-215
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f)


###################################################
### code chunk number 12: bivariate.Rnw:217-218
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, FALSE)


###################################################
### code chunk number 13: bivariate.Rnw:223-224
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F)


###################################################
### code chunk number 14: bivariate.Rnw:257-258
###################################################
f <- pbvpmf.2 (8, 8, 2)


###################################################
### code chunk number 15: bivariate.Rnw:264-265
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, theme="blue")


###################################################
### code chunk number 16: bivariate.Rnw:267-268
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, FALSE, theme="blue")


###################################################
### code chunk number 17: bivariate.Rnw:282-285
###################################################
h <- matrix (sample (1:24), 4, 6)
rownames (h) <- LETTERS [1:4]
colnames (h) <- letters [1:6]


###################################################
### code chunk number 18: bivariate.Rnw:288-289
###################################################
h


###################################################
### code chunk number 19: bivariate.Rnw:292-293
###################################################
f <- gbvpmf (h)


###################################################
### code chunk number 20: bivariate.Rnw:299-300 (eval = FALSE)
###################################################
## plot (f, FALSE, theme="blue")


###################################################
### code chunk number 21: bivariate.Rnw:302-305
###################################################
getOption("SweaveHooks")[["fig"]]()
p0 <- matrix.margins ()
plot (f, FALSE, theme="blue")
par (p0)


###################################################
### code chunk number 22: bivariate.Rnw:307-308
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, theme="blue")


###################################################
### code chunk number 23: bivariate.Rnw:316-317
###################################################
h [2, 4] / sum (h)


###################################################
### code chunk number 24: bivariate.Rnw:320-322
###################################################
f (2, 4)
f ("B", "d")


###################################################
### code chunk number 25: bivariate.Rnw:337-343
###################################################
f <- cubvpdf (
    0, 2, #first variable
    0, 2) #second variable
F <- cubvcdf (
    0, 2,
    0, 2)


###################################################
### code chunk number 26: bivariate.Rnw:349-350
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f)


###################################################
### code chunk number 27: bivariate.Rnw:352-353
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F)


###################################################
### code chunk number 28: bivariate.Rnw:369-371
###################################################
f <- nbvpdf (0, 0, 1, 1, 0)
F <- nbvcdf (0, 0, 1, 1, 0)


###################################################
### code chunk number 29: bivariate.Rnw:378-379
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, theme="blue")


###################################################
### code chunk number 30: bivariate.Rnw:381-382
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, FALSE, theme="blue")


###################################################
### code chunk number 31: bivariate.Rnw:388-389
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, theme="blue")


###################################################
### code chunk number 32: bivariate.Rnw:391-392
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, FALSE, theme="blue")


###################################################
### code chunk number 33: bivariate.Rnw:408-409
###################################################
f <- bmbvpdf (-1.5, 1.5)


###################################################
### code chunk number 34: bivariate.Rnw:415-417
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, theme="blue",
    axes = c (TRUE, FALSE), xat = c (-1.5, 1.5) )


###################################################
### code chunk number 35: bivariate.Rnw:419-421
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, FALSE, theme="blue",
    axes = c (TRUE, FALSE), xat = c (-1.5, 1.5) )


###################################################
### code chunk number 36: bivariate.Rnw:438-439
###################################################
f <- dtvpdf (2, 4, 6)


###################################################
### code chunk number 37: bivariate.Rnw:445-446
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, theme="blue")


###################################################
### code chunk number 38: bivariate.Rnw:448-449
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, FALSE, theme="blue")


###################################################
### code chunk number 39: bivariate.Rnw:467-469
###################################################
data ("geyser", package="MASS")
names (geyser)


###################################################
### code chunk number 40: bivariate.Rnw:472-473
###################################################
fh <- kbvpdf (,,0.7, 7, data = geyser [,2:1])


###################################################
### code chunk number 41: bivariate.Rnw:479-480
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (fh, FALSE, theme="blue")


###################################################
### code chunk number 42: bivariate.Rnw:486-487
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (fh, theme="blue")


###################################################
### code chunk number 43: bivariate.Rnw:508-509
###################################################
names (trees)


###################################################
### code chunk number 44: bivariate.Rnw:512-513
###################################################
Fh <- ebvcdf (data = trees [,2:3])


###################################################
### code chunk number 45: bivariate.Rnw:520-521
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (Fh, theme="blue")


###################################################
### code chunk number 46: bivariate.Rnw:523-524
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (Fh, FALSE, theme="blue")


###################################################
### code chunk number 47: bivariate.Rnw:530-531
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (Fh, FALSE, reg=TRUE, theme="blue")


###################################################
### code chunk number 48: bivariate.Rnw:579-583
###################################################
f <- ntvpdf (
    0, 0, 0,      #mean: X, Y, Z
    1, 1, 1,      #sd:   X, Y, Z
    -0.5, 0.5, 0) #cor:  X~Y, X~Z, Y~Z


###################################################
### code chunk number 49: bivariate.Rnw:586-587
###################################################
f@covariance.matrix


###################################################
### code chunk number 50: bivariate.Rnw:591-592
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, theme="blue")


###################################################
### code chunk number 51: bivariate.Rnw:605-608
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_cfield (function (y, z) f (0, y, z), c (-3, 3),
    theme="blue",
    xlab="y", ylab="z")


###################################################
### code chunk number 52: bivariate.Rnw:626-628
###################################################
f1 <- nbvpdf (0, 0, 1, 1, 0)
f1@covariance.matrix


###################################################
### code chunk number 53: bivariate.Rnw:632-633
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f1, all=TRUE, n=20, theme="blue")


###################################################
### code chunk number 54: bivariate.Rnw:640-642
###################################################
f2 <- nbvpdf (0, 0, 1, 1, 0.75)
f2@covariance.matrix


###################################################
### code chunk number 55: bivariate.Rnw:647-648
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f2, all=TRUE, n=20, theme="blue")


###################################################
### code chunk number 56: bivariate.Rnw:654-656
###################################################
f3 <- nbvpdf (0, 0, 1, 1, -0.75)
f3@covariance.matrix


###################################################
### code chunk number 57: bivariate.Rnw:661-662
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f3, all=TRUE, n=20, theme="blue")


