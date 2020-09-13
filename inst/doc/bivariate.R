### R code from vignette source 'bivariate.Rnw'

###################################################
### code chunk number 1: bivariate.Rnw:41-46
###################################################
options(continue="   ")
options(SweaveHooks=list(fig=function()
par(mar=c(4.1, 4.1, 1.35, 1.6), cex=0.7, cex.main=1)))

set.seed (1)


###################################################
### code chunk number 2: bivariate.Rnw:101-104
###################################################
library (intoo)
library (barsurf)
library (bivariate)


###################################################
### code chunk number 3: bivariate.Rnw:114-115
###################################################
set.bs.options (nhl=0, ref.arrows=FALSE, rendering.style="pdf")


###################################################
### code chunk number 4: bivariate.Rnw:123-124
###################################################
set.bs.theme ("gold")


###################################################
### code chunk number 5: bivariate.Rnw:142-148
###################################################
f <- dubvpmf (
    1, 6, #first variable
    1, 6) #second variable
F <- dubvcdf (
    1, 6,
    1, 6)


###################################################
### code chunk number 6: bivariate.Rnw:154-155
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f)


###################################################
### code chunk number 7: bivariate.Rnw:157-158
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F)


###################################################
### code chunk number 8: bivariate.Rnw:166-168
###################################################
f (2, 4)
F (2, 4)


###################################################
### code chunk number 9: bivariate.Rnw:176-180
###################################################
d.unif.pmf.eval <- function (x, a, b)    #x ignored
    1 / (b - a + 1)
d.unif.cdf.eval <- function (x, a, b)    #x used
    (x - a + 1) / (b - a + 1)


###################################################
### code chunk number 10: bivariate.Rnw:183-185
###################################################
d.unif.pmf.eval (2, 1, 6) * d.unif.pmf.eval (4, 1, 6)
d.unif.cdf.eval (2, 1, 6) * d.unif.cdf.eval (4, 1, 6)


###################################################
### code chunk number 11: bivariate.Rnw:209-211
###################################################
f <- bnbvpmf (0.5, 0.5, 10)
F <- bnbvcdf (0.5, 0.5, 10)


###################################################
### code chunk number 12: bivariate.Rnw:217-218
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f)


###################################################
### code chunk number 13: bivariate.Rnw:220-221
###################################################
set.bs.options (nhl=0, ref.arrows=FALSE, rendering.style="pdf", main="gpd.litmus.fit")


###################################################
### code chunk number 14: bivariate.Rnw:223-224
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, FALSE)


###################################################
### code chunk number 15: bivariate.Rnw:226-227
###################################################
set.bs.theme ("gold")


###################################################
### code chunk number 16: bivariate.Rnw:233-234
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F)


###################################################
### code chunk number 17: bivariate.Rnw:245-246
###################################################
set.bs.theme ("blue")


###################################################
### code chunk number 18: bivariate.Rnw:271-272
###################################################
f <- pbvpmf.2 (8, 8, 2)


###################################################
### code chunk number 19: bivariate.Rnw:278-279
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f)


###################################################
### code chunk number 20: bivariate.Rnw:281-282
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, FALSE)


###################################################
### code chunk number 21: bivariate.Rnw:296-299
###################################################
h <- matrix (sample (1:24), 4, 6)
rownames (h) <- LETTERS [1:4]
colnames (h) <- letters [1:6]


###################################################
### code chunk number 22: bivariate.Rnw:302-303
###################################################
h


###################################################
### code chunk number 23: bivariate.Rnw:306-307
###################################################
f <- gbvpmf (h)


###################################################
### code chunk number 24: bivariate.Rnw:313-314 (eval = FALSE)
###################################################
## plot (f, FALSE)


###################################################
### code chunk number 25: bivariate.Rnw:316-319
###################################################
getOption("SweaveHooks")[["fig"]]()
p0 <- matrix.margins ()
plot (f, FALSE)
par (p0)


###################################################
### code chunk number 26: bivariate.Rnw:321-322
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f)


###################################################
### code chunk number 27: bivariate.Rnw:330-331
###################################################
h [2, 4] / sum (h)


###################################################
### code chunk number 28: bivariate.Rnw:334-336
###################################################
f (2, 4)
f ("B", "d")


###################################################
### code chunk number 29: bivariate.Rnw:344-345
###################################################
set.bs.theme ("gold")


###################################################
### code chunk number 30: bivariate.Rnw:355-361
###################################################
f <- cubvpdf (
    0, 2, #first variable
    0, 2) #second variable
F <- cubvcdf (
    0, 2,
    0, 2)


###################################################
### code chunk number 31: bivariate.Rnw:367-368
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f)


###################################################
### code chunk number 32: bivariate.Rnw:370-371
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F)


###################################################
### code chunk number 33: bivariate.Rnw:378-379
###################################################
set.bs.theme ("blue")


###################################################
### code chunk number 34: bivariate.Rnw:391-393
###################################################
f <- nbvpdf (0, 0, 1, 1, 0)
F <- nbvcdf (0, 0, 1, 1, 0)


###################################################
### code chunk number 35: bivariate.Rnw:400-401
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f)


###################################################
### code chunk number 36: bivariate.Rnw:403-404
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, FALSE)


###################################################
### code chunk number 37: bivariate.Rnw:410-411
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F)


###################################################
### code chunk number 38: bivariate.Rnw:413-414
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, FALSE)


###################################################
### code chunk number 39: bivariate.Rnw:430-433
###################################################
f <- bmbvpdf (
    3.5, 0, 1, 1,    #first component distribution
    6.5, 0, 1, 1)    #second component distribution


###################################################
### code chunk number 40: bivariate.Rnw:439-441
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f,
    axes = c (TRUE, FALSE), xat = c (3.5, 6.5) )


###################################################
### code chunk number 41: bivariate.Rnw:443-445
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, FALSE,
    axes = c (TRUE, FALSE), xat = c (3.5, 6.5) )


###################################################
### code chunk number 42: bivariate.Rnw:462-463
###################################################
f <- dtvpdf (2, 4, 6)


###################################################
### code chunk number 43: bivariate.Rnw:469-470
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f)


###################################################
### code chunk number 44: bivariate.Rnw:472-473
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, FALSE)


###################################################
### code chunk number 45: bivariate.Rnw:491-492
###################################################
data ("geyser", package="MASS")


###################################################
### code chunk number 46: bivariate.Rnw:495-498
###################################################
attach (geyser)
fh <- kbvpdf (duration, waiting, 0.7, 7)
detach (geyser)


###################################################
### code chunk number 47: bivariate.Rnw:504-505
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (fh, FALSE)


###################################################
### code chunk number 48: bivariate.Rnw:511-512
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (fh)


###################################################
### code chunk number 49: bivariate.Rnw:533-536
###################################################
attach (trees)
Fh <- ebvcdf (Height, Volume)
detach (trees)


###################################################
### code chunk number 50: bivariate.Rnw:543-544
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (Fh)


###################################################
### code chunk number 51: bivariate.Rnw:546-547
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (Fh, FALSE)


###################################################
### code chunk number 52: bivariate.Rnw:553-554
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (Fh, FALSE, reg=TRUE)


###################################################
### code chunk number 53: bivariate.Rnw:604-608
###################################################
f <- ntvpdf (
    0, 0, 0,      #mean: X, Y, Z
    1, 1, 1,      #sd:   X, Y, Z
    -0.5, 0.5, 0) #cor:  X~Y, X~Z, Y~Z


###################################################
### code chunk number 54: bivariate.Rnw:611-612
###################################################
f %$% covariance.matrix


###################################################
### code chunk number 55: bivariate.Rnw:616-617
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f)


###################################################
### code chunk number 56: bivariate.Rnw:628-629
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, FALSE)


###################################################
### code chunk number 57: bivariate.Rnw:636-638
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_cfield (function (y, z) f (0, y, z), c (-3, 3),
    xlab="y", ylab="z")


###################################################
### code chunk number 58: bivariate.Rnw:656-658
###################################################
f1 <- nbvpdf (0, 0, 1, 1, 0)
f1 %$% covariance.matrix


###################################################
### code chunk number 59: bivariate.Rnw:662-663
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f1, all=TRUE, n=20)


###################################################
### code chunk number 60: bivariate.Rnw:670-672
###################################################
f2 <- nbvpdf (0, 0, 1, 1, 0.75)
f2 %$% covariance.matrix


###################################################
### code chunk number 61: bivariate.Rnw:677-678
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f2, all=TRUE, n=20)


###################################################
### code chunk number 62: bivariate.Rnw:684-686
###################################################
f3 <- nbvpdf (0, 0, 1, 1, -0.75)
f3 %$% covariance.matrix


###################################################
### code chunk number 63: bivariate.Rnw:691-692
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f3, all=TRUE, n=20)


