### R code from vignette source 'bivariate.Rnw'

###################################################
### code chunk number 1: bivariate.Rnw:41-46
###################################################
options(continue="   ")
options(SweaveHooks=list(fig=function()
par(mar=c(4.1, 4.1, 1.35, 1.6), cex=0.7, cex.main=1)))

set.seed (1)


###################################################
### code chunk number 2: bivariate.Rnw:98-102
###################################################
library (intoo)
library (barsurf)
library (bivariate)
library (MASS)


###################################################
### code chunk number 3: bivariate.Rnw:109-110
###################################################
set.bs.options (rendering.style="e")


###################################################
### code chunk number 4: bivariate.Rnw:118-119
###################################################
set.bs.theme ("gold")


###################################################
### code chunk number 5: bivariate.Rnw:137-139
###################################################
f <- dubvpmf (1, 1, 6, 6)
F <- dubvcdf (1, 1, 6, 6)


###################################################
### code chunk number 6: bivariate.Rnw:145-146
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE)


###################################################
### code chunk number 7: bivariate.Rnw:148-149
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, TRUE)


###################################################
### code chunk number 8: bivariate.Rnw:158-160
###################################################
f (2, 4)
F (2, 4)


###################################################
### code chunk number 9: bivariate.Rnw:168-172
###################################################
d.unif.pmf.eval <- function (x, a, b)    #x ignored
    1 / (b - a + 1)
d.unif.cdf.eval <- function (x, a, b)    #x used
    (x - a + 1) / (b - a + 1)


###################################################
### code chunk number 10: bivariate.Rnw:175-177
###################################################
d.unif.pmf.eval (2, 1, 6) * d.unif.pmf.eval (4, 1, 6)
d.unif.cdf.eval (2, 1, 6) * d.unif.cdf.eval (4, 1, 6)


###################################################
### code chunk number 11: bivariate.Rnw:201-203
###################################################
f <- bnbvpmf (0.5, 0.25, 10)
F <- bnbvcdf (0.5, 0.25, 10)


###################################################
### code chunk number 12: bivariate.Rnw:210-211
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE)


###################################################
### code chunk number 13: bivariate.Rnw:213-214
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, TRUE)


###################################################
### code chunk number 14: bivariate.Rnw:225-226
###################################################
set.bs.theme ("blue")


###################################################
### code chunk number 15: bivariate.Rnw:253-255
###################################################
f <- pbvpmf.2 (8, 8, 2)
F <- pbvcdf.2 (8, 8, 2)


###################################################
### code chunk number 16: bivariate.Rnw:261-262
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f)


###################################################
### code chunk number 17: bivariate.Rnw:264-265
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE)


###################################################
### code chunk number 18: bivariate.Rnw:267-268
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F)


###################################################
### code chunk number 19: bivariate.Rnw:274-275
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, TRUE)


###################################################
### code chunk number 20: bivariate.Rnw:289-293
###################################################
h <- sample (1:24)
h <- matrix (h, 6, 4)
rownames (h) <- LETTERS [1:6]
colnames (h) <- letters [1:4]


###################################################
### code chunk number 21: bivariate.Rnw:296-297
###################################################
h


###################################################
### code chunk number 22: bivariate.Rnw:300-301
###################################################
f <- cbvpmf (h)


###################################################
### code chunk number 23: bivariate.Rnw:308-309
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f)


###################################################
### code chunk number 24: bivariate.Rnw:311-313
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE,
    arrows=FALSE)


###################################################
### code chunk number 25: bivariate.Rnw:319-321
###################################################
f (2, 4)
f ("B", "d")


###################################################
### code chunk number 26: bivariate.Rnw:327-328
###################################################
set.bs.theme ("gold")


###################################################
### code chunk number 27: bivariate.Rnw:338-340
###################################################
f <- cubvpdf (0, 0, 2, 2)
F <- cubvcdf (0, 0, 2, 2)


###################################################
### code chunk number 28: bivariate.Rnw:346-347
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE)


###################################################
### code chunk number 29: bivariate.Rnw:353-354
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, TRUE)


###################################################
### code chunk number 30: bivariate.Rnw:363-364
###################################################
set.bs.theme ("blue")


###################################################
### code chunk number 31: bivariate.Rnw:376-378
###################################################
f <- nbvpdf (0, 0, 1, 1, 0)
F <- nbvcdf (0, 0, 1, 1, 0)


###################################################
### code chunk number 32: bivariate.Rnw:385-386
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f)


###################################################
### code chunk number 33: bivariate.Rnw:388-389
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE)


###################################################
### code chunk number 34: bivariate.Rnw:395-396
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F)


###################################################
### code chunk number 35: bivariate.Rnw:398-399
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, TRUE)


###################################################
### code chunk number 36: bivariate.Rnw:415-421
###################################################
f <- bmbvpdf (
    3.5, 0, 1, 1,    #first component distribution
    6.5, 0, 1, 1)    #second component distribution
F <- bmbvcdf (
    3.5, 0, 1, 1,    #first component distribution
    6.5, 0, 1, 1)    #second component distribution


###################################################
### code chunk number 37: bivariate.Rnw:427-428
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f)


###################################################
### code chunk number 38: bivariate.Rnw:434-436
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE,
    arrows = c (FALSE, TRUE), xat = c (3.5, 6.5) )


###################################################
### code chunk number 39: bivariate.Rnw:438-439
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F)


###################################################
### code chunk number 40: bivariate.Rnw:445-447
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (F, TRUE,
    arrows = c (FALSE, TRUE), xat = c (3.5, 6.5) )


###################################################
### code chunk number 41: bivariate.Rnw:464-465
###################################################
f <- dtvpdf (2, 4, 6)


###################################################
### code chunk number 42: bivariate.Rnw:472-473
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f)


###################################################
### code chunk number 43: bivariate.Rnw:475-476
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE)


###################################################
### code chunk number 44: bivariate.Rnw:484-485
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f, TRUE, log=TRUE)


###################################################
### code chunk number 45: bivariate.Rnw:505-507
###################################################
data ("geyser")
attach (geyser)


###################################################
### code chunk number 46: bivariate.Rnw:510-511
###################################################
fh <- kbvpdf (duration, waiting, 0.7, 7)


###################################################
### code chunk number 47: bivariate.Rnw:518-520
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (fh,
    xlab="duration", ylab="waiting")


###################################################
### code chunk number 48: bivariate.Rnw:523-525
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (fh, TRUE,
    xlab="duration", ylab="waiting")


###################################################
### code chunk number 49: bivariate.Rnw:529-530
###################################################
detach (geyser)


###################################################
### code chunk number 50: bivariate.Rnw:551-553
###################################################
x <- rnorm (20)
y <- rnorm (20)


###################################################
### code chunk number 51: bivariate.Rnw:556-557
###################################################
Fh <- ebvcdf (x, y)


###################################################
### code chunk number 52: bivariate.Rnw:563-565
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (Fh,
    xyrel="f")


###################################################
### code chunk number 53: bivariate.Rnw:571-572
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (Fh, TRUE)


###################################################
### code chunk number 54: bivariate.Rnw:608-610
###################################################
f1 <- nbvpdf (0, 0, 1, 1, 0)
f1 %$% matrix.variances


###################################################
### code chunk number 55: bivariate.Rnw:614-615
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f1, all=TRUE, n=20)


###################################################
### code chunk number 56: bivariate.Rnw:622-624
###################################################
f2 <- nbvpdf (0, 0, 1, 1, 0.75)
f2 %$% matrix.variances


###################################################
### code chunk number 57: bivariate.Rnw:629-630
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f2, all=TRUE, n=20)


###################################################
### code chunk number 58: bivariate.Rnw:636-638
###################################################
f3 <- nbvpdf (0, 0, 1, 1, -0.75)
f3 %$% matrix.variances


###################################################
### code chunk number 59: bivariate.Rnw:643-644
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (f3, all=TRUE, n=20)


###################################################
### code chunk number 60: bivariate.Rnw:657-658
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (dtvpdf (1, 1, 1), TRUE)


###################################################
### code chunk number 61: bivariate.Rnw:665-666
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (dtvpdf (2, 2, 2), TRUE)


###################################################
### code chunk number 62: bivariate.Rnw:674-675
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (dtvpdf (0.5, 0.5, 0.5), TRUE)


