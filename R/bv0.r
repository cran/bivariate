#bivariate: Bivariate Probability Distributions
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.CV.dubvpmf = c ("dubvpmf", "pmf", "dubv", "dbv", "bv")
.CV.dubvcdf = c ("dubvcdf", "cdf", "dubv", "dbv", "bv")
.CV.cubvpdf = c ("cubvpdf", "pdf", "cubv", "cbv", "bv")
.CV.cubvcdf = c ("cubvcdf", "cdf", "cubv", "cbv", "bv")

.CV.bnbvpmf = c ("bnbvpmf", "pmf", "bnbv", "dbv", "bv")
.CV.bnbvcdf = c ("bnbvcdf", "cdf", "bnbv", "dbv", "bv")
.CV.pbvpmf = c ("pbvpmf", "pmf", "pbv", "dbv", "bv")
.CV.pbvcdf = c ("pbvcdf", "cdf", "pbv", "dbv", "bv")

.CV.nbvpdf = c ("nbvpdf", "pdf", "nbv", "cbv", "bv")
.CV.nbvcdf = c ("nbvcdf", "cdf", "nbv", "cbv", "bv")
.CV.bmbvpdf = c ("bmbvpdf", "pdf", "bmbv", "cbv", "bv")
.CV.bmbvcdf = c ("bmbvcdf", "cdf", "bmbv", "cbv", "bv")
.CV.ntvpdf = c ("ntvpdf", "pdf", "ntv", "ctv", "tv")

.CV.gbvpmf = c ("gbvpmf", "pmf", "gbv", "dbv", "bv")
.CV.dtvpdf = c ("dtvpdf", "pdf", "dtv", "ctv", "tv")
.CV.kbvpdf = c ("kbvpdf", "pdf", "kbv", "cbv", "bv")
.CV.ebvcdf = c ("ebvcdf", "cdf", "ebv", "cbv", "bv")

bvplot = function (...) UseMethod ("bvplot")
bvrng = function (...) UseMethod ("bvrng")
bvmat = function (...) UseMethod ("bvmat")

print.bv = function (x, ...) object.summary (x, ...)
print.tv = function (x, ...) object.summary (x, ...)
plot.bv = function (x, ...) bvplot (x, ...)
plot.tv = function (x, ...) bvplot (x, ...)

rim.litmus.fit = function (x)
	rainbow.litmus.fit (x, c=10, l=82.5, start=220, end=0)

gpd.litmus.fit = function (x)
{	colvs = cbind (c (90, 80, 80, 70), 35, 70)
	litmus.fit (x, colvs, color.space="HCL")
}

.dbv.eval = function (., f, x, y, min, max)
{	x = as.integer (x)
	y = as.integer (y)
	v = cbind (x, y, deparse.level=0)
	.val.finite (v)
	if (! missing (min) )
	{	if (any (v [,1] < min [1]) )
			stop ("x evaluation value outside (below) supported region")
		if (any (v [,2] < min [2]) )
			stop ("y evaluation value outside (below) supported region")
	}
	if (! missing (max) )
	{	if (any (v [,1] > max [1]) )
			stop ("x evaluation value outside (above) supported region")
		if (any (v [,2] > max [2]) )
			stop ("y evaluation value outside (above) supported region")
	}
	f (., v [,1], v [,2])
}

.cbv.eval = function (., f, x, y)
{	x = as.numeric (x)
	y = as.numeric (y)
	v = cbind (x, y, deparse.level=0)
	.val.finite (v)
	f (., v [,1], v [,2])
}

.gbv.eval = function (., f, x, y)
{	if (is.character (x) )
	{	x = match (x, .$xnames)
		if (any (is.na (x) ) )
			.gbv.eval.err ()
	}
	if (is.character (y) )
	{	y = match (y, .$ynames)
		if (any (is.na (y) ) )
			.gbv.eval.err ()
	}
	.dbv.eval (., f, x, y, c (1, 1), c (.$ngx, .$ngy) )
}

.gbv.eval.err = function ()
	stop ("evaluation name not in category names")

.wtv.eval = function (., f, x, y, z, log)
{	x = as.numeric (x)
	y = as.numeric (y)
	z = as.numeric (z)
	v = cbind (x, y, z, deparse.level=0)
	.val.finite (v, 3)
	if (any (v <= 0 | v >= 1) )
		stop ("x, y and z must be in interval (0, 1)")
	rsum = v [,1] + v [,2] + v [,3]
	err = abs (rsum - 1)
	v = v / rsum
	if (any (err > 1e-6) )
		warning ("x, y and z don't sum to one")
	f (., v, log)
}

.ctv.eval = function (., f, x, y, z)
{	x = as.numeric (x)
	y = as.numeric (y)
	z = as.numeric (z)
	v = cbind (x, y, z, deparse.level=0)
	.val.finite (v, 3)
	f (., v [,1], v [,2], v [,3])
}

.val.finite = function (x, n=2)
{	if (nrow (x) == 0 || n != ncol (x) )
		stop ("\nunsuitable evaluation bins/points\n(possibly NULL or zero-length vectors)")
	if (! all (is.finite (x) ) )
		stop ("evaluation bins/points need to be finite")
}

.val.data = function (x, y)
{	x = as.numeric (x)
	y = as.numeric (y)
	n = length (x)
	if (n != length (y) )
		stop ("length (x) != length (y)")
	LIST (n, x, y)
}

#mockup function objects
mf.f = function (x, y) 0
mf.F = function (x, y) 0
mf.gf = function (a, b) 0
mf.wf = function (x, y, z = 1 - x - y, ..., log=FALSE) 0
mf.f3 = function (x, y, z) 0
mf.fh = function (x, y) 0
mf.Fh = function (x, y) 0
