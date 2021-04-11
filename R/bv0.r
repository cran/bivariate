#bivariate: Bivariate Probability Distributions
#Copyright (C), Abby Spurdle, 2018 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

setClass ("PDF", contains="function")
setClass ("PMF", contains="function")
setClass ("CDF", contains="function")

setClass ("BV", slots = c (.class.info="character") )
	setClass ("DBV", contains="BV")
	setClass ("SBV", contains="BV")
	setClass ("CBV", contains="BV")
setClass ("TV", slots = c (.class.info="character") )
	setClass ("CTV", contains="TV")

setClass ("PMatrix",
	slots = c (
		fv="matrix",
		x="numeric",
		y="numeric") )

setMethod ("show", "BV", function (object) print (object) )
setMethod ("show", "TV", function (object) print (object) )

.class.info = matrix (c (
	"DUBVPMF", "Bivariate Discrete Uniform Mass Function",           "f (x, y)",
	"BNBVPMF", "Bivariate Binomial Mass Function",                   "f (x, y)",
	"PBVPMF",  "Bivariate Poisson Mass Function",                    "f (x, y)",
	"GBVPMF",  "Bivariate Categorical Mass Function",                "f (x, y)",
	"CUBVPDF", "Bivariate Continuous Uniform Density Function",      "f (x, y)",
	"NBVPDF",  "Bivariate Normal Density Function",                  "f (x, y)",
	"BMBVPDF", "Bivariate Bimodal Density Function",                 "f (x, y)",
	"DUBVCDF", "Bivariate Discrete Uniform Distribution Function",   "F (x, y)",
	"BNBVCDF", "Bivariate Binomial Distribution Function",           "F (x, y)",
	"PBVCDF",  "Bivariate Poisson Distribution Function",            "F (x, y)",
	"CUBVCDF", "Bivariate Continuous Uniform Distribution Function", "F (x, y)",
	"NBVCDF",  "Bivariate Normal Distribution Function",             "F (x, y)",
	"BMBVCDF", "Bivariate Bimodal Distribution Function",            "F (x, y)",
	"NTVPDF",  "Trivariate Normal Density Function",                 "f (x, y, z)",
	"DTVPDF",  "Trivariate Dirichlet Density Function",              "f (x, y, z = 1 - x - y)",
	"KBVPDF",  "Bivariate Kernel Density Estimate",   "equivalent to fh (x, y)",
	"EBVCDF",  "Bivariate Empirical Distribution Function",         "Fh (x, y)"
	),, 3, byrow=TRUE)

.get.info = function (cn)
{	I = match (cn, .class.info [,1])
	if (is.na (I [1]) )
		stop ("constructor error")
	.class.info [I, 2:3]
}

.THIS = function ()
	sys.function (-1)

bv.printf = function (...) UseMethod ("bv.printf")
bv.plotf = function (...) UseMethod ("bv.plotf")
bvrng = function (...) UseMethod ("bvrng")
bvmat = function (...) UseMethod ("bvmat")

print.BV = function (x, ...) .bv.printf (x)
print.TV = function (x, ...) .bv.printf (x)
plot.BV = function (x, ...) bv.plotf (x, ...)
plot.TV = function (x, ...) bv.plotf (x, ...)

bv.printf.BV = function (sf, ...) .bv.printf (sf)
bv.printf.TV = function (sf, ...) .bv.printf (sf)

.bv.printf = function (sf, ...)
{	cat ("<S4-Based Function Object>\n")
	cat (sf@.class.info [1], "\n", sep="")
	cat ("    ", sf@.class.info [2], "\n", sep="")
}

rim.litmus.fit = function (x)
	rainbow.litmus.fit (x, c=10, l=82.5, start=220, end=0)

.dbv.eval = function (sf, evalf, x, y, min, max)
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
	evalf (sf, v [,1], v [,2])
}

.cbv.eval = function (sf, evalf, x, y)
{	x = as.numeric (x)
	y = as.numeric (y)
	v = cbind (x, y, deparse.level=0)
	.val.finite (v)
	evalf (sf, v [,1], v [,2])
}

.gbv.eval = function (sf, evalf, x, y)
{	if (is.character (x) )
	{	x = match (x, sf@.level.names [[1]])
		if (any (is.na (x) ) )
			.gbv.eval.err ()
	}
	if (is.character (y) )
	{	y = match (y, sf@.level.names [[2]])
		if (any (is.na (y) ) )
			.gbv.eval.err ()
	}
	.dbv.eval (sf, evalf, x, y, c (1, 1), sf@nlevels)
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
	list (n=n, x=x, y=y)
}

#mockup function objects
fobj.f = function (x, y) 0
fobj.F = function (x, y) 0
fobj.gf = function (a, b) 0
fobj.wf = function (x, y, z = 1 - x - y, ..., log=FALSE) 0
fobj.f3 = function (x, y, z) 0
fobj.fh = function (x, y) 0
fobj.Fh = function (x, y) 0

"%$%" = function (object, name)
{	warning ("%$% operator deprecated, use @")
	attr (object, as.character (substitute (name) ) )

}
