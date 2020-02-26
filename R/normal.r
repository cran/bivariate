#bivariate: Bivariate Probability Distributions
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

nbvpdf = function (mean.X, mean.Y, sd.X, sd.Y, cor)
	nbvpdf.2 (mean.X, mean.Y, sd.X ^ 2, sd.Y ^ 2, sd.X * sd.Y * cor)

nbvpdf.2 = function (mean.X, mean.Y, var.X, var.Y, cov)
{	f = function (x, y)
	{	. = THAT ()
		v = .val.numeric.args (x, y)
		.nbvpdf.eval (., v$x, v$y)
	}

	vector.means = c (X=mean.X, Y=mean.Y)
	matrix.variances = c (var.X, cov, cov, var.Y)
	matrix.variances = matrix (matrix.variances, 2, 2)
	rownames (matrix.variances) = colnames (matrix.variances) = c ("X", "Y")

	f = .bv (f)
	EXTEND (f, "nbvpdf", vector.means, matrix.variances)
}

.nbvpdf.eval = function (., x, y)
	dmvnorm (cbind (x, y), .$vector.means, .$matrix.variances)

nbvcdf = function (mean.X, mean.Y, sd.X, sd.Y, cor)
	nbvcdf.2 (mean.X, mean.Y, sd.X ^ 2, sd.Y ^ 2, sd.X * sd.Y * cor)	

nbvcdf.2 = function (mean.X, mean.Y, var.X, var.Y, cov)
{	f = function (x, y)
	{	. = THAT ()
		v = .val.numeric.args (x, y)
		.nbvcdf.eval (., v$x, v$y)
	}

	vector.means = c (mean.X, mean.Y)
	matrix.variances = c (var.X, cov, cov, var.Y)
	matrix.variances = matrix (matrix.variances, nrow=2, ncol=2)

	f = .bv (f)
	EXTEND (f, "nbvcdf", vector.means, matrix.variances)
}

.nbvcdf.eval = function (., x, y)
{   n = length (x)
    xy = cbind (x, y)
    z = numeric (n)
    for (i in 1:n)
        z [i] = pmvnorm (c (-Inf, -Inf), xy [i,], .$vector.means, sigma=.$matrix.variances)
    z
}

.plot.nbv = function (f, plot.3d, xlim, ylim, n, ..., is.cdf=FALSE)
{   . = attributes (f)
	if (missing (xlim) )
		xlim = .$vector.means [1] + c (-3, 3) * sqrt (.$matrix.variances [1, 1])
	if (missing (ylim) )
		ylim = .$vector.means [2] + c (-3, 3) * sqrt (.$matrix.variances [2, 2])
	v = .continuous.outer (f, xlim, ylim, n)
	.plot.bv (TRUE, plot.3d, is.cdf, v$x, v$y, v$z, ...)
}

plot.nbvpdf = function (x, plot.3d=FALSE, ..., xlim, ylim, n=30, all=FALSE)
{  	if (all)
	{	F = nbvcdf (0, 0, 0, 0, 0)
		.plot.bv.all (x, F, ..., xlim=xlim, ylim=ylim, n=n)
	}
	else
		.plot.nbv (x, plot.3d, xlim, ylim, n, ...)
}

plot.nbvcdf = function (x, plot.3d=FALSE, ..., xlim, ylim, n=30)
	.plot.nbv (x, plot.3d, xlim, ylim, n, ..., is.cdf=TRUE)
