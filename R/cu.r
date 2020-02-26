#bivariate: Bivariate Probability Distributions
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

cubvpdf = function (a.X, a.Y, b.X, b.Y)
{	f = function (x, y)
	{	. = THAT ()
		v = .val.numeric.args (x, y)
		.cubvpdf.eval (., v$x, v$y)
	}

	f = .bv (f)
	EXTEND (f, "cubvpdf",
		a = c (a.X, a.Y),
		b = c (b.X, b.Y)
	)
}

.cubvpdf.eval = function (., x, y)
{	z = rep (1 / (.$b [1] - .$a [1]) / (.$b [2] - .$a [2]), length (x) )
	z [x < .$a [1] | x > .$b [1] | y < .$a [2] | y > .$b [2]] = 0
	z
}

cubvcdf = function (a.X, a.Y, b.X, b.Y)
{	f = function (x, y)
	{	. = THAT ()
		v = .val.numeric.args (x, y)
		.cubvcdf.eval (., v$x, v$y)
	}

	f = .bv (f)
	EXTEND (f, "cubvcdf",
		a = c (a.X, a.Y),
		b = c (b.X, b.Y)
	)
}

.cubvcdf.eval = function (., x, y)
{	x [x > .$b [1] ] = .$b [1]
	y [y > .$b [2] ] = .$b [2]
	z1 = (x - .$a [1]) / (.$b [1] - .$a [1])
	z2 = (y - .$a [2]) / (.$b [2] - .$a [2])
	z = z1 * z2
	z [x < .$a [1] | y < .$a [2] ] = 0
	z
}

.plot.cubv = function (f, plot.3d, xlim, ylim, n, ...)
{	. = attributes (f)
	if (missing (xlim) )
		xlim = c (.$a [1], .$b [1])
	if (missing (ylim) )
		ylim = c (.$a [2], .$b [2])
	v = .continuous.outer (f, xlim, ylim, n)
	.plot.bv (TRUE, plot.3d, TRUE, v$x, v$y, v$z, ..., contours=FALSE)
}

plot.cubvpdf = function (x, plot.3d=FALSE, ..., xlim, ylim, n=20, all=FALSE)
{	if (all)
	{	F = cubvcdf (0, 0, 0, 0)
		.plot.bv.all (x, F, ..., xlim=xlim, ylim=ylim, n=n)
	}
	else
		.plot.cubv (x, plot.3d, xlim, ylim, n, ...)
}

plot.cubvcdf = function (x, plot.3d=FALSE, ..., xlim, ylim, n=20)
	.plot.cubv (x, plot.3d, xlim, ylim, n, ...)
