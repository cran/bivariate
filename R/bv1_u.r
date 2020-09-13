#bivariate: Bivariate Probability Distributions
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.dubv = function (f, CV, a.X, b.X, a.Y, b.Y)
{	a = c (a.X, a.Y)
	b = c (b.X, b.Y)
	n = 1 + b - a
	p = 1 / (n [1] * n [2])

	EXTEND (f, CV, p, n, a, b)
}

.cubv = function (f, CV, a.X, b.X, a.Y, b.Y)
{	a = c (a.X, a.Y)
	b = c (b.X, b.Y)
	p = 1 / (b [1] - a [1]) / (b [2] - a [2])

	EXTEND (f, CV, p, a, b)
}

dubvpmf = function (a.X, b.X, a.Y, b.Y)
	.dubv (.dubvpmf.eval, .CV.dubvpmf, a.X, b.X, a.Y, b.Y)
dubvcdf = function (a.X, b.X, a.Y, b.Y)
	.dubv (.dubvcdf.eval, .CV.dubvcdf, a.X, b.X, a.Y, b.Y)

cubvpdf = function (a.X, b.X, a.Y, b.Y)
	.cubv (.cubvpdf.eval, .CV.cubvpdf, a.X, b.X, a.Y, b.Y)
cubvcdf = function (a.X, b.X, a.Y, b.Y)
	.cubv (.cubvcdf.eval, .CV.cubvcdf, a.X, b.X, a.Y, b.Y)

.dubvpmf.eval = function (x, y)
{	. = THAT ()
	.dbv.eval (., .dubvpmf.eval.ext, x, y)
}

.dubvcdf.eval = function (x, y)
{	. = THAT ()
	.dbv.eval (., .dubvcdf.eval.ext, x, y)
}

.cubvpdf.eval = function (x, y)
{	. = THAT ()
	.cbv.eval (., .cubvpdf.eval.ext, x, y)
}

.cubvcdf.eval = function (x, y)
{	. = THAT ()
	.cbv.eval (., .cubvcdf.eval.ext, x, y)
}

.dubvpmf.eval.ext = function (., x, y)
{	z = rep (.$p, length (x) )
	z [x < .$a [1] | x > .$b [1] | y < .$a [2] | y > .$b [2]] = 0
	z
}

.dubvcdf.eval.ext = function (., x, y)
{	x [x > .$b [1] ] = .$b [1]
	y [y > .$b [2] ] = .$b [2]
	z1 = (1 + x - .$a [1]) / .$n [1]
	z2 = (1 + y - .$a [2]) / .$n [2]
	z = z1 * z2
	z [x < .$a [1] | y < .$a [2] ] = 0
	z
}

.cubvpdf.eval.ext = function (., x, y)
{	z = rep (.$p, length (x) )
	z [x < .$a [1] | x > .$b [1] | y < .$a [2] | y > .$b [2]] = 0
	z
}

.cubvcdf.eval.ext = function (., x, y)
{	x [x > .$b [1] ] = .$b [1]
	y [y > .$b [2] ] = .$b [2]
	z1 = (x - .$a [1]) / (.$b [1] - .$a [1])
	z2 = (y - .$a [2]) / (.$b [2] - .$a [2])
	z = z1 * z2
	z [x < .$a [1] | y < .$a [2] ] = 0
	z
}
