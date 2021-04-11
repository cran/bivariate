#bivariate: Bivariate Probability Distributions
#Copyright (C), Abby Spurdle, 2018 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

setClass ("DUBV", contains="DBV",
	slots = list (
		p="numeric",
		n="integer",
		a="numeric",
		b="numeric") )
	setClass ("DUBVPMF", contains = c ("DUBV", "DBV", "BV", "PMF", "function") )
	setClass ("DUBVCDF", contains = c ("DUBV", "DBV", "BV", "CDF", "function") )

setClass ("CUBV", contains="CBV",
	slots = list (
		p="numeric",
		a="numeric",
		b="numeric") )
	setClass ("CUBVPDF", contains = c ("CUBV", "CBV", "BV", "PDF", "function") )
	setClass ("CUBVCDF", contains = c ("CUBV", "CBV", "BV", "CDF", "function") )

.dubv = function (sf, CV, a.X, b.X, a.Y, b.Y)
{	a = as.integer (c (a.X, a.Y) )
	b = as.integer (c (b.X, b.Y) )
	n = 1L + b - a
	p = 1 / (n [1] * n [2])

	new (CV, sf,
		.class.info = .get.info (CV),
		p=p,
		n=n,
		a=a,
		b=b)
}

.cubv = function (sf, CV, a.X, b.X, a.Y, b.Y)
{	a = c (a.X, a.Y)
	b = c (b.X, b.Y)
	p = 1 / (b [1] - a [1]) / (b [2] - a [2])

	new (CV, sf,
	.class.info = .get.info (CV),
		p=p,
		a=a,
		b=b)
}

dubvpmf = function (a.X, b.X, a.Y, b.Y)
	.dubv (.dubvpmf.eval, "DUBVPMF", a.X, b.X, a.Y, b.Y)
dubvcdf = function (a.X, b.X, a.Y, b.Y)
	.dubv (.dubvcdf.eval, "DUBVCDF", a.X, b.X, a.Y, b.Y)

cubvpdf = function (a.X, b.X, a.Y, b.Y)
	.cubv (.cubvpdf.eval, "CUBVPDF", a.X, b.X, a.Y, b.Y)
cubvcdf = function (a.X, b.X, a.Y, b.Y)
	.cubv (.cubvcdf.eval, "CUBVCDF", a.X, b.X, a.Y, b.Y)

.dubvpmf.eval = function (x, y)
{	sf = .THIS ()
	.dbv.eval (sf, .dubvpmf.eval.ext, x, y)
}

.dubvcdf.eval = function (x, y)
{	sf = .THIS ()
	.dbv.eval (sf, .dubvcdf.eval.ext, x, y)
}

.cubvpdf.eval = function (x, y)
{	sf = .THIS ()
	.cbv.eval (sf, .cubvpdf.eval.ext, x, y)
}

.cubvcdf.eval = function (x, y)
{	sf = .THIS ()
	.cbv.eval (sf, .cubvcdf.eval.ext, x, y)
}

.dubvpmf.eval.ext = function (sf, x, y)
{	{{a = sf@a; b = sf@b}}
	z = rep (sf@p, length (x) )
	z [x < a [1] | x > b [1] | y < a [2] | y > b [2]] = 0
	z
}

.dubvcdf.eval.ext = function (sf, x, y)
{	{{n = sf@n; a = sf@a; b = sf@b}}
	x [x > b [1] ] = b [1]
	y [y > b [2] ] = b [2]
	z1 = (1 + x - a [1]) / n [1]
	z2 = (1 + y - a [2]) / n [2]
	z = z1 * z2
	z [x < a [1] | y < a [2] ] = 0
	z
}

.cubvpdf.eval.ext = function (sf, x, y)
{	{{a = sf@a; b = sf@b; p=sf@p}}
	z = rep (p, length (x) )
	z [x < a [1] | x > b [1] | y < a [2] | y > b [2]] = 0
	z
}

.cubvcdf.eval.ext = function (sf, x, y)
{	{{a = sf@a; b = sf@b}}
	x [x > b [1] ] = b [1]
	y [y > b [2] ] = b [2]
	z1 = (x - a [1]) / (b [1] - a [1])
	z2 = (y - a [2]) / (b [2] - a [2])
	z = z1 * z2
	z [x < a [1] | y < a [2] ] = 0
	z
}
