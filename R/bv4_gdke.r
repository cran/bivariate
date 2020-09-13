#bivariate: Bivariate Probability Distributions
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

gbvpmf = function (p)
{	f = function (x, y)
	{	. = THAT ()
		.gbv.eval (., .gbvpmf.eval.ext, x, y)
	}

	ngx = nrow (p)
	ngy = ncol (p)
	xnames = rownames (p)
	ynames = colnames (p)
	if (is.null (xnames) )
		xnames = as.character (1:ngx)
	if (is.null (ynames) )
		ynames = as.character (1:ngy)

	EXTEND (f, .CV.gbvpmf, ngx, ngy, xnames, ynames, p = p / sum (p, na.rm=TRUE) )
}

dtvpdf = function (alpha.X, alpha.Y, alpha.Z)
{	f = function (x, y, z = 1 - x - y, ..., log=FALSE)
	{	. = THAT ()
		.wtv.eval (., .dtvpdf.eval.ext, x, y, z, log=log)		
	}

	alpha = c (alpha.X, alpha.Y, alpha.Z)
	alpha = as.numeric (alpha)
	if (any (alpha <= 0) )
		stop ("alpha parameter <= 0")

	.constant = 1 / (prod (gamma (alpha) ) / gamma (sum (alpha) ) )

	EXTEND (f, .CV.dtvpdf, .constant, alpha)
}

kbvpdf = function (x, y, xbw, ybw)
{	xname = as.character (substitute (x) )
	yname = as.character (substitute (y) )

	f = function (x, y)
		stop ("\nkbvpdf objects can't be evaluated\n(consider the probhat package, or use the bvmat function)")

	n = 0
	UNPACK (.val.data (x, y) )
	if (missing (xbw) ) xbw = as.vector (diff (quantile (x, c (0.0938, 0.9062) ) ) )
	if (missing (ybw) ) ybw = as.vector (diff (quantile (y, c (0.0938, 0.9062) ) ) )

	EXTEND (f, .CV.kbvpdf,
		xname, yname, xbw, ybw, n, x, y)
}

ebvcdf = function (x, y)
{	xname = as.character (substitute (x) )
	yname = as.character (substitute (y) )

	f = function (x, y)
	{	. = THAT ()
		.cbv.eval (., .ebvcdf.eval.ext, x, y)
	}

	n = 0
	UNPACK (.val.data (x, y) )
	EXTEND (f, .CV.ebvcdf, xname, yname, n, x, y)
}

.gbvpmf.eval.ext = function (., x, y)
{	n = length (x)
	z = numeric (n)
	for (i in 1:n)
		z [i] = .$p [x [i], y [i] ]
	z
}

.dtvpdf.eval.ext = function (., x, log)
{	n = nrow (x)
	v = numeric (n)
	for (i in seq_len (n) )
		v [i] = .dtvpdf.eval.ext2 (., x [i,], log)
	v
}

.dtvpdf.eval.ext2 = function (., x, log)
{	v = .$.constant * prod (x ^ (.$alpha - 1) )
	if (log) log (v)
	else v
}

.ebvcdf.eval.ext = function (., x, y)
{	n = length (x)
	z = numeric (n)
	for (i in 1:n)
		z [i] = .ebvcdf.eval.ext2 (., x [i], y [i])
	z
}

.ebvcdf.eval.ext2 = function (., x, y)
{	Ix = (.$x <= x)
	Iy = (.$y <= y)
	sum (Ix & Iy) / .$n
}
