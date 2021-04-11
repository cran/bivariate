#bivariate: Bivariate Probability Distributions
#Copyright (C), Abby Spurdle, 2018 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

setClass ("GBV", contains="DBV",
	slots = list (
		.level.names="list",
		nlevels="integer",
		p="matrix") )
	setClass ("GBVPMF", contains = c ("GBV", "DBV", "BV", "PMF", "function") )
setClass ("DTV", contains="CTV",
	slots = list (
		.constant="numeric",
		alpha="numeric") )
	setClass ("DTVPDF", contains = c ("DTV", "CTV", "TV", "PDF", "function") )

setClass ("KBV", contains="CBV",
	slots = list (
		variable.names="character",
		bw="numeric",
		n="integer",
		data="matrix") )
	setClass ("KBVPDF", contains = c ("KBV", "CBV", "BV", "PDF", "function") )

setClass ("EBV", contains="SBV",
	slots = list (
		variable.names="character",
		n="integer",
		data="matrix") )
	setClass ("EBVCDF", contains = c ("EBV", "SBV", "BV", "CDF", "function") )

gbvpmf = function (p)
{	sf = function (x, y)
	{	sf = .THIS ()
		.gbv.eval (sf, .gbvpmf.eval.ext, x, y)
	}

	nlevels.X = nrow (p)
	nlevels.Y = ncol (p)
	names.Xlevels = rownames (p)
	names.Ylevels = colnames (p)
	if (is.null (names.Xlevels) )
		names.Xlevels = as.character (1:nlevels.X)
	if (is.null (names.Ylevels) )
		names.Ylevels = as.character (1:nlevels.Y)
	nlevels = c (nlevels.X, nlevels.Y)

	new ("GBVPMF", sf,
		.class.info = .get.info ("GBVPMF"),
		.level.names = list (names.Xlevels, names.Ylevels),
		nlevels=nlevels,
		p = p / sum (p, na.rm=TRUE) )
}

dtvpdf = function (alpha.X, alpha.Y, alpha.Z)
{	sf = function (x, y, z = 1 - x - y, ..., log=FALSE)
	{	sf = .THIS ()
		.wtv.eval (sf, .dtvpdf.eval.ext, x, y, z, log=log)		
	}

	alpha = c (alpha.X, alpha.Y, alpha.Z)
	alpha = as.numeric (alpha)
	if (any (alpha <= 0) )
		stop ("alpha parameter <= 0")

	new ("DTVPDF", sf,
		.class.info = .get.info ("DTVPDF"),
		.constant = 1 / (prod (gamma (alpha) ) / gamma (sum (alpha) ) ),
		alpha=alpha)
}

kbvpdf = function (x, y, xbw, ybw, ..., xsmoothness=1, ysmoothness=1, data)
{	if (missing (data) )
	{	xname = as.character (substitute (x) )[1]
		yname = as.character (substitute (y) )[1]
		variable.names = c (xname, yname)
		x = as.numeric (x)
		y = as.numeric (y)
		if (length (x) != length (y) )
			stop ("lengths of x and y, not same")
		data = cbind (x, y)
		colnames (data) = variable.names
	}
	else
	{	data = as.matrix (data)
		variable.names = colnames (data)
	}

	sf = function (x, y)
		stop ("\nkbvpdf function objects can't be evaluated\n(consider the probhat package, or use the bvmat function)")

	if (missing (xbw) ) xbw = as.vector (diff (quantile (data [,1], c (0.0938, 0.9062) ) ) ) * xsmoothness
	if (missing (ybw) ) ybw = as.vector (diff (quantile (data [,2], c (0.0938, 0.9062) ) ) ) * ysmoothness

	new ("KBVPDF", sf,
		.class.info = .get.info ("KBVPDF"),
		variable.names=variable.names,
		bw = c (xbw, ybw),
		n = nrow (data),
		data=data)
}

ebvcdf = function (x, y, ..., data)
{	if (missing (data) )
	{	xname = as.character (substitute (x) )[1]
		yname = as.character (substitute (y) )[1]
		variable.names = c (xname, yname)
		x = as.numeric (x)
		y = as.numeric (y)
		if (length (x) != length (y) )
			stop ("lengths of x and y, not same")
		data = cbind (x, y)
		colnames (data) = variable.names
	}
	else
	{	data = as.matrix (data)
		variable.names = colnames (data)
	}

	sf = function (x, y)
	{	sf = .THIS ()
		.cbv.eval (sf, .ebvcdf.eval.ext, x, y)
	}

	new ("EBVCDF", sf,
		.class.info = .get.info ("EBVCDF"),
		variable.names=variable.names,
		n = nrow (data),
		data=data)
}

.gbvpmf.eval.ext = function (sf, x, y)
{	{{p = sf@p}}

	n = length (x)
	z = numeric (n)
	for (i in 1:n)
		z [i] = p [x [i], y [i] ]
	z
}

.dtvpdf.eval.ext = function (sf, x, log)
{	{{const = sf@.constant; alpha = sf@alpha}}
	n = nrow (x)
	v = numeric (n)
	for (i in seq_len (n) )
		v [i] = .dtvpdf.eval.ext2 (const, alpha, x [i,], log)
	v
}

.dtvpdf.eval.ext2 = function (const, alpha, x, log)
{	v = const * prod (x ^ (alpha - 1) )
	if (log) log (v)
	else v
}

.ebvcdf.eval.ext = function (sf, x, y)
{	n0 = sf@n
	x0 = sf@data [,1]
	y0 = sf@data [,2]

	n = length (x)
	z = numeric (n)
	for (i in seq_len (n) )
	{	Ix = (x0 <= x [i])
		Iy = (y0 <= y [i])
		z [i] = sum (Ix & Iy) / n0
	}
	z
}
