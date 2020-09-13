#bivariate: Bivariate Probability Distributions
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.discrete.outer = function (f, xlim, ylim)
{	x = xlim [1]:xlim [2]
	y = ylim [1]:ylim [2]
	fv = outer (x, y, f)
	LIST (fv, x, y)
}

.continuous.outer = function (f, xlim, ylim, n)
{	n = rep_len (n, 2)
	x = seq (xlim [1], xlim [2], length.out = n [1])
	y = seq (ylim [1], ylim [2], length.out = n [2])
	fv = outer (x, y, f)
	LIST (fv, x, y)
}

bvmat.dubv = function (sf, xlim, ylim, ...)
{	. = attributes (sf)
	if (missing (xlim) ) xlim = c (.$a [1], .$b [1])
	if (missing (ylim) ) ylim = c (.$a [2], .$b [2])
	.discrete.outer (sf, xlim, ylim)
}

bvmat.cubv = function (sf, xlim, ylim, ..., n=10)
{	. = attributes (sf)
	if (missing (xlim) ) xlim = c (.$a [1], .$b [1])
	if (missing (ylim) ) ylim = c (.$a [2], .$b [2])
	.continuous.outer (sf, xlim, ylim, n)
}

bvmat.bnbv = function (sf, xlim, ylim, ...)
{	. = attributes (sf)
	if (missing (xlim) ) xlim = c (0, .$n)
	if (missing (ylim) ) ylim = c (0, .$n)
	.discrete.outer (sf, xlim, ylim)
}
	
bvmat.pbv = function (sf, xlim, ylim, ...)
{	. = attributes (sf)
	if (missing (xlim) ) xlim = c (0, 2 * (.$lambda [1] + .$lambda [3]))
	if (missing (ylim) ) ylim = c (0, 2 * (.$lambda [2] + .$lambda [3]))
	if (inherits (sf, "pbvpmf") )
		.discrete.outer (sf, xlim, ylim)
	else
	{	lambda = sf %$% "lambda"
		sf = pbvpmf (lambda [1], lambda [2], lambda [3])
		s = .discrete.outer (sf, c (0, xlim [2]), c (0, ylim [2]) )

		nx = diff (xlim) + 1
		ny = diff (ylim) + 1
		fv = matrix (0, nx, ny)

		for (i in 1:nx)
		{	for (j in 1:ny)
			{	I = 1:(xlim [1] + i - 1)
				J = 1:(ylim [1] + j - 1)
				fv [i, j] = sum (s$fv [I, J])
			}
		}

		list (fv=fv, x=s$x, y=s$y)
	}
}

bvmat.nbv = function (sf, xlim, ylim, ..., n=10)
{	. = attributes (sf)
	if (missing (xlim) ) xlim = .$mean.vector [1] + c (-3, 3) * sqrt (.$covariance.matrix [1, 1])
	if (missing (ylim) ) ylim = .$mean.vector [2] + c (-3, 3) * sqrt (.$covariance.matrix [2, 2])
	.continuous.outer (sf, xlim, ylim, n)
}

bvmat.bmbv = function (sf, xlim, ylim, ..., n=10)
{	. = attributes (sf)
	if (missing (xlim) )
	{	xlim1 = .$mean.vector.1 [1] + c (-3, 3) * sqrt (.$covariance.matrix.1 [1, 1])
		xlim2 = .$mean.vector.2 [1] + c (-3, 3) * sqrt (.$covariance.matrix.2 [1, 1])
		xlim = range (c (xlim1, xlim2) )
	}
	if (missing (ylim) )
	{	ylim1 = .$mean.vector.1 [2] + c (-3, 3) * sqrt (.$covariance.matrix.1 [2, 2])
		ylim2 = .$mean.vector.2 [2] + c (-3, 3) * sqrt (.$covariance.matrix.2 [2, 2])
		ylim = range (c (ylim1, ylim2) )
	}
	.continuous.outer (sf, xlim, ylim, n)
}

bvmat.gbv = function (sf, ...)
{	p = sf %$% "p"
	dims = dim (p)
	list (fv=p, x = 1:dims [1], y = 1:dims [2])
}

bvmat.dtv = function (sf, ..., log=FALSE, n=10)
{	. = attributes (sf)

	x = y = seq (0.001, 0.998, length.out=n)
	fv = matrix (NA, n, n)
	for (i in 1:n)
	{	for (j in 1:(1 + n - i) )
			fv [i, j] = .dtvpdf.eval.ext2 (., c (x [i], y [j], 1 - x [i] - y [j]), log)
	}
	LIST (fv, x, y)
}

bvmat.kbv = function (sf, xlim, ylim, ..., n=10)
{	. = attributes (sf)
	if (missing (xlim) ) xlim = range (.$x) + c (-1, 1) * .$xbw
	if (missing (ylim) ) ylim = range (.$y) + c (-1, 1) * .$ybw
	n = rep_len (n, 2)
	s = bkde2D (cbind (.$x, .$y), cbind (.$xbw, .$ybw), n, list (xlim, ylim), FALSE)
	list (fv=s$fhat, x=s$x1, y=s$x2)
}

bvmat.ebv = function (sf, ..., reg=TRUE)
{	if (reg) ebvmat_reg (sf, ...)
	else ebvmat_step (sf, ...)
}

ebvmat_reg = function (sf, xlim, ylim, ..., n=10)
{	. = attributes (sf)
	if (missing (xlim) ) xlim = range (.$x)
	if (missing (ylim) ) ylim = range (.$y)
	.continuous.outer (sf, xlim, ylim, n)
}

ebvmat_step = function (sf, ..., extend=FALSE)
{	. = attributes (sf)
	extend = rep_len (extend, 2)

	x = sort (unique (.$x) )
	y = sort (unique (.$y) )
	nx = length (x)
	ny = length (y)
	if (extend [1])
	{	xlim = range (x)
		xr = diff (xlim) / 10
		x = c (xlim [1] - xr, x, xlim [2] + xr)
	}
	if (extend [2])
	{	ylim = range (y)
		yr = diff (ylim) / 10
		y = c (ylim [1] - yr, y, ylim [2] + yr)
	}
	xc = (x [-nx] + x [-1]) / 2
	yc = (y [-ny] + y [-1]) / 2

	fv = outer (xc, yc, sf)
	LIST (fv, x, y)
}
