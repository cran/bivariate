#bivariate: Bivariate Probability Distributions
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.compute.constant = function (alpha)
	1 / (prod (gamma (alpha) ) / gamma (sum (alpha) ) )

dtvpdf = function (alpha.X, alpha.Y, alpha.Z)
{	f = function (x, y, z, log=FALSE)
	{	. = THAT ()
		v = .val.dirichlet.args (x, y, z, 0.001)
		.dtvpdf.eval (., v, log)
	}
	alpha = .val.dirichlet.pars (alpha.X, alpha.Y, alpha.Z)

	f = .bv (f)
	EXTEND (f, "dtvpdf",
		.constant = .compute.constant (alpha),
		alpha
	)
}

.dtvpdf.eval = function (., x, log)
{	d = .$.constant * prod (x ^ (.$alpha - 1) )
	if (log)
		log (d)
	else
		d
}

plot.dtvpdf = function (x, plot.3d=FALSE, ..., n=30, log=FALSE)
{	f = x

	. = attributes (f)
	x = y = seq (0, 1, length.out=n)
	z = matrix (NA, n, n)
	for (i in 1:n)
		for (j in 1:(1 + n - i) )
		{	v = .dirichlet.scale (x [i], y [j])
			z [i, j] = f (v [1], v [2], v [3], log)
		}
	if (all (.$alpha == 1) )
		zlim = c (0, 4)
	else if (log)
		zlim = range (z, na.rm=TRUE)
	else 
		zlim = .inzm (z)
	if (plot.3d)
		plot_trisurface (,,z, ..., zlim=zlim)
	else
		plot_tricontour (,,z, ...)
}

.dirichlet.scale = function (x1, x2, crop=0.02)
{	x3 = 1 - x1 - x2
	x = c (x1, x2, x3)
	(1 - crop) * x + crop / 3
}
