#bivariate: Bivariate Probability Distributions
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

kbvpdf = function (x, y, bw.X, bw.Y)
{	f = function (x, y)
		stop ("can plot but can't evaluate")
	f = .bv (f)
	EXTEND (f, "kbvpdf",
		bw = c (bw.X, bw.Y),
		data = cbind (as.numeric (x), as.numeric (y) )
	)
}

plot.kbvpdf = function (x, plot.3d=FALSE, ..., xlim, ylim, n=30, points=TRUE, point.color="#00000030")
	.plot.kbvpdf (x, plot.3d, xlim, ylim, n, points, point.color, ...)

.plot.kbvpdf = function (x, plot.3d, xlim, ylim, n, points, point.color, ..., xyrel="m", contours=TRUE, heatmap=TRUE)
{   f = x

	. = attributes (f)
	p = .outer.kbvpdf (., n, xlim, ylim)
	if (plot.3d)
		.plot.bv (TRUE, TRUE, FALSE, p$x, p$y, p$fv, ...)
	else
	{	plot_cfield (p$x, p$y, p$fv, ..., xyrel=xyrel, contours=FALSE, heatmap=heatmap)
		if (points)
			points (.$data [,1], .$data [,2], pch=16, col=point.color)
		p = .outer.kbvpdf (., 60, xlim, ylim)
		plot_cfield (p$x, p$y, p$fv, add=TRUE, contours=contours, heatmap=FALSE, ...)
	}
}

.outer.kbvpdf = function (., n, xlim, ylim)
{	if (missing (xlim) )
		xlim = range (.$data [,1]) + c (-1, 1) * .$bw [1]
	if (missing (ylim) )
		ylim = range (.$data [,2]) + c (-1, 1) * .$bw [2]
	x = seq (xlim [1], xlim [2], length.out=n)
	y = seq (ylim [1], ylim [2], length.out=n)
	fv =	bkde2D (.$data, .$bw, c (n, n), list (xlim, ylim), FALSE)$fhat
	LIST (x, y, fv)
}
