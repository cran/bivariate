#bivariate: Bivariate Probability Distributions
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

cbvpmf = function (p)
{	f = function (x, y)
	{	. = THAT ()
		v = .val.categorical.args (., x, y)
		.cbvpmf.eval (., v$x, v$y)
	}
	nx = nrow (p)
	ny = ncol (p)
	xvalues = rownames (p)
	yvalues = colnames (p)
	if (is.null (xvalues) )
		xvalues = as.character (1:nx)
	if (is.null (yvalues) )
		yvalues = as.character (1:ny)

	f = .bv (f)
	EXTEND (f, "cbvpmf",
		nx, ny, xvalues, yvalues,
		p = p / sum (p, na.rm=TRUE)
	)
}

.cbvpmf.eval = function (., x, y)
{	n = length (x)
	z = numeric (n)
	for (i in 1:n)
		z [i] = .$p [x [i], y [i] ]
	z
}

.plot.cbvpmf = function (x, plot.3d, labels, ..., grid.lines=TRUE, contours=FALSE, xlab="x", ylab="y", as.matrix=FALSE, hcv=TRUE)
{	f = x
	. = attributes (f)
	xlim = c (1, .$nx)
	ylim = c (1, .$ny)
	if (plot.3d)
		plot_bar (,,.$p, ..., zlim=.inzm (.$p) )
	else
	{	if (as.matrix)
		{	temp = xlab
			xlab = ylab
			ylab = temp
		}
		plot_dfield (,,.$p, ..., grid.lines=grid.lines, contours=contours, xlab=xlab, ylab=ylab, as.matrix=as.matrix, hcv=hcv)
		if (labels)
		{	x = rep (1:.$nx, times=.$ny)
			y = rep (1:.$ny, each=.$nx)
			if (as.matrix)
				text (y, x, round (.$p, 2) )
			else
				text (x, y, round (.$p, 2) )
		}
	}
}

plot.cbvpmf = function (x, plot.3d=FALSE, ..., labels=TRUE)
	.plot.cbvpmf (x, plot.3d, labels, ...)
