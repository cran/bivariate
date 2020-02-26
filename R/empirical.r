#bivariate: Bivariate Probability Distributions
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

ebvcdf = function (x, y)
{	f = function (x, y)
	{	. = THAT ()
		v = .val.numeric.args (x, y)
		.ebvcdf.eval (., v$x, v$y)
	}
	v = .val.numeric.args (x, y)

	f = .bv (f)
	EXTEND (f, "ebvcdf",
		n = length (v$x),
		x = v$x,
		y = v$y
	)
}

.ebvcdf.eval = function (., x, y)
{	n = length (x)
	z = numeric (n)
	for (i in 1:n)
		z [i] = .ebvcdf.eval.ext (., x [i], y [i])
	z
}

.ebvcdf.eval.ext = function (., x, y)
{	Ix = (.$x <= x)
	Iy = (.$y <= y)
	sum (Ix & Iy) / .$n
}

plot.ebvcdf = function (x, plot.3d=FALSE, ..., as.smooth)
{	if (missing (as.smooth) )
		as.smooth = (x %$% "n" > 40)
	if (as.smooth)
		plot_ebvcdf_smooth (x, plot.3d, ...)
	else
		plot_ebvcdf_step (x, plot.3d, ...)
}

plot_ebvcdf_step = function (x, plot.3d=FALSE, ...,
	steps=TRUE, points=TRUE, point.color="#00000030")
{	.plot.ebvcdf.step (x, plot.3d, ...,
		steps=steps, points=points, point.color=point.color)
}

plot_ebvcdf_smooth = function (x, plot.3d=FALSE, ..., xlim, ylim, n=30)
	.plot.ebvcdf.smooth (x, plot.3d, ..., xlim=xlim, ylim=ylim, n=n)

.plot.ebvcdf.step = function (f, plot.3d, ...,
	contours=FALSE, steps, points, point.color,
	xyrel="m", color.function, color.fit, cols)
{	. = attributes (f)
	xb = sort (.$x)
	yb = sort (.$y)
	xlim = range (xb)
	ylim = range (yb)
	x.offset = diff (xlim) / 10
	y.offset = diff (ylim) / 10
	xb2 = c (xlim [1] - x.offset, xb, xlim [2] + x.offset)
	yb2 = c (ylim [1] - y.offset, yb, ylim [2] + y.offset)

	n = .$n + 1
	x = (xb2 [-1] + xb2 [-(.$n + 2)]) / 2
	y = (yb2 [-1] + yb2 [-(.$n + 2)]) / 2
	fv = outer (x, y, f)

	I = c (1, n)
	if (plot.3d)
	{	if (missing (color.function) )
		{	color.function = getOption ("barsurf")$barface
			color.function = eval (str2lang (color.function) )()
		}
		cols.t = matrix (color.function (TRUE), n, n)
		cols.f = matrix (color.function (FALSE), n, n)
		cols.t [I,] = cols.t [,I] = "#D0D0D0"
		plot_bar (xb2, yb2, fv, cols = list (cols.t, cols.f), ...)
	}
	else
	{	if (missing (cols) )
		{	if (missing (color.function) )
			{	if (missing (color.fit) )
				{	color.fit = getOption ("barsurf")$litmus.fit
					color.fit = eval (str2lang (color.fit) )
				}
				color.function = color.fit (fv)
			}
			cols = color.function (fv)
			cols2 = litmus.rainbow.fit (fv, c=12.5)(fv)
			cols [I,] = cols2 [I,]
			cols [,I] = cols2 [,I]
		}
		plot_dfield (xb2, yb2, fv, ..., xyrel=xyrel, contours=contours, xat=xb, yat=yb, cols=cols)
		rect (xlim [1], ylim [1], xlim [2], ylim [2], lty=3, border="#808080")
		x = .$x
		y = .$y
		if (points)
			points (x, y, pch=16, col=point.color)
		if (steps)
		{	mx = xb2 [.$n + 2]
			my = yb2 [.$n + 2]
			for (i in 1:(.$n) )
			{	lines (c (x [i], x [i]), c (y [i], my) )
				lines (c (x [i], mx), c (y [i], y [i]) )
			}
		}
	}
}

.plot.ebvcdf.smooth = function (f, plot.3d, ..., xyrel="m", xlim, ylim, n)
{	. = attributes (f)
	if (missing (xlim) )
		xlim = range (.$x) + c (-0.001, 0.001) * var (.$x)
	if (missing (ylim) )
		ylim = range (.$y) + c (-0.001, 0.001) * var (.$y)
	v = .continuous.outer (f, xlim, ylim, n)
	.plot.bv (TRUE, plot.3d, TRUE, v$x, v$y, v$z, ..., xyrel=xyrel)
}
