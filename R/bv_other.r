.p2c = function (f, F)
{	class.name = class (F)
	attributes (F) = attributes (f)
	class (F) = class.name
	F
}

.val.dirichlet.pars = function (alpha.1, alpha.2, alpha.3)
{	alpha = c (alpha.1, alpha.2, alpha.3)
	alpha = as.numeric (alpha)
	if (any (alpha <= 0) )
		stop ("parameter <= 0")
	alpha
}

.plot.bv = function (is.continuous, use.plot3d, in.zero.one, x, y, z, ..., contours=TRUE, contrast=0)
{	if (in.zero.one)
		zlim = c (0, 1)
	else
		zlim = .inzm (z)
	if (is.continuous)
	{	if (use.plot3d)
			plot3d.surface (,,z, zlim=zlim, ...)
		else
			plot2d.contour (x, y, z, contours=contours, contrast=contrast, ...)
	}
	else
	{	if (use.plot3d)
			plot3d.bar (,,z, zlim=zlim, ...)
		else
			plot2d.cell (,,z, xlabs=x, ylabs=y, contrast=contrast, ...)
	}
}

.plot.bv.2 = function (is.continuous, use.plot3d, in.zero.one, x, y, z, ..., contours=TRUE, contrast=-0.8)
	.plot.bv (is.continuous, use.plot3d, in.zero.one, x, y, z, ..., contours=contours, contrast=contrast)

.plot.bv.all = function (f, F.default, ..., cex=0.65)
{	mar.2d = c (3.25, 3.75, 2.75, 3.75)

	F = .p2c (f, F.default)
	p0 = par (mfrow=c (2, 2), cex=cex, mar=mar.2d)
	plot (f, FALSE, xlab="", ylab="", ...)
	plot (f, TRUE, ...)
	par (mar=mar.2d)
	plot (F, FALSE, xlab="", ylab="", ...)
	plot (F, TRUE, ...)
	par (p0)
}

.val.args = function (x, y)
{	stopifnot (length (x) == length (y) )
	LIST (x, y)
}

.val.integer.args = function (x, y)
{	x = as.integer (x)
	y = as.integer (y)
	.val.args (x, y)
}

.val.categorical.args = function (., x, y)
{	if (is.character (x) )
		x = match (x, .$x.values)
	else
		x = as.integer (x)
	if (is.character (y) )
		y = match (y, .$y.values)
	else
		y = as.integer (y)
	.val.args (x, y)
}

.val.numeric.args = function (x, y)
{	x = as.numeric (x)
	y = as.numeric (y)
	.val.args (x, y)
}

.val.dirichlet.args = function (x1, x2, x3, tol)
{	x1 = as.numeric (x1)
	x2 = as.numeric (x2)
	x3 = as.numeric (x3)
	equal = (length (x1) == c (length (x2), length (x3) ) )
	if (!all (equal) )
		stop ("length() of x1, x2 and x3 must be equal")
	x1.in = all (x1 > 0 && x1 < 1)
	x2.in = all (x2 > 0 && x2 < 1)
	x3.in = all (x3 > 0 && x3 < 1)
	if (! (x1.in && x2.in && x3.in) )
		stop ("x1, x2 and x3 must be in interval (0, 1)")
	xerr = abs (1 - (x1 + x2 + x3) )
	if (any (xerr > tol ) )
		stop ("x1 + x2 + x3 != 1")
	cbind (x1, x2, x3)
}

.discrete.outer = function (f, xlim, ylim)
{	x = xlim [1]:xlim [2]
	y = ylim [1]:ylim [2]
	z = outer (x, y, f)
	LIST (x, y, z)
}

.continuous.outer = function (f, xlim, ylim, n)
{	x = seq (xlim [1], xlim [2], length.out=n)
	y = seq (ylim [1], ylim [2], length.out=n)
	z = outer (x, y, f)
	LIST (x, y, z)
}

.inzm = function (z)
	c (0, max (z, na.rm=TRUE) )
