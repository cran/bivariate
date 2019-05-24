cbvpmf = function (z)
{   f = function (x, y)
	{	. = THAT ()
		v = .val.categorical.args (., x, y)
		.cbvpmf.eval (., v$x, v$y)
	}
	nx = nrow (z)
	ny = ncol (z)
	x.values = rownames (z)
	y.values = colnames (z)
	if (is.null (x.values) )
		x.values = as.character (1:nx)
	if (is.null (y.values) )
		y.values = as.character (1:ny)

	f = .bv (f)
    EXTEND (f, "cbvpmf",
		nx, ny, x.values, y.values,
		z = z / sum (z, na.rm=TRUE)
	)
}

.cbvpmf.eval = function (., x, y)
{	n = length (x)
	z = numeric (n)
	for (i in 1:n)
		z [i] = .$z [x [i], y [i] ]
	z
}

plot.cbvpmf = function (x, use.plot3d=FALSE, ...)
{	f = x
	. = attributes (f)
	xlim = c (1, .$nx)
	ylim = c (1, .$ny)
	v = .discrete.outer (f, xlim, ylim)
	if (use.plot3d)
		plot3d.bar (,,v$z, xlabs=.$x.values, ylabs=.$y.values, zlim=.inzm (v$z), ...)
	else
		plot2d.cell (,,v$z, xlabs=.$x.values, ylabs=.$y.values, ...)
}
