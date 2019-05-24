cubvpdf = function (a.X, b.X, a.Y, b.Y)
{   f = function (x, y)
	{	. = THAT ()
		v = .val.numeric.args (x, y)
		.cubvpdf.eval (., v$x, v$y)
	}

	f = .bv (f)
    EXTEND (f, "cubvpdf",
		a = c (a.X, a.Y),
		b = c (b.X, b.Y)
	)
}

.cubvpdf.eval = function (., x, y)
{   z = rep (1 / (.$b [1] - .$a [1]) / (.$b [2] - .$a [2]), length (x) )
	z [x < .$a [1] | x > .$b [1] | y < .$a [2] | y > .$b [2]] = 0
    z
}

cubvcdf = function (a.X, b.X, a.Y, b.Y)
{   f = function (x, y)
	{	. = THAT ()
		v = .val.numeric.args (x, y)
		.cubvcdf.eval (., v$x, v$y)
	}

	f = .bv (f)
	EXTEND (f, "cubvcdf",
		a = c (a.X, a.Y),
		b = c (b.X, b.Y)
	)
}

.cubvcdf.eval = function (., x, y)
{   x [x > .$b [1] ] = .$b [1]
	y [y > .$b [2] ] = .$b [2]
    z1 = (x - .$a [1]) / (.$b [1] - .$a [1])
	z2 = (y - .$a [2]) / (.$b [2] - .$a [2])
	z = z1 * z2
	z [x < .$a [1] | y < .$a [2] ] = 0
	z
}

.plot.cubv = function (f, use.plot3d, npoints, xlim, ylim, ...)
{	. = attributes (f)
	if (missing (xlim) )
		xlim = c (.$a [1], .$b [1])
	if (missing (ylim) )
		ylim = c (.$a [2], .$b [2])
	v = .continuous.outer (f, xlim, ylim, npoints)
	.plot.bv (TRUE, use.plot3d, TRUE, v$x, v$y, v$z, ..., contours=FALSE)
}

plot.cubvpdf = function (x, use.plot3d=FALSE, npoints=20, xlim, ylim, ..., all=FALSE)
{   if (all)
	{	F = cubvcdf (0, 0, 0, 0)
		.plot.bv.all (x, F, npoints, xlim, ylim, ...)
	}
	else
		.plot.cubv (x, use.plot3d, npoints, xlim, ylim, ...)
}

plot.cubvcdf = function (x, use.plot3d=FALSE, npoints=20, xlim, ylim, ...)
	.plot.cubv (x, use.plot3d, npoints, xlim, ylim, ...)
