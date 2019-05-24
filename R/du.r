dubvpmf = function (a.X, b.X, a.Y, b.Y)
{   f = function (x, y)
	{	. = THAT ()
		v = .val.integer.args (x, y)
		.dubvpmf.eval (., v$x, v$y)
	}
	a = c (a.X, a.Y)
	b = c (b.X, b.Y)

	f = .bv (f)
	EXTEND (f, "dubvpmf", n = 1 + b - a, a, b)
}

.dubvpmf.eval = function (., x, y)
{   z = rep (1 / .$n [1] / .$n [2], length (x) )
	z [x < .$a [1] | x > .$b [1] | y < .$a [2] | y > .$b [2]] = 0
    z
}

dubvcdf = function (a.X, b.X, a.Y, b.Y)
{   f = function (x, y)
	{	. = THAT ()
		v = .val.integer.args (x, y)
		.dubvcdf.eval (., v$x, v$y)
	}
	a = c (a.X, a.Y)
	b = c (b.X, b.Y)

	f = .bv (f)
    EXTEND (f, "dubvcdf", n = 1 + b - a, a, b)
}

.dubvcdf.eval = function (., x, y)
{   x [x > .$b [1] ] = .$b [1]
	y [y > .$b [2] ] = .$b [2]
    z1 = (1 + x - .$a [1]) / .$n [1]
	z2 = (1 + y - .$a [2]) / .$n [2]
	z = z1 * z2
	z [x < .$a [1] | y < .$a [2] ] = 0
	z
}

.plot.dubv = function (f, use.plot3d, xlim, ylim, ...)
{   . = attributes (f)
    if (missing (xlim) )
		xlim = c (.$a [1], .$b [1])
	if (missing (ylim) )
		ylim = c (.$a [2], .$b [2])
	v = .discrete.outer (f, xlim, ylim)
	.plot.bv (FALSE, use.plot3d, TRUE, v$x, v$y, v$z, ...)
}

plot.dubvpmf = function (x, use.plot3d=FALSE, xlim, ylim, ..., all=FALSE)
{	if (all)
	{	F = dubvcdf (0, 0, 0, 0)
		.plot.bv.all (x, F, xlim, ylim, ...)
	}
	else
		.plot.dubv (x, use.plot3d, xlim, ylim, ...)
}

plot.dubvcdf = function (x, use.plot3d=FALSE, xlim, ylim, ...)
      .plot.dubv (x, use.plot3d, xlim, ylim, ...)
