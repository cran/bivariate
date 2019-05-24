ebvcdf = function (x, y)
{   f = function (x, y)
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

plot.ebvcdf = function (x, use.plot3d=FALSE, mpoints=30, ...)
{	if (x %$% "n" <= mpoints)
		.plot.ebvcdf.as.step (x, use.plot3d, ...)
	else
		.plot.ebvcdf.as.surface (x, use.plot3d, mpoints, ...)
}

.plot.ebvcdf.as.step = function (f, use.plot3d, ...)
{	. = attributes (f)
	xb = sort (.$x)
	yb = sort (.$y)
	xlim = range (xb)
	ylim = range (yb)
	x.offset = diff (xlim) / 10
	y.offset = diff (ylim) / 10
	xb.2 = c (xlim [1] - x.offset, xb, xlim [2] + x.offset)
	yb.2 = c (ylim [1] - y.offset, yb, ylim [2] + y.offset)

	n = .$n + 1
	x = (xb.2 [-1] + xb.2 [-(.$n + 2)]) / 2
	y = (yb.2 [-1] + yb.2 [-(.$n + 2)]) / 2
	z = outer (x, y, f)
	
	if (use.plot3d)
	{	sm.1 = P.smatrix (1, 1, 1, n)
		sm.2 = P.smatrix (n, n, 1, n)
		sm.3 = P.smatrix (1, n, 1, 1)
		sm.4 = P.smatrix (1, n, n, n)
		nm = P.nmatrix (z, list (sm.1, sm.2, sm.3, sm.4) )
		plot3d.bar (xb.2, yb.2, nm, colvs.sms=c (0, 0, 75), ...)
	}
	else
	{	sm = P.smatrix (2, .$n, 2, .$n)
		nm = P.nmatrix (z, list (sm) )
		plot2d.cell (xb.2, yb.2, nm, xat=xb, yat=yb, contrast=-0.8, ...)
	}
}

.plot.ebvcdf.as.surface = function (f, use.plot3d, npoints, ...)
{	. = attributes (f)
	xlim = range (.$x) + c (-0.001, 0.001) * var (.$x)
	ylim = range (.$y) + c (-0.001, 0.001) * var (.$y)
	v = .continuous.outer (f, xlim, ylim, npoints)
	.plot.bv.2 (TRUE, use.plot3d, TRUE, v$x, v$y, v$z, ...)
}
