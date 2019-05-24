kbvpdf = function (x, y, bw.X, bw.Y)
{   f = function (x, y)
	{	stop ("can plot but can't evaluate")
	}

	f = .bv (f)
	EXTEND (f, "kbvpdf",
		bw = c (bw.X, bw.Y),
		data = cbind (as.numeric (x), as.numeric (y) )
	)
}

plot.kbvpdf = function (x, use.plot3d=FALSE, npoints=30, xlim, ylim, ...)
{   f = x

	. = attributes (f)
	if (missing (xlim) )
		xlim = range (.$data [,1]) + c (-1, 1) * .$bw [1]
	if (missing (ylim) )
		ylim = range (.$data [,2]) + c (-1, 1) * .$bw [2]
	x = seq (xlim [1], xlim [2], length.out=npoints)
	y = seq (ylim [1], ylim [2], length.out=npoints)
	z =	bkde2D (.$data, .$bw, c (npoints, npoints), list (xlim, ylim), FALSE)$fhat

	.plot.bv.2 (TRUE, use.plot3d, FALSE, x, y, z, ...)
}
