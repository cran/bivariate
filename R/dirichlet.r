.compute.constant = function (alpha)
	1 / (prod (gamma (alpha) ) / gamma (sum (alpha) ) )

dtvpdf = function (alpha.1, alpha.2, alpha.3, tol=0.001)
{   f = function (x1, x2, x3, log=FALSE)
	{	. = THAT ()
		x = .val.dirichlet.args (x1, x2, x3, .$tol)
		.dtvpdf.eval (., x, log)
	}
	alpha = .val.dirichlet.pars (alpha.1, alpha.2, alpha.3)

	f = .bv (f)
	EXTEND (f, "dtvpdf",
		.constant = .compute.constant (alpha),
		alpha, tol
	)
}

.dtvpdf.eval = function (., x, log)
{	d = .$.constant * prod (x ^ (.$alpha - 1) )
	if (log)
		log (d)
	else
		d
}

plot.dtvpdf = function (x, use.plot3d=FALSE, npoints=20, log=FALSE, ...)
	.plot.dtvpdf.2 (x, use.plot3d, npoints, log, ...)

.plot.dtvpdf.2 = function (x, use.plot3d, npoints, log, xlab="x1", ylab="x2", contrast=-0.8, ...)
{	f = x

	. = attributes (f)
	x = y = seq (0, 1, length.out=npoints)
	z = matrix (NA, npoints, npoints)
	for (i in 1:npoints)
		for (j in 1:(1 + npoints - i) )
		{	v = .dirichlet.scale (x [i], y [j])
			z [i, j] = f (v [1], v [2], v [3], log)
		}
	if (all (.$alpha == 1) )
		zlim = c (0, 4)
	else if (log)
		zlim = range (z, na.rm=TRUE)
	else 
		zlim = .inzm (z)
	if (use.plot3d)
		plot3d.trisurface (,,z, xlab=xlab, ylab=ylab, zlim=zlim, contrast=contrast, ...)
	else
		plot2d.tricontour (x, y, z, xlab=xlab, ylab=ylab, contrast=contrast, ...)
}

.dirichlet.scale = function (x1, x2, crop=0.02)
{	x3 = 1 - x1 - x2
	x = c (x1, x2, x3)
	(1 - crop) * x + crop / 3
}
