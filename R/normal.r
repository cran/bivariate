nbvpdf = function (mean.X, mean.Y, sd.X, sd.Y, cor)
	nbvpdf.2 (mean.X, mean.Y, sd.X ^ 2, sd.Y ^ 2, sd.X * sd.Y * cor)

nbvpdf.2 = function (mean.X, mean.Y, var.X, var.Y, cov)
{   f = function (x, y)
	{	. = THAT ()
		v = .val.numeric.args (x, y)
		.nbvpdf.eval (., v$x, v$y)
	}

	vector.means = c (mean.X, mean.Y)
    matrix.variances = c (var.X, cov, cov, var.Y)
    matrix.variances = matrix (matrix.variances, nrow=2, ncol=2)

	f = .bv (f)
    EXTEND (f, "nbvpdf", vector.means, matrix.variances)
}

.nbvpdf.eval = function (., x, y)
	dmvnorm (cbind (x, y), .$vector.means, .$matrix.variances)

nbvcdf = function (mean.X, mean.Y, sd.X, sd.Y, cor)
	nbvcdf.2 (mean.X, mean.Y, sd.X ^ 2, sd.Y ^ 2, sd.X * sd.Y * cor)	

nbvcdf.2 = function (mean.X, mean.Y, var.X, var.Y, cov)
{   f = function (x, y)
	{	. = THAT ()
		v = .val.numeric.args (x, y)
		.nbvcdf.eval (., v$x, v$y)
	}

	vector.means = c (mean.X, mean.Y)
    matrix.variances = c (var.X, cov, cov, var.Y)
    matrix.variances = matrix (matrix.variances, nrow=2, ncol=2)

	f = .bv (f)
    EXTEND (f, "nbvcdf", vector.means, matrix.variances)
}

.nbvcdf.eval = function (., x, y)
{   n = length (x)
    xy = cbind (x, y)
    z = numeric (n)
    for (i in 1:n)
        z [i] = pmvnorm (c (-Inf, -Inf), xy [i,], .$vector.means, sigma=.$matrix.variances)
    z
}

.plot.nbv = function (f, use.plot3d, npoints, xlim, ylim, ..., is.cdf=FALSE)
{   . = attributes (f)
	if (missing (xlim) )
		xlim = .$vector.means [1] + c (-3, 3) * sqrt (.$matrix.variances [1, 1])
	if (missing (ylim) )
		ylim = .$vector.means [2] + c (-3, 3) * sqrt (.$matrix.variances [2, 2])
	v = .continuous.outer (f, xlim, ylim, npoints)
	.plot.bv.2 (TRUE, use.plot3d, is.cdf, v$x, v$y, v$z, ...)
}

plot.nbvpdf = function (x, use.plot3d=FALSE, npoints=20, xlim, ylim, ..., all=FALSE)
{  	if (all)
	{	F = nbvcdf (0, 0, 0, 0, 0)
		.plot.bv.all (x, F, npoints, xlim, ylim, ...)
	}
	else
		.plot.nbv (x, use.plot3d, npoints, xlim, ylim, ...)
}

plot.nbvcdf = function (x, use.plot3d=FALSE, npoints=20, xlim, ylim, ...)
	.plot.nbv (x, use.plot3d, npoints, xlim, ylim, ..., is.cdf=TRUE)
