bmbvpdf = function (mean.X1, mean.Y1, sd.X1, sd.Y1, mean.X2, mean.Y2, sd.X2, sd.Y2)
	bmbvpdf.2 (mean.X1, mean.Y1, sd.X1 ^ 2, sd.Y1 ^ 2, mean.X2, mean.Y2, sd.X2 ^ 2, sd.Y2 ^ 2)

bmbvpdf.2 = function (mean.X1, mean.Y1, var.X1, var.Y1, mean.X2, mean.Y2, var.X2, var.Y2)
{   f = function (x, y)
	{	. = THAT ()
		v = .val.numeric.args (x, y)
		.bmbvpdf.eval (., v$x, v$y)
	}

	vector.1.means = c (mean.X1, mean.Y1)
    matrix.1.variances = diag (c (var.X1, var.Y1) )
    vector.2.means = c (mean.X2, mean.Y2)
    matrix.2.variances = diag (c (var.X2, var.Y2) )

	f = .bv (f)
    EXTEND (f, "bmbvpdf",
		vector.1.means, matrix.1.variances,
		vector.2.means, matrix.2.variances
	)
}

.bmbvpdf.eval = function (., x, y)
{   xy = cbind (x, y)
    f1 = dmvnorm (xy, .$vector.1.means, .$matrix.1.variances)
    f2 = dmvnorm (xy, .$vector.2.means, .$matrix.2.variances)
    (f1 + f2) / 2
}

bmbvcdf = function (mean.X1, mean.Y1, sd.X1, sd.Y1, mean.X2, mean.Y2, sd.X2, sd.Y2)
	bmbvcdf.2 (mean.X1, mean.Y1, sd.X1 ^ 2, sd.Y1 ^ 2, mean.X2, mean.Y2, sd.X2 ^ 2, sd.Y2 ^ 2)

bmbvcdf.2 = function (mean.X1, mean.Y1, var.X1, var.Y1, mean.X2, mean.Y2, var.X2, var.Y2)
{   f = function (x, y)
	{	. = THAT ()
		v = .val.numeric.args (x, y)
		.bmbvcdf.eval (., v$x, v$y)
	}

	vector.1.means = c (mean.X1, mean.Y1)
    matrix.1.variances = diag (c (var.X1, var.Y1) )
    vector.2.means = c (mean.X2, mean.Y2)
    matrix.2.variances = diag (c (var.X2, var.Y2) )

	f = .bv (f)
    EXTEND (f, "bmbvcdf",
		vector.1.means, matrix.1.variances,
		vector.2.means, matrix.2.variances
	)
}

.bmbvcdf.eval = function (., x, y)
{   n = length (x)
    xy = cbind (x, y)
    f1 = f2 = numeric (n)
    for (i in 1:n)
    {    f1 [i] = pmvnorm (c (-Inf, -Inf), xy [i,], .$vector.1.means, sigma=.$matrix.1.variances)
         f2 [i] = pmvnorm (c (-Inf, -Inf), xy [i,], .$vector.2.means, sigma=.$matrix.2.variances)
    }
    (f1 + f2) / 2
}

.plot.bmbv = function (f, use.plot3d, npoints, xlim, ylim, ..., is.cdf=FALSE)
{   . = attributes (f)
	if (missing (xlim) )
	{   xlim1 = .$vector.1.means [1] + c (-3, 3) * sqrt (.$matrix.1.variances [1, 1])
		xlim2 = .$vector.2.means [1] + c (-3, 3) * sqrt (.$matrix.2.variances [1, 1])
		xlim = range (c (xlim1, xlim2) )
	}
	if (missing (ylim) )
	{   ylim1 = .$vector.1.means [2] + c (-3, 3) * sqrt (.$matrix.1.variances [2, 2])
		ylim2 = .$vector.2.means [2] + c (-3, 3) * sqrt (.$matrix.2.variances [2, 2])
		ylim = range (c (ylim1, ylim2) )	
	}
	v = .continuous.outer (f, xlim, ylim, npoints)
	.plot.bv.2 (TRUE, use.plot3d, is.cdf, v$x, v$y, v$z, ...)
}

plot.bmbvpdf = function (x, use.plot3d=FALSE, npoints=20, xlim, ylim, ..., all=FALSE)
{   if (all)
    {	F = bmbvcdf (0, 0, 0, 0, 0, 0, 0, 0)
		.plot.bv.all (x, F, npoints=30, xlim, ylim, ...)
	}
    else
		.plot.bmbv (x, use.plot3d, npoints, xlim, ylim, ...)
}

plot.bmbvcdf = function (x, use.plot3d=FALSE, npoints=20, xlim, ylim, ...)
      .plot.bmbv (x, use.plot3d, npoints, xlim, ylim, ..., is.cdf=TRUE)
