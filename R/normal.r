nbvpdf = function (mean.X, mean.Y, variance.X, variance.Y, covariance)
{   nbvpdf.f = function (x, y)
	{	x = as.numeric (x)
		y = as.numeric (y)
		stopifnot (length (x) == length (y) )
		.nbvpdf.eval (x, y)
	}
	vector.means = c (mean.X, mean.Y)
    matrix.variances = c (variance.X, covariance, covariance, variance.Y)
    matrix.variances = matrix (matrix.variances, nrow=2, ncol=2)
    attributes (nbvpdf.f) = list (class="nbvpdf", vector.means=vector.means, matrix.variances=matrix.variances)
    nbvpdf.f
}

.nbvpdf.eval = function (x, y)
{   . = attributes (sys.function (-1) )
    dmvnorm (cbind (x, y), .$vector.means, .$matrix.variances)
}

nbvcdf = function (mean.X, mean.Y, variance.X, variance.Y, covariance)
{   nbvcdf.f = function (x, y)
	{	x = as.numeric (x)
		y = as.numeric (y)
		stopifnot (length (x) == length (y) )
		.nbvcdf.eval (x, y)
	}
	vector.means = c (mean.X, mean.Y)
    matrix.variances = c (variance.X, covariance, covariance, variance.Y)
    matrix.variances = matrix (matrix.variances, nrow=2, ncol=2)
    attributes (nbvcdf.f) = list (class="nbvcdf", vector.means=vector.means, matrix.variances=matrix.variances)
    nbvcdf.f
}

.nbvcdf.eval = function (x, y)
{   . = attributes (sys.function (-1) )
	n = length (x)
    xy = cbind (x, y)
    z = numeric (n)
    for (i in 1:n)
        z [i] = pmvnorm (c (-Inf, -Inf), xy [i,], .$vector.means, sigma=.$matrix.variances)
    z
}

plot.nbvpdf = function (x, use.plot3d=FALSE, npoints=20, xlim, ylim, ..., all=FALSE)
{   nbvpdf.f = x
    
    . = attributes (nbvpdf.f)
    if (all)
    {   par (mfrow=c (2, 2) )
        p0 = par (mar=c (2, 2.5, 1, 0.175) )
        nbvcdf.f = nbvcdf (0, 0, 1, 1, 0)
        attributes (nbvcdf.f)$vector.means = .$vector.means
        attributes (nbvcdf.f)$matrix.variances = .$matrix.variances
        plot (nbvpdf.f, FALSE, xlab="", ylab="", npoints, xlim, ylim, drawlabels=FALSE)
        plot (nbvpdf.f, TRUE, npoints, xlim, ylim, ...)
        plot (nbvcdf.f, FALSE, xlab="", ylab="", npoints, xlim, ylim, drawlabels=FALSE)
        plot (nbvcdf.f, TRUE, npoints, xlim, ylim, ...)
        par (p0)
    }
    else
    {   if (missing (xlim) )
            xlim = .$vector.means [1] + c (-3, 3) * .$matrix.variances [1, 1]
        if (missing (ylim) )
            ylim = .$vector.means [2] + c (-3, 3) * .$matrix.variances [2, 2]
        x = seq (xlim [1], xlim [2], length.out=npoints)
        y = seq (ylim [1], ylim [2], length.out=npoints)
        z = outer (x, y, nbvpdf.f)
        if (use.plot3d)
            plot3d.surf (,,z, ...)
        else
        {   p0 = par (pty="s")
            .contour (x, y, z, ...)
            par (p0)
        }
    }
}

plot.nbvcdf = function (x, use.plot3d=FALSE, npoints=20, xlim, ylim, ...)
      plot.nbvpdf (x, use.plot3d, npoints, xlim, ylim, ...)
