bmbvpdf = function (mean.X1, mean.Y1, variance.X1, variance.Y1, mean.X2, mean.Y2, variance.X2, variance.Y2, w=0.5)
{   vector1.means = c (mean.X1, mean.Y1)
    matrix1.variances = diag (c (variance.X1, variance.Y1) )
    vector2.means = c (mean.X2, mean.Y2)
    matrix2.variances = diag (c (variance.X2, variance.Y2) )
    bmbvpdf.f = function (x, y)
	{	x = as.numeric (x)
		y = as.numeric (y)
		stopifnot (length (x) == length (y) )
		.bmbvpdf.eval (x, y)
	}
    attributes (bmbvpdf.f) = list (class="bmbvpdf", vector1.means=vector1.means, matrix1.variances=matrix1.variances, vector2.means=vector2.means, matrix2.variances=matrix2.variances, w=w)
    bmbvpdf.f
}

.bmbvpdf.eval = function (x, y)
{   . = attributes (sys.function (-1) )
	xy = cbind (x, y)
    f1 = dmvnorm (xy, .$vector1.means, .$matrix1.variances)
    f2 = dmvnorm (xy, .$vector2.means, .$matrix2.variances)
    .$w * f1 + (1 - .$w) * f2
}

bmbvcdf = function (mean.X1, mean.Y1, variance.X1, variance.Y1, mean.X2, mean.Y2, variance.X2, variance.Y2, w=0.5)
{   vector1.means = c (mean.X1, mean.Y1)
    matrix1.variances = diag (c (variance.X1, variance.Y1) )
    vector2.means = c (mean.X2, mean.Y2)
    matrix2.variances = diag (c (variance.X2, variance.Y2) )
    bmbvcdf.f = function (x, y)
	{	x = as.numeric (x)
		y = as.numeric (y)
		stopifnot (length (x) == length (y) )
		.bmbvcdf.eval (x, y)
	}
    attributes (bmbvcdf.f) = list (class="bmbvcdf", vector1.means=vector1.means, matrix1.variances=matrix1.variances, vector2.means=vector2.means, matrix2.variances=matrix2.variances, w=w)
    bmbvcdf.f
}

.bmbvcdf.eval = function (x, y)
{   . = attributes (sys.function (-1) )
	n = length (x)
    xy = cbind (x, y)
    f1 = f2 = numeric (n)
    for (i in 1:n)
    {    f1 [i] = pmvnorm (c (-Inf, -Inf), xy [i,], .$vector1.means, sigma=.$matrix1.variances)
         f2 [i] = pmvnorm (c (-Inf, -Inf), xy [i,], .$vector2.means, sigma=.$matrix2.variances)
    }
    .$w * f1 + (1 - .$w) * f2
}

plot.bmbvpdf = function (x, use.plot3d=FALSE, npoints=30, xlim, ylim, ..., all=FALSE)
{   bmbvpdf.f = x
    
    . = attributes (bmbvpdf.f)
    if (all)
    {   par (mfrow=c (2, 2) )
        p0 = par (mar=c (2, 2.5, 1, 0.175) )
        bmbvcdf.f = bmbvcdf (0, 0, 1, 1, 0, 0, 1, 1)
        attributes (bmbvcdf.f)$vector1.means = .$vector1.means
		attributes (bmbvcdf.f)$vector2.means = .$vector2.means
        attributes (bmbvcdf.f)$matrix1.variances = .$matrix1.variances
		attributes (bmbvcdf.f)$matrix2.variances = .$matrix2.variances
        plot (bmbvpdf.f, FALSE, xlab="", ylab="", npoints, xlim, ylim, drawlabels=FALSE)
        plot (bmbvpdf.f, TRUE, npoints, xlim, ylim, ...)
        plot (bmbvcdf.f, FALSE, xlab="", ylab="", npoints, xlim, ylim, drawlabels=FALSE)
        plot (bmbvcdf.f, TRUE, npoints, xlim, ylim, ...)
        par (p0)
    }
    else
    {   if (missing (xlim) )
        {   xlim1 = .$vector1.means [1] + c (-3, 3) * .$matrix1.variances [1, 1]
			xlim2 = .$vector2.means [1] + c (-3, 3) * .$matrix2.variances [1, 1]
			xlim = range (c (xlim1, xlim2) )
		}
        if (missing (ylim) )
        {   ylim1 = .$vector1.means [2] + c (-3, 3) * .$matrix1.variances [2, 2]
			ylim2 = .$vector2.means [2] + c (-3, 3) * .$matrix2.variances [2, 2]
			ylim = range (c (ylim1, ylim2) )	
		}
        x = seq (xlim [1], xlim [2], length.out=npoints)
        y = seq (ylim [1], ylim [2], length.out=npoints)
        z = outer (x, y, bmbvpdf.f)
        if (use.plot3d)
            plot3d.surf (,,z, ...)
        else
        {   p0 = par (pty="s")
            .contour (x, y, z, ...)
            par (p0)
        }
    }
}

plot.bmbvcdf = function (x, use.plot3d=FALSE, npoints=30, xlim, ylim, ...)
      plot.bmbvpdf (x, use.plot3d, npoints, xlim, ylim, ...)
