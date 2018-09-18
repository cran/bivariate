nbvpdf = function (mean.X, mean.Y, variance.X, variance.Y, covariance)
{   nbvpdf.f = function (x, y) {.nbvpdf.eval (x, y)}
	vector.means = c (mean.X, mean.Y)
    matrix.variances = c (variance.X, covariance, covariance, variance.Y)
    matrix.variances = matrix (matrix.variances, nrow=2, ncol=2)
    attributes (nbvpdf.f) = list (class="nbvpdf", vector.means=vector.means, matrix.variances=matrix.variances)
    nbvpdf.f
}

.nbvpdf.eval = function (x, y)
{   this = attributes (sys.function (-1) )
    dmvnorm (cbind (x, y), this$vector.means, this$matrix.variances)
}

nbvcdf = function (mean.X, mean.Y, variance.X, variance.Y, covariance)
{   nbvcdf.f = function (x, y) {.nbvcdf.eval (x, y)}
	vector.means = c (mean.X, mean.Y)
    matrix.variances = c (variance.X, covariance, covariance, variance.Y)
    matrix.variances = matrix (matrix.variances, nrow=2, ncol=2)
    attributes (nbvcdf.f) = list (class="nbvcdf", vector.means=vector.means, matrix.variances=matrix.variances)
    nbvcdf.f
}

.nbvcdf.eval = function (x, y)
{   this = attributes (sys.function (-1) )
    x = cbind (x, y)
    n = nrow (x)
    z = numeric (n)
    for (i in 1:n)
        z [i] = pmvnorm (c (-Inf, -Inf), x [i,], this$vector.means, sigma=this$matrix.variances)
    z
}

print.nbvpdf = function (x, ...)
    print.dubvpmf (x, ...)

print.nbvcdf = function (x, ...)
    print.dubvpmf (x, ...)

plot.nbvpdf = function (x, use.plot3d=FALSE, xlab="x", ylab="y", xlim, ylim, zlim, ..., all=FALSE)
{   nbvpdf.f = x
    
    this = attributes (nbvpdf.f)
    if (all)
    {   par (mfrow=c (2, 2) )
        p0 = par (mar=c (2, 2.5, 1, 0.175) )
        nbvcdf.f = nbvcdf (0, 0, 1, 1, 0)
        attributes (nbvcdf.f)$vector.means = this$vector.means
        attributes (nbvcdf.f)$matrix.variances = this$matrix.variances
        plot (nbvpdf.f, FALSE, xlab, ylab, xlim, ylim, zlim, drawlabels=FALSE, ...)
        plot (nbvpdf.f, TRUE, xlab, ylab, xlim, ylim, zlim, ...)
        plot (nbvcdf.f, FALSE, xlab, ylab, xlim, ylim, zlim, drawlabels=FALSE, ...)
        plot (nbvcdf.f, TRUE, xlab, ylab, xlim, ylim, zlim, ...)
        par (p0)
    }
    else
    {   if (missing (xlim) )
            xlim = this$vector.means [1] + c (-3, 3) * this$matrix.variances [1, 1]
        if (missing (ylim) )
            ylim = this$vector.means [2] + c (-3, 3) * this$matrix.variances [2, 2]
        x = seq (xlim [1], xlim [2], length.out=20)
        y = seq (ylim [1], ylim [2], length.out=20)
        z = outer (x, y, nbvpdf.f)
        if (use.plot3d)
            plot3d.continuous (,, z, zlim=zlim, ...)
        else
        {   p0 = par (pty="s")
            contour.default (z=z, xlab=xlab, ylab=ylab, ...)
            par (p0)
        }
    }
}

plot.nbvcdf = function (x, use.plot3d=FALSE, xlab="x", ylab="y", xlim, ylim, zlim, ...)
      plot.nbvpdf (x, use.plot3d, xlab, ylab, xlim, ylim, zlim, ...)
