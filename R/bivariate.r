nbpdf = function (mean.x, mean.y, variance.x, variance.y, covariance)
{   vector.means = c (mean.x, mean.y)
    matrix.variances = c (variance.x, covariance, covariance, variance.y)
    matrix.variances = matrix (matrix.variances, nrow=2, ncol=2)
    f = function (x, y) .nbpdf.eval (x, y)
    attributes (f) = list (class="nbpdf", vector.means=vector.means, matrix.variances=matrix.variances)
    f
}

.nbpdf.eval = function (x, y)
{   this = attributes (sys.function (-1) )
    dmvnorm (cbind (x, y), this$vector.means, this$matrix.variances)
}

nbcdf = function (mean.x, mean.y, variance.x, variance.y, covariance)
{   vector.means = c (mean.x, mean.y)
    matrix.variances = c (variance.x, covariance, covariance, variance.y)
    matrix.variances = matrix (matrix.variances, nrow=2, ncol=2)
    f = function (x, y) .nbcdf.eval (x, y)
    attributes (f) = list (class="nbcdf", vector.means=vector.means, matrix.variances=matrix.variances)
    f
}

.nbcdf.eval = function (x, y)
{   this = attributes (sys.function (-1) )
    x = cbind (x, y)
    n = nrow (x)
    z = numeric (n)
    for (i in 1:n)
        z [i] = pmvnorm (c (-Inf, -Inf), x [i,], this$vector.means, sigma=this$matrix.variances)
    z
}

#this is here so that I can document the function returned by nbpdf
nbpdf.f = function (x, y) NULL

#this is here so that I can document the function returned by nbcdf
nbcdf.f = function (x, y) NULL

print.nbpdf = function (x, ...)
{   environment (x) = .GlobalEnv
    print.function (x, ...)
}

print.nbcdf = function (x, ...)
    print.nbpdf (x, ...)

plot.nbpdf = function (x, use.plot3d=FALSE, xlab="x", ylab="y", xlim, ylim, zlim, ..., all=FALSE)
{   nbpdf.f = x
    
    this = attributes (nbpdf.f)
    if (all)
    {   par (mfrow=c (2, 2) )
        p0 = par (mar=c (2, 2.5, 1, 0.175) )
        nbcdf.f = nbcdf (0, 0, 1, 1, 0)
        attributes (nbcdf.f)$vector.means = this$vector.means
        attributes (nbcdf.f)$matrix.variances = this$matrix.variances
        plot (nbpdf.f, FALSE, xlab, ylab, xlim, ylim, zlim, drawlabels=FALSE, ...)
        plot (nbpdf.f, TRUE, xlab, ylab, xlim, ylim, zlim, ...)
        plot (nbcdf.f, FALSE, xlab, ylab, xlim, ylim, zlim, drawlabels=FALSE, ...)
        plot (nbcdf.f, TRUE, xlab, ylab, xlim, ylim, zlim, ...)
        par (p0)
    }
    else
    {   if (missing (xlim) )
            xlim = this$vector.means [1] + c (-3, 3) * this$matrix.variances [1, 1]
        if (missing (ylim) )
            ylim = this$vector.means [2] + c (-3, 3) * this$matrix.variances [2, 2]
        x = seq (xlim [1], xlim [2], length.out=20)
        y = seq (ylim [1], ylim [2], length.out=20)
        z = outer (x, y, nbpdf.f)
        if (use.plot3d)
            plot3d.continuous.regular (z, zlim=zlim, ...)
        else
        {   p0 = par (pty="s")
            contour.default (z=z, xlab=xlab, ylab=ylab, ...)
            par (p0)
        }
    }
}

plot.nbcdf = function (x, use.plot3d=FALSE, xlab="x", ylab="y", xlim, ylim, zlim, ...)
      plot.nbpdf (x, use.plot3d, xlab, ylab, xlim, ylim, zlim, ...)
