bmbvpdf = function (mean.X1, mean.Y1, variance.X1, variance.Y1, mean.X2, mean.Y2, variance.X2, variance.Y2)
{   vector1.means = c (mean.X1, mean.Y1)
    matrix1.variances = diag (c (variance.X1, variance.Y1) )
    vector2.means = c (mean.X2, mean.Y2)
    matrix2.variances = diag (c (variance.X2, variance.Y2) )
    bmbvpdf.f = function (x, y) {.bmbvpdf.eval (x, y)}
    attributes (bmbvpdf.f) = list (class="bmbvpdf", vector1.means=vector1.means, matrix1.variances=matrix1.variances, vector2.means=vector2.means, matrix2.variances=matrix2.variances)
    bmbvpdf.f
}

.bmbvpdf.eval = function (x, y)
{   this = attributes (sys.function (-1) )
    f1 = dmvnorm (cbind (x, y), this$vector1.means, this$matrix1.variances)
    f2 = dmvnorm (cbind (x, y), this$vector2.means, this$matrix2.variances)
    (f1 + f2) / 2
}

bmbvcdf = function (mean.X1, mean.Y1, variance.X1, variance.Y1, mean.X2, mean.Y2, variance.X2, variance.Y2)
{   vector1.means = c (mean.X1, mean.Y1)
    matrix1.variances = diag (c (variance.X1, variance.Y1) )
    vector2.means = c (mean.X2, mean.Y2)
    matrix2.variances = diag (c (variance.X2, variance.Y2) )
    bmbvcdf.f = function (x, y) {.bmbvcdf.eval (x, y)}
    attributes (bmbvcdf.f) = list (class="bmbvcdf", vector1.means=vector1.means, matrix1.variances=matrix1.variances, vector2.means=vector2.means, matrix2.variances=matrix2.variances)
    bmbvcdf.f
}

.bmbvcdf.eval = function (x, y)
{   this = attributes (sys.function (-1) )
    x = cbind (x, y)
    n = nrow (x)
    f1 = f2 = numeric (n)
    for (i in 1:n)
    {    f1 [i] = pmvnorm (c (-Inf, -Inf), x [i,], this$vector1.means, sigma=this$matrix1.variances)
         f2 [i] = pmvnorm (c (-Inf, -Inf), x [i,], this$vector2.means, sigma=this$matrix2.variances)
    }
    (f1 + f2) / 2
}

print.bmbvpdf = function (x, ...)
    print.dubvpmf (x, ...)

print.bmbvcdf = function (x, ...)
    print.dubvpmf (x, ...)

plot.bmbvpdf = function (x, use.plot3d=FALSE, np=20, xlab="x", ylab="y", xlim, ylim, zlim, ..., all=FALSE)
{   bmbvpdf.f = x
    
    this = attributes (bmbvpdf.f)
    if (all)
    {   par (mfrow=c (2, 2) )
        p0 = par (mar=c (2, 2.5, 1, 0.175) )
        bmbvcdf.f = bmbvcdf (0, 0, 1, 1, 0, 0, 1, 1)
        attributes (bmbvcdf.f)$vector1.means = this$vector1.means
		attributes (bmbvcdf.f)$vector2.means = this$vector2.means
        attributes (bmbvcdf.f)$matrix1.variances = this$matrix1.variances
		attributes (bmbvcdf.f)$matrix2.variances = this$matrix2.variances
        plot (bmbvpdf.f, FALSE, np, xlab, ylab, xlim, ylim, zlim, drawlabels=FALSE, ...)
        plot (bmbvpdf.f, TRUE, np, xlab, ylab, xlim, ylim, zlim, ...)
        plot (bmbvcdf.f, FALSE, np, xlab, ylab, xlim, ylim, zlim, drawlabels=FALSE, ...)
        plot (bmbvcdf.f, TRUE, np, xlab, ylab, xlim, ylim, zlim, ...)
        par (p0)
    }
    else
    {   if (missing (xlim) )
        {   xlim1 = this$vector1.means [1] + c (-3, 3) * this$matrix1.variances [1, 1]
			xlim2 = this$vector2.means [1] + c (-3, 3) * this$matrix2.variances [1, 1]
			xlim = range (c (xlim1, xlim2) )
		}
        if (missing (ylim) )
        {   ylim1 = this$vector1.means [2] + c (-3, 3) * this$matrix1.variances [2, 2]
			ylim2 = this$vector2.means [2] + c (-3, 3) * this$matrix2.variances [2, 2]
			ylim = range (c (ylim1, ylim2) )	
		}
        x = seq (xlim [1], xlim [2], length.out=np)
        y = seq (ylim [1], ylim [2], length.out=np)
        z = outer (x, y, bmbvpdf.f)
        if (use.plot3d)
            plot3d.continuous (,, z, xlab=xlab, ylab=ylab, zlim=zlim, ...)
        else
        {   p0 = par (pty="s")
            contour.default (z=z, xlab=xlab, ylab=ylab, ...)
            par (p0)
        }
    }
}

plot.bmbvcdf = function (x, use.plot3d=FALSE, np=20, xlab="x", ylab="y", xlim, ylim, zlim, ...)
      plot.bmbvpdf (x, use.plot3d, np, xlab, ylab, xlim, ylim, zlim, ...)
