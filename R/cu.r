cubvpdf = function (a.X, b.X, a.Y, b.Y)
{   cubvpdf.f = function (x, y) {.cubvpdf.eval (x, y)}
	a = c (a.X, a.Y)
	b = c (b.X, b.Y)
    attributes (cubvpdf.f) = list (class="cubvpdf", a=a, b=b)
    cubvpdf.f
}

.cubvpdf.eval = function (x, y)
{   this = attributes (sys.function (-1) )
    n = length (x)
	if (n != length (y) )
		stop ("length (x) must equal length (y)")
    z = rep (1 / (this$b [1] - this$a [1]) / (this$b [2] - this$a [2]), n)
	z [x < this$a [1] | x > this$b [1] | y < this$a [2] | y > this$b [2]] = 0
    z
}

cubvcdf = function (a.X, b.X, a.Y, b.Y)
{   cubvcdf.f = function (x, y) {.cubvcdf.eval (x, y)}
	a = c (a.X, a.Y)
	b = c (b.X, b.Y)
    attributes (cubvcdf.f) = list (class="cubvcdf", a=a, b=b)
    cubvcdf.f
}

.cubvcdf.eval = function (x, y)
{   this = attributes (sys.function (-1) )
    n = length (x)
	if (n != length (y) )
		stop ("length (x) must equal length (y)")
	x [x > this$b [1] ] = this$b [1]
	y [y > this$b [2] ] = this$b [2]
    z1 = (x - this$a [1]) / (this$b [1] - this$a [1])
	z2 = (y - this$a [2]) / (this$b [2] - this$a [2])
	z = z1 * z2
	z [x < this$a [1] | y < this$a [2] ] = 0
	z
}

print.cubvpdf = function (x, ...)
	print.dubvpmf (x, ...)

print.cubvcdf = function (x, ...)
    print.dubvpmf (x, ...)

plot.cubvpdf = function (x, use.plot3d=FALSE, xlab="x", ylab="y", xlim, ylim, zlim, ..., all=FALSE)
{   is.cubvcdf = (any (class (x) == "cubvcdf") )
	cubvpdf.f = x

    this = attributes (cubvpdf.f)
	if (all)
	{   par (mfrow=c (2, 2) )
        p0 = par (mar=c (2, 2.5, 1, 0.175) )
        cubvcdf.f = cubvcdf (0, 1, 0, 1)
        attributes (cubvcdf.f)$a = this$a
        attributes (cubvcdf.f)$b = this$b
        plot (cubvpdf.f, FALSE, xlab, ylab, xlim, ylim, zlim, drawlabels=FALSE, ...)
        plot (cubvpdf.f, TRUE, xlab, ylab, xlim, ylim, zlim, ...)
        plot (cubvcdf.f, FALSE, xlab, ylab, xlim, ylim, zlim, drawlabels=FALSE, ...)
        plot (cubvcdf.f, TRUE, xlab, ylab, xlim, ylim, zlim, ...)
        par (p0)
    }
	else
	{	if (missing (xlim) )
			xlim = c (this$a [1], this$b [1])
		if (missing (ylim) )
			ylim = c (this$a [2], this$b [2])
		if (missing (zlim) )
			zlim = c (0, 1)
		x = seq (xlim [1], xlim [2], length.out=20)
		y = seq (ylim [1], ylim [2], length.out=20)
		z = outer (x, y, cubvpdf.f)
		if (use.plot3d)
			plot3d.continuous (,, z, xlab=xlab, ylab=ylab, zlim=zlim, ...)
		else
		{   p0 = par (pty="s")
			if (is.cubvcdf)
				contour.default (z=z, xlab=xlab, ylab=ylab, ...)
			else
			{	plot.new ()
				plot.window (xlim=xlim, ylim=ylim)
				box ()
				axis (1)
				axis (2)
			}
            par (p0)
        }
	}
}

plot.cubvcdf = function (x, use.plot3d=FALSE, xlab="x", ylab="y", xlim, ylim, zlim, ...)
      plot.cubvpdf (x, use.plot3d, xlab, ylab, xlim, ylim, zlim, ...)
