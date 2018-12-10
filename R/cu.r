cubvpdf = function (a.X, b.X, a.Y, b.Y)
{   cubvpdf.f = function (x, y)
	{	x = as.numeric (x)
		y = as.numeric (y)
		stopifnot (length (x) == length (y) )
		.cubvpdf.eval (x, y)
	}
	a = c (a.X, a.Y)
	b = c (b.X, b.Y)
    attributes (cubvpdf.f) = list (class="cubvpdf", a=a, b=b)
    cubvpdf.f
}

.cubvpdf.eval = function (x, y)
{   . = attributes (sys.function (-1) )
    z = rep (1 / (.$b [1] - .$a [1]) / (.$b [2] - .$a [2]), length (x) )
	z [x < .$a [1] | x > .$b [1] | y < .$a [2] | y > .$b [2]] = 0
    z
}

cubvcdf = function (a.X, b.X, a.Y, b.Y)
{   cubvcdf.f = function (x, y)
	{	x = as.numeric (x)
		y = as.numeric (y)
		stopifnot (length (x) == length (y) )
		.cubvcdf.eval (x, y)
	}
	a = c (a.X, a.Y)
	b = c (b.X, b.Y)
    attributes (cubvcdf.f) = list (class="cubvcdf", a=a, b=b)
    cubvcdf.f
}

.cubvcdf.eval = function (x, y)
{   . = attributes (sys.function (-1) )
	x [x > .$b [1] ] = .$b [1]
	y [y > .$b [2] ] = .$b [2]
    z1 = (x - .$a [1]) / (.$b [1] - .$a [1])
	z2 = (y - .$a [2]) / (.$b [2] - .$a [2])
	z = z1 * z2
	z [x < .$a [1] | y < .$a [2] ] = 0
	z
}

plot.cubvpdf = function (x, use.plot3d=FALSE, npoints=20, xlim, ylim, ..., all=FALSE)
{   is.cubvcdf = (any (class (x) == "cubvcdf") )
	cubvpdf.f = x

    . = attributes (cubvpdf.f)
	if (all)
	{   par (mfrow=c (2, 2) )
        p0 = par (mar=c (2, 2.5, 1, 0.175) )
        cubvcdf.f = cubvcdf (0, 1, 0, 1)
        attributes (cubvcdf.f)$a = .$a
        attributes (cubvcdf.f)$b = .$b
        plot (cubvpdf.f, FALSE, xlab="", ylab="", npoints, xlim, ylim, drawlabels=FALSE)
        plot (cubvpdf.f, TRUE, npoints, xlim, ylim, ...)
        plot (cubvcdf.f, FALSE, xlab="", ylab="", npoints, xlim, ylim, drawlabels=FALSE)
        plot (cubvcdf.f, TRUE, npoints, xlim, ylim, ...)
        par (p0)
    }
	else
	{	if (missing (xlim) )
			xlim = c (.$a [1], .$b [1])
		if (missing (ylim) )
			ylim = c (.$a [2], .$b [2])
		x = seq (xlim [1], xlim [2], length.out=npoints)
		y = seq (ylim [1], ylim [2], length.out=npoints)
		z = outer (x, y, cubvpdf.f)
		if (is.cubvcdf)
		{	if (use.plot3d)
				plot3d.surf (,,z, ...)
			else
			{	p0 = par (pty="s")
				.contour (x, y, z, ...)
				par (p0)
			}
		}
		else
		{	y = 1 / (.$b [1] - .$a [1]) / (.$b [2] - .$a [2])
			if (use.plot3d)
				plot3d.surf (,,z, zlim=c(0, y), ...)
			else
			{   p0 = par (pty="s")
				.plot.cubvpdf (y, xlim, ylim, ...)
				par (p0)
			}
        }
	}
}

plot.cubvcdf = function (x, use.plot3d=FALSE, npoints=20, xlim, ylim, ...)
      plot.cubvpdf (x, use.plot3d, npoints, xlim, ylim, ...)
