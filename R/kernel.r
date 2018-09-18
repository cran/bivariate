kbvpdf = function (x, y, bandwidth.X, bandwidth.Y)
{   kbvpdf.f = function (x, y) {.kbvpdf.eval (x, y)}
	xy = cbind (x, y)
	bandwidth = c (bandwidth.X, bandwidth.Y)
    attributes (kbvpdf.f) = list (class="kbvpdf", xy=xy, bandwidth=bandwidth)
    kbvpdf.f
}

.kbvpdf.eval = function (x, y)
	stop (".kbvpdf.eval () not implemented yet")
	
kbvcdf = function (x, y, bandwidth.X, bandwidth.Y)
{   kbvcdf.f = function (x, y) {.kbvcdf.eval (x, y)}
	xy = cbind (x, y)
	bandwidth = c (bandwidth.X, bandwidth.Y)
    attributes (kbvcdf.f) = list (class="kbvcdf", xy=xy, bandwidth=bandwidth)
    kbvcdf.f
}

.kbvcdf.eval = function (x, y)
	stop (".kbvcdf.eval () not implemented yet")

.integrate.model = function (model)
{	x = model$x1
	y = model$x2
	h = model$fhat

	h = rbind (0, h, 0)
	h = cbind (0, h, 0)

	dx = diff (x [1:2])
	dy = diff (y [1:2])
	
	nr = nrow (h) - 1
	nc = ncol (h) - 1
	V = matrix (0, nrow=nr, ncol=nc)

	#compute cuboid volumes
	#note that trapezoids (in 3D) would be better
	for (i in 1:nr)
		for (j in 1:nc)
		{	sub.h = h [i:(i + 1), j:(j + 1)]
			mean.h = mean (sub.h)
			V [i, j] = dx * dy * mean.h
		}

	V = rbind (0, V)
	V = cbind (0, V)
	V
	
	nr = nr + 1
	nc = nc + 1
	F = matrix (0, nrow=nr, ncol=nc)
	for (i in 1:nr)
		for (j in 1:nc)
		{	I = 1:i
			J = 1:j
			F [i, j] = sum (V [I, J])
		}

	F
}

print.kbvpdf = function (x, ...)
{	kbvpdf.f = x

	n = nrow (attributes (kbvpdf.f)$xy)
	if (n > 30)
		attributes (kbvpdf.f)$xy = NULL
	print.dubvpmf (kbvpdf.f, ...)
	if (n > 30)
		cat ("note that x attribute not printed\n")
}

print.kbvcdf = function (x, ...)
	print.kbvpdf (x, ...)

plot.kbvpdf = function (x, use.plot3d=FALSE, np=20, xlab="x", ylab="y", xlim, ylim, zlim, ..., all=FALSE)
{   kbvpdf.f = x

	this = attributes (kbvpdf.f)
	if (all)
    {   par (mfrow=c (2, 2) )
        p0 = par (mar=c (2, 2.5, 1, 0.175) )
        kbvcdf.f = kbvcdf (0, 0, 0, 0)
		attributes (kbvcdf.f)$xy = attributes (kbvpdf.f)$xy
		attributes (kbvcdf.f)$bandwidth = attributes (kbvpdf.f)$bandwidth
        plot (kbvpdf.f, FALSE, np, xlab, ylab, xlim, ylim, zlim, drawlabels=FALSE, ...)
        plot (kbvpdf.f, TRUE, np, xlab, ylab, xlim, ylim, zlim, ...)
        plot (kbvcdf.f, FALSE, np, xlab, ylab, xlim, ylim, zlim, drawlabels=FALSE, ...)
        plot (kbvcdf.f, TRUE, np, xlab, ylab, xlim, ylim, zlim, ...)
        par (p0)
    }
	else
	{	if (missing (xlim) )
			xlim = range (this$xy [,1])
		if (missing (ylim) )
			ylim = range (this$xy [,2])
		x = seq (xlim [1], xlim [2], length.out=np)
		y = seq (ylim [1], ylim [2], length.out=np)
		z =	bkde2D (this$xy, this$bandwidth, c (length (x), length (y) ), list (xlim, ylim), FALSE)$fhat
		if (use.plot3d)
			plot3d.continuous (,, z, zlim=zlim, ...)
		else
		{   p0 = par (pty="s")
			contour.default (z=z, xlab=xlab, ylab=ylab, ...)
			par (p0)
		}
	}
}

plot.kbvcdf = function (x, use.plot3d=FALSE, np=20, xlab="x", ylab="y", xlim, ylim, zlim, ...)
{   this = attributes (x)

    if (missing (xlim) )
		xlim = range (this$xy [,1])
	if (missing (ylim) )
		ylim = range (this$xy [,2])
	x = seq (xlim [1], xlim [2], length.out=np)
	y = seq (ylim [1], ylim [2], length.out=np)
	xlim2 = c (x [2], x [np - 1])
	ylim2 = c (y [2], y [np - 1])
	model =	bkde2D (this$xy, this$bandwidth, c (np - 2, np - 2), list (xlim2, ylim2), FALSE)
	z = .integrate.model (model)
	if (use.plot3d)
		plot3d.continuous (,, z, zlim=zlim, ...)
	else
	{   p0 = par (pty="s")
		contour.default (z=z, xlab=xlab, ylab=ylab, ...)
		par (p0)
	}
}
