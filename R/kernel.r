kbvpdf = function (x, y, bw.X, bw.Y)
{   kbvpdf.f = function (x, y) {NULL}
	data = cbind (x, y)
	bw = c (bw.X, bw.Y)
    attributes (kbvpdf.f) = list (class="kbvpdf", bw=bw, data=data)
    kbvpdf.f
}

kbvcdf = function (x, y, bw.X, bw.Y)
{   kbvcdf.f = function (x, y) {NULL}
	data = cbind (x, y)
	bw = c (bw.X, bw.Y)
    attributes (kbvcdf.f) = list (class="kbvcdf", data=data, bw=bw)
    kbvcdf.f
}

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

plot.kbvpdf = function (x, use.plot3d=FALSE, npoints=30, xlim, ylim, ..., all=FALSE)
{   kbvpdf.f = x

	. = attributes (kbvpdf.f)
	if (all)
    {   par (mfrow=c (2, 2) )
        p0 = par (mar=c (2, 2.5, 1, 0.175) )
        kbvcdf.f = kbvcdf (0, 0, 0, 0)
		attributes (kbvcdf.f)$data = attributes (kbvpdf.f)$data
		attributes (kbvcdf.f)$bw = attributes (kbvpdf.f)$bw
        plot (kbvpdf.f, FALSE, xlab="", ylab="", npoints, xlim, ylim, drawlabels=FALSE)
        plot (kbvpdf.f, TRUE, npoints, xlim, ylim, ...)
        plot (kbvcdf.f, FALSE, xlab="", ylab="", npoints, xlim, ylim, drawlabels=FALSE)
        plot (kbvcdf.f, TRUE, npoints, xlim, ylim, ...)
        par (p0)
    }
	else
	{	if (missing (xlim) )
			xlim = range (.$data [,1]) + c (-1, 1) * .$bw [1]
		if (missing (ylim) )
			ylim = range (.$data [,2]) + c (-1, 1) * .$bw [2]
		x = seq (xlim [1], xlim [2], length.out=npoints)
		y = seq (ylim [1], ylim [2], length.out=npoints)
		z =	bkde2D (.$data, .$bw, c (npoints, npoints), list (xlim, ylim), FALSE)$fhat
		if (use.plot3d)
			plot3d.surf (,,z, ...)
		else
		{   p0 = par (pty="s")
			.contour (x, y, z, ...)
			par (p0)
		}
	}
}

plot.kbvcdf = function (x, use.plot3d=FALSE, npoints=30, xlim, ylim, ...)
{   . = attributes (x)

    if (missing (xlim) )
		xlim = range (.$data [,1]) + c (-1, 1) * .$bw [1]
	if (missing (ylim) )
		ylim = range (.$data [,2]) + c (-1, 1) * .$bw [2]
	x = seq (xlim [1], xlim [2], length.out=npoints)
	y = seq (ylim [1], ylim [2], length.out=npoints)
	xlim2 = c (x [2], x [npoints - 1])
	ylim2 = c (y [2], y [npoints - 1])
	model =	bkde2D (.$data, .$bw, c (npoints - 2, npoints - 2), list (xlim2, ylim2), FALSE)
	z = .integrate.model (model)
	if (use.plot3d)
		plot3d.surf (,,z, ...)
	else
	{   p0 = par (pty="s")
		.contour (x, y, z, ...)
		par (p0)
	}
}
