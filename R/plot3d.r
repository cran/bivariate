plot3d.empty = function ()
{   p0 = par (mar=c (1.75, 0.175, 0.9, 0.175) )
	plot.new ()
	plot.window (c (-0.75, 0.75), c (0, 1.5) )
	.plotbpdf.plane.xy ()
	.plotbpdf.poly (1, c (0, 0, 1, 1), c (0, 1, 1, 0) )
	.plotbpdf.poly (c (0, 0, 1, 1), 1, c (0, 1, 1, 0) )
	.plotbpdf.labs (c ("x", "y") )
	par (p0)
}

#length (x) = nrow (z) + 1
#length (y) = ncol (z) + 1
plot3d.discrete = function (x, y, z, xlab="x", ylab="y", zlim, ..., boundary=FALSE)
{   p0 = par (mar=c (1.75, 0.175, 0.9, 0.175) )
	plot.new ()
	plot.window (c (-0.75, 0.75), c (0, 1.5) )
	.plotbpdf.plane.xy ()
	.plotbpdf.poly (1, c (0, 0, 1, 1), c (0, 1, 1, 0) )
	.plotbpdf.poly (c (0, 0, 1, 1), 1, c (0, 1, 1, 0) )
	.plotbpdf.labs (c (xlab, ylab) )
	if (missing (x) )
		x = 1:(nrow (z) + 1)
	if (missing (y) )
		y = 1:(ncol (z) + 1)
	nx = length (x) - 1
	ny = length (y) - 1
	x = (x - min (x) ) / diff (range (x) )
	y = (y - min (y) ) / diff (range (y) )
	if (missing (zlim) )
		zlim = range (z)
    d = diff (zlim)
    if (d == 0)
		z [TRUE] = 0
	else
	    z = (z - zlim [1]) / d
	fc = rgb (0.7, 0.75, 1)
	ec = "grey"
	for (i in nx:1)
	    for (j in ny:1)
    	{	x1 = x [i]
    		x2 = x [i + 1]
    		y1 = y [j]
    		y2 = y [j + 1]
    		xp = c (x1, x1, x2, x2)
    		yp = c (y1, y2, y2, y1)
    		w0 = z [i, j]
    		w = c (w0, w0, w0, w0)
    		xp1 = c (x1, x1, x1, x1)
    		yp1 = c (y1, y1, y2, y2)
    		w1 = c (0, w0, w0, 0)
    		xp2 = c (x1, x1, x2, x2)
    		yp2 = c (y1, y1, y1, y1)
    		dir = (w [3] - w [1]) / sqrt ( (x2 - x1)^2 + (y2 - y1)^2)
    		if (boundary && (i == 1 || i == nx || j == 1 || j == ny) )
    		{	.plotbpdf.poly (xp1, yp1, w1, col=ec)
    			.plotbpdf.poly (xp2, yp2, w1, col=ec)
    			.plotbpdf.poly (xp, yp, w, col=ec)
    		}
    		else
    		{	.plotbpdf.poly (xp1, yp1, w1, col=fc)
    			.plotbpdf.poly (xp2, yp2, w1, col=fc)
    			.plotbpdf.poly (xp, yp, w, col=rgb (0.3, 0.6, 1) )
    		}
	}
    par (p0)
}

#length (x) = nrow (z)
#length (y) = ncol (z)
plot3d.continuous = function (x, y, z, xlab="x", ylab="y", zlim, ..., col1=rgb (0.3, 0.6, 1), col2="white")
{	p0 = par (mar=c (1.75, 0.175, 0.9, 0.175) )
	plot.new ()
	plot.window (c (-0.75, 0.75), c (0, 1.5) )
	.plotbpdf.plane.xy ()
	.plotbpdf.poly (1, c (0, 0, 1, 1), c (0, 1, 1, 0) )
	.plotbpdf.poly (c (0, 0, 1, 1), 1, c (0, 1, 1, 0) )
	.plotbpdf.labs (c (xlab, ylab) )
	if (missing (x) )
		x = 1:nrow (z)
	if (missing (y) )
		y = 1:ncol (z)
	nx = length (x) - 1
	ny = length (y) - 1
	x = (x - min (x) ) / diff (range (x) )
	y = (y - min (y) ) / diff (range (y) )
	if (missing (zlim) )
		zlim = range (z)
    d = diff (zlim)
    if (d == 0)
		z [TRUE] = 0
	else
	    z = (z - zlim [1]) / d
	for (i in nx:1)
	    for (j in ny:1)
    	{	x1 = x [i]
    		x2 = x [i + 1]
    		y1 = y [j]
    		y2 = y [j + 1]
    		xp = c (x1, x1, x2, x2)
    		yp = c (y1, y2, y2, y1)
    		w = c (z [i, j], z [i, j + 1], z [i + 1, j + 1], z [i + 1, j])
    		dir = (w [3] - w [1]) / sqrt ( (x2 - x1)^2 + (y2 - y1)^2)
    		if (dir < 0)
    		    dir = 0
    		dir = 2 * atan (dir) / pi
    		.plotbpdf.poly (xp, yp, w, "black", col=.colinterp (col1, col2, dir) )
	    }
	par (p0)
}
