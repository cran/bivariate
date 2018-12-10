.plot.cubvpdf = function (y, xlim, ylim, main="", xlab="x", ylab="y", ...)
{	plot.new ()
	plot.window (xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim)
	box ()
	title (main=main, xlab=xlab, ylab=ylab)
	axis (1)
	axis (2)
	rect (xlim [1], ylim [1], xlim [2], ylim [2])
	text (mean (xlim), mean (ylim), y)
}

.contour = function (x, y, z, xlab="x", ylab="y", ...)
	contour (x, y, z, xlab=xlab, ylab=ylab, ...)
