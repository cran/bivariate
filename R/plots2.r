#bivariate: Bivariate Probability Distributions
#Copyright (C), Abby Spurdle, 2018 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.plot.gbv = function (sf, data, ..., bin.labels, xlab="x", ylab="y", blabs)
{	fv = bvmat (sf)@fv
	if (missing (blabs) )
		blabs = round (fv, 2)
	plot_matrix (,,fv, ..., xlab=xlab, ylab=ylab, bin.labels=data, blabs=blabs)
}

.plot.dtv = function (sf, in3d, log, ...,  xlab="x", ylab="y", zlab, contours, zlim, n=30)
{	is.const = all (sf@alpha == 1)

	fv = bvmat (sf, log=log, n=n)@fv
	if (in3d)
	{	if (missing (zlab) )
			zlab = paste0 ("f (", xlab, ", ", ylab, ")")
		if (missing (zlim) )
		{	if (log)
				zlim = range (fv, na.rm=TRUE)
			else if (is.const)
				zlim = c (0, 4)
			else
				zlim = c (0 , max (fv, na.rm=TRUE) )
		}
		plot_trisurface (,,fv, xlab=xlab, ylab=ylab, zlab=zlab, zlim=zlim, ...)
	}
	else
	{	if (missing (contours) )
			contours = (! is.const)
		plot_tricontour (,,fv, xlab=xlab, ylab=ylab, contours=contours, ...)
	}
}

.plot.kbv = function (fh, xlim, ylim, point.color, ..., n,
	xlab, ylab, zlab,
	add=FALSE, contours=TRUE, heatmap=TRUE, xyrel="m")
{	{{vname = fh@variable.names; x = fh@data [,1]; y = fh@data [,2]}}

	if (missing (xlab) ) xlab = vname [1]
	if (missing (ylab) ) ylab = vname [2]
	{{	. = bvmat (fh, xlim, ylim, n=n)
		ux = .@x; uy = .@y; fv = .@fv
	}}

	plot_cfield (ux, uy, fv, xlab=xlab, ylab=ylab, add=add, contours=FALSE, heatmap=heatmap, xyrel=xyrel, ...)
	points (x, y, pch=16, col=point.color)
	if (contours)
		plot_cfield (ux, uy, fv, add=TRUE, contours=TRUE, heatmap=FALSE, ...)
}


.ebvcdf_plot_step_2d = function (Fh, data.points, data.lines, point.color, line.color, border.color, xmain.colff, rim.colff, ...,
	xlab, ylab,
	add=FALSE, contours=FALSE, heatmap=TRUE,
	xyrel="m", continuous.axes=TRUE)
{	{{n = Fh@n; vname = Fh@variable.names; x = Fh@data [,1]; y = Fh@data [,2]}}

	if (missing (xlab) ) xlab = vname [1]
	if (missing (ylab) ) ylab = vname [2]

	. = bvmat (Fh, reg=FALSE, extend=TRUE)
	{{ux = .@x; uy = .@y; fv = .@fv}}

	I = c (1, nrow (fv) )
	J = c (1, ncol (fv) )

	colors = xmain.colff (fv)(fv)
	rim.colors = rim.colff (fv)(fv)
	colors [I,] = rim.colors [I,]
	colors [,J] = rim.colors [,J]
	plot_dfield (ux, uy, fv, xlab=xlab, ylab=ylab, add=add, contours=FALSE, heatmap=heatmap, xyrel=xyrel,
		continuous.axes=continuous.axes, colors=colors, ...)
	rect (min (x), min (y), max (x), max (y), lty=3, border=border.color)

	if (data.points)
		points (x, y, pch=16, col=point.color)
	if (data.lines)
	{	xmax = max (ux)
		ymax = max (uy)
		for (i in 1:(n) )
		{	lines (c (x [i], x [i]), c (y [i], ymax), col=line.color)
			lines (c (x [i], xmax), c (y [i], y [i]), col=line.color )
		}
	}
	if (contours)
		plot_dfield (ux, uy, fv, add=TRUE, contours=TRUE, heatmap=FALSE, ...)
}

.ebvcdf_plot_step_3d = function (Fh, top.color, side.color, rim.color, ...,
	xlab, ylab, zlab,
	z.axis = TRUE, zlim = c (0, 1), continuous.axes=TRUE)
{	{{vname = Fh@variable.names}}

	if (missing (xlab) ) xlab = vname [1]
	if (missing (ylab) ) ylab = vname [2]
	if (missing (zlab) ) zlab = paste0 ("Fh (", xlab, ", ", ylab, ")")

	{{	. = bvmat (Fh, reg=FALSE, extend=TRUE)
		ux = .@x; uy = .@y; fv = .@fv
	}}
	nr = nrow (fv)
	nc = ncol (fv)
	I = c (1, nr)
	J = c (1, nc)

	top.color = matrix (top.color, nr, nc)
	side.color = matrix (side.color, nr, nc)
	rim.color = matrix (rim.color, nr, nc)
	top.color [I,] = side.color [I,] = rim.color [I,]
	top.color [,J] = side.color [,J] = rim.color [,J]
	plot_bar (ux, uy, fv, xlab=xlab, ylab=ylab, zlab=zlab,
		z.axis=z.axis, zlim=zlim, top.color=top.color, side.color=side.color, continuous.axes=continuous.axes, ...)
}
