#bivariate: Bivariate Probability Distributions
#Copyright (C), Abby Spurdle, 2018 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.plot.bv = function (f, in3d, xlim, ylim, ..., .all=FALSE)
{	if (.all) .plot.bv4 (f, in3d, xlim, ylim, ...)
	else .plot.bv1 (f, in3d, xlim, ylim, ...)
}

.plot.bv1 = function (f, in3d, xlim, ylim, ..., n,
	xlab, ylab, zlab,
	z.axis, zlim,
	contours, ncontours, contour.labels, fb, xyrel)
{	is.cont = is (f, "CBV")
	is.uniform = (is (f, "DUBV") || is (f, "CUBV") )
	is.cdf = is (f, "CDF")
	is.k = is (f, "KBVPDF")
	is.e = is (f, "EBVCDF")
	is.np = (is.k || is.e)

	if (is.e)
		is.cont = TRUE

	if (is.np)
	{	if (missing (xlab) ) xlab = f@variable.names [1]
		if (missing (ylab) ) ylab = f@variable.names [2]
	}
	else
	{	if (missing (xlab) ) xlab = "x"
		if (missing (ylab) ) ylab = "y"
	}
	
	{{	. = bvmat (f, xlim, ylim, reg=TRUE, n=n)
		x = .@x; y = .@y; fv = .@fv
	}}

	if (in3d)
	{	if (missing (zlab) )
		{	if (is.k) zlab = "fh"
			else if (is.e) zlab = "Fh"
			else if (is.cdf) zlab = "F"
			else zlab = "f"

			zlab = paste0 (zlab, " (", xlab, ", ", ylab, ")")
		}
		if (missing (z.axis) )
		{	if (is.cdf) z.axis=TRUE
			else z.axis=FALSE
		}
		if (missing (zlim) )
		{	if (is.uniform)
			{	if (f@p <= 1 || is.cdf) zlim = c (0, 1)
				else zlim = c (0, f@p)
			}
			else if (is.cdf) zlim = c (0, 1)
			else zlim = c (0, max (fv) )
		}

		if (is.cont) plot_surface (x, y, fv, xlab=xlab, ylab=ylab, zlab=zlab, z.axis=z.axis, zlim=zlim, ...)
		else plot_bar (x, y, fv, xlab=xlab, ylab=ylab, zlab=zlab, z.axis=z.axis, zlim=zlim, ...)
	}
	else
	{	if (missing (contours) )
		{	if (is (f, "DUBVPMF") || is (f, "CUBVPDF") )
				contours = FALSE
			else
				contours = TRUE
		}
		if (missing (contour.labels) )
		{	if (is.cdf) contour.labels = TRUE
			else contour.labels = FALSE
		}
		if (missing (ncontours) )
		{	if (is.cdf)
				ncontours=4
			else
			{	if (is.cont) ncontours=6
				else ncontours=2
			}
		}
		if (is.cdf && missing (fb) )
		{	if (ncontours == 1) fb = 0.5
			else if (ncontours == 2) fb = c (0.33, 0.67)
			else if (ncontours == 3) fb = c (0.25, 0.5, 0.75)
			else if (ncontours == 4) fb = c (0.2, 0.4, 0.6, 0.8)
			else
			{	fb = seq (0, 1, length.out = ncontours + 2)
				fb = fb [2:(ncontours + 1)]
				fb = round (fb, 2)
			}
		}
		if (missing (xyrel) )
		{	if (is.np) xyrel="m"
			else xyrel="f"
		}

		if (is.cont) plot_cfield (x, y, fv, xlab=xlab, ylab=ylab, contours=contours, contour.labels=contour.labels, ncontours=ncontours, fb=fb, xyrel=xyrel, ...)
		else plot_dfield (x, y, fv, xlab=xlab, ylab=ylab, contours=contours, contour.labels=contour.labels, ncontours=ncontours, fb=fb, xyrel=xyrel, ...)
	}
}

.plot.bv4 = function (f, in3d, xlim, ylim, ..., main, xlab, ylab, contour.labels, z.axis, ref.arrows=TRUE, cex=0.65)
{	if (is (f, "DUBV") ) F = dubvcdf (0, 0, 0, 0)
	else if (is (f, "CUBV") ) F = cubvcdf (0, 0, 0, 0)
	else if (is (f, "BNBV") ) F = bnbvcdf (0, 0)
	else if (is (f, "PBV") ) F = pbvcdf (1, 1, 0)
	else if (is (f, "NBV") ) F = nbvcdf (0, 0, 1, 1, 0)
	else if (is (f, "BMBV") ) F = bmbvcdf (0, 1)
	else stop ()

	cv = class (F)
	attributes (F) = attributes (f)
	class (F) = cv

	p0 = par (cex=cex, mfrow=c (2, 2), mar = c (2.75, 2.75, 0.25, 0.25) )
	plot (f, FALSE, xlim=xlim, ylim=ylim, ..., xlab="", ylab="")
	plot (f, TRUE, xlim=xlim, ylim=ylim, ..., xlab=xlab, ylab=ylab, nhl=0, z.axis=FALSE, ref.arrows=ref.arrows)
	plot (F, FALSE, xlim=xlim, ylim=ylim, ..., xlab="", ylab="", contour.labels=FALSE)
	plot (F, TRUE, xlim=xlim, ylim=ylim, ..., xlab=xlab, ylab=ylab, nhl=0, z.axis=FALSE, ref.arrows=ref.arrows)
	par (p0)
}

.plot.ntv = function (f, iso, xlim, ylim, zlim, ...,
	xlab="x", ylab="y", zlab="z",
	nsurfaces=3, nslides=7, emph="h")
{	. = attributes (f)
	m = .$mean.vector
	v = .$covariance.matrix
	k = c (-3, 3)
	if (missing (xlim) ) xlim = m [1] + k * sqrt (v [1, 1])
	if (missing (ylim) ) ylim = m [2] + k * sqrt (v [2, 2])
	if (missing (zlim) ) zlim = m [3] + k * sqrt (v [3, 3])
	if (iso)
		plotf_isosurface (f, xlim, ylim, zlim, ..., xlab=xlab, ylab=ylab, zlab=zlab, nsurfaces=nsurfaces)
	else
		plotf_cfield3 (f, xlim, ylim, zlim, ..., xlab=xlab, ylab=ylab, zlab=zlab, nslides=nslides, emph=emph)
}

bv.plotf.DUBVPMF = function (f, in3d=TRUE, ..., xlim, ylim, all=FALSE)
	.plot.bv (f, in3d, xlim, ylim, ..., .all=all)
bv.plotf.DUBVCDF = function (F, in3d=TRUE, ..., xlim, ylim)
	.plot.bv (F, in3d, xlim, ylim, ...)

bv.plotf.BNBVPMF = function (f, in3d=TRUE, ..., xlim, ylim, all=FALSE)
	.plot.bv (f, in3d, xlim, ylim, ..., .all=all)
bv.plotf.BNBVCDF = function (F, in3d=TRUE, ..., xlim, ylim)
	.plot.bv (F, in3d, xlim, ylim, ...)

bv.plotf.PBVPMF = function (f, in3d=TRUE, ..., xlim, ylim, all=FALSE)
	.plot.bv (f, in3d, xlim, ylim, ..., .all=all)
bv.plotf.PBVCDF = function (F, in3d=TRUE, ..., xlim, ylim)
	.plot.bv (F, in3d, xlim, ylim, ...)

bv.plotf.CUBVPDF = function (f, in3d=TRUE, ..., all=FALSE, n=20)
	.plot.bv (f, in3d, ..., .all=all, n=n)
bv.plotf.CUBVCDF = function (F, in3d=TRUE, ..., n=20)
	.plot.bv (F, in3d, ..., n=n)

bv.plotf.NBVPDF = function (f, in3d=TRUE, ..., xlim, ylim, all=FALSE, n=30)
	.plot.bv (f, in3d, xlim, ylim, ..., .all=all, n=n)
bv.plotf.NBVCDF = function (F, in3d=TRUE, ..., xlim, ylim, n=30)
	.plot.bv (F, in3d, xlim, ylim, ..., n=n)

bv.plotf.NTVPDF = function (f, iso=TRUE, ..., xlim, ylim, zlim)
	.plot.ntv (f, iso, xlim, ylim, zlim, ...)

bv.plotf.BMBVPDF = function (f, in3d=TRUE, ..., xlim, ylim, all=FALSE, n=40)
	.plot.bv (f, in3d, xlim, ylim, ..., .all=all, n=n)
bv.plotf.BMBVCDF = function (F, in3d=TRUE, ..., xlim, ylim, n=40)
	.plot.bv (F, in3d, xlim, ylim, ..., n=n)

bv.plotf.GBVPMF = function (f, in3d=TRUE, data, ...)
{	if (in3d) .plot.bv (f, in3d, ...)
	else .plot.gbv (f, data, ...)
}

bv.plotf.DTVPDF = function (f, in3d=TRUE, ..., log=FALSE, n=30)
	.plot.dtv (f, in3d, log, ..., n=n)

bv.plotf.KBVPDF = function (fh, in3d=TRUE, data = (fh@n <= 2000), ..., xlim, ylim, n=30, point.color="#00000030")
{	if (in3d || (! data) ) .plot.bv (fh, in3d, xlim, ylim, ..., n=n)
	else .plot.kbv (fh, xlim, ylim, point.color, ..., n=n)
}

bv.plotf.EBVCDF = function (Fh, in3d=TRUE, ..., reg = (Fh@n > 40) )
{	if (reg) ebvcdf_plot_reg (Fh, in3d, ...)
	else if (in3d) ebvcdf_plot_step_3d (Fh, ...)
	else ebvcdf_plot_step_2d (Fh, ...)
}

ebvcdf_plot_reg = function (Fh, in3d=TRUE, ..., xlim, ylim, n=30)
	.plot.bv (Fh, in3d, xlim, ylim, ..., n=n)

ebvcdf_plot_step_2d = function (Fh, data=TRUE, steps=data, ...,
	point.color="#00000030", line.color="#000000", border.color="#808080",
	main.colff = st.litmus.fit (theme), rim.colff=rim.litmus.fit,
	theme)
	.ebvcdf_plot_step_2d (Fh, data, steps, point.color, line.color, border.color, main.colff, rim.colff, ...)

ebvcdf_plot_step_3d = function (Fh, ...,
	top.color = st.top.color (theme), side.color = st.side.color (theme), rim.color = "#D0D0D0",
	theme)
	.ebvcdf_plot_step_3d (Fh, top.color, side.color, rim.color, ...)
