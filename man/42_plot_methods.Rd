\name{42_plot_methods}
\alias{Main Plotting Functions}
\alias{bv.plotf.DUBVPMF}
\alias{bv.plotf.DUBVCDF}
\alias{bv.plotf.BNBVPMF}
\alias{bv.plotf.BNBVCDF}
\alias{bv.plotf.PBVPMF}
\alias{bv.plotf.PBVCDF}
\alias{bv.plotf.CUBVPDF}
\alias{bv.plotf.CUBVCDF}
\alias{bv.plotf.NBVPDF}
\alias{bv.plotf.NBVCDF}
\alias{bv.plotf.NTVPDF}
\alias{bv.plotf.BMBVPDF}
\alias{bv.plotf.BMBVCDF}
\alias{bv.plotf.DTVPDF}
\alias{bv.plotf.GBVPMF}
\alias{bv.plotf.KBVPDF}
\alias{bv.plotf.EBVCDF}
\alias{ebvcdf_plot_reg}
\alias{ebvcdf_plot_step_2d}
\alias{ebvcdf_plot_step_3d}
\title{Plot Methods}
\description{
Plot methods for bv (bivariate) and tv (trivariate) objects.

Note that you can use the standard plot function.

Also, note that the misc3d package needs to be installed and loaded, in order to plot the trivariate normal distribution.
}
\usage{
########################################
#discrete uniform
########################################
\method{bv.plotf}{DUBVPMF}(f, in3d=TRUE, \dots, xlim, ylim, all=FALSE)
\method{bv.plotf}{DUBVCDF}(F, in3d=TRUE, \dots, xlim, ylim)

########################################
#binomial
########################################
\method{bv.plotf}{BNBVPMF}(f, in3d=TRUE, \dots, xlim, ylim, all=FALSE)
\method{bv.plotf}{BNBVCDF}(F, in3d=TRUE, \dots, xlim, ylim)

########################################
#poisson
########################################
\method{bv.plotf}{PBVPMF}(f, in3d=TRUE, \dots, xlim, ylim, all=FALSE)
\method{bv.plotf}{PBVCDF}(F, in3d=TRUE, \dots, xlim, ylim)

########################################
#continuous uniform
########################################
\method{bv.plotf}{CUBVPDF}(f, in3d=TRUE, \dots, all=FALSE, n=20)
\method{bv.plotf}{CUBVCDF}(F, in3d=TRUE, \dots, n=20)

########################################
#normal
#(bivariate and trivariate)
########################################
\method{bv.plotf}{NBVPDF}(f, in3d=TRUE, \dots, xlim, ylim, all=FALSE, n=30)
\method{bv.plotf}{NBVCDF}(F, in3d=TRUE, \dots, xlim, ylim, n=30)

\method{bv.plotf}{NTVPDF}(f, iso=TRUE, \dots, xlim, ylim, zlim)

########################################
#bimodal
########################################
\method{bv.plotf}{BMBVPDF}(f, in3d=TRUE, \dots, xlim, ylim, all=FALSE, n=40)
\method{bv.plotf}{BMBVCDF}(F, in3d=TRUE, \dots, xlim, ylim, n=40)

########################################
#categorical, dirichlet, kernel, ecdf
########################################
\method{bv.plotf}{GBVPMF}(f, in3d=TRUE, data, \dots)

\method{bv.plotf}{DTVPDF}(f, in3d=TRUE, \dots, log=FALSE, n=30)

\method{bv.plotf}{KBVPDF}(fh, in3d=TRUE, data = (fh@n <= 2000), \dots,
    xlim, ylim, n=30, point.color="#00000030")

\method{bv.plotf}{EBVCDF}(Fh, in3d=TRUE, \dots, reg = (Fh@n > 40) )

########################################
#additional plotting functions
#(called by the ebvcdf method, above)
########################################
ebvcdf_plot_reg (Fh, in3d=TRUE, \dots, xlim, ylim, n=30)

ebvcdf_plot_step_2d (Fh, data=TRUE, steps=data, \dots,
    point.color="#00000030", line.color="#000000", border.color="#808080",
    main.colff = st.litmus.fit (theme), rim.colff=rim.litmus.fit,
    theme)

ebvcdf_plot_step_3d (Fh, \dots,
    top.color = st.top.color (theme), side.color = st.side.color (theme),
    rim.color="#D0D0D0",
    theme)
}
\arguments{
\item{f, F, fh, Fh}{
	A function representing a PMF/PDF or CDF, from this package.\cr
	Refer to the see also section.}
\item{in3d}{Logical value, if true, a 3d plot, if false, a 2d plot.}
\item{xlim, ylim, zlim}{The x, y and z ranges for the plot.}
\item{all}{
	Logical value, if true, plot a two by two array of both the PMF/PDF and CDF using both 2d and 3d plots.\cr\cr
	Note that by default, ref.arrows (reference arrows in 3d plots) will be true, regardless of global options.\cr
	And some plotting arguments are ignored.}
\item{n}{
	Integer vector of length one or two, the number of grid bins/points in each x and y direction.\cr
	(Except for Dirichlet distributions, where it needs to be scalar).\cr\cr
	Note that, n, is also an attribute of kbvpdf and ebvcdf objects, representing sample size.}
\item{iso}{Logical value, if true, plot isosurfaces.}
\item{log}{Logical value, if true, plot the log density.}
\item{reg}{
	Logical value, if true, plot the probability distribution as a continuous surface, evaluated over a regularly-spaced grid.\cr
	(Refer to details).}
\item{data}{
	Logical value, if true, plot the data points/labels.\cr
	Ignored, if in3d is true.}
\item{steps}{Logical value, if true, plot the steps.}
\item{point.color, line.color, border.color}{String, the color for points, lines and borders.}
\item{top.color, side.color, rim.color}{
	Strings, giving the bar top, bar side and rim bar colors.}
\item{main.colff, rim.colff}{
	Litmus-fitting functions.\cr
	Refer to the barsurf package.\cr\cr
	Note that the default for rim.colff is rim.litmus.fit, which is defined in this package.}
\item{theme}{Refer to barsurf package.}
\item{\dots}{Other arguments for plotting functions in the barsurf package.\cr Refer to the details section.}
}
\details{
You can use the standard plot function.\cr
i.e. Calling plot will call the bv.plotf function.\cr

\emph{GENERAL INFORMATION}\cr

These functions call functions in the barsurf package:

2d plotting functions call plot_dfield, plot_cfield or plot_tricontour.\cr
(The categorical method, calls plot_matrix, which calls plot_dfield).

3d plotting functions call plot_bar, plot_surface or plot_trisurface.\cr
Except for the trivariate normal method, which calls plotf_isosurface or plotf_cfield3.

Re-iterating, the misc3d package needs to be installed and loaded, in order to plot the trivariate normal distribution.

The barsurf package is built on top of the base graphics system, but uses a different color system.\cr
You will need to refer to the documentation for that package to change surface and heatmap colors.\cr

\emph{DEFAULTS}\cr

As of bivariate version 0.6.0, 3d plots are the default.

By default, 3d plots use reference arrows.\cr
(This can be changed by first calling set.bs.options with ref.arrows=FALSE).

By default, 3d PMFs/PDFs have no z axis markup, but 3d CDFs do.\cr
(This can be changed by setting the z.axis argument to true or false).

By default, 3d PMFs/PDFs have zlim equal to c (0, zmax).\cr
And by default, 3d CDFs have zlim equal to c (0, 1).

Where in general, zmax will be the maximum value of the PMF/PDF, within the evaluation window.\cr
Exceptions include uniform distributions (which use zero to one, if the maximum function value is less than or equal to one) and Dirichlet distributions (which vertically center the surface, if all alpha parameters are equal to one).

By default, 2d PMFs/PDFs have unlabelled contour lines, and 2d CDFs have labelled contour lines at 0.2, 0.4, 0.6 and 0.8.\cr
(The ncontours, contour.labels, fb and clabs arguments, can be used to change this).\cr

\emph{CATEGORICAL DISTRIBUTIONS}\cr

Note that the 2d plot has a different orientation to other 2d plots in this package.

For optimal results, you should update the plot margins, before and after the calling the plot method.\cr
(This can be done, using the matrix.margins function, from the barsurf package).\cr

\emph{EMPIRICAL CUMULATIVE DISTRIBUTION FUNCTIONS}\cr

The plot method for ebvcdf objects calls either plot_ebvcdf_reg, plot_ebvcdf_step_2d or plot_ebvcdf_step_3d, depending on the in3d and reg arguments.

The plot_ebvcdf_step_2d and plot_ebvcdf_step_3d functions plot the ECDF as a step function.\cr
(This could be regarded as an exact representation).

The plot_ebvcdf_reg function plots the ECDF as a continuous surface, evaluated over a regularly-spaced grid.\cr
(This could be regarded as an approximation).

Note that setting reg=FALSE for large sample sizes, is probably a bad idea.\cr
(But the continuous approximation improves as the sample size increases).\cr
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.}
\seealso{
\link{Uniform}\cr
For uniform distributions.

\link{Binomial}, \link{Poisson} and \link{Categorical}\cr
For other probability distributions of discrete random variables.

\link{Normal}, \link{Bimodal}, \link{Dirichlet} and \link{Nonparametric}\cr
For other probability distributions of continuous random variables.

\link{Density Matrices}
}
\examples{
########################################
#PDF
########################################
f <- nbvpdf ()

plot (f)
plot (f, z.axis=TRUE, ref.arrows=FALSE)
plot (f, grid=FALSE, n=60)

plot (f, FALSE)
plot (f, FALSE, hcv=TRUE)

########################################
#CDF
########################################
F <- nbvcdf ()

plot (F)

plot (F, FALSE)
plot (F, FALSE, fb=0.5)

tempff <- function (x)
    barsurf::hot.and.cold.fit (x, t = c (0.45, 0.55) )

plot (F, gradient.shading=FALSE, colff=tempff, n=60)
}
