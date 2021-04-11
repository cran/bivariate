#bivariate: Bivariate Probability Distributions
#Copyright (C), Abby Spurdle, 2018 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

setClass ("NBV", contains="CBV",
	slots = list (
		mean.vector="numeric",
		covariance.matrix="matrix") )
	setClass ("NBVPDF", contains = c ("NBV", "CBV", "BV", "PDF", "function") )
	setClass ("NBVCDF", contains = c ("NBV", "CBV", "BV", "CDF", "function") )

setClass ("BMBV", contains="CBV",
	slots = list (
		mean.vector.1="numeric",
		mean.vector.2="numeric",
		covariance.matrix.1="matrix",
		covariance.matrix.2="matrix") )
	setClass ("BMBVPDF", contains = c ("BMBV", "CBV", "BV", "PDF", "function") )
	setClass ("BMBVCDF", contains = c ("BMBV", "CBV", "BV", "CDF", "function") )

setClass ("NTV", contains="CTV",
	slots = list (
		mean.vector="numeric",
		covariance.matrix="matrix") )
	setClass ("NTVPDF", contains = c ("NTV", "CTV", "TV", "PDF", "function") )

.is.pos = function (x, y, z=1)
{	if (x <= 0 || y <= 0 || z <= 0)
		stop ("sd <= 0")
}

.is.pos.def = function (x)
{	v = eigen (x, TRUE, TRUE)$values
	if (! all (v > 0) )
		stop ("resulting covariance matrix not positive definite")
}

.nbv = function (sf, CV, mean.X, mean.Y, var.X, var.Y, cov)
{	mean.vector = c (X=mean.X, Y=mean.Y)
	covariance.matrix = c (var.X, cov, cov, var.Y)
	covariance.matrix = matrix (covariance.matrix, 2, 2)
	rownames (covariance.matrix) = colnames (covariance.matrix) = c ("X", "Y")
	.is.pos.def (covariance.matrix)

	new (CV, sf,
		.class.info = .get.info (CV),
		mean.vector=mean.vector,
		covariance.matrix=covariance.matrix)
}

.ntv = function (sf, CV, mean.X, mean.Y, mean.Z, var.X, var.Y, var.Z, cov.XY, cov.XZ, cov.YZ)
{	mean.vector = c (X=mean.X, Y=mean.Y, Z=mean.Z)
	covariance.matrix = c (var.X, cov.XY, cov.XZ, cov.XY, var.Y, cov.YZ, cov.XZ, cov.YZ, var.Z)
	covariance.matrix = matrix (covariance.matrix, 3, 3)
	rownames (covariance.matrix) = colnames (covariance.matrix) = c ("X", "Y", "Z")
	.is.pos.def (covariance.matrix)

	new (CV, sf,
		.class.info = .get.info (CV),
		mean.vector=mean.vector,
		covariance.matrix=covariance.matrix)
}

.bmbv = function (sf, CV, mean.X1, mean.Y1, var.X1, var.Y1, mean.X2, mean.Y2, var.X2, var.Y2)
{	mean.vector.1 = c (mean.X1, mean.Y1)
	covariance.matrix.1 = diag (c (var.X1, var.Y1) )
	.is.pos (var.X1, var.Y1)

	mean.vector.2 = c (mean.X2, mean.Y2)
	covariance.matrix.2 = diag (c (var.X2, var.Y2) )
	.is.pos (var.X2, var.Y2)
	
	new (CV, sf,
		.class.info = .get.info (CV),
		mean.vector.1=mean.vector.1,
		covariance.matrix.1=covariance.matrix.1,
		mean.vector.2=mean.vector.2,
		covariance.matrix.2=covariance.matrix.2)
}

nbvpdf = function (mean.X=0, mean.Y=0, sd.X=1, sd.Y=1, cor=0)
{	.is.pos (sd.X, sd.Y)
	nbvpdf.2 (mean.X, mean.Y, sd.X^2, sd.Y^2, sd.X * sd.Y * cor)
}
nbvpdf.2 = function (mean.X=0, mean.Y=0, var.X=1, var.Y=1, cov=0)
	.nbv (.nbvpdf.eval, "NBVPDF", mean.X, mean.Y, var.X, var.Y, cov)

nbvcdf = function (mean.X=0, mean.Y=0, sd.X=1, sd.Y=1, cor=0)
{	.is.pos (sd.X, sd.Y)
	nbvcdf.2 (mean.X, mean.Y, sd.X^2, sd.Y^2, sd.X * sd.Y * cor)
}
nbvcdf.2 = function (mean.X=0, mean.Y=0, var.X=1, var.Y=1, cov=0)
	.nbv (.nbvcdf.eval, "NBVCDF", mean.X, mean.Y, var.X, var.Y, cov)

ntvpdf = function (mean.X=0, mean.Y=0, mean.Z=0, sd.X=1, sd.Y=1, sd.Z=1, cor.XY=0, cor.XZ=0, cor.YZ=0)
{	.is.pos (sd.X, sd.Y, sd.Z)
	ntvpdf.2 (mean.X, mean.Y, mean.Z,
		sd.X^2, sd.Y^2, sd.Z^2,
		sd.X * sd.Y * cor.XY, sd.X * sd.Z * cor.XZ, sd.Y * sd.Z * cor.YZ)
}
ntvpdf.2 = function (mean.X=0, mean.Y=0, mean.Z=0, var.X=1, var.Y=1, var.Z=1, cov.XY=0, cov.XZ=0, cov.YZ=0)
	.ntv (.ntvpdf.eval, "NTVPDF", mean.X, mean.Y, mean.Z, var.X, var.Y, var.Z, cov.XY, cov.XZ, cov.YZ)

bmbvpdf = function (mean.X1=0, mean.X2=0, mean.Y1=0, mean.Y2=0, sd.X1=1, sd.X2=1, sd.Y1=1, sd.Y2=1)
{	.is.pos (sd.X1, sd.Y1)
	.is.pos (sd.X2, sd.Y2)
	bmbvpdf.2 (mean.X1, mean.X2, mean.Y1, mean.Y2, sd.X1^2, sd.X2^2, sd.Y1^2, sd.Y2^2)
}
bmbvpdf.2 = function (mean.X1=0, mean.X2=0, mean.Y1=0, mean.Y2=0, var.X1=1, var.X2=1, var.Y1=1, var.Y2=1)
	.bmbv (.bmbvpdf.eval, "BMBVPDF", mean.X1, mean.Y1, var.X1, var.Y1, mean.X2, mean.Y2, var.X2, var.Y2)

bmbvcdf = function (mean.X1=0, mean.X2=0, mean.Y1=0, mean.Y2=0, sd.X1=1, sd.X2=1, sd.Y1=1, sd.Y2=1)
{	.is.pos (sd.X1, sd.Y1)
	.is.pos (sd.X2, sd.Y2)
	bmbvcdf.2 (mean.X1, mean.X2, mean.Y1, mean.Y2, sd.X1^2, sd.X2^2, sd.Y1^2, sd.Y2^2)
}
bmbvcdf.2 = function (mean.X1=0, mean.X2=0, mean.Y1=0, mean.Y2=0, var.X1=1, var.X2=1, var.Y1=1, var.Y2=1)
	.bmbv (.bmbvcdf.eval, "BMBVCDF", mean.X1, mean.Y1, var.X1, var.Y1, mean.X2, mean.Y2, var.X2, var.Y2)

bvrng.NBVPDF = function (f, n=1, ...)
{	s = rmvnorm (n, f@mean.vector, f@covariance.matrix)
	rownames (s) = NULL
	colnames (s) = c ("x", "y")
	s
}

bvrng.NTVPDF = function (f, n=1, ...)
{	s = rmvnorm (n, f@mean.vector, f@covariance.matrix)
	rownames (s) = NULL
	colnames (s) = c ("x", "y", "z")
	s
}

.nbvpdf.eval = function (x, y)
{	sf = .THIS ()
	.cbv.eval (sf, .nbvpdf.eval.ext, x, y)
}

.nbvcdf.eval = function (x, y)
{	sf = .THIS ()
	.cbv.eval (sf, .nbvcdf.eval.ext, x, y) 
}

.bmbvpdf.eval = function (x, y)
{	sf = .THIS ()
	.cbv.eval (sf, .bmbvpdf.eval.ext, x, y)
}

.bmbvcdf.eval = function (x, y)
{	sf = .THIS ()
	.cbv.eval (sf, .bmbvcdf.eval.ext, x, y)
}

.ntvpdf.eval = function (x, y, z)
{	sf = .THIS ()
	.ctv.eval (sf, .ntvpdf.eval.ext, x, y, z)
}

.nbvpdf.eval.ext = function (sf, x, y)
	dmvnorm (cbind (x, y), sf@mean.vector, sf@covariance.matrix)

.nbvcdf.eval.ext = function (sf, x, y)
{   n = length (x)
    xy = cbind (x, y)
    z = numeric (n)
    for (i in 1:n)
        z [i] = pmvnorm (c (-Inf, -Inf), xy [i,], sf@mean.vector, sigma=sf@covariance.matrix)
    z
}

.ntvpdf.eval.ext = function (sf, x, y, z)
	dmvnorm (cbind (x, y, z), sf@mean.vector, sf@covariance.matrix)

.bmbvpdf.eval.ext = function (sf, x, y)
{   xy = cbind (x, y)
    f1 = dmvnorm (xy, sf@mean.vector.1, sf@covariance.matrix.1)
    f2 = dmvnorm (xy, sf@mean.vector.2, sf@covariance.matrix.2)
    (f1 + f2) / 2
}

.bmbvcdf.eval.ext = function (sf, x, y)
{   n = length (x)
    xy = cbind (x, y)
    f1 = f2 = numeric (n)
    for (i in 1:n)
    {    f1 [i] = pmvnorm (c (-Inf, -Inf), xy [i,], sf@mean.vector.1, sigma=sf@covariance.matrix.1)
         f2 [i] = pmvnorm (c (-Inf, -Inf), xy [i,], sf@mean.vector.2, sigma=sf@covariance.matrix.2)
    }
    (f1 + f2) / 2
}
