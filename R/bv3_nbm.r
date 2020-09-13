#bivariate: Bivariate Probability Distributions
#Copyright (C), Abby Spurdle, 2020

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

.is.pos = function (x, y, z=1)
{	if (x <= 0 || y <= 0 || z <= 0)
		stop ("sd <= 0")
}

.is.pos.def = function (x)
{	v = eigen (x, TRUE, TRUE)$values
	if (! all (v > 0) )
		stop ("resulting covariance matrix not positive definite")
}

.nbv = function (f, CV, mean.X, mean.Y, var.X, var.Y, cov)
{	mean.vector = c (X=mean.X, Y=mean.Y)
	covariance.matrix = c (var.X, cov, cov, var.Y)
	covariance.matrix = matrix (covariance.matrix, 2, 2)
	rownames (covariance.matrix) = colnames (covariance.matrix) = c ("X", "Y")
	.is.pos.def (covariance.matrix)

	EXTEND (f, CV, mean.vector, covariance.matrix)
}

.ntv = function (f, CV, mean.X, mean.Y, mean.Z, var.X, var.Y, var.Z, cov.XY, cov.XZ, cov.YZ)
{	mean.vector = c (X=mean.X, Y=mean.Y, Z=mean.Z)
	covariance.matrix = c (var.X, cov.XY, cov.XZ, cov.XY, var.Y, cov.YZ, cov.XZ, cov.YZ, var.Z)
	covariance.matrix = matrix (covariance.matrix, 3, 3)
	rownames (covariance.matrix) = colnames (covariance.matrix) = c ("X", "Y", "Z")
	.is.pos.def (covariance.matrix)

	EXTEND (f, CV, mean.vector, covariance.matrix)
}

.bmbv = function (f, CV, mean.X1, mean.Y1, var.X1, var.Y1, mean.X2, mean.Y2, var.X2, var.Y2)
{	mean.vector.1 = c (mean.X1, mean.Y1)
	covariance.matrix.1 = diag (c (var.X1, var.Y1) )
	.is.pos (var.X1, var.Y1)

	mean.vector.2 = c (mean.X2, mean.Y2)
	covariance.matrix.2 = diag (c (var.X2, var.Y2) )
	.is.pos (var.X2, var.Y2)
	
	EXTEND (f, CV, mean.vector.1, covariance.matrix.1, mean.vector.2, covariance.matrix.2)
}

nbvpdf = function (mean.X=0, mean.Y=0, sd.X=1, sd.Y=1, cor=0)
{	.is.pos (sd.X, sd.Y)
	nbvpdf.2 (mean.X, mean.Y, sd.X ^ 2, sd.Y ^ 2, sd.X * sd.Y * cor)
}
nbvpdf.2 = function (mean.X=0, mean.Y=0, var.X=1, var.Y=1, cov=0)
	.nbv (.nbvpdf.eval, .CV.nbvpdf, mean.X, mean.Y, var.X, var.Y, cov)

nbvcdf = function (mean.X=0, mean.Y=0, sd.X=1, sd.Y=1, cor=0)
{	.is.pos (sd.X, sd.Y)
	nbvcdf.2 (mean.X, mean.Y, sd.X ^ 2, sd.Y ^ 2, sd.X * sd.Y * cor)
}
nbvcdf.2 = function (mean.X=0, mean.Y=0, var.X=1, var.Y=1, cov=0)
	.nbv (.nbvcdf.eval, .CV.nbvcdf, mean.X, mean.Y, var.X, var.Y, cov)

ntvpdf = function (mean.X=0, mean.Y=0, mean.Z=0, sd.X=1, sd.Y=1, sd.Z=1, cor.XY=0, cor.XZ=0, cor.YZ=0)
{	.is.pos (sd.X, sd.Y, sd.Z)
	ntvpdf.2 (mean.X, mean.Y, mean.Z,
		sd.X^2, sd.Y^2, sd.Z^2,
		sd.X * sd.Y * cor.XY, sd.X * sd.Z * cor.XZ, sd.Y * sd.Z * cor.YZ)
}
ntvpdf.2 = function (mean.X=0, mean.Y=0, mean.Z=0, var.X=1, var.Y=1, var.Z=1, cov.XY=0, cov.XZ=0, cov.YZ=0)
	.ntv (.ntvpdf.eval, .CV.ntvpdf, mean.X, mean.Y, mean.Z, var.X, var.Y, var.Z, cov.XY, cov.XZ, cov.YZ)

bmbvpdf = function (mean.X1, mean.Y1, sd.X1, sd.Y1, mean.X2, mean.Y2, sd.X2, sd.Y2)
{	.is.pos (sd.X1, sd.Y1)
	.is.pos (sd.X2, sd.Y2)
	bmbvpdf.2 (mean.X1, mean.Y1, sd.X1 ^ 2, sd.Y1 ^ 2, mean.X2, mean.Y2, sd.X2 ^ 2, sd.Y2 ^ 2)
}
bmbvpdf.2 = function (mean.X1, mean.Y1, var.X1, var.Y1, mean.X2, mean.Y2, var.X2, var.Y2)
	.bmbv (.bmbvpdf.eval, .CV.bmbvpdf, mean.X1, mean.Y1, var.X1, var.Y1, mean.X2, mean.Y2, var.X2, var.Y2)

bmbvcdf = function (mean.X1, mean.Y1, sd.X1, sd.Y1, mean.X2, mean.Y2, sd.X2, sd.Y2)
{	.is.pos (sd.X1, sd.Y1)
	.is.pos (sd.X2, sd.Y2)
	bmbvcdf.2 (mean.X1, mean.Y1, sd.X1 ^ 2, sd.Y1 ^ 2, mean.X2, mean.Y2, sd.X2 ^ 2, sd.Y2 ^ 2)
}
bmbvcdf.2 = function (mean.X1, mean.Y1, var.X1, var.Y1, mean.X2, mean.Y2, var.X2, var.Y2)
	.bmbv (.bmbvcdf.eval, .CV.bmbvcdf, mean.X1, mean.Y1, var.X1, var.Y1, mean.X2, mean.Y2, var.X2, var.Y2)

bvrng.nbvpdf = function (f, n=1, ...)
{	. = attributes (f)
	s = rmvnorm (n, .$mean.vector, .$covariance.matrix)
	rownames (s) = NULL
	colnames (s) = c ("x", "y")
	s
}

bvrng.ntvpdf = function (f, n=1, ...)
{	. = attributes (f)
	s = rmvnorm (n, .$mean.vector, .$covariance.matrix)
	rownames (s) = NULL
	colnames (s) = c ("x", "y", "z")
	s
}

.nbvpdf.eval = function (x, y)
{	. = THAT ()
	.cbv.eval (., .nbvpdf.eval.ext, x, y)
}

.nbvcdf.eval = function (x, y)
{	. = THAT ()
	.cbv.eval (., .nbvcdf.eval.ext, x, y) 
}

.bmbvpdf.eval = function (x, y)
{	. = THAT ()
	.cbv.eval (., .bmbvpdf.eval.ext, x, y)
}

.bmbvcdf.eval = function (x, y)
{	. = THAT ()
	.cbv.eval (., .bmbvcdf.eval.ext, x, y)
}

.ntvpdf.eval = function (x, y, z)
{	. = THAT ()
	.ctv.eval (., .ntvpdf.eval.ext, x, y, z)
}

.nbvpdf.eval.ext = function (., x, y)
	dmvnorm (cbind (x, y), .$mean.vector, .$covariance.matrix)

.nbvcdf.eval.ext = function (., x, y)
{   n = length (x)
    xy = cbind (x, y)
    z = numeric (n)
    for (i in 1:n)
        z [i] = pmvnorm (c (-Inf, -Inf), xy [i,], .$mean.vector, sigma=.$covariance.matrix)
    z
}

.ntvpdf.eval.ext = function (., x, y, z)
	dmvnorm (cbind (x, y, z), .$mean.vector, .$covariance.matrix)

.bmbvpdf.eval.ext = function (., x, y)
{   xy = cbind (x, y)
    f1 = dmvnorm (xy, .$mean.vector.1, .$covariance.matrix.1)
    f2 = dmvnorm (xy, .$mean.vector.2, .$covariance.matrix.2)
    (f1 + f2) / 2
}

.bmbvcdf.eval.ext = function (., x, y)
{   n = length (x)
    xy = cbind (x, y)
    f1 = f2 = numeric (n)
    for (i in 1:n)
    {    f1 [i] = pmvnorm (c (-Inf, -Inf), xy [i,], .$mean.vector.1, sigma=.$covariance.matrix.1)
         f2 [i] = pmvnorm (c (-Inf, -Inf), xy [i,], .$mean.vector.2, sigma=.$covariance.matrix.2)
    }
    (f1 + f2) / 2
}
