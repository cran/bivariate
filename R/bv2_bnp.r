#bivariate: Bivariate Probability Distributions
#Copyright (C), Abby Spurdle, 2018 to 2021

#This program is distributed without any warranty.

#This program is free software.
#You can modify it and/or redistribute it, under the terms of:
#The GNU General Public License, version 2, or (at your option) any later version.

#You should have received a copy of this license, with R.
#Also, this license should be available at:
#https://cran.r-project.org/web/licenses/GPL-2

setClass ("BNBV", contains="DBV",
	slots = list (
		p="numeric",
		n="integer") )
	setClass ("BNBVPMF", contains = c ("BNBV", "DBV", "BV", "PMF", "function") )
	setClass ("BNBVCDF", contains = c ("BNBV", "DBV", "BV", "CDF", "function") )

setClass ("PBV", contains="DBV",
	slots = list (
		lambda="numeric") )
	setClass ("PBVPMF", contains = c ("PBV", "DBV", "BV", "PMF", "function") )
	setClass ("PBVCDF", contains = c ("PBV", "DBV", "BV", "CDF", "function") )

.bnbv = function (sf, CV, p.Xi, p.Yi, n)
{	p = c (p.Xi, p.Yi)
	p = as.numeric (p)
	n = as.integer (n)
	if (any (p < 0 | p > 1) )
		stop ("probability of success (p.X or p.Y) not in [0, 1]")
	if (n <= 0)
		stop ("number of trials (n) <= 0")

	new (CV, sf,
		.class.info = .get.info (CV),
		p=p,
		n=n)
}

.pbv = function (sf, CV, lambda.1, lambda.2, lambda.3)
{	lambda = c (lambda.1, lambda.2, lambda.3)
	lambda = as.numeric (lambda)
	if (any (lambda [1:2] <= 0) )
		stop ("\nlambda 1 and 2 need to be > 0\n(and mean.X and mean.Y need to be > cov)")

	new (CV, sf,
		.class.info = .get.info (CV),
		lambda=lambda)
}

bnbvpmf = function (p.X=0.5, p.Y=0.5, n=1)
	.bnbv (.bnbvpmf.eval, "BNBVPMF", p.X, p.Y, n)
bnbvcdf = function (p.X=0.5, p.Y=0.5, n=1)
	.bnbv (.bnbvcdf.eval, "BNBVCDF", p.X, p.Y, n)

pbvpmf = function (lambda.1, lambda.2, lambda.3)
	.pbv (.pbvpmf.eval, "PBVPMF", lambda.1, lambda.2, lambda.3)
pbvcdf = function (lambda.1, lambda.2, lambda.3)
	.pbv (.pbvcdf.eval, "PBVCDF", lambda.1, lambda.2, lambda.3)
pbvpmf.2 = function (mean.X, mean.Y, cov)
	pbvpmf (mean.X - cov, mean.Y - cov, cov)
pbvcdf.2 = function (mean.X, mean.Y, cov)
	pbvcdf (mean.X - cov, mean.Y - cov, cov)

.bnbvpmf.eval = function (x, y)
{	sf = .THIS ()
	.dbv.eval (sf, .bnbvpmf.eval.ext, x, y)
}

.bnbvcdf.eval = function (x, y)
{	sf = .THIS ()
	.dbv.eval (sf, .bnbvcdf.eval.ext, x, y)
}

.pbvpmf.eval = function (x, y)
{	sf = .THIS ()
	.dbv.eval (sf, .pbvpmf.eval.ext, x, y, c (0, 0) )
}

.pbvcdf.eval = function (x, y)
{	sf = .THIS ()
	.dbv.eval (sf, .pbvcdf.eval.ext, x, y, c (0, 0) )
}

.bnbvpmf.eval.ext = function (sf, x, y)
{	{{p = sf@p; n = sf@n}}
	dbinom (x, n, p [1]) * dbinom (y, n, p [2])
}

.bnbvcdf.eval.ext = function (sf, x, y)
{	{{p = sf@p; n = sf@n}}
	pbinom (x, n, p [1]) * pbinom (y, n, p [2])
}

.pbvpmf.eval.ext = function (sf, x, y)
{	{{lambda = sf@lambda}}
	t1 = exp (-sum (lambda) )
	t4b = lambda [3] / lambda [1] / lambda [2]
	n = length (x)
	z = numeric (n)
	for (i in 1:n)
	{	if (x [i] < 0 || y [i] < 0)
			z [i] = 0
		else
		{	t2 = lambda [1] ^ x [i] / factorial (x [i])
			t3 = lambda [2] ^ y [i] / factorial (y [i])
			t4 = 0
			for (k in 0:min (c (x [i], y [i]) ) )
				t4 = t4 + choose (x [i], k) * choose (y [i], k) * factorial (k) * t4b ^ k
			z [i] = t1 * t2 * t3 * t4
		}
	}
	z
}

.pbvcdf.eval.ext = function (sf, x, y)
{	n = length (x)
	z = numeric (n)
	for (i in 1:n)
	{	if (x [i] < 0 || y [i] < 0)
			z [i] = 0
		else
			z [i] = .pbvcdf.eval.ext2 (sf, x [i], y [i])
	}
	z
}

.pbvcdf.eval.ext2 = function (sf, x, y)
{	z = 0
	for (i in 0:x)
		for (j in 0:y)
			z = z + .pbvpmf.eval.ext (sf, i, j)
	z
}
