.check.lambda.ok = function (lambda)
{	if (lambda [1] <= 0 || lambda [2] <= 0)
		stop ("\nlambda.1 and lambda.2 need to be > 0\nso, mean.X and mean.Y need to be > cov")
}

pbvpmf = function (lambda.1, lambda.2, lambda.3)
{   f = function (x, y)
	{	. = THAT ()
		v = .val.integer.args (x, y)
		.pbvpmf.eval (., v$x, v$y)
	}
	lambda = c (lambda.1, lambda.2, lambda.3)
	.check.lambda.ok (lambda)

	f = .bv (f)
    EXTEND (f, "pbvpmf", lambda)
}

pbvpmf.2 = function (mean.X, mean.Y, cov)
	pbvpmf (mean.X - cov, mean.Y - cov, cov)

.pbvpmf.eval = function (., x, y)
{   lambda = .$lambda
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

pbvcdf = function (lambda.1, lambda.2, lambda.3)
{   f = function (x, y)
	{	. = THAT ()
		v = .val.integer.args (x, y)
		.pbvcdf.eval (., v$x, v$y)
	}
	lambda = c (lambda.1, lambda.2, lambda.3)
	.check.lambda.ok (lambda)

	f = .bv (f)
    EXTEND (f, "pbvcdf", lambda)
}

pbvcdf.2 = function (mean.X, mean.Y, cov)
	pbvcdf (mean.X - cov, mean.Y - cov, cov)

.pbvcdf.eval = function (., x, y)
{   n = length (x)
    z = numeric (n)
    for (i in 1:n)
	{	if (x [i] < 0 || y [i] < 0)
			z [i] = 0
		else
			z [i] = .pbvcdf.eval.ext (.$lambda, x [i], y [i])
	}
    z
}

.pbvcdf.eval.ext = function (lambda, x, y)
{	z = 0
	for (i in 0:x)
		for (j in 0:y)
			z = z + .pbvpmf.eval (lambda, i, j)
	z
}

.pbvcdf.outer = function (pbvcdf.f, xmax, ymax)
{	pbvpmf.f = pbvpmf (1, 1, 0)
	attributes (pbvpmf.f)$lambda = attributes (pbvcdf.f)$lambda
	zf = outer (0:xmax, 0:ymax, pbvpmf.f)
	zF = matrix (0, xmax + 1, ymax + 1)
	for (i in 1:(xmax + 1) )
		for (j in 1:(ymax + 1) )
			{	I = 1:i
				J = 1:j
				zF [i, j] = sum (zf [I, J])
			}
	zF
}

.plot.pbv = function (f, use.plot3d, xmax, ymax, ..., is.cdf=FALSE)
{   . = attributes (f)
    if (missing (xmax) )
	{	mean.x = .$lambda [1] + .$lambda [3]
		xmax = as.integer (2.5 * mean.x)
	}
	if (missing (ymax) )
	{	mean.y = .$lambda [2] + .$lambda [3]
		ymax = as.integer (2.5 * mean.y)
	}
	x = 0:xmax
	y = 0:ymax
	if (is.cdf)
		z = .pbvcdf.outer (f, xmax, ymax)
	else
		z = outer (x, y, f)
	.plot.bv.2 (FALSE, use.plot3d, is.cdf, x, y, z, ...)
}

plot.pbvpmf = function (x, use.plot3d=FALSE, xmax, ymax, ..., all=FALSE)
{	if (all)
	{	F = pbvcdf (1, 1, 0)
		.plot.bv.all (x, F, xmax, ymax, ...)
	}
	else
		.plot.pbv (x, use.plot3d, xmax, ymax, ...)
}

plot.pbvcdf = function (x, use.plot3d=FALSE, xmax, ymax, ...)
	.plot.pbv (x, use.plot3d, xmax, ymax, ..., is.cdf=TRUE)
