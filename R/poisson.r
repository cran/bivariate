.is.lambda.ok = function (lambda)
{	if (lambda [1] <= 0 || lambda [2] <= 0)
		stop ("\nlambda[1] and lambda[2] need to be > 0\nso, mean.X and mean.Y need to be > covariance")
}

pbvpmf = function (mean.X, mean.Y, covariance)
{   pbvpmf.f = function (x, y)
	{	x = as.integer (x)
		y = as.integer (y)
		stopifnot (length (x) == length (y) )
		.pbvpmf.eval (x, y)
	}
	lambda = c (mean.X - covariance, mean.Y - covariance, covariance)
	.is.lambda.ok (lambda)
    attributes (pbvpmf.f) = list (class="pbvpmf", lambda=lambda)
    pbvpmf.f
}

.pbvpmf.eval = function (x, y)
{   . = attributes (sys.function (-1) )
	.pbvpmf.eval.2 (.$lambda, x, y)
}

.pbvpmf.eval.2 = function (lambda, x, y)
{   t1 = exp (-sum (lambda) )
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

pbvcdf = function (mean.X, mean.Y, covariance)
{   pbvcdf.f = function (x, y)
	{	x = as.integer (x)
		y = as.integer (y)
		stopifnot (length (x) == length (y) )
		.pbvcdf.eval (x, y)
	}
	lambda = c (mean.X - covariance, mean.Y - covariance, covariance)
	.is.lambda.ok (lambda)
    attributes (pbvcdf.f) = list (class="pbvcdf", lambda=lambda)
    pbvcdf.f
}

.pbvcdf.eval = function (x, y)
{   . = attributes (sys.function (-1) )
    n = length (x)
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
			z = z + .pbvpmf.eval.2 (lambda, i, j)
	z
}

.outer.pbvcdf = function (pbvcdf.f, xmax, ymax)
{	pbvpmf.f = pbvpmf (1, 1, 0)
	attributes (pbvpmf.f)$lambda = attributes (pbvcdf.f)$lambda
	zf = outer (0:xmax, 0:ymax, pbvpmf.f)
	zF = matrix (0, nrow=xmax + 1, ncol=ymax + 1)
	for (i in 1:(xmax + 1) )
		for (j in 1:(ymax + 1) )
			{	I = 1:i
				J = 1:j
				zF [i, j] = sum (zf [I, J])
			}
	zF
}

plot.pbvpmf = function (x, xmax, ymax, ...)
{   is.pbvpmf = (any (class (x) == "pbvpmf") )
	f = x

    . = attributes (f)
    if (missing (xmax) )
	{	mean.x = .$lambda [1] + .$lambda [3]
		xmax = as.integer (2.5 * mean.x)
	}
	if (missing (ymax) )
	{	mean.y = .$lambda [2] + .$lambda [3]
		ymax = as.integer (2.5 * mean.y)
	}
	if (is.pbvpmf)
	{	x = 0:xmax
		y = 0:ymax
		z = outer (x, y, f)
	}
	else
		z = .outer.pbvcdf (f, xmax, ymax)
	plot3d.bar (,,z, ...)
}

plot.pbvcdf = function (x, xmax, ymax, ...)
      plot.pbvpmf (x, xmax, ymax, ...)
