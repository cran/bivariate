bnbvpmf = function (p.X, p.Y, n=1)
{   f = function (x, y)
	{	. = THAT ()
		v = .val.integer.args (x, y)
		.bnbvpmf.eval (., v$x, v$y)
	}
	f = .bv (f)
    EXTEND (f, "bnbvpmf",
		p = c (p.X, p.Y),
		n
	)
}

.bnbvpmf.eval = function (., x, y)
{   z1 = choose (.$n, x) * .$p [1] ^ x * (1 - .$p [1]) ^ (.$n - x)
	z2 = choose (.$n, y) * .$p [2] ^ y * (1 - .$p [2]) ^ (.$n - y)
	z = z1 * z2
	z [x < 0 | x > .$n | y < 0 | y > .$n] = 0
	z
}

bnbvcdf = function (p.X, p.Y, n=1)
{   f = function (x, y)
	{	. = THAT ()
		v = .val.integer.args (x, y)
		.bnbvcdf.eval (., v$x, v$y)
	}
	f = .bv (f)
    EXTEND (f, "bnbvcdf",
		p = c (p.X, p.Y),
		n
	)
}

.bnbvcdf.eval = function (., x, y)
{   n = length (x)
	z = numeric (n)
	for (i in 1:n)
		z [i] = .bnbvcdf.eval.2 (.$n, .$p, x [i], y [i])
	z
}

.bnbvcdf.eval.2 = function (n, p, x, y)
{	z1 = z2 = 0
	for (i in 0:floor (x) )
		z1 = z1 + choose (n, i) * p [1] ^ i * (1 - p [1]) ^ (n - i)
	for (i in 0:floor (y) )
		z2 = z2 + choose (n, i) * p [2] ^ i * (1 - p [2]) ^ (n - i)
	z1 * z2
}

.plot.bnbv = function (f, use.plot3d, xlim, ylim, ..., is.cdf=FALSE)
{   . = attributes (f)
    if (missing (xlim) )
		xlim = as.integer (c (0, .$n) )
	if (missing (ylim) )
		ylim = as.integer (c (0, .$n) )
	v = .discrete.outer (f, xlim, ylim)
	.plot.bv.2 (FALSE, use.plot3d, is.cdf, v$x, v$y, v$z, ...)
}

plot.bnbvpmf = function (x, use.plot3d=FALSE, xlim, ylim, ..., all=FALSE)
{	if (all)
	{	F = bnbvcdf (0, 0, 0)
		.plot.bv.all (x, F, xlim, ylim, ...)
	}
	else
		.plot.bnbv (x, use.plot3d, xlim, ylim, ...)
}

plot.bnbvcdf = function (x, use.plot3d=FALSE, xlim, ylim, ...)
	.plot.bnbv (x, use.plot3d, xlim, ylim, ..., is.cdf=TRUE)
