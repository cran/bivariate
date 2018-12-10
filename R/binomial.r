bnbvpmf = function (n, p.X, p.Y)
{   bnbvpmf.f = function (x, y)
	{	x = as.integer (x)
		y = as.integer (y)
		stopifnot (length (x) == length (y) )
		.bnbvpmf.eval (x, y)
	}
	p = c (p.X, p.Y)
    attributes (bnbvpmf.f) = list (class="bnbvpmf", n=n, p=p)
    bnbvpmf.f
}

.bnbvpmf.eval = function (x, y)
{   . = attributes (sys.function (-1) )
    z1 = choose (.$n, x) * .$p [1] ^ x * (1 - .$p [1]) ^ (.$n - x)
	z2 = choose (.$n, y) * .$p [2] ^ y * (1 - .$p [2]) ^ (.$n - y)
	z = z1 * z2
	z [x < 0 | x > .$n | y < 0 | y > .$n] = 0
	z
}

bnbvcdf = function (n, p.X, p.Y)
{   bnbvcdf.f = function (x, y)
	{	x = as.integer (x)
		y = as.integer (y)
		stopifnot (length (x) == length (y) )
		.bnbvcdf.eval (x, y)
	}
	p = c (p.X, p.Y)
    attributes (bnbvcdf.f) = list (class="bnbvcdf", n=n, p=p)
    bnbvcdf.f
}

.bnbvcdf.eval = function (x, y)
{   . = attributes (sys.function (-1) )
    n = length (x)
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

plot.bnbvpmf = function (x, xlim, ylim, ...)
{   bnbvpmf.f = x

    . = attributes (bnbvpmf.f)
    if (missing (xlim) )
		xlim = as.integer (c (0, .$n) )
	if (missing (ylim) )
		ylim = as.integer (c (0, .$n) )
	x = xlim [1]:xlim [2]
	y = ylim [1]:ylim [2]
	z = outer (x, y, bnbvpmf.f)
	plot3d.bar (,,z, ...)
}

plot.bnbvcdf = function (x, xlim, ylim, ...)
      plot.bnbvpmf (x, xlim, ylim, ...)
