bnbvpmf = function (n, p.X, p.Y)
{   bnbvpmf.f = function (x, y) {.bnbvpmf.eval (x, y)}
	p = c (p.X, p.Y)
    attributes (bnbvpmf.f) = list (class="bnbvpmf", n=n, p=p)
    bnbvpmf.f
}

.bnbvpmf.eval = function (x, y)
{   this = attributes (sys.function (-1) )
    n = length (x)
	if (n != length (y) )
		stop ("length (x) must equal length (y)")
    z1 = choose (this$n, x) * this$p [1] ^ x * (1 - this$p [1]) ^ (this$n - x)
	z2 = choose (this$n, y) * this$p [2] ^ y * (1 - this$p [2]) ^ (this$n - y)
	z = z1 * z2
	z [x < 0 | x > this$n | y < 0 | y > this$n] = 0
	z
}

bnbvcdf = function (n, p.X, p.Y)
{   bnbvcdf.f = function (x, y) {.bnbvcdf.eval (x, y)}
	p = c (p.X, p.Y)
    attributes (bnbvcdf.f) = list (class="bnbvcdf", n=n, p=p)
    bnbvcdf.f
}

.bnbvcdf.eval = function (x, y)
{   this = attributes (sys.function (-1) )
    n = length (x)
	if (n != length (y) )
		stop ("length (x) must equal length (y)")
	z = numeric (n)
	for (i in 1:n)
		z [i] = .bnbvcdf.eval.2 (this$n, this$p, x [i], y [i])
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

print.bnbvpmf = function (x, ...)
	print.dubvpmf (x, ...)

print.bnbvcdf = function (x, ...)
    print.dubvpmf (x, ...)

plot.bnbvpmf = function (x, xlab="x", ylab="y", xlim, ylim, zlim, ...)
{   bnbvpmf.f = x

    this = attributes (bnbvpmf.f)
    if (missing (xlim) )
		xlim = as.integer (c (0, this$n) )
	if (missing (ylim) )
		ylim = as.integer (c (0, this$n) )
	x = xlim [1]:xlim [2]
	y = ylim [1]:ylim [2]
	z = outer (x, y, bnbvpmf.f)
	plot3d.discrete (,, z, xlab=xlab, ylab=ylab, zlim=zlim, ...)
}

plot.bnbvcdf = function (x, xlab="x", ylab="y", xlim, ylim, zlim, ...)
      plot.bnbvpmf (x, xlab, ylab, xlim, ylim, zlim, ...)
