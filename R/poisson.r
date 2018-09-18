pbvpmf = function (mean.X, mean.Y, covariance)
{   pbvpmf.f = function (x, y) {.pbvpmf.eval (x, y)}
	lambda = c (mean.X - covariance, mean.Y - covariance, covariance)
    attributes (pbvpmf.f) = list (class="pbvpmf", lambda=lambda)
    pbvpmf.f
}

.pbvpmf.eval = function (x, y)
{   this = attributes (sys.function (-1) )
	.pbvpmf.eval.2 (this$lambda, x, y)
}

.pbvpmf.eval.2 = function (lambda, x, y)
{   t1 = exp (-sum (lambda) )
	t4b = lambda [3] / lambda [1] / lambda [2]
    n = length (x)
	if (n != length (y) )
		stop ("length (x) must equal length (y)")
    z = numeric (n)
    for (i in 1:n)
	{	t2 = lambda [1] ^ x [i] / factorial (x [i])
		t3 = lambda [2] ^ y [i] / factorial (y [i])
		t4 = 0
		for (k in 0:min (c (x [i], y [i]) ) )
			t4 = t4 + choose (x [i], k) * choose (y [i], k) * factorial (k) * t4b ^ k
		z [i] = t1 * t2 * t3 * t4 
	}
    z
}

pbvcdf = function (mean.X, mean.Y, covariance)
{   pbvcdf.f = function (x, y) {.pbvcdf.eval (x, y)}
	lambda = c (mean.X - covariance, mean.Y - covariance, covariance)
    attributes (pbvcdf.f) = list (class="pbvcdf", lambda=lambda)
    pbvcdf.f
}

.pbvcdf.eval = function (x, y)
{   this = attributes (sys.function (-1) )
    n = length (x)
	if (n != length (y) )
		stop ("length (x) must equal length (y)")
    z = numeric (n)
    for (i in 1:n)
		z [i] = .pbvcdf.eval.2 (this$lambda, x [i], y [i])
    z
}

.pbvcdf.eval.2 = function (lambda, x, y)
{	z = 0
	for (i in 0:x)
		for (j in 0:y)
			z = z + .pbvpmf.eval.2 (lambda, i, j)
	z
}

.outer.pbvcdf = function (pbvcdf.f, xmax, ymax)
{	pbvpmf.f = pbvpmf (0, 0, 0)
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

print.pbvpmf = function (x, ...)
	print.nbvpdf (x, ...)

print.pbvcdf = function (x, ...)
    print.nbvpdf (x, ...)

plot.pbvpmf = function (x, xlab="x", ylab="y", xmax, ymax, zlim, ...)
{   is.pbvpmf = (any (class (x) == "pbvpmf") )
	f = x

    this = attributes (f)
    if (missing (xmax) )
	{	mean.x = this$lambda [1] + this$lambda [3]
		xmax = as.integer (2.5 * mean.x)
	}
	if (missing (ymax) )
	{	mean.y = this$lambda [2] + this$lambda [3]
		ymax = as.integer (2.5 * mean.y)
	}
	if (is.pbvpmf)
	{	x = 0:xmax
		y = 0:ymax
		z = outer (x, y, f)
	}
	else
		z = .outer.pbvcdf (f, xmax, ymax)
	plot3d.discrete (,, z, xlab=xlab, ylab=ylab, zlim=zlim, ...)
}

plot.pbvcdf = function (x, xlab="x", ylab="y", xmax, ymax, zlim, ...)
      plot.pbvpmf (x, xlab, ylab, xmax, ymax, zlim, ...)
