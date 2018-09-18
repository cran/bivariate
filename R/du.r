dubvpmf = function (a.X, b.X, a.Y, b.Y)
{   dubvpmf.f = function (x, y) {.dubvpmf.eval (x, y)}
	a = c (a.X, a.Y)
	b = c (b.X, b.Y)
	n = 1 + b - a
    attributes (dubvpmf.f) = list (class="dubvpmf", n=n, a=a, b=b)
    dubvpmf.f
}

.dubvpmf.eval = function (x, y)
{   this = attributes (sys.function (-1) )
    n = length (x)
	if (n != length (y) )
		stop ("length (x) must equal length (y)")
    z = rep (1 / this$n [1] / this$n [2], n)
	z [x < this$a [1] | x > this$b [1] | y < this$a [2] | y > this$b [2]] = 0
    z
}

dubvcdf = function (a.X, b.X, a.Y, b.Y)
{   dubvcdf.f = function (x, y) {.dubvcdf.eval (x, y)}
	a = c (a.X, a.Y)
	b = c (b.X, b.Y)
	n = 1 + b - a
    attributes (dubvcdf.f) = list (class="dubvcdf", n=n, a=a, b=b)
    dubvcdf.f
}

.dubvcdf.eval = function (x, y)
{   this = attributes (sys.function (-1) )
    n = length (x)
	if (n != length (y) )
		stop ("length (x) must equal length (y)")
	x [x > this$b [1] ] = this$b [1]
	y [y > this$b [2] ] = this$b [2]
    z1 = (1 + x - this$a [1]) / this$n [1]
	z2 = (1 + y - this$a [2]) / this$n [2]
	z = z1 * z2
	z [x < this$a [1] | y < this$a [2] ] = 0
	z
}

print.dubvpmf = function (x, ...)
{   environment (x) = .GlobalEnv
    print.function (x, ...)
}

print.dubvcdf = function (x, ...)
    print.dubvpmf (x, ...)
	
plot.dubvpmf = function (x, xlab="x", ylab="y", xlim, ylim, zlim, ...)
{   dubvpmf.f = x

    this = attributes (dubvpmf.f)
    if (missing (xlim) )
		xlim = c (this$a [1], this$b [1])
	if (missing (ylim) )
		ylim = c (this$a [2], this$b [2])
	if (missing (zlim) )
		zlim = c (0, 1)
	x = seq (xlim [1], xlim [2], length.out=20)[-20]
	y = seq (ylim [1], ylim [2], length.out=20)[-20]
	z = outer (x, y, dubvpmf.f)
	plot3d.discrete (,, z, xlab=xlab, ylab=ylab, zlim=zlim, ...)
}

plot.dubvcdf = function (x, xlab="x", ylab="y", xlim, ylim, zlim, ...)
      plot.dubvpmf (x, xlab, ylab, xlim, ylim, zlim, ...)
