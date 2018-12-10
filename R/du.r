dubvpmf = function (a.X, b.X, a.Y, b.Y)
{   dubvpmf.f = function (x, y)
	{	x = as.integer (x)
		y = as.integer (y)
		stopifnot (length (x) == length (y) )
		.dubvpmf.eval (x, y)
	}
	a = c (a.X, a.Y)
	b = c (b.X, b.Y)
	n = 1 + b - a
    attributes (dubvpmf.f) = list (class="dubvpmf", n=n, a=a, b=b)
    dubvpmf.f
}

.dubvpmf.eval = function (x, y)
{   . = attributes (sys.function (-1) )
    z = rep (1 / .$n [1] / .$n [2], length (x) )
	z [x < .$a [1] | x > .$b [1] | y < .$a [2] | y > .$b [2]] = 0
    z
}

dubvcdf = function (a.X, b.X, a.Y, b.Y)
{   dubvcdf.f = function (x, y)
	{	x = as.integer (x)
		y = as.integer (y)
		stopifnot (length (x) == length (y) )
		.dubvcdf.eval (x, y)
	}
	a = c (a.X, a.Y)
	b = c (b.X, b.Y)
	n = 1 + b - a
    attributes (dubvcdf.f) = list (class="dubvcdf", n=n, a=a, b=b)
    dubvcdf.f
}

.dubvcdf.eval = function (x, y)
{   . = attributes (sys.function (-1) )
    x [x > .$b [1] ] = .$b [1]
	y [y > .$b [2] ] = .$b [2]
    z1 = (1 + x - .$a [1]) / .$n [1]
	z2 = (1 + y - .$a [2]) / .$n [2]
	z = z1 * z2
	z [x < .$a [1] | y < .$a [2] ] = 0
	z
}

plot.dubvpmf = function (x, xlim, ylim, ...)
{   is.dubvcdf = (any (class (x) == "dubvcdf") )
	dubvpmf.f = x

    . = attributes (dubvpmf.f)
    if (missing (xlim) )
		xlim = c (.$a [1], .$b [1])
	if (missing (ylim) )
		ylim = c (.$a [2], .$b [2])
	zlim = c (0, 1 / .$n [1] / .$n [2])

	x = xlim [1]:xlim [2]
	y = ylim [1]:ylim [2]
	z = outer (x, y, dubvpmf.f)
	if (is.dubvcdf)
		plot3d.bar (,,z, ...)
	else
		plot3d.bar (,,z, zlim=zlim, ...)
}

plot.dubvcdf = function (x, xlim, ylim, ...)
      plot.dubvpmf (x, xlim, ylim, ...)
