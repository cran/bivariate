.plotbpdf.plane.xy = function ()
{	.plotbpdf.poly (c (0, 0, 1, 1), c (0, 1, 1, 0), 0)
	p1 = .project (0, 0, 0)
	p2 = .project (0, 1, 0)
	p3 = .project (1, 0, 0)
	z = 0.03
	arrows (p1 [1] - z, p1 [2] - z, p2 [1] - z, p2 [2] - z, length=0.12)
	arrows (p1 [1] + z, p1 [2] - z, p3 [1] + z, p3 [2] - z, length=0.12)
}

.plotbpdf.labs = function (labs)
{	x = c (0.525, -0.525)
	y = 0.16
	text (x, y, labs)
}

.plotbpdf.poly = function (u, v, w, border="black", col="white")
{	m = .project (u, v, w)
	polygon (m [,1], m [,2], border=border, col=col)
}

.plotbpdf.lines = function (u, v, w, col="black")
{	m = .project (u, v, w)
	lines (m [,1], m [,2], col=col)
}

.project = function (u, v, w)
{	x = u * cos (pi / 4) + v * cos (pi * 3 / 4)
	y = u * sin (pi / 4) + v * sin (pi * 3 / 4)
	y = 0.71 * y + 0.5 * w
	cbind (x, y)
}

.colinterp = function (col1, col2, x)
{	col1 = col2rgb (col1) / 255
    col2 = col2rgb (col2) / 255
    col = col1 + x * (col2 - col1)
    col [col < 0] = 0
    col [col > 1] = 1
	rgb (col [1], col [2], col [3])
}
