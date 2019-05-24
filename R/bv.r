.onLoad = function (...)
	use.bv.theme ()

.bv = function (f)
	EXTEND (f, "bv")

print.bv = function (x, ...)
	object.summary (x, ...)

use.bv.theme = function ()
{	use.theme ("blue")

	bso = getOption ("barsurf")
	bso$plot2d.contour.colv.1 = c (157.5, 30, 80)
	bso$plot2d.contour.colv.2 = c (282.5, 30, 80)
	options (barsurf=bso)
}
