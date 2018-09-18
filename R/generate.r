generate.bivariate.sample = function (n)
{	vector.means = c (4, 4)
	matrix.variances = matrix (c (1, 0.25, 0.25, 1), nrow=2)
	x = rmvnorm (n, mean=vector.means, sigma=matrix.variances)
	x = x ^ 2
	x
}
