library(smoof)
library(ecr)

# TODO: correct GA parameters and/or set popSize & maxiter as arguments
calculate_ga <- function(fitness, iters) {
	lower = getLowerBoxConstraints(fitness)
	upper = getUpperBoxConstraints(fitness)

	single_ga <- function() {
		res <- ecr(
			fitness.fun = fitness, representation = "float",
			n.dim = getNumberOfParameters(fitness),
			mu = 10L, lambda = 10L,
			mutator = setup(mutGauss, lower = lower, upper = upper),
			# terminators = list(stopOnIters(1000L))
		)

		return(res$best.y)
	}

	return(replicate(iters, single_ga()))
}

print(calculate_ga(makeAckleyFunction(2), 50))