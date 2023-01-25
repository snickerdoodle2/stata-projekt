library(smoof)
library(ecr)
library(mcreplicate)


# TODO: correct GA parameters and/or set popSize & maxiter as arguments
calculate_ga <- function(fitness, iters) {
    lower <- getLowerBoxConstraints(fitness)
    upper <- getUpperBoxConstraints(fitness)
    dims <- getNumberOfParameters(fitness)

    single_ga <- function() {
        res <- ecr(
            fitness.fun = fitness, representation = "float",
            n.dim = dims,
            mu = 10L, lambda = 1L,
            mutator = setup(mutGauss, sdev = 2, lower = lower, upper = upper),
            terminators = list(stopOnIters(1000))
        )

        return(res$best.y)
    }

    return(replicate(iters, single_ga()))
}

print(calculate_ga(makeAckleyFunction(2), 5))
