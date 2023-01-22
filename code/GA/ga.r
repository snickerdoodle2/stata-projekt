library(GA)
library(smoof)

# TODO: correct GA parameters and/or set popSize & maxiter as arguments
calculate_ga <- function(fitness, iters) {
    lower <- getLowerBoxConstraints(fitness)
    upper <- getUpperBoxConstraints(fitness)


    single <- function() {
        current_ga <- ga(
            type = "real-valued",
            fitness = function(x) (-1) * fitness(x),
            lower = lower,
            upper = upper,
            popSize = 50,
            maxiter = 20
        )

        return(fitness(current_ga@solution))
    }

    return(replicate(iters, single()))
}