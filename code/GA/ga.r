library(GA)

# TODO: remove import
library(smoof)

# TODO: correct GA parameters and/or set popSize & maxiter as arguments
calculate_ga <- function(fitness, iters) {
	lower <- getLowerBoxConstraints(fitness)
	upper <- getUpperBoxConstraints(fitness)

	out = c();

	for (i in 1:iters){
        single_GA <- ga(
            type = "real-valued",
            fitness = function(x) (-1) * fitness(x),
            lower = lower,
            upper = upper,
            popSize = 50,
            maxiter = 200
        )

        out = c(out, fitness(single_GA@solution))
	}

    return(mean(out))
}

# TODO: remove test
print(calculate_ga(makeAckleyFunction(1), 20))