library(smoof)

# fuctions takes in a function from smoof package and number of iterations
# and performs the pure random search iterating 'iters' number of times
# it returns the minimal value of function found
calculate_prs <- function(fitness, iters) {
    # getting constraints and number of dimensions of fitness function
    lower <- getLowerBoxConstraints(fitness)
    upper <- getUpperBoxConstraints(fitness)
    dimensions <- length(lower)

    single_prs <- function() {
        # preparing the vector used in pure random search

        # pre-setting the min value
        min_value <- Inf
        random_vector <- numeric(dimensions)
        for (i in 1:1000) {
            for (j in 1:dimensions) {
                # generating a new vector with values unifomly distrubuted
                # in range <lower[i], upper[i]>
                random_vector[j] <- runif(1, min = lower[j], max = upper[j])
            }
            # updating the min value
            min_value <- min(min_value, fitness(random_vector))
        }

        return(min_value)
    }

    return(replicate(iters, single_prs()))
}

print(calculate_prs(makeAckleyFunction(2), 50))