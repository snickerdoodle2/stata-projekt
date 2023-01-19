library(GA)

# TODO: Write fitness function.
fitness <- function(vec) {
    sum <- 0
    for (value in vec){
        sum <- sum + value
    }
    return((-1) * sum)
}

my_ga <- function(dimension) {
# TODO: correct GA parameters
    GA <- ga(
        type = "real-valued",
        fitness = fitness,
        lower = rep(-10, dimension),
        upper = rep(10, dimension),
        popSize = 50,
        maxiter = 100
    )

    print(GA@solution)
}

my_ga(2)