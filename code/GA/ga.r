library(GA)

fitness <- function(vec) {
    sum <- 0
    for (value in vec){
        sum <- sum + value
    }
    return((-1) * sum)
}

my_ga <- function() {

    GA <- ga(
        type = "real-valued",
        fitness = fitness,
        lower = c(-10, -10),
        upper = c(10, 10),
        popSize = 50,
        maxiter = 1000
    )

    print(GA@solution)
}

my_ga()