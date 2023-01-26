# Porównanie algorytmów PRS i GA

Autorzy: Krzysztof Dziechciarz, Dominik Pilipczuk

### Potrzebne biblioteki:

    library(ecr)
    library(smoof)
    library(mcreplicate)

### Definicje funkcji:

`calculate_prs` - zwraca liste najmniejszych wartości znalezionych przez
algorytm PRS (dla `iters` wywołań)

    calculate_prs <- function(fitness, iters) {
        lower <- getLowerBoxConstraints(fitness)
        upper <- getUpperBoxConstraints(fitness)
        dimensions <- length(lower)

        single_prs <- function() {
            min_value <- Inf
            random_vector <- numeric(dimensions)
            for (i in 1:1000) {
                for (j in 1:dimensions) {
                    random_vector[j] <- runif(1, min = lower[j], max = upper[j])
                }
                min_value <- min(min_value, fitness(random_vector))
            }

            return(min_value)
        }

        return(replicate(iters, single_prs()))
    }

`calculate_prs` - zwraca liste najmniejszych wartości znalezionych przez
algorytm GA (dla `iters` wywołań)

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

------------------------------------------------------------------------

# Porównanie

## Dla dwóch wymiarów - Funkcja Ackley’a

    ackley_2 <- makeAckleyFunction(2)
    ackley_2_ga <- calculate_ga(ackley_2, 50)
    ackley_2_prs <- calculate_prs(ackley_2, 50)

### GA

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-5-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-6-1.png)

Podsumowanie:

    summary(ackley_2_ga)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.01351 0.20091 0.52122 0.71032 0.90795 6.10501

Średnia wartość:

    ## [1] 0.7103152

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-9-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-10-1.png)

Podsumowanie:

    summary(ackley_2_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.043   3.272   4.283   4.175   4.747   6.557

Średnia wartość:

    ## [1] 4.175376

## Dla dwóch wymiarów - Funkcja Rastrigina

    rastrigin_2 <- makeRastriginFunction(2)
    rastrigin_2_ga <- calculate_ga(rastrigin_2, 50)
    rastrigin_2_prs <- calculate_prs(rastrigin_2, 50)

### GA

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-14-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-15-1.png)

Podsumowanie:

    summary(rastrigin_2_ga)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.04836 0.71105 1.31889 1.52012 2.05221 5.28956

Średnia wartość:

    ## [1] 1.520118

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-18-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-19-1.png)

Podsumowanie:

    summary(rastrigin_2_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.1523  1.0818  1.7330  1.9465  2.5536  5.1506

Średnia wartość:

    ## [1] 1.946462

## Dla dziesięciu wymiarów - Funkcja Ackley’a

    ackley_10 <- makeAckleyFunction(10)
    ackley_10_ga <- calculate_ga(ackley_10, 50)
    ackley_10_prs <- calculate_prs(ackley_10, 50)

### GA

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-23-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-24-1.png)

Podsumowanie:

    summary(ackley_10_ga)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   3.248   4.592   5.295   5.700   5.740  19.868

Średnia wartość:

    ## [1] 5.70045

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-27-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-28-1.png)

Podsumowanie:

    summary(ackley_10_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   16.35   17.82   18.37   18.18   18.74   19.10

Średnia wartość:

    ## [1] 18.18314

## Dla dziesięciu wymiarów - Funkcja Rastringa

    rastrigin_10 <- makeRastriginFunction(10)
    rastrigin_10_ga <- calculate_ga(rastrigin_10, 50)
    rastrigin_10_prs <- calculate_prs(rastrigin_10, 50)

### GA

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-32-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-33-1.png)

Podsumowanie:

    summary(rastrigin_10_ga)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   48.11   64.28   73.30   74.57   83.76  113.35

Średnia wartość:

    ## [1] 74.57171

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-36-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-37-1.png)

Podsumowanie:

    summary(rastrigin_10_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   61.67   78.60   86.10   85.63   92.34  102.65

Średnia wartość:

    ## [1] 85.62977

## Dla dwudziestu wymiarów - Funkcja Ackley’a

    ackley_20 <- makeAckleyFunction(20)
    ackley_20_ga <- calculate_ga(ackley_20, 50)
    ackley_20_prs <- calculate_prs(ackley_20, 50)

### GA

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-41-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-42-1.png)

Podsumowanie:

    summary(ackley_20_ga)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   6.083   7.825   8.592   9.256   9.577  19.517

Średnia wartość:

    ## [1] 9.255787

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-45-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-46-1.png)

Podsumowanie:

    summary(ackley_20_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   19.19   19.76   19.97   19.91   20.10   20.26

Średnia wartość:

    ## [1] 19.90994

## Dla dwudziestu wymiarów - Funkcja Rastringa

    rastrigin_20 <- makeRastriginFunction(20)
    rastrigin_20_ga <- calculate_ga(rastrigin_20, 50)
    rastrigin_20_prs <- calculate_prs(rastrigin_20, 50)

### GA

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-50-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-51-1.png)

Podsumowanie:

    summary(rastrigin_20_ga)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   152.5   183.9   197.5   198.1   210.8   231.9

Średnia wartość:

    ## [1] 198.0576

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-54-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-55-1.png)

Podsumowanie:

    summary(rastrigin_20_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   160.9   216.8   232.8   227.6   240.5   261.4

Średnia wartość:

    ## [1] 227.6346
