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

        return(mc_replicate(iters, single_prs()))
    }

`calculate_ga` - zwraca liste najmniejszych wartości znalezionych przez
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

        return(mc_replicate(iters, single_ga()))
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
    ## 0.03216 0.21794 0.42402 0.59028 0.66128 2.46359

Średnia wartość:

    ## [1] 0.5902753

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-9-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-10-1.png)

Podsumowanie:

    summary(ackley_2_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.906   3.222   3.832   3.911   4.425   7.026

Średnia wartość:

    ## [1] 3.911098

### Porównanie

    t.test(ackley_2_ga, ackley_2_prs)

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  ackley_2_ga and ackley_2_prs
    ## t = -18.575, df = 73.332, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -3.677103 -2.964542
    ## sample estimates:
    ## mean of x mean of y 
    ## 0.5902753 3.9110977

## Dla dwóch wymiarów - Funkcja Rastrigina

    rastrigin_2 <- makeRastriginFunction(2)
    rastrigin_2_ga <- calculate_ga(rastrigin_2, 50)

    rastrigin_2_prs <- calculate_prs(rastrigin_2, 50)

### GA

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-15-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-16-1.png)

Podsumowanie:

    summary(rastrigin_2_ga)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.00366 1.06867 1.50479 1.57228 2.08760 3.84409

Średnia wartość:

    ## [1] 1.572275

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-19-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-20-1.png)

Podsumowanie:

    summary(rastrigin_2_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 0.07412 1.03024 1.74475 1.78662 2.54177 3.97188

Średnia wartość:

    ## [1] 1.786621

### Porównanie

    t.test(rastrigin_2_ga, rastrigin_2_prs)

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  rastrigin_2_ga and rastrigin_2_prs
    ## t = -1.1403, df = 97.014, p-value = 0.257
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.5874107  0.1587189
    ## sample estimates:
    ## mean of x mean of y 
    ##  1.572275  1.786621

## Dla dziesięciu wymiarów - Funkcja Ackley’a

    ackley_10 <- makeAckleyFunction(10)
    ackley_10_ga <- calculate_ga(ackley_10, 50)

    ackley_10_prs <- calculate_prs(ackley_10, 50)

### GA

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-25-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-26-1.png)

Podsumowanie:

    summary(ackley_10_ga)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   3.774   4.681   5.556   5.611   6.035  10.018

Średnia wartość:

    ## [1] 5.61068

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-29-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-30-1.png)

Podsumowanie:

    summary(ackley_10_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   15.74   18.06   18.49   18.30   18.74   19.36

Średnia wartość:

    ## [1] 18.29893

### Porównanie

    t.test(ackley_10_ga, ackley_10_prs)

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  ackley_10_ga and ackley_10_prs
    ## t = -58.843, df = 74.686, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -13.11783 -12.25867
    ## sample estimates:
    ## mean of x mean of y 
    ##   5.61068  18.29893

## Dla dziesięciu wymiarów - Funkcja Rastringa

    rastrigin_10 <- makeRastriginFunction(10)
    rastrigin_10_ga <- calculate_ga(rastrigin_10, 50)

    rastrigin_10_prs <- calculate_prs(rastrigin_10, 50)

### GA

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-35-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-36-1.png)

Podsumowanie:

    summary(rastrigin_10_ga)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   47.39   68.34   77.16   75.55   83.45   96.84

Średnia wartość:

    ## [1] 75.54835

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-39-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-40-1.png)

Podsumowanie:

    summary(rastrigin_10_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   66.18   83.10   86.39   87.10   93.85  102.41

Średnia wartość:

    ## [1] 87.10429

### Porównanie

    t.test(rastrigin_10_ga, rastrigin_10_prs)

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  rastrigin_10_ga and rastrigin_10_prs
    ## t = -5.6474, df = 86.671, p-value = 2.029e-07
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -15.623257  -7.488621
    ## sample estimates:
    ## mean of x mean of y 
    ##  75.54835  87.10429

## Dla dwudziestu wymiarów - Funkcja Ackley’a

    ackley_20 <- makeAckleyFunction(20)
    ackley_20_ga <- calculate_ga(ackley_20, 50)

    ackley_20_prs <- calculate_prs(ackley_20, 50)

### GA

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-45-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-46-1.png)

Podsumowanie:

    summary(ackley_20_ga)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   6.031   7.177   8.095   8.971   9.283  19.268

Średnia wartość:

    ## [1] 8.970637

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-49-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-50-1.png)

Podsumowanie:

    summary(ackley_20_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   19.39   19.70   19.83   19.85   20.01   20.40

Średnia wartość:

    ## [1] 19.84623

### Porównanie

    t.test(ackley_20_ga, ackley_20_prs)

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  ackley_20_ga and ackley_20_prs
    ## t = -25.533, df = 49.589, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -11.73129 -10.01989
    ## sample estimates:
    ## mean of x mean of y 
    ##  8.970637 19.846227

## Dla dwudziestu wymiarów - Funkcja Rastringa

    rastrigin_20 <- makeRastriginFunction(20)
    rastrigin_20_ga <- calculate_ga(rastrigin_20, 50)

    rastrigin_20_prs <- calculate_prs(rastrigin_20, 50)

### GA

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-55-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-56-1.png)

Podsumowanie:

    summary(rastrigin_20_ga)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   146.0   183.4   195.8   196.1   207.2   253.8

Średnia wartość:

    ## [1] 196.0614

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-59-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-60-1.png)

Podsumowanie:

    summary(rastrigin_20_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   187.5   219.1   227.7   226.1   234.8   252.4

Średnia wartość:

    ## [1] 226.1335

### Porównanie

    t.test(rastrigin_20_ga, rastrigin_20_prs)

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  rastrigin_20_ga and rastrigin_20_prs
    ## t = -8.2922, df = 77.834, p-value = 2.589e-12
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -37.29227 -22.85199
    ## sample estimates:
    ## mean of x mean of y 
    ##  196.0614  226.1335

# Wnioski
