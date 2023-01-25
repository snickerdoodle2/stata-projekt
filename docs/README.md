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
    ## 0.01486 0.15179 0.34969 0.54694 0.68592 2.37585

Średnia wartość:

    ## [1] 0.5469414

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-9-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-10-1.png)

Podsumowanie:

    summary(ackley_2_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.8964  3.2435  4.0078  4.0499  5.2466  7.1488

Średnia wartość:

    ## [1] 4.049896

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
    ##  0.1221  0.9320  1.2760  1.5608  1.9742  4.6783

Średnia wartość:

    ## [1] 1.560783

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-18-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-19-1.png)

Podsumowanie:

    summary(rastrigin_2_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.6852  1.2421  1.8017  1.9249  2.2943  4.2690

Średnia wartość:

    ## [1] 1.924854

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
    ##   2.667   4.406   4.708   4.841   5.260   6.565

Średnia wartość:

    ## [1] 4.841204

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-27-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-28-1.png)

Podsumowanie:

    summary(ackley_10_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   17.31   17.94   18.24   18.30   18.62   19.40

Średnia wartość:

    ## [1] 18.29903

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
    ##   48.73   64.95   71.22   73.01   81.01   95.08

Średnia wartość:

    ## [1] 73.01218

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-36-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-37-1.png)

Podsumowanie:

    summary(rastrigin_10_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   62.71   79.63   84.68   84.42   92.39  100.56

Średnia wartość:

    ## [1] 84.41884

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
    ##   6.117   7.573   8.574   9.896  10.996  19.722

Średnia wartość:

    ## [1] 9.896396

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-45-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-46-1.png)

Podsumowanie:

    summary(ackley_20_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   18.86   19.65   19.90   19.81   20.05   20.24

Średnia wartość:

    ## [1] 19.80556

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
    ##   136.5   185.5   197.2   195.6   208.2   237.2

Średnia wartość:

    ## [1] 195.5636

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-54-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-55-1.png)

Podsumowanie:

    summary(rastrigin_20_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   157.8   221.3   231.6   228.8   239.6   258.0

Średnia wartość:

    ## [1] 228.8318
