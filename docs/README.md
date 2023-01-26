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
    ## 0.02999 0.20099 0.46826 0.60629 0.92401 2.13830

Średnia wartość:

    ## [1] 0.6062938

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-9-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-10-1.png)

Podsumowanie:

    summary(ackley_2_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.6788  3.1141  3.9176  4.1437  5.3528  8.1354

Średnia wartość:

    ## [1] 4.14371

### Porównanie

    t.test(ackley_2_ga, ackley_2_prs)

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  ackley_2_ga and ackley_2_prs
    ## t = -14.48, df = 58.068, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -4.026420 -3.048413
    ## sample estimates:
    ## mean of x mean of y 
    ## 0.6062938 4.1437103

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
    ## 0.01221 1.02541 1.22151 1.47542 1.93272 5.67109

Średnia wartość:

    ## [1] 1.475421

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-19-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-20-1.png)

Podsumowanie:

    summary(rastrigin_2_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.1931  1.1598  1.5356  1.6456  2.2262  3.4321

Średnia wartość:

    ## [1] 1.645605

### Porównanie

    t.test(rastrigin_2_ga, rastrigin_2_prs)

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  rastrigin_2_ga and rastrigin_2_prs
    ## t = -0.95726, df = 93.458, p-value = 0.3409
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.5232036  0.1828346
    ## sample estimates:
    ## mean of x mean of y 
    ##  1.475421  1.645605

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
    ##   3.345   4.901   5.451   5.771   6.015  18.479

Średnia wartość:

    ## [1] 5.770527

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-29-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-30-1.png)

Podsumowanie:

    summary(ackley_10_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   14.61   17.65   18.33   18.02   18.77   19.30

Średnia wartość:

    ## [1] 18.01777

### Porównanie

    t.test(ackley_10_ga, ackley_10_prs)

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  ackley_10_ga and ackley_10_prs
    ## t = -36.884, df = 72.761, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -12.90906 -11.58543
    ## sample estimates:
    ## mean of x mean of y 
    ##  5.770527 18.017775

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
    ##   51.03   67.03   77.11   76.15   84.11  103.37

Średnia wartość:

    ## [1] 76.15127

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-39-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-40-1.png)

Podsumowanie:

    summary(rastrigin_10_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   63.52   82.77   88.38   87.42   93.24  104.84

Średnia wartość:

    ## [1] 87.4223

### Porównanie

    t.test(rastrigin_10_ga, rastrigin_10_prs)

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  rastrigin_10_ga and rastrigin_10_prs
    ## t = -5.3386, df = 89.359, p-value = 7.046e-07
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -15.465800  -7.076265
    ## sample estimates:
    ## mean of x mean of y 
    ##  76.15127  87.42230

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
    ##   6.790   7.414   8.038   8.697   9.033  18.878

Średnia wartość:

    ## [1] 8.69688

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-49-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-50-1.png)

Podsumowanie:

    summary(ackley_20_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   18.17   19.59   19.90   19.80   20.05   20.25

Średnia wartość:

    ## [1] 19.79936

### Porównanie

    t.test(ackley_20_ga, ackley_20_prs)

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  ackley_20_ga and ackley_20_prs
    ## t = -35.142, df = 52.146, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -11.73640 -10.46856
    ## sample estimates:
    ## mean of x mean of y 
    ##   8.69688  19.79936

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
    ##   146.2   178.7   189.9   192.6   207.7   239.3

Średnia wartość:

    ## [1] 192.557

### PRS

Histogram:

![](README_files/figure-markdown_strict/unnamed-chunk-59-1.png)

Wykres pudełkowy:

![](README_files/figure-markdown_strict/unnamed-chunk-60-1.png)

Podsumowanie:

    summary(rastrigin_20_prs)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   183.6   220.0   229.8   227.2   238.5   253.1

Średnia wartość:

    ## [1] 227.1903

### Porównanie

    t.test(rastrigin_20_ga, rastrigin_20_prs)

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  rastrigin_20_ga and rastrigin_20_prs
    ## t = -9.2081, df = 85.996, p-value = 1.855e-14
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -42.11021 -27.15629
    ## sample estimates:
    ## mean of x mean of y 
    ##  192.5570  227.1903

# Wnioski

Z histogramów oraz z wykresów pudełkowych widać, że algorytm genetyczny
jest bardziej konsekwentny w swoim działaniu - wartości każdego z 50
wywołań są do siebie bardziej zbliżone niż wyniki z PRS - w
szczególności dla mniejszej ilości wymiarów.

<!-- o co mi tu chodzi?? -->

Jest to najpewniej spowodowane tym, że algorytm GA ma więcej “kierunków
w strone których może iść”

Algorytm PRS jest, jak sama nazwa wskazuje, w pełni losowy, przez co nie
ma aż tak wielkiej różnicy pomiędzy dwoma a dwudziestoma wymiarami.
