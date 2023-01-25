source(file.path("code", "GA", "ga.r"))
source(file.path("code", "PRS", "prs.r"))

ackley_2 <- makeAckleyFunction(2)
ackley_10 <- makeAckleyFunction(10)
ackley_20 <- makeAckleyFunction(20)

# ---------------------2 dimensions ackley--------------------------
ackley_2_ga <- calculate_ga(ackley_2, 50)
# > summary(ackley_2_ga)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.01757 0.29217 0.49695 0.70513 0.93920 2.05831
ackley_2_prs <- calculate_prs(ackley_2, 50)
# > summary(ackley_2_prs)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  0.3878  3.4457  4.2248  4.0764  4.6867  6.6019

# > t.test(ackley_2_ga, ackley_2_prs)

#         Welch Two Sample t-test

# data:  ackley_2_ga and ackley_2_prs
# t = -17.395, df = 72.813, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -3.757570 -2.985018
# sample estimates:
# mean of x mean of y
# 0.7051326 4.0764269

# --------------------10 dimensions ackley-------------------------

ackley_10_ga <- calculate_ga(ackley_10, 50)
# > summary(ackley_10_ga)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   3.812   4.603   5.054   5.222   5.810   7.681
ackley_10_prs <- calculate_prs(ackley_10, 50)
# > summary(ackley_10_prs)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   16.33   17.85   18.33   18.19   18.71   19.39

# > t.test(ackley_10_ga, ackley_10_prs)

#         Welch Two Sample t-test

# data:  ackley_10_ga and ackley_10_prs
# t = -80.265, df = 94.791, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -13.28563 -12.64427
# sample estimates:
# mean of x mean of y
#  5.222258 18.187207


# -----------------------------20 dimensions ackley-------------------------------

ackley_20_ga <- calculate_ga(ackley_20, 50)
# > summary(ackley_20_prs)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   18.40   19.82   19.97   19.87   20.05   20.22
ackley_20_prs <- calculate_prs(ackley_20, 50)
# > summary(ackley_20_ga)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   6.143   7.614   8.624   9.567  10.265  19.587

# > t.test(ackley_20_ga, ackley_20_prs)

#         Welch Two Sample t-test

# data:  ackley_20_ga and ackley_20_prs
# t = -22.382, df = 50.028, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -11.232597  -9.382636
# sample estimates:
# mean of x mean of y
#  9.567064 19.874681

# -----------------------------------------------------------------------
# ------------------------------------------------------------------------
# -------------------------------------------------------------------------

rastrigin_2 <- makeRastriginFunction(2)
rastrigin_10 <- makeRastriginFunction(10)
rastrigin_20 <- makeRastriginFunction(20)

# ----------------------------2 dimensions rastrigin------------------------
rastrigin_2_ga <- calculate_ga(rastrigin_2, 50)
# > summary(rastrigin_2_ga)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  0.4035  1.0553  1.6976  1.8144  2.2206  4.6877
rastrigin_2_prs <- calculate_prs(rastrigin_2, 50)
# > summary(rastrigin_2_prs)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  0.0549  1.1518  1.5065  1.6387  2.1802  3.4262

# > t.test(rastrigin_2_ga, rastrigin_2_prs)

#         Welch Two Sample t-test

# data:  rastrigin_2_ga and rastrigin_2_prs
# t = 1.042, df = 91.385, p-value = 0.3002
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.1592207  0.5106218
# sample estimates:
# mean of x mean of y
#  1.814445  1.638744

rastrigin_10_ga <- calculate_ga(rastrigin_10, 50)
# > summary(rastrigin_10_ga)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   52.29   69.77   75.67   76.51   83.31   98.54
rastrigin_10_prs <- calculate_prs(rastrigin_10, 50)
# > summary(rastrigin_10_prs)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   61.13   77.60   84.36   84.48   90.55  102.76

# > t.test(rastrigin_10_ga, rastrigin_10_prs)

#         Welch Two Sample t-test

# data:  rastrigin_10_ga and rastrigin_10_prs
# t = -3.9987, df = 95.875, p-value = 0.0001252
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -11.929331  -4.014598
# sample estimates:
# mean of x mean of y
#  76.50947  84.48144

rastrigin_20_ga <- calculate_ga(rastrigin_20, 50)
# > summary(rastrigin_20_ga)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   162.7   179.2   192.3   194.8   206.3   250.3
rastrigin_20_prs <- calculate_prs(rastrigin_20, 50)
# > summary(rastrigin_20_prs)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   174.6   217.1   226.9   224.7   235.3   251.2

# > t.test(rastrigin_20_ga, rastrigin_20_prs)

#         Welch Two Sample t-test

# data:  rastrigin_20_ga and rastrigin_20_prs
# t = -8.3652, df = 93.668, p-value = 5.562e-13
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -37.04452 -22.83187
# sample estimates:
# mean of x mean of y
#  194.7968  224.7350
