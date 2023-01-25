source(file.path("code", "GA", "ga.r"))
source(file.path("code", "PRS", "prs.r"))

ackley_2 <- makeAckleyFunction(2)
ackley_10 <- makeAckleyFunction(10)
ackley_20 <- makeAckleyFunction(20)

ackley_2_ga <- calculate_ga(ackley_2, 50)
ackley_2_prs <- calculate_prs(ackley_2, 50)

ackley_10_ga <- calculate_ga(ackley_10, 50)
ackley_10_prs <- calculate_prs(ackley_10, 50)

ackley_20_ga <- calculate_ga(ackley_20, 50)
ackley_20_prs <- calculate_prs(ackley_20, 50)
