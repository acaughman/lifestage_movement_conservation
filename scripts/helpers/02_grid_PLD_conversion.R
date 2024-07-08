PLD = c(5, 10, 15, 30)

((pi * (1.33 * (PLD)^1.3)^2)) / 100 / 2

#there is an adjustment for drift


# "Paralabrax clathratus" HR: 0.003349, PLD 25 - 36 not in data set, found online
# "Semicossyphus pulcher" HR:0.111954514 PLD:47.46666667
# "Sebastes mystinus" HR: 0.017957634	PLD: 52.7057187
# "Sebastes carnatus" HR: 0.001092107	PLD: 47.74623829
# "Sebastes melanostomus" HR: 0.001809314	PLD: 56.80472276
# "Sebastes paucispinis" HR: 0.047456191 PLD: 45.75724411
# "Sebastes melanops" HR: 0.25	PLD: 45.04911932
# "Sebastes pinniger" HR: 0.163563979	PLD: 55.25874663

PLD = c(30, 47.46666667, 52.7057187, 47.74623829, 56.80472276, 45.75724411, 45.04911932, 55.25874663)

((pi * (1.33 * (PLD)^1.3)^2)) / 100 / 2
