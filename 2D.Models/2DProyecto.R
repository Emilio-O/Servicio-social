# ============================================================================ #
# ============================ Pueba de funciones ============================ #
# ============================================================================ #


setwd("~/MyRstuff/Modelaje/Plots_proyecto")
# install.packages("plot.matrix") # Es indispensable esta libreria para hacer plots
library(plot.matrix)
library(ggplot2)

#save(SARS, SARS2, SARS3, SARS4, 
     #SARS5, SARS6, C_cold, C_cold2, file = "Stuff.RData")

load("Stuff.RData")

citation(package = "ggplot2")
citation(package = "plot.matrix")


# =============================================== #
# ================ Recomendaciones ============== #
# =============================================== #

# no hay un límite de personas que se quieran tomar en cuenta, siempre y cuando
# no se guarden los plots

# Hay resultados interesantes en donde hay inmunidad de rebaño, si y solo si, el
# periodo de infeccioso es relativamente corto (ejemplo un dia)


# =============================================== #
# ============ 2D.SEIR de SARS-CoV-2 ============ #
# =============================================== #


# Periodo de incubacion == 6 dias (los dos ultimos ya es infeccioso)
# Periodo de transmision == 12
#  10 000 personas (100^2)
# 500 dias

# Referencia: 

SARS <- SEIR.2D(Population = 10000, Suceptible = 0, Exposed = 1, Infectious = 7,
                Recovered = 19, rCoor = 50, nCoor = 50, Days = 500, R0 = 2,
                SavePlots = F)
head(SARS)



SARS2 <- SEIR.2D(Population = 10000, Suceptible = 0, Exposed = 1, Infectious = 7,
                Recovered = 19, rCoor = 50, nCoor = 50, Days = 500, R0 = 3,
                SavePlots = F)
head(SARS2)



SARS3 <- SEIR.2D(Population = 10000, Suceptible = 0, Exposed = 1, Infectious = 7,
                Recovered = 19, rCoor = 50, nCoor = 50, Days = 500, R0 = 1,
                SavePlots = F)
head(SARS3)


SARS4 <- SEIR.2D(Population = 10000, Suceptible = 0, Exposed = 1, Infectious = 7,
                 Recovered = 13, rCoor = 50, nCoor = 50, Days = 500, R0 = 3,
                 SavePlots = F)
head(SARS4)


SARS5 <- SEIR.2D(Population = 10000, Suceptible = 0, Exposed = 1, Infectious = 7,
                 Recovered = 13, rCoor = 50, nCoor = 50, Days = 500, R0 = 2,
                 SavePlots = F)
head(SARS5)


SARS6 <- SEIR.2D(Population = 10000, Suceptible = 0, Exposed = 1, Infectious = 7,
                 Recovered = 13, rCoor = 50, nCoor = 50, Days = 500, R0 = 1,
                 SavePlots = F)
head(SARS6)


# =============================================== #
# ========== 2D.SEIR de Resfriado comun ========= #
# =============================================== #

# Periodo de incubacion == 3 dias (los dos ultimos ya es infeccioso)
# Periodo de transmision == 7
# Días de inmunidad == 8
#  10 000 personas (100^2)
# 500 dias

# Referencia: 

C_cold <- SEIR.2D(Population = 10000, Suceptible = 0, Exposed = 1, Infectious = 4,
                  Recovered = 11, rCoor = 50, nCoor = 50, Days = 500, R0 = 2,
                  SavePlots = F, Sag = 19)
head(C_cold)

C_cold2 <- SEIR.2D(Population = 10000, Suceptible = 0, Exposed = 1, Infectious = 4,
                  Recovered = 11, rCoor = 5, nCoor = 5, Days = 500, R0 = 2,
                  SavePlots = F, Sag = 19)
head(C_cold2)





# =============================================== #
# ========== Graficas de los resultados ========= #
# =============================================== #


ggplot(SARS, aes(x=Day, y=Count, group = State, colour =State )) + 
  geom_point( size=1, shape=20, fill="white") + 
  theme_minimal() +  scale_y_discrete(breaks = 0) +
  scale_x_discrete(breaks = c(seq(0,500, 50)))

ggplot(SARS2, aes(x=Day, y=Count, group = State, colour =State )) + 
  geom_point( size=1, shape=20, fill="white") + 
  theme_minimal() +  scale_y_discrete(breaks = 0) +
  scale_x_discrete(breaks = c(seq(0,500, 50)))

ggplot(SARS3, aes(x=Day, y=Count, group = State, colour =State )) + 
  geom_point( size=1, shape=20, fill="white") + 
  theme_minimal() +  scale_y_discrete(breaks = 0) +
  scale_x_discrete(breaks = c(seq(0,500, 50)))

ggplot(C_cold, aes(x=Day, y=Count, group = State, colour =State )) + 
  geom_point( size=1, shape=20, fill="white") + 
  theme_minimal() +  scale_y_discrete(breaks = 0) +
  scale_x_discrete(breaks = c(seq(0,500, 50)))

ggplot(C_cold2, aes(x=Day, y=Count, group = State, colour =State )) + 
  geom_point( size=1, shape=20, fill="white") + 
  theme_minimal() +  scale_y_discrete(breaks = 0) +
  scale_x_discrete(breaks = c(seq(0,500, 50)))


citation(package = "ggplot2")
citation(package = "plot.matrix")

# =============================================== #
# =========== Fallas de la herramienta ========== #
# =============================================== #

# No es semejante a la realidad... por ahora
# No es de mundo pequeño... por ahora
# No es de cola larga... por ahora
# No hay tasas de mortalidad ni de riesgo... por ahora
# No se estan tomando en cuenta grupos de riesgo... por ahora
# El R0 es limitado... por ahora

