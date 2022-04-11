# Por practicidad se elimin? la funci?n de SIR

# ============================================================================ #
# ================================== SEIR.2D ================================= #
# ============================================================================ #

# Cambios propuestos
# Inclusi?n de tasa de muerte natural y aumentada por la enfermedad



SEIR.2D <- function(x, Suceptible = 0, Exposed, 
                    Infectious, Recovered, Subject, # rCoor, nCoor eliminados
                    Days, R0, SavePlots = F,
                    Sag = F, NaDeath = 0, InDeath = 0) {
  
  library(plot.matrix)

  C <- x # Sea "x" un objeto que contiene una matriz de adyacencias == C
  D <- C 
  D[,] <- 0 # Sea "D" una matriz con las mismas dimensiones que C, pero con 0
  
  A <- matrix(0, sqrt(ncol(C)), sqrt(ncol(C))) # crear la matriz de personas
  B <- A # Sea "B" una matriz que nos diga quienes se van a infectar
  
  
  
  S <- Suceptible
  E <- Exposed
  I <- Infectious
  R <- Recovered
  
  NaD <- NaDeath
  InD <- InDeath
  
  
  if (SavePlots == F){} else{
    png(paste0("Day",0,".png"), width = 720, height = 720)
    plot(A, col = c("#E8FFFF", "#A6F6F1", "#41AEA9", "#213E3B"), 
         breaks = c(S, E, I, R, Days), axis.col=NULL, axis.row=NULL, 
         xlab='', ylab='', main = paste("Day", 0), border = "White", 
         key = NULL, asp = T, na.col='black')
    dev.off()
  }
  

  A[Subject] <- 1
  
  
  
  
  
  # Probabilidad de muerte 
  for (i in 1:length(A)) {
    if (A[i] %in% 0:(I-1)) { A[i] <- sample(c(NA ,A[i]), 1, prob = c(NaD, 1-NaD))
    } else if (A[i] %in% I:(R-1)) {sample(c(NA ,A[i]), 1, prob = c(NaD+InD, 1-NaD-InD))}
  }
  
  
  
  
  if (SavePlots == F){} else{
    png(paste0("Day",1,".png"), width = 720, height = 720)
    plot(A, col = c("#E8FFFF", "#A6F6F1", "#41AEA9", "#213E3B"), 
         breaks = c(S, E, I, R, Days), axis.col=NULL, axis.row=NULL, 
         xlab='', ylab='', main = paste("Day", 1), border = "White", 
         key = NULL, asp = T, na.col='black')
    dev.off()
  }
  
  
  
  
  
  # Probabilidad de muerte
  for (i in 1:length(A)) {
    if (A[i] %in% 0:(I-1)) { A[i] <- sample(c(NA ,A[i]), 1, prob = c(NaD, 1-NaD))
    } else if (A[i] %in% I:(R-1)) {sample(c(NA ,A[i]), 1, prob = c(NaD+InD, 1-NaD-InD))}
  }
  
  
  
  
  
  Days <- Days-1 # Ajuste de los dias (porque ya se hizo un plot del dia 1)
  
  d <- 2 # Dia del plot
  
  Summary <- c(0, 0, 0, 0,  # Crear la tabla para la grafica de resultados
               1, 1, 1, 1,
               "S", "E", "I", "R", 
               "S", "E", "I", "R",
               ncol(C), 0, 0, 0,
               ncol(C)-1, 1, 0, 0)
  
  Summary <- data.frame(matrix(Summary, nrow = 8, ncol = 3))
  colnames(Summary) <- c("Day", "State", "Count")
  
  
  
  
  # ESTO NO ES SIMPLIFICABLE
  
  for (z in 1:Days) { # Lo va a correr dependiendo de los dias
    
    for (i in 1:ncol(C)) {
      
      positions <- which(C[ ,i] == 1)
      
      
      # Esto quita la ambiguedad de la funcion sample, 
      if (length(positions) == 1) {positions <- c(positions, positions)} else {}
      
      positions <- sample(positions, R0, replace = T)
      # replace == T nos da la posibilidad de que en el muestreo vuelva a salir
      # el mismo numero
      
      for (n in 1:length(positions)) {
        
        D[positions[n], i] <- 1
        
      }
      
    }
    
    
    # ESTO NO ES SIMPLIFICABLE
    ppl <- which(A[,] >= I ) # saber quienes estan infectados (1 corresponde al d?a
    # en que puede infectar, en teor?a seria I en la funcion)
    
    for (v in 1:length(ppl)) {
      
      nI <- D[ ,ppl[v]]
      nI <- which(nI[] == 1) # Esto te da a que personas va a infectar
      B[nI] <- 1
      
    }
    
    
    
    
    
    
    
    
    # Agregar un d?a a todos aquellos que est?n infectados

    
    # for (i in 1:nrow(A)) {
      
      # for (v in 1:ncol(A)) {
        
        # if (A[i,v] == "Na") {} else if (A[i,v] >= 1) {A[i,v] <- A[i,v]+1} else {}
        
      # }
      
    # }
    
    AddDay <- which(A[] >= 1)
    A[AddDay] <- A[AddDay] + 1
    
    
    
    # Solo agregar 1 (ya infectado) a todo aquello == 0 en A cuando hubo una
    # interaccion con un infectado (si B[i,v] == 0 no agregar? nada, es decir,
    # no hubo interacci?n con un infectado)
    
    new <- which(B[] == 1 & A[] == 0)
    A[new] <- 1
    
    

    if (SavePlots == F){} else{
      png(paste0("Day",d,".png"), width = 720, height = 720)
      plot(A, col = c("#E8FFFF", "#A6F6F1", "#41AEA9", "#213E3B"), 
           breaks = c(S, E, I, R, Days+1), axis.col=NULL, axis.row=NULL, 
           xlab='', ylab='', main = paste("Day", d), border = "White", 
           key = NULL, asp = T, na.col='black')
      dev.off()
    }
    
    
    
    
    # Probabilidad de muerte
    for (i in 1:length(A)) {
      if (A[i] %in% 0:(I-1)) { A[i] <- sample(c(NA ,A[i]), 1, prob = c(NaD, 1-NaD))
      } else if (A[i] %in% I:(R-1)) {sample(c(NA ,A[i]), 1, prob = c(NaD+InD, 1-NaD-InD))}
    }
    
    
    
    
    
    d <- d+1
    
    
    if (Sag == F) {} else {
      
      for (i in 1:nrow(A)) { # Especificar si despues de recuperarse se vuelven a infectar
        
        for (v in 1:ncol(A)) {
          
          if (A[i,v] > Sag) {A[i,v] <- 0} else {} 
          
        }
        
      }}
    
    
    # Resetear matriz
    # La explicaci?n de porque hago esto es demasiada complicada
    # Dejemoslo en: para que no se acumule la informaci?n y la matriz A y B
    # no trabajen con informaci?n que no est? actualizada
    D[,] <- 0
    
  }
  
  return(Summary) # Tabla de resultados
}



################################################################################
##############################      Idea      ##################################
################################################################################

#  Un programa que decida por persona si se muere o no

# Los argumentos de la funcion plot() del paquete "plot.matrix" para tomar en
# cuenta un muertin seran

## na. col = 'black' ## con esto lo asocias a un color

# o 

## na.cell = FALSE   ## con esto eliminas la celda como tal

# Para indicar si alguien esta muerto debe de reemplazarse la casilla de la
# matriz A como "NA"



# Me surgen preguntas:
# Que har?a el programa si en la matriz A hay una celda con NA?  
# Respuesta: Nada, no los toma en cuenta para ver a quien infecta (AFORTUNADAMENTE)

# Donde debe de ser incluido el codigo?
# Respuesta: antes de cada plot a partir del d?a 1


# Como asignas un valor basado en una probabilidad?

NaDeath <- .50 # Na == natural death rate
InDeath <- .75 # In == Increased deth rate

sample(1:2, 1, prob = c(.1,.9))

# Argumentos en la funci?n
SARS <- SEIR.2D(x = Adj, Suceptible = 0, Exposed = 1, Infectious = 7,
                Recovered = 19, Subject = 45, Days = 200, R0 = 2,
                SavePlots = T, NaDeath == 0, InDeath == 0)


NaD <- NaDeath
sample(1:2, 1, prob = c(NaD, 1-NaD)) # Probabilidades


InD <- InDeath
sample(1:2, 1, prob = c(NaD+InD, 1-NaD-InD)) # Probabilidades


# opcion 1
which(A[] >= 0)
for (i in vector) {
  
}


# opcion 2


# Probabilidad de muerte
for (i in 1:nrow(A)*nrow(A)) {
  if (A[i] %in% 0:I-1) { A[i] <- sample(c(NA ,A[i]), 1, prob = c(NaD, 1-NaD))
  } else if (A[i] %in% I:R-1) {sample(c(NA ,A[i]), 1, prob = c(NaD+InD, 1-NaD-InD))}
}

# Ya esta, ahora... donde se pone




# Sustituir el for para la matriz
mmmmmmmatriz <- matrix(0, 5,5)
mmmmmmmatriz[c(1:5)] <- 1
mmmmmmmatriz

uwu <- which(mmmmmmmatriz[] %in% 1:2)
mmmmmmmatriz[uwu] <- mmmmmmmatriz[uwu]+1 
mmmmmmmatriz

# En la funcion
AddDay <- which(A[] >= 1)
A[AddDay] <- A[AddDay] + 1
################################################################################
#############################      Pruebas     #################################
################################################################################

library(igraph)
Adj <- barabasi.game(100, directed = FALSE)
# Asi se hace solamente la matriz de adyacencias
Adj <- as.matrix(get.adjacency(Adj))
dim(Adj)


SARS <- SEIR.2D(x = Adj, Suceptible = 0, Exposed = 1, Infectious = 7,
                Recovered = 19, Subject = 45, Days = 100, R0 = 2,
                SavePlots = T, NaDeath = .001, InDeath = 0)
head(SARS)





# Error in if (A[i, v] > 0) { : missing value where TRUE/FALSE needed

# Al parecer los if tienen muchisimos problemas con un valor de NA, y hace cosas
# raras

