# Por practicidad se eliminó la función de SIR

# ============================================================================ #
# ================================== SEIR.2D ================================= #
# ============================================================================ #

# Cambios propuestos
# Eliminar iCoor y nCoor como argumentos de la funcion y sustituirlos por
# Subject, con el fin de que la elección de la persona sea más fácil y esté
# basada en la matriz de adyacencia



SEIR.2D <- function(x, Suceptible = 0, Exposed, 
                    Infectious, Recovered, Subject, # rCoor, nCoor eliminados
                    Days, R0, SavePlots = F,
                    Sag = F) {
  
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
  
  if (SavePlots == F){} else{
    png(paste0("Day",0,".png"), width = 720, height = 720)
    plot(A, col = c("#E8FFFF", "#A6F6F1", "#41AEA9", "#213E3B"), 
         breaks = c(S, E, I, R, Days), axis.col=NULL, axis.row=NULL, 
         xlab='', ylab='', main = paste("Day", 0), border = "White", 
         key = NULL, asp = T)
    dev.off()
  }
  
  
  
  # ir <- rCoor # Establecer coordenadas para ver quien es el primer infectado
  # ic <- nCoor
  # A[ir,ic] <- 1
  A[Subject] <- 1
  
  
  if (SavePlots == F){} else{
    png(paste0("Day",1,".png"), width = 720, height = 720)
    plot(A, col = c("#E8FFFF", "#A6F6F1", "#41AEA9", "#213E3B"), 
         breaks = c(S, E, I, R, Days), axis.col=NULL, axis.row=NULL, 
         xlab='', ylab='', main = paste("Day", 1), border = "White", 
         key = NULL, asp = T)
    dev.off()
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
      
      
      # Esto quita la ambiguedad de la función sample, 
      if (length(positions) == 1) {positions <- c(positions, positions)} else {}
      
      positions <- sample(positions, R0, replace = T)
      # replace == T nos da la posibilidad de que en el muestreo vuelva a salir
      # el mismo numero
      
      for (n in 1:length(positions)) {
        
        D[positions[n], i] <- 1
        
      }
      
    }
    
    
   # ESTO NO ES SIMPLIFICABLE
    ppl <- which(A[,] >= I ) # saber quienes están infectados (1 corresponde al día
    # en que puede infectar, en teoría seria I en la funcion)
    
    for (v in 1:length(ppl)) {
      
      nI <- D[ ,ppl[v]]
      nI <- which(nI[] == 1) # Esto te da a que personas va a infectar
      B[nI] <- 1
      
    }
    
    
    
    
    
    
    
    
    # Agregar un día a todos aquellos que están infectados
    
    for (i in 1:nrow(A)) {
      
      for (v in 1:ncol(A)) {
        
        if (A[i,v] > 0) {A[i,v] <- A[i,v]+1} else {}
        
      }
      
    }
    
    
    # Solo agregar 1 (ya infectado) a todo aquello == 0 en A cuando hubo una
    # interaccion con un infectado (si B[i,v] == 0 no agregará nada, es decir,
    # no hubo interacción con un infectado)
    
    # for (i in 1:nrow(A)) {
      
    #   for (v in 1:ncol(A)) {
        
    #     if (A[i,v] == 0) {A[i,v] <- B[i,v]} else {}
        
    #   }
      
    # }
    
    new <- which(B[] == 1 & A[] == 0)
    A[new] <- 1
    
    
    # Hacer la tabla de resultados
    
    SCount <- 0 # Conteo de Suceptibles en A
    ECount <- 0 # Conteo de Expuestos en A
    ICount <- 0 # Conteo de Infectados en A
    RCount <- 0 # Conteo de Recuperados en A
    
    for (m in 1:ncol(A)) {
      
      for (n in 1:nrow(A)) {
        
        if (A[m,n] < E) {SCount <- SCount+1
        } else if (A[m,n] < I) {ECount <- ECount+1
        } else if (A[m,n] < R) {ICount <- ICount+1
        } else {RCount <- RCount+1}
        
      }
      
    }
    
    newS <- c(d, "S", SCount)
    newE <- c(d, "E", ECount)
    newI <- c(d, "I", ICount)
    newR <- c(d, "R", RCount)
    
    Summary <- rbind(Summary, newS, newE, newI, newR) # Actualizar tabla de resu
    
    if (SavePlots == F){} else{
      png(paste0("Day",d,".png"), width = 720, height = 720)
      plot(A, col = c("#E8FFFF", "#A6F6F1", "#41AEA9", "#213E3B"), 
           breaks = c(S, E, I, R, Days+1), axis.col=NULL, axis.row=NULL, 
           xlab='', ylab='', main = paste("Day", d), border = "White", 
           key = NULL, asp = T)
      dev.off()
    }
    
    d <- d+1
    
    
    if (Sag == F) {} else {
      
      for (i in 1:nrow(A)) { # Especificar si despues de recuperarse se vuelven a infectar
        
        for (v in 1:ncol(A)) {
          
          if (A[i,v] > Sag) {A[i,v] <- 0} else {} 
          
        }
        
      }}
    
    
    # Resetear matriz
    # La explicación de porque hago esto es demasiada complicada
    # Dejemoslo en: para que no se acumule la información y la matriz A y B
    # no trabajen con información que no está actualizada
    D[,] <- 0
    
  }
  
  return(Summary) # Tabla de resultados
}



################################################################################
##############################      Idea      ##################################
################################################################################

# Evaluar el tiempo de computo de algunas cosas de la funcion

# Asi se evaluara la funcion
system.time( replicate(10000, myfunction(with,arguments) ) )



# Solo agregar 1 (ya infectado) a todo aquello == 0 en A cuando hubo una
# interaccion con un infectado (si B[i,v] == 0 no agregará nada, es decir,
# no hubo interacción con un infectado)

 for (i in 1:nrow(A)) {

   for (v in 1:ncol(A)) {

    if (A[i,v] == 0) {A[i,v] <- B[i,v]} else {}

   }

 }



# O tambien funciona esto

new <- which(B[] == 1 & A[] == 0)
A[new] <- 1

################################################################################
#############################      Pruebas     #################################
################################################################################

# No tengo que hacer ninguna prueba para saber cual es mas rapido, es obvio ¿no?



# Contagio basada en mundo pequeño (HECHO)
library(igraph)
Adj <- barabasi.game(100, directed = FALSE)
# Asi se hace solamente la matriz de adyacencias
Adj <- as.matrix(get.adjacency(Adj))
dim(Adj)


SARS <- SEIR.2D(x = Adj, Suceptible = 0, Exposed = 1, Infectious = 7,
                Recovered = 19, Subject = 45, Days = 50, R0 = 2,
                SavePlots = T) # Funciona
head(SARS)


# Comprobar que SI FUNCIONA con los plots

which(Adj[,45]==1) # correcto 
which(Adj[,1]==1) # correcto
which(Adj[,67]==1) # correcto
which(Adj[,2]==1) # correcto





