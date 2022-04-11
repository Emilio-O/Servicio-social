# Por practicidad se eliminó la función de SIR

# ============================================================================ #
# ================================== SEIR.2D ================================= #
# ============================================================================ #

# Cambios propuestos
# Simplificar las variables de la función
# Convertir el modo de infección con base en una matriz de adyacencias



SEIR.2D <- function(x, Suceptible = 0, Exposed, 
                    Infectious, Recovered, rCoor, 
                    nCoor, Days, R0, SavePlots = F,
                    Sag = F) {
  
  library(plot.matrix)
  
  
  # A <- Population
  
  # A <- sqrt(A)
  
  # B <- matrix(data = 0, A+2,A+2) 
  # A <- matrix(data = 0, A,A)
  
  
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
  
  
  
  ir <- rCoor # Establecer coordenadas para ver quien es el primer infectado
  ic <- nCoor
  A[ir,ic] <- 1
  
  
  
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
  
  
  for (z in 1:Days) { # Lo va a correr dependiendo de los dias
    
    # for (i in 1:nrow(A)) { #Escanear renglones
      
      # for (v in 1:ncol(A)) { # Escanear columnas
        
        # r <- sample(1:8, R0) # Elegir a quien va a infectar
        
        # if (A[i,v] < I){} else { # descartar a los que no son infecciosos
          
          # for (k in 1:length(r)) { # a quien va a infectar
            
            # if(r[k] == 1) { B[c(i-1),c(v-1)] <- 1 
            # } else if (r[k] == 2) { B[c(i-1),c(v)] <- 1
            # } else if (r[k] == 3) { B[c(i-1),c(v+1)] <- 1
            # } else if (r[k] == 4) { B[c(i),c(v-1)] <- 1
            # } else if (r[k] == 5) { B[c(i),c(v+1)] <- 1
            # } else if (r[k] == 6) { B[c(i+1),c(v-1)] <- 1
            # } else if (r[k] == 7) { B[c(i+1),c(v)] <- 1
            # } else if (r[k] == 8) { B[c(i+1),c(v+1)] <- 1
            # } # Acaba el if para asignar valores a B
            
          # } # for de k
          
        # } # if == 0
        
      # } # for de v
 # } # for de i    
    
    
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
    
    for (i in 1:nrow(A)) {
      
      for (v in 1:ncol(A)) {
        
        if (A[i,v] == 0) {A[i,v] <- B[i,v]} else {}
        
      }
      
    }
    
    
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


# Contagio basada en mundo pequeño (HECHO)
library(igraph)
Adj <- barabasi.game(100, directed = FALSE)

# Asi se hace solamente la matriz de adyacencias
Adj <- as.matrix(get.adjacency(Adj))

C <- Adj
D <- B
D[,] <- 0

R0 <- 2



for (i in 1:ncol(Adj)) {
  
  positions <- which(C[ ,1] == 1)
  positions <- sample(positions, R0, replace = T)
  
  for (n in 1:length(positions)) {
      
      D[positions[1], 1] <- 1
      
    }
  
}

# Al final se obtiene una matriz de adyacencias en donde se ve a quien infecta
# Pero no podemos ver quien esta infectado


# Infeccion desde el primer caso
A <- matrix(0, sqrt(ncol(B)), sqrt(ncol(B))) # crear la matriz de personas
B <- A
A[5,5] <- 1


ppl <- which(A[,] >= 1 ) # saber quienes están infectados (1 corresponde al día
#                       en que puede infectar, en teoría seria I en la funcion)
ppl



for (i in 1:length(ppl)) {
  
  nI <- D[ ,ppl[i]]
  nI <- which(nI[] == 1) # Esto te da a que personas va a infectar
  B[nI] <- 1
  
}





# Sin ciclo
C <- Adj
D <- C
D[,] <- 0

A <- matrix(0, 10,10)
B <- A

R0 <- 2

for (i in 1:ncol(C)) {
positions <- which(C[ ,i] == 1)
print(positions)
positions <- sample(positions, R0, replace = T)
print(positions)
}

test1 <- which(C[,4] == 1)
test1
test2 <- sample(test1, R0)
test2

#Aqui hay algo raro en la funcion sample
#


D[positions[2], 45] <- 1
which(D[,45] == 1) 


# Con ciclo
for (i in 1:ncol(Adj)) {
  positions <- which(C[ ,i] == 1)
  positions <- sample(positions, R0, replace = T)
  for (n in 1:length(positions)) {
    D[positions[n], i] <- 1
  }
}

A[,] <- 0
A[5,5] <- 1

ppl <- which(A[,] >= 1 ) # saber quienes están infectados (1 corresponde al día
# en que puede infectar, en teoría seria I en la funcion)
ppl

  nI <- D[ ,ppl[1]]
  nI
  nI <- which(nI[] == 1) # Esto te da a que personas va a infectar
  nI
  B[nI] <- 1



  
  
  
  
  # 
  uwu <- matrix(0, 10,10)
  prueba <- c(1,2,3)
  uwu[prueba] <- 1
  uwu
  kk <- which(uwu[,]==1)
  kk
  
  

  owo <- D[ , kk[1]]
  owo
  owo <- which(owo[] == 1) # Esto te da a que personas va a infectar
  B[owo] <- 1

################################################################################
#############################      Pruebas     #################################
################################################################################


# Contagio basada en mundo pequeño (HECHO)
library(igraph)
Adj <- barabasi.game(100, directed = FALSE)
# Asi se hace solamente la matriz de adyacencias
Adj <- as.matrix(get.adjacency(Adj))
dim(Adj)


SARS <- SEIR.2D(x = Adj, Suceptible = 0, Exposed = 1, Infectious = 7,
                Recovered = 19, rCoor = 5, nCoor = 5, Days = 50, R0 = 2,
                SavePlots = T) # Funciona
head(SARS)


# Comprobar que SI FUNCIONA con los plots

which(Adj[,45]==1) # correcto
which(Adj[,99]==1) # correcto
which(Adj[,10]==1) # correcto
which(Adj[,63]==1) # correcto

# Falla a partir del día 14, cuando los nuevos infectados pueden infectar




for (i in 1:ncol(C)) {
  if (A[i] >= I) {
    ppl <- which(D[,i] == 1)
    B[ppl]
  } else {}
}

for (i in 1:ncol(C)) {
  if (A[i] == 0) {A[i] <- B[i]} else {}
}



sample(c(2,3), 3)
x <- 43
x <- c(x,x)
x
