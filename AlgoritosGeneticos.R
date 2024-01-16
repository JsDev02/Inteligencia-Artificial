#install.packages("GA")

#Establecimiento de libreria
library(GA)

Pueblos = 8
ListaVectorPueblos = 1:Pueblos

#distancias
VectorInicial = sample(1:100, 64)#Vetor aleatorio de 64 numeros
MatrizEntrada = matrix(VectorInicial,nrow = Pueblos, ncol = Pueblos,T)

for (i in 1:Pueblos) {
  for (j in 1:Pueblos) {
    if(i==j){
      MatrizEntrada[i,j]=0
    }
  }
}

MatrizEntrada

TamañoPoblacion = Pueblos
TamañoMuestra = 3
IteracionesMax = 1500
TazaCruzamiento = 0.5 #puede ser entre 0.1 y 0.9
TasaMutacion = 0.01 #puede ser entre 0.01 y 0.09

Pueblos

funcion_adaptacion = function(vector, distancia, puntos){
  
  contador=0
  
  for (i in 1:(puntos-1)) {
    posicion1=i
    posicion2=i+1
    
    contador = contador + distancia[posicion1,posicion2]
  }
  
  primera = vector[1]
  ultima = vector[puntos]
  
  distancia_total = contador + distancia[ultima,primera]
  return(distancia_total)
}

funcion_adaptacion(ListaVectorPueblos,MatrizEntrada,Pueblos)

PoblacionInicial = matrix(0, nrow = TamañoPoblacion, ncol = Pueblos)
FuncionEvaluacion = 0
for (i in 1:TamañoPoblacion) {
   PoblacionInicial[i,]=sample(ListaVectorPueblos,Pueblos)
   FuncionEvaluacion[i] = funcion_adaptacion(PoblacionInicial[i,])
}
PoblacionInicial
FuncionEvaluacion

Seleccion = function(){
  posibles = sample(1:TamañoPoblacion,TamañoMuestra)
  minimo = min(funcionEvaluacion[posibles])
  a = which(FuncionEvaluacion[posibles] ==minimo)
  b = a[1]
  k = posibles[b]
  return(PoblacionInicial[k,])
}

Seleccion()

FuncionCruzamiento = function(padre,madre){
  Pueblos
}