# Practica Ampliacion

power<-function(){ 
  
  vecm=c()
  vec=c()
  print("indique longitud de la matriz")
  length=scan(,,1)
  print("rellene los valores de la matriz")
  for (i in 1:length){
    vecm=c(vecm,vecm[i])
    cat("valor ",i)
    vecm[i]=scan(,,1)
  }
  print("gracias, ahora indique nº filas")
  fil=scan(,,1)
  
  A=matrix(vecm,nrow=fil,byrow = T)
  
  print("indique un vector arbitrario (de tamaño correspondiente a filas de matriz)")
  for (i in 1:fil){
    vec=c(vec,vec[i])
    cat("valor ",i)
    vec[i]=scan(,,1)
  }
  B=matrix(c(vec))
  
  print("esta es la parte crucial del programa, cuántas iteraciones desea usted la realizar?")
  iteraciones=scan(,,1)
  vecav=c()
  for (p in 1:iteraciones){
  for(i in 1:p){
    B=A%*%B 
  }
  C=A%*%B 
  
  autovalor=(t(B)%*%C)/(t(B)%*%B)
  autovector=C/C[nrow(C)]
  
  cat("su autovalor es",autovalor,"y su autovector es","\n")
  print(autovector)
  
  vecav=c(vecav,autovalor)
  
  }
  print(A)
  cat("vector autovalores","\n",vecav,"\n")
  for(i in 1:iteraciones){
  cat("autovalor con",i,"iteraciones es",vecav[i],"\n")
  }
}
power()





