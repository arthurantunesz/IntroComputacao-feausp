numeros <- sample(80, replace=TRUE)
cat("sequencia dada:",numeros,"\n")
i = 1
j = 1
while (j < length(numeros)){
  
  while (i <= length(numeros)-j){
    if (numeros[j] > numeros[j+i]){
      a <- numeros[j]
      numeros[j] <- numeros[j+i]
      numeros[j+i] <- a
    }
    i <- i+1
}
 j = j + 1
 i = 1
}
cat("ordem arrumada:",numeros,"")