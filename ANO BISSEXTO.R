ano <- as.integer(as.numeric(readline("Digite o numero do ano: ")))


while (ano!=0 | ano==0) {
  if (ano<0){
    ano <- as.integer(as.numeric(readline("Digite um numero positivo: ")))
  }
  
  if(ano>=0){
  if (ano%%4==0 & ano%%100!=0 | ano%%400==0){
    cat("O ano é bissexto")
  } else cat("O ano não é bissexto")
  ano <- as.integer(as.numeric(readline("Digite o numero do ano: ")))
  }
}