#################################################################
## AO PREENCHER ESSE CABEÇALHO COM O MEU NOME E O MEU NUMERO USP,
## DECLARO QUE SOU O UNICO AUTOR E RESPONSAVEL POR ESSE PROGRAMA.
## TODAS AS PARTES ORIGINAIS DESSE EXERCICIO PROGRAMA (EP) FORAM
## DESENVOLVIDAS E IMPLEMENTADAS POR MIM SEGUINDO AS INSTRUÇOES
## DESSE EP E QUE PORTANTO NAO CONSTITUEM DESONESTIDADE ACADEMICA
## OU PLAGIO.
## DECLARO TAMBEM QUE SOU RESPONSAVEL POR TODAS AS COPIAS
## DESSE PROGRAMA E QUE EU NAO DISTRIBUI OU FACILITEI A
## SUA DISTRIBUIÇAO. ESTOU CIENTE QUE OS CASOS DE PLAGIO E
## DESONESTIDADE ACADEMICA SERAO TRATADOS SEGUNDO OS CRITERIOS
## DIVULGADOS NA PAGINA DA DISCIPLINA.
## ENTENDO QUE EPS SEM ASSINATURA NAO SERAO CORRIGIDOS E,
## AINDA ASSIM, PODERAO SER PUNIDOS POR DESONESTIDADE ACADEMICA.
## Nome : ARTHUR ANTUNES ESTRELA
## NUSP : 12512880
## Turma: 21
## Prof.: Roberto Hirata Jr.
## Referencias: Com excecao das rotinas fornecidas no enunciado
## e em sala de aula, caso voce tenha utilizado alguma refencia,
## liste-as abaixo para que o seu programa nao seja considerado
## plagio ou irregular.
## Exemplo:
## - O algoritmo Quicksort foi baseado em
## http://wiki.python.org.br/QuickSort
##
## - Pesquisei como criar e alterar alguns elementos do gráfico em:
## https://didatica.tech/regressao-linear-com-linguagem-r/
## https://pt.stackoverflow.com/questions/86990/alterar-a-escala-do-eixo-x
##
## - Função repeat:
## http://www.estatisticacomr.uff.br/?p=199
##
## - Regressão linear e função para encontrar coeficientes da reta:
## https://rstudio-pubs-static.s3.amazonaws.com/46495_3f8078811c5d44a5b7951bf68a230c04.html
## https://posgraduando.com/como-fazer-uma-analise-de-regressao-linear-simples-no-r/
#################################################################

# As etapas deste exercício programa estão marcadas com números e suas 
# respectivas explicações estão no relatório entregue em formato PDF, 
# bem como a análise dos resultados.

#E1
dados = read.csv("C:/Users/Arthur/PASTA DO R/DADOSDOEP1-ARTHUR.csv", sep = ";")

#E2
circunferencia = dados[,2]
diametro = dados[,3]

#E3
meupi = c()
i = 1
while(i <= length(circunferencia)){
  meupi = c(meupi,circunferencia[i]/diametro[i])
  i = i + 1
}

#E4
somaPi = 0
i = 1
while (i <= length(meupi)){
  somaPi <- somaPi + meupi[i]
  i = i + 1
}
piMedio = somaPi/length(meupi)
cat("O valor do pi médio (piMedio) é",piMedio,"\n")

#E5
somatorio = 0
i = 1
while (i <= length(meupi)) {
  somatorio <- somatorio + (meupi[i]-piMedio)**2
  i = i + 1
}
piVar = somatorio/(length(meupi)-1)
desvioPadrao = sqrt(piVar)
cat("O valor da variância (piVar) é",piVar,"
O valor do desvio padrão (desvioPadrao) é",desvioPadrao,"\n")

##########################################
#1
k1 = coef(lm(circunferencia~diametro))
pi1 = k1[2]

#2
x = 1:length(meupi)
y = meupi
k2 = coef(lm(y~x))
pi2 = k2[1]

#11
novaCircunferencia = c()
novoDiametro = c()
i = 1
while (i <= length(meupi)){
  if (meupi[i] > piMedio - piVar && meupi[i] < piMedio + piVar){
    novaCircunferencia = c(novaCircunferencia, circunferencia[i])
    novoDiametro = c(novoDiametro, diametro[i])
  }
  i = i + 1
}
k11 = coef(lm(novaCircunferencia~novoDiametro))
pi11 = k11[2]

#22
novosPontos = c()
i = 1
while (i <= length(meupi)){
  if (meupi[i] > piMedio - piVar && meupi[i] < piMedio + piVar){
    novosPontos = c(novosPontos, meupi[i])
  }
  i = i + 1
}
a = 1:length(novosPontos)
b = novosPontos
k22 = coef(lm(b~a))
pi22 = k22[1]
##########################################

repeat{
  cat("----------------------------------------------------------------------\n")
  escolha = readline("Escolha uma opção:\n
Gráfico 1 - Distribuição dos objetos medidos: digite 1
Gráfico 2 - Distribuição dos valores de pi: digite 2
Gráfico 1 melhorado: digite 11
Gráfico 2 melhorado: digite 22
Melhor estimativa de pi, digite 3
Para parar, digite 4\n")
  
#E6
  if (escolha == 1){
    plot(diametro, circunferencia,
         main = "Distribuição dos objetos medidos",
         xlab = "Diâmetro", ylab = "Circunferência",
         xlim = c(0,max(diametro)), ylim = c(0,max(circunferencia)))
    abline(lm(circunferencia~diametro), col = "red")
    cat("Os coeficientes angular e linear da reta são:",k1[2],"e",k1[1],"
Estimativa do pi, pelos dados fornecidos:",k1[2],"\n")
    } 

#E7
   else if (escolha == 2) {
    plot(x, y, main = "Distribuição de todos os valores de pi",
         xlab = "Quantidade de objetos (índices de meupi)", ylab = "Valores de pi obtidos",
         xlim = c(0, length(meupi)), ylim = c(0, 12),)
    abline(lm(y~x), col = "red")
    cat("Estimativa do pi, pelos dados fornecidos:",k2[1],"\n")
  } 

#E8
    else if (escolha == 11){
    plot(novoDiametro, novaCircunferencia,
         main = "Distribuição dos objetos medidos (filtrado)",
         xlab = "Diâmetro", ylab = "Circunferência",
         xlim = c(0,max(diametro)), ylim = c(0,max(circunferencia)))
    abline(lm(novaCircunferencia~novoDiametro), col = "green")
    cat("Quantidade de objetos filtrados:",length(meupi)-length(novoDiametro),"
Os coeficientes angular e linear da reta são:",k11[2],"e",k11[1],"
Estimativa do pi, pelo novo gráfico:",k11[2],"\n")
  }
    else if (escolha == 22){
    plot(a, b, main = "Distribuição dos melhores valores de pi",
         xlab = "Quantidade de objetos (filtrado)", ylab = "Valores de pi obtidos",
         ylim = c(0, 12), xlim = c(0, length(meupi)))
    abline(lm(y~x), col = "green")
    cat("Quantidade de objetos filtrados:",length(meupi)-length(novosPontos),"
Estimativa do pi, pelo novo gráfico:",k22[1],"\n")
    }
  else if (escolha == 3){
    pi = (pi1 + pi2 + pi11 + pi22)/4
    cat("A estimativa de pi é:",pi,"\n")
  }
  else if (escolha == 4){
    break
  }
  else {
    cat("Digite 1, 11, 2, 22, 3 ou 4 apenas\n")
  }
}
