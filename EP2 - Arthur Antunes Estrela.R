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
## Como usar as funções require e library:
## https://pt.stackoverflow.com/questions/153450/quais-s%C3%A3o-as-diferen%C3%A7as-entre-require-e-library
##  
## Além das aulas gravadas, revisei as matrizes em:
## https://rpubs.com/jean_97/pratica03
##
## Função order que usei para ordenar elementos no vetor (os melhores filmes)
## https://www.datacamp.com/community/tutorials/sorting-in-r
##
## Tive que relembrar como utilizar o cbind:
## https://statisticsglobe.com/cbind-r-command-example/#:~:text=(my_data%2C%20new_column)-,cbind(my_data%2C%20new_column),or%20data%20frames%20by%20columns.
##
## Sturges' Rule e outros métodos para definir quantidade ótima de barras no histograma:
## https://www.statology.org/sturges-rule/
#################################################################

# E1
# Para carregar os dados corretamente, é bom verificarmos se o pacote readr já
# está instalado. O "require" abaixo verifica isso. Se ainda não o estiver,
# vamos instalá-lo:
  
if(!require(readr)){
    install.packages("readr")
}

# Vamos carregar nossos dados. Para facilitar, adicionei títulos às colunas.
  
titulos = c("User", "Item", "Rating", "Time")
library(readr)
dados <- read_table2("u.data", col_names = titulos)
View(dados)

# E2
# Vamos extrair as colunas e guardar nos vetores a seguir
# e então criar a matriz "Recomendacao" apenas com valores NA:

user_id = dados$User
item_id = dados$Item
rating = dados$Rating

Recomendacao = matrix(NA, nrow = 943, ncol = 1682)

# Agora vamos verificar cada linha da tabela e inserir os respectivos
# valores (v) de rating na matriz, de acordo com row (r) e column (c):

i = 1
while (i <= length(user_id)){
  r = user_id[i]
  c = item_id[i]
  v = rating[i]
Recomendacao[r,c] = v
i = i + 1
}
View(Recomendacao)

# E3
# Pede-se: quantidade de avaliações feitas por usuário.
# Para fazer essa função, vamos pratcar o while duplo visto em aula.
# Para cada linha da matriz (user, letra u), verifica-se todas as colunas
# (item, letra i).
# Se o valor da célula M[u,i] for diferente de NA, "conta" recebe + 1:
contaLinha = function(M){
  avaliacoes_poruser = c()
  
  u = 1
  while (u <= nrow(M)){
    conta = 0
    i = 1
    while (i <= ncol(M)){
      if (!is.na(M[u,i])){
        conta = conta + 1
      }
        i = i + 1
    }
    avaliacoes_poruser = c(avaliacoes_poruser, conta)
    u = u + 1
  }
  
  return(avaliacoes_poruser)
}


# 4
# Pede-se: quantidade de avaliações recebidas por filme.
# De forma semelhante ao passo E3, agora vamos, para cada coluna (item, letra i),
# verificar todas as linhas (user, letra u), e contar as occorrências diferentes de NA:

contaColuna = function(M){
  M = as.matrix(M)
  avaliacoes_poritem = c()
  
  i = 1
  while (i <= ncol(M)){
    conta = 0
    u = 1
    while (u <= nrow(M)){
      if (!is.na(M[u,i])){
        conta = conta + 1
      }
      u = u + 1
    }
    avaliacoes_poritem = c(avaliacoes_poritem, conta)
    i = i + 1
  }
  
  return(avaliacoes_poritem)
}

# E5
# Pede-se: média dos valores por coluna, ou seja, rating médio de cada filme.
# Também parecido com o passo anterior, mas para calcular a média podemos
# guardar no vetor "rating_medio" o resultado da expressão "soma/conta":

mediaColuna = function(M){
  M = as.matrix(M)
  rating_medio = c()
  
  i = 1
  while (i <= ncol(M)){
    soma = 0
    conta = 0
    u = 1
    while (u <= nrow(M)){
      if (!is.na(M[u,i])){
        soma = soma + M[u,i]
        conta = conta + 1
      }
      u = u + 1
    }
    rating_medio = c(rating_medio, soma/conta)
    i = i + 1
  }
  
  return(rating_medio)
}

# E6
# Usar a função mediaColuna e guardar seus valores no vetor mediaFilmes:
mediaFilmes = mediaColuna(Recomendacao)

# E7
# Vamos carregar e visualizar os dados dos filmes. Por conveniência, também
# coloquei os títulos nas colunas:

titulos = c("movie id", "movie title", "release date", "video release date", 
"IMDb URL","unknown","Action","Adventure","Animation","Children's","Comedy",
"Crime","Documentary","Drama","Fantasy","Film-Noir","Horror","Musical",
"Mystery","Romance","Sci-Fi","Thriller","War","Western")
library(readr)
dadosFilmes = read_delim("u.item", "|", col_names = titulos,
                          escape_double = FALSE, trim_ws = TRUE)

dadosFilmes = cbind(dadosFilmes, mediaFilmes) # Aqui, achei pertinente adicionar uma coluna com o rating médio dos filmes.
View(dadosFilmes)

# Agora devemos selecionar os filmes com rating médio igual ou maior que 4.3 (melhoresFilmes).
# Ainda, quis apresentá-los em ordem. Para isso, criei também o vetor "ratingDoFilme", 
# que será o parâmetro de classificação em ordem decrescente.

melhoresFilmes = c()
ratingDoFilme = c()

i = 1
while (i <= ncol(Recomendacao)){
  if (mediaFilmes[i] >= 4.3){
    melhoresFilmes = c(melhoresFilmes, dadosFilmes$`movie title`[i])
    ratingDoFilme = c(ratingDoFilme, dadosFilmes$mediaFilmes[i])
  }
  i = i + 1
}

melhoresFilmes_ordem = c()
melhoresFilmes_ordem = melhoresFilmes[order(ratingDoFilme, decreasing = TRUE)]
cat("Filmes com rating igual ou maior que 4.3, em ordem:\n\n")
print(as.array(melhoresFilmes_ordem))

# E8
# Usar a função contaLinha e guardar seus valores no vetor contaUsers:
contaUsers = contaLinha(Recomendacao)

# E9
# Usar a função contaColuna e guardar seus valores no vetor contaFilmes:
contaFilmes = contaColuna(Recomendacao)

# E10
# Agora vamos criar os 3 gráficos de histograma para cada 
# um dos dois vetores "contaUsers" (Avaliações feitas por usuário) 
# e "contaFilmes" (Avaliações recebidas por filme).

repeat{
  cat("______________________________________________________________________
Gráficos de histograma:

Avaliações feitas por usuário:
  Método: Sturge's Rule, digite 1
  Método: The Square-Root rule, digite 2
  Método: The Rice Rule, digite 3
  
Avaliações recebidas por filme:
  Método: Sturge's Rule, digite 4
  Método: The Square-Root rule, digite 5
  Método: The Rice Rule, digite 6
 
Para parar, digite 7\n\n")
  
escolha = readline("Escolha um gráfico: ")

  if (escolha == 7) break

# Para definir o parâmetro "breaks" eu pesquisei sobre histogramas e há diversos
# métodos para se determinar a quantidade ótima de barras, de acordo com o
# número de dados. Eu escolhi três para a comparação dos gráficos: 
# Sturge's Rule (argumento padrão do R), The Square-root Rule e The Rice Rule. 
# Deixei o link de referência, que contém a definição matemática dos métodos
# no cabeçalho deste trabalho.

# Avaliações feitas por usuário:

# Método: Sturge's Rule
  else if (escolha == 1){
    hist(contaUsers, breaks = "Sturges", col = "Sienna",
         main = "Nº de avaliações por usuário",xlab = "Usuário" ,ylab = "Avaliações")
  }

# Método: The Square-root Rule
  else if (escolha == 2){
    hist(contaUsers, breaks = ceiling(sqrt(length(contaUsers))), col = "Sienna",
         main = "Nº de avaliações por usuário",xlab = "Usuário" ,ylab = "Avaliações")
  }
  
# Método: The Rice Rule
# (Aqui o valor do breaks acabou sendo igual ao sturges)
  else if (escolha == 3){
  hist(contaUsers, breaks = ceiling(2*(length(contaUsers)**(1/3))), col = "Sienna",
       main = "Nº de avaliações por usuário",xlab = "Usuário" ,ylab = "Avaliações")
  }  





# Avaliações recebidas por filme:

# Método: Sturge's Rule
  else if (escolha == 4){
    hist(contaFilmes, breaks = "Sturges", col = "MediumOrchid",
         main = "Nº de avaliações por filme",xlab = "Filme",ylab = "Avaliações")
  }

# Método: The Square-root Rule
  else if (escolha == 5){
    hist(contaFilmes, breaks = ceiling(sqrt(length(contaFilmes))), col = "MediumOrchid",
         main = "Nº de avaliações por filme",xlab = "Filme",ylab = "Avaliações")
  }

# Método: The Rice Rule
  else if (escolha == 6){
    hist(contaFilmes, breaks = ceiling(2*(length(contaFilmes)**(1/3))), col = "MediumOrchid",
         main = "Nº de avaliações por filme",xlab = "Filme",ylab = "Avaliações")
  }

  else if (escolha == 8){
    plot(contaFilmes, mediaFilmes)
    
  }



  else cat("Digite um número entre 1 e 7\n\n")
}