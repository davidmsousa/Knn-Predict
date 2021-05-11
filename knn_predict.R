knn.df <- read.csv('dermatology.data', header = FALSE, sep = ',')

euclideanDist <- function(a, b){
  d = 0
  for(i in c(1:(length(a)-1) ))
  {
    d = d + (a[[i]]-b[[i]])^2
  }
  d = sqrt(d)
  return(d)
}

manhattam <- function(a,b){
  d = 0
  for(i in c(1:(length(a)-1) ))
  {
    d = d + abs((a[[i]]-b[[i]]))
  }

  return(d)
}

knn_predict <- function(test_data, train_data, k_value){
  pred <- c()  # vetor Vazio 
  #LOOP 
  for(i in c(1:nrow(test_data))){   #loop no número de linhas do meu teste
    Dist =c()          #Dist e Var são vetores vazio
    Var = c()
    g1 = 0              #inicializando algumas variaveis das minhas classes
    g2 = 0
    g3 = 0
    g4 = 0
    g5 = 0
    g6 = 0
    
    #LOOP-2-loop agora nos dados do treino
    for(j in c(1:nrow(train_data))){
      
      #colocando a distancia (podemos colocar tanto a euclidiana como a de manhattam)
      Dist <- c(Dist, manhattam(test_data[i,], train_data[j,]))
      
      #Adicionando as classes para o meu Var
      Var <- c(Var, as.character(train_data[j,][[34]]))
    }
    
    eu <- data.frame(Var, Dist) #dataframe criado com Var & Dist
    
    eu <- eu[order(eu$Dist),]       #ordenando os K vizinhos
    eu <- eu[1:k_value,]               #dataframe com os K valores
    
    #Loop 3: loops over eu and counts classes of neibhors.
    for(k in c(1:nrow(eu))){
      if(as.character(eu[k,"Var"]) == 1){
        g1 = g1+ 1
        
      }
      else if(as.character(eu[k,"Var"]) == 2){
        g2 = g2 + 1
        
      }
      else if(as.character(eu[k,"Var"]) == 3){
        g3 = g3+ 1
        
      }
      else if(as.character(eu[k,"Var"]) == 4){
        g4 = g4 + 1
        
      }
      else if(as.character(eu[k,"Var"]) == 5){
        g5 = g5 + 1
        
      }
      else if(as.character(eu[k,"Var"]) == 6){
        g6 = g6 + 1
        
      }
      
    }#1
    
    testeaa <- data.frame(g1,g2,g3,g4,g5,g6)
    #transformando em indice (estou pegando aqui o indice do meu dataframe a cima, se não fizesse isso, ele iria pegar os valores e não o seu indice que indicará sua classe)
    indice <- which.max(testeaa)
    #juntando as predições
    pred <- c(pred,indice)
    
    
  }
  #retorna vetor pred
  return(pred)
}

accuracy <- function(test_data){
  correct = 0
  for(i in c(1:nrow(test_data))){
    if(test_data[i,34] == test_data[i,35]){ 
      correct = correct+1
    }
  }
  accu = correct/nrow(test_data) * 100  
  return(accu)
}

library (caret)#precisaremos dessa livraria para fazer nossa matriz de confusão
#inicializando algumas variáveis!
ITERACAO=100 #100 IRECAÇAO
#As variáveis abaixo está sendo inicializada para fazermos quais as classes que mais acertam e as que mais erram
cont1=cont2=cont3=cont4=cont5=cont6=conta=0
z1=0
z2=0
z3=0
z4=0
z5=0
z6=0
az=0

i=0
az=0 #com essa variável irei medir a melhor matriz de confusão
azz=101 #com essa, medirei a pior
acu=NULL

#retirando o "?" 
knn.df$V34[knn.df$V34=="?"] <- 0
knn.df$V34<- as.numeric(knn.df$V34)
\end{lstlisting}

\subsection{Chamando funções, dentre outras.}
\begin{lstlisting}[language=R]
for (i in 1:ITERACAO) 
{
  ran <- sample(1:nrow(knn.df), 0.8 * nrow(knn.df))
  knn.df<- knn.df[sample(nrow(knn.df)),]
  train.df <- knn.df[ran,] ##extract testing set
  test.df <- knn.df[-ran,]
  K = 5
  predictions <- knn_predict(test.df, train.df, K) #chamando a nossa função knn_predict()
  
  test.df[,35] <- predictions #Adicionando os preditores na sua ultima coluna. Quando printarmos o "test.df" ficará melhor para vermos quais erraram e quais foram certas
  print(accuracy(test.df)) #printando a acurácia
  
  acu=c(acu, accuracy(test.df))
  
  #Ver quais as classe que mais acertam e as que mais erram.
  for (j in c(1:nrow(test.df)))
  {
    if(test.df[j,34]==test.df[j,35])
    {
      if(test.df[j,34]==1){
        z1=z1+1
        cont1=cont1+1
      }
      else if(test.df[j,34]==2){
        z2=z2+1
        cont2=cont2+1
      }
      else if(test.df[j,34]==3){
        z3=z3+1
        cont3=cont3+1
      }
      else if(test.df[j,34]==4){
        z4=z4+1
        cont4=cont4+1
      }
      else if(test.df[j,34]==5){
        z5=z5+1
        cont5=cont5+1
      }
      else if(test.df[j,34]==6){
        z6=z6+1
        cont6=cont6+1
      }
    }
    else
    {
      if(test.df[j,34]==1){#z1=z1+1
        cont1=cont1+1}
      else if(test.df[j,34]==2){#z2=z2-1
        cont2=cont2+1}
      else if(test.df[j,34]==3){#z3=z3-1
        cont3=cont3+1}
      else if(test.df[j,34]==4){#z4=z4-1
        cont4=cont4+1}
      else if(test.df[j,34]==5){#z5=z5-1
        cont5=cont5+1}
      else if(test.df[j,34]==6){#z6=z6-1
        cont6=cont6+1}
    }
  }
  #dataFrame das variaveis que acertei
  azul=data.frame(z1,z2,z3,z4,z5,z6)
  
  
  #com esses "if's" pegaremos a melhor e a pior matriz de confusão
  if(acu[i]>az)
  {
    az=acu[i]
    xtable<-table(test.df[,34],predictions)
  }
  if(acu[i]<azz)
  {
    azz=acu[i]
    ytable<-table(test.df[,34],predictions)
  }
  #az é uma variável auxiliar!
  az=acu
  print(az) #printando a acurácia em cada iteração
  azz=acu
  print(i) #printando em qual índice estou
  #posso fazer a taxa media
  print(acu)
}

#Transformando a quantidade de Erros e acertos em porcentagem
conta=data.frame(cont1,cont2,cont3,cont4,cont5,cont6)
testeClasse= azul[order(-azul)]
PorcentagemPorClasse=((azul*100)/(conta))

#matriz de confusão para o melhor caso
print(xtable)
#matriz de confusão para o pior caso
print(ytable)

#media das acurácias
mean(acu)

#Media de cada Classe
PorcentagemPorClasse










