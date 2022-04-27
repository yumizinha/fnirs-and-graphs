##### ANALISE DE Modularidade
require(igraph)
Z = NULL


# Leitura dos dados de cada uma das cinco duplas. Os BDs indicados por _P são professores
# BDs indicados sem _P são os alunos. Somente oxyhb foi indicado (VER JOAO).


for(j in 1:5){ #for da dupla
  #Leitura de Dados
  prof = read.table(paste("Subj", j, "_P_oxyhb.txt", sep=""))
  aluno = read.table(paste("Subj", j, "_oxyhb.txt", sep=""))
  
  #GSR - pega os resíduos do sinal médio de cada canal dos professores e alunos
  GSR = rowMeans(prof)
  for(i in 1:ncol(prof)){prof[, i] = lm(prof[, i]~GSR)$resid}
  
  GSR = rowMeans(aluno)
  for(i in 1:ncol(aluno)){aluno[, i ] = lm(aluno[, i]~GSR)$resid}
  

# Preprocessamento
# Matriz de conectividade funcional - encontra a correlação dos canais
# conexão entre cada canal
# matriz 36x36: correlação dos 18 canais de professor com 18 canais de alunos.
# todos os canais contra todos os canais e zerando corr<0.1
  
  CONNECT = cor(cbind(prof, aluno))   #,method="spearman")
 #para imprimir cada matriz write.csv(CONNECT, 'MATRIZ5.csv')
  
  CONNECT[which(CONNECT < 0.15)] = 0
  SUBJECT = c(rep(1, ncol(prof)), rep(2, ncol(aluno)))
  
  
  #Analise de Grafos a partir da modularidade (zorig)
  g = graph.adjacency(CONNECT, weighted=TRUE, mode="undirected", diag=FALSE)
  Zorig = modularity(g, SUBJECT)

  
  #Bootstrap
  Zboot = NULL
  NBOOT = 10000
  for(boot in 1:NBOOT){
    aux = sample(nrow(prof)-1, 1)
    auxsign = sample(c(-1, 1), 1)
    BOOTprof = prof[c((aux+1):nrow(prof), 1:aux),]*auxsign
    
    #Matriz de conectividade funcional
    CONNECT = cor(cbind(BOOTprof, aluno))  #,method="spearman")
    CONNECT[which(CONNECT < 0.15)] = 0
    SUBJECT = c(rep(1, ncol(prof)), rep(2, ncol(aluno)))
    g = graph.adjacency(CONNECT, weighted = TRUE, mode = "undirected", diag = FALSE)
    Zboot = c(Zboot, modularity(g, SUBJECT))
  }
  pvalue = length(which(Zboot<=Zorig))/NBOOT
  print(c(j, Zorig, pvalue))
}


##### Analise de hubs intra e intercerebro (bridge)
require(igraph)
DegreePROF = matrix(0, 5, 18)
DegreeALUNO = matrix(0, 5, 18)
INTERCEREBRO = matrix(0, 5, 36)
par(mfrow = c(2, 2))

for(j in 2:5){ #for da dupla #Tira o primeiro porque nao deu modularidade significante
  print(j)
  #PROCESSAMENTO IDENTICO AO ANTERIOR
  #Leitura de Dados
  prof = read.table(paste("Subj", j, "_P_oxyhb.txt", sep=""))
  aluno = read.table(paste("Subj", j, "_oxyhb.txt", sep=""))
  
  #GSR
  GSR = rowMeans(prof)
  for(i in 1:ncol(prof)){prof[, i] = lm(prof[, i]~GSR)$resid}
  GSR = rowMeans(aluno)
  for(i in 1:ncol(aluno)){aluno[, i] = lm(aluno[, i]~GSR)$resid}
  
  CONNECT = cor(cbind(prof, aluno))   #,method="spearman")
  CONNECT[which(CONNECT < 0.1)] = 0
  
  #grafos intracerebro
  matrizPROF = CONNECT[1:18, 1:18]
  diag(matrizPROF) = 0 #tira a diagonal principal
  matrizALUNO = CONNECT[19:36, 19:36]
  diag(matrizALUNO) = 0
  
  #calculo do degree intracerebro
  # VERIFICAR PQ colMeans ao invés de colSums - corrigi de acordo com link
  DegreePROF[j,] = colSums(matrizPROF)
  DegreeALUNO[j,] = colSums(matrizALUNO)
  
  #Analise do intercerebro
  #Zera todas as conexoes intracerebro
  CONNECT[1:18, 1:18] = 0
  CONNECT[19:36, 19:36] = 0
  SUBJECT = c(rep(1, ncol(prof)), rep(2, ncol(aluno)))
  g = graph.adjacency(CONNECT, weighted=TRUE, mode="undirected", diag=FALSE)
  plot(g, vertex.color = SUBJECT)
  Zorig = modularity(g, SUBJECT)
  INTERCEREBRO[j,] = colSums(CONNECT)
}


par(mfrow =c(2, 2))
#Descritivas Degree PROFESSOR
Z = DegreePROF
MEDIA = array(0, ncol(Z))
SD = array(0, ncol(Z))

for(i in 1:ncol(Z)){
  MEDIA[i] = mean(Z[, i])
  SD[i] = sd(Z[, i])
}

# PQ COHENd DÁ SEMPRE CONSTANTE? PROVAR! TROCANDO MEANS POR SUM RESOLVE?
COHENd = MEDIA/SD
names(COHENd) = colnames(prof)
barplot(COHENd, main = "Hub Teacher", xlab = "Channel", ylab = "Cohen-D")

#Descritivas Degree ALUNO
Z = DegreeALUNO
MEDIA = array(0, ncol(Z))
SD = array(0, ncol(Z))

for(i in 1:ncol(Z)){
  MEDIA[i] = mean(Z[, i])
  SD[i] = sd(Z[, i])
}
COHENd = MEDIA/SD
names(COHENd) = colnames(aluno)
barplot(COHENd, main = "Hub Student", xlab = "Channel", ylab = "Cohen-D")


#Descritivas Degre INTERCEREBRO - Bridges
Z = INTERCEREBRO
MEDIA = array(0, ncol(Z))
SD = array(0, ncol(Z))
for(i in 1:ncol(Z)){
  MEDIA[i] = mean(Z[, i])
  SD[i] = sd(Z[, i])
}
COHENd = MEDIA/SD
names(COHENd) = colnames(CONNECT)
barplot(COHENd[1:18], main="Bridges Teacher", xlab="Channel", ylab="Cohen-D")
barplot(COHENd[19:36], main="Bridges Student", xlab="Channel", ylab="Cohen-D")



#Paper
#1) Analise da modularidade
#2) Figura dos grafos intracerebro
#3) Figura de Analise dos hubs (Cohen D do degree intracerebros)
#4) FIgura dos grafos intercerebro
#5) Figura de Analise dos bridges (Cohen D do degree intercerebros)




# Análise de clustering Espectral
# tentativa de construir os grafos com o clustering espectral 
# usando a matriz de correlacao como similaridade

require(stats)

# cria matriz de afinidade para clustering espectral
make.affinity <- function(S, n.neighboors=2) {
  N <- length(S[,1])
  
  if (n.neighboors >= N) {  # fully connected
    A <- S
  } else {
    A <- matrix(rep(0,N^2), ncol=N)
    for(i in 1:N) { # for each line
      # only connect to those points with larger similarity 
      best.similarities <- sort(S[i,], decreasing=TRUE)[1:n.neighboors]
      for (s in best.similarities) {
        j <- which(S[i,] == s)
        A[i,j] <- S[i,j]
        A[j,i] <- S[i,j] # to make an undirected graph, ie, the matrix becomes symmetric
      }
    }
  }
  A  
}

par(mfrow = c(2, 2))
for(j in 2:5) { #for da dupla #Tira o primeiro porque nao deu modularidade significante
  print(j)
  #PROCESSAMENTO IDENTICO AO ANTERIOR
  #Leitura de Dados
  prof = read.table(paste("Subj", j, "_P_oxyhb.txt", sep=""))
  aluno = read.table(paste("Subj", j, "_oxyhb.txt", sep=""))
  
  #GSR
  GSR = rowMeans(prof)
  for(i in 1:ncol(prof)){prof[, i] = lm(prof[, i]~GSR)$resid}
  GSR = rowMeans(aluno)
  for(i in 1:ncol(aluno)){aluno[, i] = lm(aluno[, i]~GSR)$resid}
  
  data = cbind(prof, aluno)
  S_orig = cor(data)
  S = S_orig
  #S[1:18, 1:18] = 0
  #S[19:36, 19:36] = 0
  S = S + diag(1, 36)
  A <- make.affinity(S, 5)
  D <- diag(apply(A, 1, sum))
  U <- D - A
  k   <- 3
  evL <- eigen(U, symmetric=TRUE)
  Z   <- evL$vectors[,(ncol(evL$vectors)-k+1):ncol(evL$vectors)]
  cc = 3
  km <- kmeans(Z, centers = cc, nstart=5)
  cl = km$cluster
  g = graph.adjacency(S, weighted=TRUE, mode="undirected", diag=FALSE)
  modularity(g, cl)
  
  MM = matrix(NA, nrow = 36, ncol = 36)
  for(aa in 1:36)
  {
    for(bb in 1:36)
    {
      MM[aa, bb] <- (cl[aa] == cl[bb])
    }
  }
  colnames(MM) = rep(1:18, 2)
  rownames(MM) = rep(1:18, 2)
  g = graph.adjacency(MM, weighted=TRUE, mode="undirected", diag=FALSE)
  SUBJECT = c(rep(1, ncol(prof)), rep(2, ncol(aluno)))
  plot(g, vertex.color = SUBJECT)
}


# BOOTSTRAP RETIRANDO 1 CANAL DE CADA VEZ
# Leitura dos dados de cada uma das cinco duplas. Os BDs indicados por _P são professores
# BDs indicados sem _P são os alunos. Somente oxyhb foi indicado (VER JOAO).

data = as.list(rep(NA, 18))
for(y in 1:18) {
  print(y)
  aux_da_yuyu = matrix(NA, nrow = 5, ncol = 3)
  for(j in 1:5){ #for da dupla
    print(j)
    #Leitura de Dados
    prof = read.table(paste("Subj", j, "_P_oxyhb.txt", sep=""))
    prof = prof[,-y]
    aluno = read.table(paste("Subj", j, "_oxyhb.txt", sep=""))
    aluno = aluno[,-y]
    
    #GSR - pega os resíduos do sinal médio de cada canal dos professores e alunos
    GSR = rowMeans(prof)
    for(i in 1:ncol(prof)){prof[, i] = lm(prof[, i]~GSR)$resid}
    
    GSR = rowMeans(aluno)
    for(i in 1:ncol(aluno)){aluno[, i ] = lm(aluno[, i]~GSR)$resid}
    
    # Preprocessamento
    # Matriz de conectividade funcional - encontra a correlação dos canais
    # conexão entre cada canal
    # matriz 36x36: 18 canais de professor com 18canais de alunos.
    # todos os canais contra todos os canais e zerando corr<0.1
    
    CONNECT = cor(cbind(prof, aluno))   #,method="spearman")
    CONNECT[which(CONNECT < 0.1)] = 0
    SUBJECT = c(rep(1, ncol(prof)), rep(2, ncol(aluno)))
    
    
    #Analise de Grafos
    g = graph.adjacency(CONNECT, weighted=TRUE, mode="undirected", diag=FALSE)
    Zorig = modularity(g, SUBJECT)
    
    
    #Bootstrap
    Zboot = NULL
    NBOOT = 10000
    for(boot in 1:NBOOT){
      aux = sample(nrow(prof)-1, 1)
      auxsign = sample(c(-1, 1), 1)
      BOOTprof = prof[c((aux+1):nrow(prof), 1:aux),]*auxsign
      
      #Matriz de conectividade funcional
      CONNECT = cor(cbind(BOOTprof, aluno))  #,method="spearman")
      CONNECT[which(CONNECT < 0.1)] = 0
      SUBJECT = c(rep(1, ncol(prof)), rep(2, ncol(aluno)))
      g = graph.adjacency(CONNECT, weighted = TRUE, mode = "undirected", diag = FALSE)
      Zboot = c(Zboot, modularity(g, SUBJECT))
    }
    pvalue = length(which(Zboot<=Zorig))/NBOOT
    aux_da_yuyu[j, ] = c(j, Zorig, pvalue)
    print(aux_da_yuyu)
  }
  data[[y]] = aux_da_yuyu
  print(paste("Terminei o canal!", y))
  print(data[[y]])
}

#################################################
# ANALISE CENTRALIDADE DIVISAO EM DOIS CEREBROS #
# PLOT DE GRAFOS COM TEMPLATE DAS CABEÇAS       #
#################################################

require(igraph)
require(caret)
require(tidyverse)
require(png)
require(lmtest)
require(xts)

# molde de duas cabecas
molde <- readPNG("molde_hypper.png")


# Calculando ajuste das coordenadas calculadas no eeg_positions
# deslocando cérebro do aluno para separar as cabeças corretamente
# Alterei o deslocamento para coincidir com o template do cérebro carregado
coords_fnirs_prof = read.csv("coords_fNIRS.csv") %>%
  mutate(x= x-1.52) %>%
  mutate(y=y-0.028)

coords_fnirs_aluno = read.csv("coords_fNIRS.csv") %>% 
  mutate(x = x+1.53) %>%
  mutate(y=y-0.028)

# unindo as novas coordenadas para plot do grafo
coords_fnirs = rbind(coords_fnirs_prof, coords_fnirs_aluno) %>% 
  select(x, y) %>% 
  as.matrix()


# Leitura dos dados de cada uma das cinco duplas. Os BDs indicados por _P são professores
# BDs indicados sem _P são os alunos. Somente oxyhb foi indicado.

# se tiver c(1, 1) é porque gerei da última vez um grafo de cada vez.
# Ver dentro do for se está fixando uma única dupla e deletar se for gerar todos

par(mfrow =c(1, 1))

for(j in 2:5){ #for da dupla
  #Leitura de Dados


  prof = read.table(paste("Subj", j, "_P_oxyhb.txt", sep=""))
  aluno = read.table(paste("Subj", j, "_oxyhb.txt", sep=""))
  
  #GSR - pega os resíduos do sinal médio de cada canal dos professores e alunos
  GSR = rowMeans(prof)
  for(i in 1:ncol(prof)){prof[, i] = lm(prof[, i]~GSR)$resid}
  
  GSR = rowMeans(aluno)
  for(i in 1:ncol(aluno)){aluno[, i ] = lm(aluno[, i]~GSR)$resid}
  
  # Preprocessamento
  # Matriz de conectividade funcional - encontra a correlação dos canais
  # conexão entre cada canal
  # matriz 36x36: correlação dos 18 canais de professor com 18 canais de alunos.
  # todos os canais contra todos os canais e zerando corr<0.1
  
  CONNECT = cor(cbind(prof, aluno))   #,method="spearman")
  CONNECT[which(CONNECT < 0.15)] = 0
  SUBJECT = c(rep(1, ncol(prof)), rep(2, ncol(aluno)))
  
  # zerando conexoes intracerebrais
  CONNECT[1:18, 1:18] = 0
  CONNECT[19:36, 19:36] = 0

  # grafo nao direcionado a partir da matriz de adjacencia e calculo centralidade
  g = graph.adjacency(CONNECT, weighted = TRUE, mode = "undirected", diag = FALSE)
  centrality = eigen_centrality(g, directed = FALSE, scale = TRUE,
                                weights = NULL, options = arpack_defaults)

  
  
  # configurando paleta de cor por centralidade
  fine = 500 # this will adjust the resolving power.
  pal = colorRampPalette(c('red','green'))
  #this gives you the colors you want for every point
  graphCol = pal(fine)[as.numeric(cut(centrality$vector,breaks = fine))]
  
  
  # colocar centrality$vector como variavel em vertex.color
  plot(g, vertex.color=graphCol, layout = coords_fnirs, vertex.size = 10, rescale = FALSE)
  

  # Tentando colocar o pano de fundo do hypper
  lim <- par()
  rasterImage(molde,
              xleft=-2.6, xright=2.6, 
              ybottom=-1.1, ytop=1.1)
  
    
}


###########################################################
# USANDO CAUSALIDADE DE GRANGER COMO MATRIZ DE ADJACENCIA #
###########################################################


# Ajustamos dois modelos para cada uma das "vias": prever série do professor em função
# do aluno + prever professor só com lag da série do professor 
# e prever o aluno em função do professor + prever aluno em função apenas do lag do aluno.
# Com o resultado, pegaria a razão da variância dos resídios e inverteria, para ter uma matriz
# de adjacência com os índices. Ainda, zeraria os casos em que o teste de Granger mostrariam
# que o modelo não é significativo.
# Resultado: Não deu certo para ajuste dos modelos.



for(j in 2:5){ #for da dupla
  #Leitura de Dados
  j=2
  
  prof = read.table(paste("Subj", j, "_P_oxyhb.txt", sep=""))
  aluno = read.table(paste("Subj", j, "_oxyhb.txt", sep=""))
  
  #GSR - pega os resíduos do sinal médio de cada canal dos professores e alunos
  GSR = rowMeans(prof)
  for(i in 1:ncol(prof)){prof[, i] = lm(prof[, i]~GSR)$resid}
  
  GSR = rowMeans(aluno)
  for(i in 1:ncol(aluno)){aluno[, i ] = lm(aluno[, i]~GSR)$resid}
  
  # Preprocessamento
  # Matriz de conectividade funcional - encontra a correlação dos canais
  # conexão entre cada canal
  # matriz 36x36: correlação dos 18 canais de professor com 18 canais de alunos.
  # todos os canais contra todos os canais e zerando corr<0.1
  
  # Resíduos dos modelos com as séries temporais
  CONNECT_g = matrix(0, nrow = 36, ncol = 36)
  
  
  dt = 10
  # criar variável t-1 até t-10
  # pra cada equação vou ter 10 do passado x e 10 do passado y
  for (k in 1:18) {
    for (l in 1:18) {
      y = xts(aluno[,l], as.Date(1:length(aluno[,l])))
      x = xts(prof[, k], as.Date(1:length(prof[, k])))
      
      y1 = lm(y[-(1:dt),] ~ lag(x, k=1:dt)[-(1:dt),] + lag(y, k=1:dt)[-(1:dt),])$resid
      y2 = lm(y[-(1:dt),] ~ lag(y, k=1:dt)[-(1:dt),])$resid
      x1 = lm(x[-(1:dt),] ~ lag(x, k=1:dt)[-(1:dt),] + lag(y, k=1:dt)[-(1:dt),])$resid
      x2 = lm(x[-(1:dt),] ~ lag(x, k=1:dt)[-(1:dt),])$resid
      
      CONNECT_g[k,l+18] = var(y1)/var(y2)
      CONNECT_g[l+18,k] = var(x1)/var(x2)
    }
  }
  
  
  
  
  # matriz pra pegar o p-valor das séries do teste Granger
  CONNECT_g = matrix(0, nrow = 36, ncol = 36)
  # pra cada coluna de prof, calcular a causalidade de Granger pra mesma coluna de aluno:
  for (k in 1:18) {
    for (l in 1:18) {
      A = grangertest(prof[,k], aluno[,l], order = 1)
      B = grangertest(aluno[,l], prof[,k], order = 1)
      CONNECT_g[k,l+18] = A$"Pr(>F)"[2]
      CONNECT_g[l+18,k] = B$"Pr(>F)"[2]
      
      }
  }
  
  CONNECT_g < 0.005
  
}




