##########################################################################
######################## Análise de Discriminante ########################
############################ Por RAMON VIANA  ############################
#install.packages("digest")
#install.packages("ggplot2")
require(digest)
require(ggplot2)
g1=matrix(c(33,36,35,38,40,60,61,64,63,65),ncol=2,byrow=F)
g2=matrix(c(35,36,38,39,41,43,41,57,59,59,61,63,65,59),ncol=2,byrow=F)
mg1 = apply(g1, 2, mean)
mg2 = apply(g2, 2, mean)
n1=nrow(g1) # tamanho do vetor 1
n2=nrow(g2) # tamanho do vetor 1

g=as.data.frame(rbind(g1, g2))
g$grupos=as.factor(rep(c('1','2'),c(nrow(g1),nrow(g2)))) # adicionando os grupos


#descriminantes (autovalor)
s1 = var(g1) # variância do grupo 1
s2 = var(g2) # variância do grupo 2
sc = ((n1-1)*s1+(n2-1)*s2)/(n1+n2-2) # matriz de variâncias combinadas
a = solve(sc)%*%(mg1-mg2) # Autovalores
g
pc = t(a)%*%(mg1+mg2)/2;pc # ponto de corte
plot(g1,pch=16,col='red',xlim=c(33,41),ylim = c(55,66)) 
points(g2,pch=16,col='purple')
abline((pc/a[2]) ,-a[1]/a[2], col = "blue")

ggplot(g,aes(x=V1,y=V2,col=grupos))+
  geom_point()+
  geom_abline(aes(slope=-a[1]/a[2] , intercept=(pc/a[2]) ))

#transformações dos dados
tg1 = t(t(a)%*%t(g1)) #vetor coluna
tg2 = t(t(a)%*%t(g2)) #vetor coluna

#média dos valores transformados
mtg1=mean(tg1)
mtg2=mean(tg2)

# adicionando um novo ponto para saber a qual grupo ele pertence
x1=40
x2=63
ggplot(g,aes(x=V1,y=V2,col=grupos))+geom_point()+
  geom_abline(aes(slope=-a[1]/a[2] , intercept=(pc/a[2])))+
  geom_point(aes(x=x1, y=x2 ), colour="blue")

pc
np=(mg1-mg2)%*%solve(sc)%*%as.matrix(c(x1,x2));np
if(pc>np){
  cat('Grupo 2')
}else{
  cat('Grupo 1')
}

require(MASS)
m=lda(as.factor(grupos)~.,data=g)
teste=data.frame('V1' = x1,'V2'=x2)
predict(m,teste)
#### usando o pacote
require(klaR)
partimat(as.factor(grupos) ~ V2+V1,data=g,method='lda')
#A Área azul representa todos os pontos preditos pela reta de decisão do grupo 1. A Área rosa representa
#todos os pontos preditos pela reta de decisão do grupo 2. Os número representam os pontos do conjunto de
#dados associados a cada grupo (1 ou 2). Os centróides representam as médias de cada grupo. O valor zero
#indica que a reta de decisão separou perfeitamente os dois grupos, ou seja, não houve
#erros.

#### usando o pacote MASS
require(MASS)
ad=lda(grupos~.,data=g ) # verifica a classificação
plot(ad)
tpg1=g1%*%ad$scaling## transformada do pacote - grupo 1
tpg2=g2%*%ad$scaling## transformada do pacote - grupo 2 
mtpg1=mean(tpg1)
mtpg2=mean(tpg2)
a2=-ad$scaling[1]/ad$scaling[2]
pc2=(t(ad$scaling)%*%(ad$means[1,]+ad$means[2,])/2)
plot(g1,pch=16,col='red',xlim=c(33,41),ylim = c(55,66)) 
points(g2,pch=16,col='purple')
abline((pc2/ad$scaling[2]) ,-ad$scaling[1]/ad$scaling[2])

# Iris
data("iris")  #Banco de dados
require(MASS)
m2=lda(iris$Species ~ .,data=iris) #
plot(m2)
pm2=predict(m2)
table(pm2$class,iris$Species) # Verifica a qualidade da classificação
sum(diag(table(pm2$class,iris$Species))) # Quantidade de amostras classificadas corretamente
sum(diag(table(pm2$class,iris$Species)))/nrow(iris) # Taxa de classificação dos corretos
1-sum(diag(table(pm2$class,iris$Species)))/nrow(iris) # Taxa de classificação dos errados

## Pacotes
require(caret)
require(e1071)
confusionMatrix(predict(m2)$class,iris$Species)
ldahist(predict(m2)$x,iris$Species)
