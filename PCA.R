##########################################################################
######################## Componentes Princi�is ########################
##########################  Por RAMON VIANA  #############################
library(HSAUR3) ## Pacote com Dataset
dados = USairpollution[,-1]   # Somente as vari�veis num�ricas
head(dados) # Visualizar o banco de dados
rho=cor(dados) # A matriz de correla��o � usada quando as m�tricas das vari�veis s�o diferentes ou quando a vari�ncia � 'grande'
require(REdaS) # pacote para fazer o teste esfericidade
# H0: rho = I_p
# H1: rho != I_p
# Se rejeitarmos o H0, podemos prosseguir com a an�lise
# Rejeitamos H0 se p-valor < nivel de signific�ncia

# Conclus�o:  rejeitamos H0, pvalor < nsignificancia, podemos fazer componentes principais
# como rejeitamos, pelo menos 2 vari�veis est�o correlacionadas (N�o � a Matriz Identidade)

bart_spher(rho)
comp=prcomp(dados, scale=T); ## scale=T -> matriz rho ; scale = F -> matriz sigma
summary(comp)
comp

## se o auto valor for maior que 1, "n�o tira"
# cidades com temp alta, as outras vari�veis ser�o baixas
# install.packages('FactoMineR')
# install.packages('factoextra')
require(FactoMineR)
require(factoextra)
m2=PCA(dados) # se as vari�veis estiverem opostas, formando um angulo de 90�, podemos considerar ind
round(m2$eig,4)
m2$svd
fviz_pca_biplot(m2) # Gr�fico com so dois plots conjutamente!

# precip tem maior influencia na dimensão 2 que a temperatura
# temperatura tem maior influ�ncia na dimens�o 1 que a precip

########## Caso entregue a matriz sigma
# verificar se as medidas estudadas s�o da mesma m�trica ou possuem alta varia��o
require(qcc)
sigma=matrix(c(1,-2,0,
               -2, 5,0,
               0, 0,2),ncol=3,byrow=T);sigma # Matriz sigma
autov=eigen(sigma)
varexp=autov$values/sum(autov$values);varexp ## Explica��o da varia��o individual
cumsum(varexp) ## Explica��o da varia��o acumudala
pareto.chart(varexp) ## Gr�fico de pareto


# e^t * sigma * e
round(t(autov$vectors)%*%sigma%*%autov$vectors,10) ## diagonal dos autovalores
ro=solve(sqrt(diag(c(diag(sigma)))))%*%sigma%*%solve(sqrt(diag(c(diag(sigma)))));ro

#cov2cor(sigma)
autov2=eigen(ro)
autov2$values/sum(autov2$values)

## Exemplo 2
# matriz sigma
sigma=matrix(c(1, 4,
               4,100),ncol=2,byrow=T);sigma
autov=eigen(sigma) # autovalores e autovetores da matriz sigma
varexp=autov$values/sum(autov$values);varexp # componentes principais matriz sigma
cumsum(varexp) # acumulada dos componentes principais matriz sigma
library(qcc) # gr�fico para o pacote
pareto.chart(varexp) # gr�fico de pareto para as componentes de sigma
# e^t * sigma * e (matriz de covariancias)
round(t(autov$vectors)%*%sigma%*%autov$vectors,10) # matriz dos autovalores
# matriz rho
ro=solve(sqrt(diag(c(diag(sigma)))))%*%sigma%*%solve(sqrt(diag(c(diag(sigma)))));ro
cov2cor(sigma)
#cov2cor(sigma)
autov2=eigen(ro) # autovalores e autovetores da matriz rho
varexpro=autov2$values/sum(autov2$values);varexpro # componentes principais matriz rho
cumsum(varexpro) # acumulada dos componentes principais matriz rho
pareto.chart(varexpro) # gr�fico de pareto para as componentes de sigma
