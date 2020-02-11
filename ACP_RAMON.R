## Componentes Principais no R ##

## LIVRO: Métodos de Análise Multivariada em R de Anderson Silva

# Carregando o banco de dados:

data("mtcars")
mtcars

# Análise Descritiva:

# vetor de totais:
totais = colSums(mtcars); totais
 
# Vetor de médias
media = colMeans(mtcars); media
      #ou
media = apply(mtcars, 2, mean); media

# matriz de variancia e covariancia:
S = cov(mtcars)
cov(mtcars);S

# Coeficiente de Variação:
cv = function(x){100* sd(x) / mean(x)}
apply(mtcars,2,cv)

# Matriz de Correlação:
R = cor(mtcars); R

#### Elipses de Correlação:
library(plotrix)
library(ellipse)

## Matriz  de cores:
mcores = color.scale(1-abs(R))
plotcorr(R,col = mcores)

### Componentes Principais

acp = princomp(mtcars, cor = T); acp
summary(acp)
screeplot(acp)

print(acp$loadings, cutoff = 0)

imp_var = function(dados, componente){
acp = princomp(dados, cor = T)
barplot(acp$loadings[,componente], beside = T, 
        col = sign(acp$loadings[,componente]) + 3, ylim = c(-0.5,0.5),
           ylab = "importância", xlab = "Variáveis",
           main = "Importancia da variável")
            }
imp_var(dados = mtcars,1)



plot(acp$loadings[,1],acp$loadings[,2])
biplot(acp,scale = 0)
