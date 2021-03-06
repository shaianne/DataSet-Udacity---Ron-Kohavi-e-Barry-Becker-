---
title: "Poder de Compra do individo"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r readTrain, results="hide"}
library(readr)
```

diretorio_padrao <- "C:/Users/aluno/Documents/desafio/"
train <- paste0(diretorio_padrao,"census.csv")
train <- read_csv(train)


```Definindo as vari�veis categoricas que ser�o utilizadas no modelo.
```

train$classtrb <- as.factor(train$workclass)
train$niveledu <- as.factor(train$education_level)
train$sitconjugal <- as.factor(train$'marital-status')
train$ocupacao <- as.factor(train$occupation)
train$relacionamento <- as.factor(train$relationship)
train$raca <- as.factor(train$race)
train$sexo <- as.factor(train$sex)
train$cidadenativa <- as.factor(train$'native-country')
train$renda <- as.factor(train$income)

```Definindo as vari�veis num�ricas
```
train$idade <- as.numeric(train$age) 
train$capitalganho <- as.numeric(train$'capital-gain')
train$capitalperdido <- as.numeric(train$'capital-loss')
train$hrsemana <- as.numeric(train$'hours-per-week')

```Analisar as vari�veis descritivas num�ricas
  ```
```Idade:
  ```

hist(train$idade)
boxplot(train$idade)
boxplot(train$idade~train$renda, outline = FALSE)


```Ganho de capital:
  ```
  
hist(train$capitalganho)
boxplot(train$capitalganho)
boxplot(train$capitalganho~train$renda, outline = FALSE)


```Perda de capital:
  ```  
  
hist(train$capitalperdido)
boxplot(train$capitalperdido)
boxplot(train$capitalperdido~train$renda, outline = FALSE)



```Horas trabalhadas por semana:
  ```
  
hist(train$hrsemana)
boxplot(train$hrsemana)
boxplot(train$hrsemana~train$renda, outline = FALSE)


```An�lise da vari�veis categoricas
```
```sexo
```

summary(train$sexo)
plot(train$sexo)
plot(train$Target~train$sexo)

```Classe de trabalho
```


summary(train$classtrb)
plot(train$classtrb)
plot(train$Target~train$classtrb)

```Relacionamento
```


summary(train$relacionamento)
plot(train$relacionamento)
plot(train$Target~train$relacionamento)

```Ocupa��o
```

summary(train$ocupacao)
plot(train$ocupacao)
plot(train$Target~train$ocupacao)

```ra�a
```

summary(train$raca)
plot(train$raca)
plot(train$Target~train$raca)

---Tratando as vari�veis fortes para a an�lise de dados
---

trat1 <- subset(train, (train$ocupacao == 'Craft-repair') | (train$ocupacao == 'Protective-serv')| (train$ocupacao == 'Sales')| (train$ocupacao == 'Prof-specialty')| (train$ocupacao == 'Exec-managerial'))
print(trat1)
trat2 <- subset(trat1,(trat1$capitalganho >0 )& (trat1$idade >=30 & trat1$idade <=60 ))

---N�mero de linhas ap�s o segundo tratamento
---
nrow(trat2)
trat2$renda

trat2$grupo <- sample.int(n=2, size=nrow(trat2), replace=TRUE, prob=c(.7, .3))
treinamento <- subset(trat2, trat2$set == 1)
treinamento <- subset(trat2, trat2$grupo == 1)

---N�mero de linhas do treinamento
---
nrow(treinamento)

teste <- subset(trat2, trat2$grupo == 2)
---N�mero de linhas do teste
---
nrow(teste)
treinamento$grupo = NULL
teste$grupo = NULL
rpart(renda ~ idade + classtrb + sexo + ocupacao , data = treinamento, method="class")
arvore = rpart(renda ~ idade + classtrb + sexo + ocupacao , data = treinamento, method="class")

arvore$variable.importance
pred <- predict(arvore, teste, type='class')

vp=0
vn=0
fp=0
fn=0

for (i in 1:nrow(teste)) {
  if (as.character(pred[i]) == '>50K' & teste[i, 'renda'] == '>50K' ) vp = vp + 1
  if (as.character(pred[i]) == '<=50K' & teste[i, 'renda'] == '<=50K' ) vn = vn + 1
  if (as.character(pred[i]) == '>50K' & teste[i, 'renda'] == '<=50K' ) fp = fp + 1
  if (as.character(pred[i]) == '<=50K' & teste[i, 'renda'] == '>50K' ) fn = fn + 1
}

