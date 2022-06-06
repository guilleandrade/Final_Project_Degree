
library('ggplot2') # visualización
library('ggthemes') # visualización
library('scales') # visualización
library('dplyr') # Manipulación de datos
library('mice') # imputación
library('randomForest') # Algoritmo de clasificación



train <- read.csv('train.csv', stringsAsFactors = F)
test  <- read.csv('test.csv', stringsAsFactors = F)

#Juntamos train y test
full  <- bind_rows(train, test) 


str(full)




# De la columna Name, nos queremos quedar con el título: Mr, Mrs o Miss
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name) # .* todo lo que haya hasta la coma lo reemplaza
# //..* el punto y todo lo que haya después del punto lo reemplaza

# Títulos distribuidos por sexo
table(full$Sex, full$Title)

# Todos los que no sean Mr, Mrs o Miss, los llamamos raros
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')


full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Títulos distribuidos por sexo
table(full$Sex, full$Title)

# Visualizamos la variable título con supervivencia
ggplot(full[1:891,], aes(x = Title, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  theme_few()
# Vemos la clara diferencia de ahogados respecto a salvados en Mr, 
#y sin embargo Mrs y Miss se salvaron muchas más que ahogaron


# Apellido de cada pasajero
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])



## Veamos si las familias se ahogaron o sobrevivieron juntas

#Creamos la variable tamaño de la familia
full$Fsize <- full$SibSp + full$Parch + 1

table(full$Fsize)
#Da fallos. por ejemplo en familias de 7 miembros, vemos 9 Andersson. A priori parece fallo
# de los datos y no de nuestro código
filter(full, Surname=="Andersson")


# Creamos la variable familia 
full$Family <- paste(full$Surname, full$Fsize, sep='_')


# Visualizamos tamaño de la familia con supervivencia
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()


#Vemos que familias pequeñas (2,3 y 4 miembros) sobrevivieron más que ahogaron
#Por el contrario, familias de más de 4 miembros se ahogaron más que sobrevivieron


# Discretizamos la variable tamño de la familia
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'





# La variable cabina tiene muchos valores perdidos, por lo que no vamos a trabajar con ella
full$Cabin[1:28]




## Imputación de datos en embarque y edad


# Pasajeros 62 y 830 tienen valor perdido en embarque
full[c(62, 830), 'Embarked']


embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

# Visualizamos embarque, clase; para ver donde se ajusta mejor el embarque de los dos valores perdidos
# Los 2 valores perdidos de embarque, son de la clase 1 y pagaron 80$ por el ticket
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

#La mediana del embarque C y clase 1 está muy cerca de 80$, por lo que vamos a asignarle la clase C a ambos

full$Embarked[c(62, 830)] <- 'C'



# Tenemos 1 valor perdido en fare, que es l pasajero 1044
full[1044, ]
#Su clase es 3 y el embarque S

ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()


# Reemplazamos el valor perdido fare con la mediana para fare en clase 3 y embarque S
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)



## Imputación

# tenemos 263 valores perdidos en la edad
sum(is.na(full$Age))


# Convertimos estas variables en factor
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

set.seed(129)

#Imputación mice, quitando las variables que no nos resultan útiles
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 

#Datos con la variable edad imputada
mice_output <- complete(mice_mod)


#Comparamos la distribución de la variable Age original y con los valores imputados
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))
par(mfrow=c(1,1))
#Parece correcta, por lo que reemplazamos la variable Age del modelo mice
full$Age <- mice_output$Age

# Vemos que ya no tenemos valores perdidos
sum(is.na(full$Age))
sum(is.na(full)) #Los 418 NA se corresponden a la variable Survived del conjunto test







# Modelo

# Dividimos en conjunto entrenamiento y test
train <- full[1:891,]
test <- full[892:1309,]

set.seed(129)

#Usamos el algoritmo de clasificación RandomForest
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Title + FsizeD, data = train)

# Error del modelo
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

#Porcentaje de acierto conjunto entrenamiento
predictrain<- predict(rf_model,train)
aciertos<-table(valorReal=train$Survived,predictrain)
aciertos
(aciertos[1,1]+aciertos[2,2])/sum(aciertos)*100 #93.94%


## Importancia de las variables

importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Visualizamos la importancia de las variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

#Vemos que la variable Título es la que más influye en el modelo


# Predicción en el conjunto test
prediction <- predict(rf_model, test)

# dataframe con dos columnas: PassengerId y Survived
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

#write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)





