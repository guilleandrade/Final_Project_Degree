
library('dplyr')

train <- read.csv('train.csv', stringsAsFactors = F)
test  <- read.csv('test.csv', stringsAsFactors = F)

#Juntamos train y test
full  <- bind_rows(train, test) 


str(full)

attach(train)

table(color,type)

datos<-train[,-c(1,6)]
str(datos)
datos$type<-as.factor(datos$type)




###
modelosvm.tune<- tune(svm,type~., data=datos, 
                      ranges=list(cost=c(5,seq(50,1000,200))))
summary(modelosvm.tune)

modelosvm.tune2<- tune(svm,type~., data=datos, 
                       ranges=list(cost=5,gamma=c(0.1,0.5,1,1.5,2)))
summary(modelosvm.tune2)

#Obtener el mejor modelo
modelo.svm<- modelosvm.tune2$best.model
summary(modelo.svm)
####



predient<- predict(modelo.svm,datos,decision.values=TRUE)
margenes<-attr(predient, "decision.values")  
margenes[1:20,]
boxplot(margenes[,1]~datos$type,main="Ghoul/Goblin")  
boxplot(margenes[,2]~datos$type,main="Ghoul/Ghost")
boxplot(margenes[,3]~datos$type,main="Goblin/Ghost")
round(attr(predient, "probabilities")[1:20,],3)

resul<-table(predient,datos$type)
(resul[1,1]+resul[2,2]+resul[3,3])/sum(resul)


submission = data.frame(test$id,y_pred)
colnames(submission) = c('id','type')
write.csv(submission,'prueba_svm.csv',row.names=FALSE)





library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(reshape)
library(RColorBrewer)
library(randomForest)
library(dplyr)
library(e1071) ## parameter tunning 
library(caret) ## parameter tunning 
library(ranger) ## randomforest

train <- read.csv('train.csv', stringsAsFactors = F)
test  <- read.csv('test.csv', stringsAsFactors = F)


head(train)

any(is.na(train))



ggplot(data=  train,aes(x = bone_length ,y=rotting_flesh,colour=type)) + 
  geom_point(size=2)+
  facet_grid(.~type)




train %>% group_by(type) %>% summarise(Count = n())





ggplot(data=train,aes(x = bone_length)) +
  geom_density(alpha=.5,aes(group=type,fill=type,colour=type)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  ggtitle('Bone length distribution per type')




ggplot(data=train,aes(x = hair_length)) +
  geom_density(alpha=.5,aes(group=type,fill=type,colour=type)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  ggtitle('Hair length distribution per type')





ggplot(data=train,aes(x = has_soul)) +
  geom_density(alpha=.5,aes(group=type,fill=type,colour=type)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  ggtitle('has soul distribution per type')



ggplot(data=train,aes(x = rotting_flesh)) +
  geom_density(alpha=.5,aes(group=type,fill=type,colour=type)) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  ggtitle('Rotting flesh distribution per type')

train %>% group_by(color,type) %>% summarise(Count = n()) %>%
  ggplot(aes(x  = color , y = Count , fill = type )) + 
  geom_bar(stat = 'identity' , position = 'dodge',colour='black') + 
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5)) + 
  ggtitle('Color vs Type')

all_df = rbind(train[,!names(train) %in% c('id','type','color')] , 
               test[,!names(test) %in% c('id','color')])

all_df = all_df %>% mutate(
  hair_bone = bone_length * hair_length,
  bone_rotting = bone_length * rotting_flesh,
  bone_soul = bone_length * has_soul,
  rotting_hair = rotting_flesh * hair_length ,
  rotting_soul = rotting_flesh * has_soul
)
y = train['type']
test_ids = test['id']
features = names(all_df)
#all_df = scale(all_df)
x_train = all_df[1:nrow(train),]
x_test = all_df[(1+nrow(train)):nrow(all_df),]
x_train = cbind(x_train,y)

control = trainControl(method ='cv',number=10, repeats = 20,verboseIter = FALSE)

set.seed(42)
grid = expand.grid(mtry = c(3,5,7,9))
rf_model = train(
  type ~ bone_length + rotting_flesh + hair_length +
    has_soul + hair_bone + bone_rotting + bone_soul + 
    rotting_hair + rotting_soul,
  tuneLength = 3, #  defines the total number of parameter combinations that will be evaluated.
  data = x_train, 
  method = "ranger", 
  trControl = control, # 10-fold cross  validation 
  importance = 'impurity',
  tuneGrid = grid
)
y_pred = predict(rf_model,x_test)
submission = data.frame(test_ids,y_pred)
colnames(submission) = c('id','type')
write.csv(submission,'simple_randomforest_withR.csv',row.names=FALSE)


# LASSO
glm_model = train(
  type ~ bone_length + rotting_flesh + hair_length +
    has_soul + hair_bone + bone_rotting + bone_soul + 
    rotting_hair + rotting_soul,
  tuneLength = 3, #  defines the total number of parameter combinations that will be evaluated.
  data = x_train, 
  #method = "ranger",
  method = 'glmnet',
  trControl = control, # 10-fold cross  validation 
  #importance = 'impurity',
  tuneGrid = expand.grid(alpha = 0:1,
                         lambda = seq(0.001,1,length=30))
)
y_pred = predict(glm_model,x_test)
submission = data.frame(test_ids,y_pred)
colnames(submission) = c('id','type')
write.csv(submission,'glm_model.csv',row.names=FALSE)
