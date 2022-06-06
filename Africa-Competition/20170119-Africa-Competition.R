

library(e1071)
library(prospectr)

# Preparación de datos; primera derivada de la mitad de los datos, ya que parece que
# ahí está la información valiosa

train <- read.csv("training.csv", header = TRUE)

sg21 <- savitzkyGolay(train[ , c(1800:3579)], p = 3, w = 21, m = 1)

sg21 <- data.frame(sg21)

# Modelo final, después de haber elegido los parámetros

svm.Ca <- svm(sg21, train$Ca, cost = 50, gam = 0.00005, ep = 0.01)

svm.SOC <- svm(sg21, train$SOC, cost = 40, gam = 0.00005, ep = 0.05)

svm.pH <- svm(sg21, train$pH, cost = 30, gam = 0.0001, ep = 0.15)

svm.P <- svm(sg21, train$P, cost = 10, gam = 0.0001, ep = 0.1)

svm.Sand <- svm(sg21, train$Sand, cost = 5, gam = 0.0001, ep = 0.12)

# Lee el conjunto test y hacemos las predicciones

testX <- read.csv("sorted_test.csv", header = TRUE)

test.sg21 <- data.frame(savitzkyGolay(testX[, c(1800:3579)],p = 3, w = 21, m = 1))

sub <- read.csv("sample_submission.csv", header = TRUE)

sub$Ca <- predict(svm.Ca, test.sg21)

sub$SOC <- predict(svm.SOC, test.sg21)

sub$pH <- predict(svm.pH, test.sg21)

sub$P <- predict(svm.P, test.sg21)

sub$Sand <- predict(svm.Sand, test.sg21)

write.csv(sub, file = "prueba.csv", row.names = FALSE)
