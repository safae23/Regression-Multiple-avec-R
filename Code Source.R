rm(list=ls())
# 1. Installer les bibliothèques nécessaires
install.packages("ggplot2")
install.packages("performance")
install.packages("ellipse")
install.packages("caret")
install.packages("lmtest")
install.packages("car")
install.packages("carData")

# 2. Charger les bibliothèques nécessaires
library(ggplot2)
library(performance)
library(ellipse)
library(zoo)
library(lmtest)
library(car)
library(carData)
# 3. Charger les données
insurance <- read.csv("insurance.csv")  

# 4. Exploration des données
# Afficher un aperçu et un résumé statistique
View(insurance)
summary(insurance)
str(insurance)

# Vérifier les valeurs manquantes
cat("Nombre de valeurs manquantes :", sum(is.na(insurance)), "\n")

# 5. Prétraitement des données
# Conversion des colonnes catégoriques en facteurs
insurance$sex <- factor(insurance$sex)
insurance$smoker <- factor(insurance$smoker)
insurance$region <- factor(insurance$region)

# Vérifiez les changements
str(insurance)


# 6. Visualisation initiale
#visualisation des donnnees 
plot(insurance)

# Scatter plots pour explorer les relations entre les variables
ggplot(insurance, aes(x = age, y = charges)) +
  geom_point(color = "blue") +
  labs(title = "Charges vs Age", x = "Age", y = "Charges")


# Scatter plot entre "sex" et "charges" 
ggplot(insurance, aes(x = sex, y = charges, color = sex)) +
  geom_jitter(width = 0.2, height = 0) +  # Ajouter un léger bruit pour visualisation
  labs(title = "Scatter Plot - Charges vs Sex", x = "Sex", y = "Charges")

# Scatter plot entre "charges" et "bmi"
ggplot(insurance, aes(x = bmi, y = charges)) +
  geom_point(color = "green") +
  labs(title = "Charges vs BMI", x = "BMI", y = "Charges")

# Scatter plot entre "charges" et "children"
ggplot(insurance, aes(x = children, y = charges)) +
  geom_point(color="red") +
  labs(title = "Scatter Plot - Charges vs Children", x = "Children", y = "Charges")

# Scatter plot entre "charges" et "smoker"
ggplot(insurance, aes(x = smoker, y = charges, color = factor(smoker))) +
  geom_jitter(width = 0.2, height = 0) +
  labs(title = "Charges vs Smoker", x = "Smoker", y = "Charges")

# Scatter plot entre "charges" et "region"
ggplot(insurance, aes(x = region, y = charges, color = region)) +
  geom_jitter(width = 0.2, height = 0) +
  labs(title = "Scatter Plot - Charges vs Region", x = "Region", y = "Charges")

# 7. Division des données en jeu d'entraînement et jeu de test
# Diviser les données de manière séquentielle
print(class(insurance)) 
print(dim(insurance))    

n <- nrow(insurance)  # Nombre total de lignes dans le dataset
train_size <- floor(0.8 * n)  # Calculer 80 % des lignes pour l'entraînement
# Jeu d'entraînement : des lignes 1 à 80 %
train <- insurance[1:train_size, ]
# Jeu de test : le reste des lignes
test <- insurance[(train_size + 1):n, ]

# Vérifier les tailles
cat("Nombre de lignes dans le jeu d'entraînement :", nrow(train), "\n")
cat("Nombre de lignes dans le jeu de test :", nrow(test),"\n")

# 8. Modélisation (Régression linéaire multiple)
# Modèle complet
model_full <- lm(charges ~ age + sex + bmi + children + smoker + region, data = train)
summary(model_full)

# Modèle simplifié (sans "sex")
model_reduced <- lm(charges ~ age + bmi + children + smoker+region , data = train)
summary(model_reduced)

# Modèle simplifié (sans "region" )
model_reduced1 <- lm(charges ~ age + bmi + children + smoker, data = train)
summary(model_reduced1)

# Comparaison des modèles
cat("AIC du modèle complet :", AIC(model_full), "\n")
cat("AIC du modèle réduit :", AIC(model_reduced1), "\n")

coef(model_reduced1)

confint(model_reduced1)

fitted(model_reduced1)

# 9. Diagnostic du modèle
# Vérification des résidus
residuals <- resid(model_reduced1)
fitted_values <- fitted(model_reduced1)

# Graphique des résidus
plot(fitted_values, residuals, main = "Résidus vs. Valeurs Prédites", xlab = "Valeurs Prédites", ylab = "Résidus")
abline(h = 0, col = "red")

# Test d'homoscédasticité (Breusch-Pagan)
bptest_result <- bptest(model_reduced1)
print(bptest_result)

# Vérification de la normalité des résidus
shapiro_result <- shapiro.test(residuals)
print(shapiro_result)

# Vérification de la colinéarité
vif_values <-car::vif(model_reduced1)
cat("Facteur d'inflation de la variance (VIF):\n")
print(vif_values)

# 10. Validation croisée
train_control <- trainControl(method = "cv", number = 10)
cv_model <- train(charges ~ age + bmi + children + smoker, data = train, method = "lm", trControl = train_control)
print(cv_model)

# 11. Prédictions
predictions <- predict(model_reduced, newdata = test)
test$predicted <- predictions

print(test$predicted)
# Calcul de l'erreur (MSE, RMSE)
mse <- mean((test$charges - test$predicted)^2)
rmse <- sqrt(mse)
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Exemple de prédiction sur de nouvelles données
new_data <- data.frame(age = 40, bmi = 25, children = 2, smoker = "yes")
predicted_charges <- predict(model_reduced1, newdata = new_data)
cat("Prédiction pour les données nouvelles :", round(predicted_charges, 2), "\n")

# 12. Visualisation des résultats
# Histogramme des résidus
hist(residuals, breaks = 30, main = "Histogramme des Résidus", xlab = "Résidus", col = "blue")

# Scatter plot des charges réelles vs prédites
ggplot(test, aes(x = charges, y = predicted)) +
  geom_point(color = "purple") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Charges Réelles vs Charges Prédites", x = "Charges Réelles", y = "Charges Prédites")



# 13. Points levier
alpha <- 0.05
n <- dim(train)[1]
p <- 2 
analyses <- data.frame(obs=1:n)
analyses$levier <- hat(model.matrix(model_reduced1))
seuil_levier <- 2*p/n


# Visualisation des leviers
ggplot(data=analyses,aes(x=obs,y=levier))+
  geom_bar(stat="identity",fill="steelblue")+
  geom_hline(yintercept=seuil_levier,col="red")+
  theme_minimal()+
  xlab("Observation")+
  ylab("Leviers")+
  scale_x_continuous(breaks=seq(0,n,by=5))

# Sélectionner les points leviers
idl <- analyses$levier>seuil_levier
idl
analyses$levier[idl]
which(idl == TRUE)

## La distance de Cook (leviers)
influence <- influence.measures(model_reduced1)
names(influence)
colnames(influence$infmat)
analyses$dcook <- influence$infmat[,"cook.d"]  
seuil_dcook <- 4/(n-p)


# Visualisation des leviers
ggplot(data=analyses,aes(x=obs,y=dcook))+
  geom_bar(stat="identity",fill="steelblue")+
  geom_hline(yintercept=seuil_dcook,col="red")+
  theme_minimal()+
  xlab("Observation")+
  ylab("Distance de cook")+
  scale_x_continuous(breaks=seq(0,n,by=5))



# Sélectionner des points
idl <- analyses$dcook>seuil_dcook
idl
analyses$dcook[idl]
which(idl == TRUE)


check_collinearity(model_reduced1)



# Visualisation des coefficients avec intervalles de confiance
n_coef <- length(coef(model_reduced1))
par(mfrow = c(n_coef, n_coef))

# Ajustez les marges ici
par(mar = c(2, 2, 2, 2))

for (i in 1:n_coef) {
  for (j in 1:n_coef) {
    if (i != j) {
      plot(ellipse(model_reduced1, c(i, j), level = 0.95, type = "l", 
                   xlab = paste("beta", i - 1, sep = ""), 
                   ylab = paste("beta", j - 1, sep = "")))
      
      # Utiliser coef(ll3) plutôt que coef(resume)
      points(coef(model_reduced1)[i], coef(model_reduced1)[j], pch = 3)
      
      IC <- rbind(coef(model_reduced1)[i] - coef(model_reduced1)[i + 1] * qt(0.975, model_reduced1$df.res),
                  coef(model_reduced1)[i] + qt(0.975, model_reduced1$df.res) * coef(model_reduced1)[i + 1])
      
      lines(c(IC[1], IC[1], IC[2], IC[2], IC[1]), 
            c(IC[1 + 1], IC[2 + 1], IC[2 + 1], IC[1 + 1], IC[1 + 1]), 
            lty = 2)
      
      plot(c(IC[1], IC[1]), c(IC[1 + 1], IC[2 + 1]), lty = 2)
      lines(c(IC[1], IC[1]), lty = 2)
    }
  }
}
