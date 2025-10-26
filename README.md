# Régression Linéaire Multiple avec R

## Objectif du projet

L’objectif de ce projet est de **prédire les charges médicales** des assurés en fonction de variables telles que :

* l’âge,
* le sexe,
* l’indice de masse corporelle (BMI),
* le nombre d’enfants,
* le tabagisme,
* et la région géographique.

Le modèle de **régression linéaire multiple** permet d’analyser l’influence simultanée de ces variables sur le coût médical total.

---

## Étapes principales

### 1️⃣ Chargement et exploration des données

* Lecture du jeu de données `insurance.csv`
* Analyse descriptive (`summary()`, `str()`, `View()`)
* Vérification des valeurs manquantes
* Transformation des variables catégoriques (`sex`, `smoker`, `region`) en facteurs

### 2️⃣ Visualisation exploratoire

Utilisation de **ggplot2** pour explorer les relations entre les variables :

* `Charges vs Age`
* `Charges vs BMI`
* `Charges vs Children`
* `Charges vs Smoker`
* `Charges vs Region`

Exemple :

```r
ggplot(insurance, aes(x = age, y = charges)) +
  geom_point(color = "blue") +
  labs(title = "Charges vs Age", x = "Age", y = "Charges")
```

### 3️⃣ Préparation et séparation des données

* Division du dataset en **80% entraînement / 20% test**
* Création des jeux `train` et `test`

### 4️⃣ Modélisation

Trois modèles de régression ont été testés :

```r
model_full <- lm(charges ~ age + sex + bmi + children + smoker + region, data = train)
model_reduced <- lm(charges ~ age + bmi + children + smoker + region, data = train)
model_reduced1 <- lm(charges ~ age + bmi + children + smoker, data = train)
```

➡️ Le modèle `model_reduced1` s’est révélé le plus performant selon le critère **AIC**.

---

## Validation du modèle

### Diagnostic

* Analyse des **résidus** et vérification de la normalité (`shapiro.test`)
* Test d’homoscédasticité (`bptest`)
* Vérification de la colinéarité via le **VIF (Variance Inflation Factor)**
* Analyse des **points leviers** et de la **distance de Cook**

### Validation croisée

Utilisation de la **validation croisée à 10 plis** avec le package `caret` :

```r
train_control <- trainControl(method = "cv", number = 10)
cv_model <- train(charges ~ age + bmi + children + smoker,
                  data = train, method = "lm", trControl = train_control)
```

### Évaluation

Sur le jeu de test :

* Calcul du **MSE** et du **RMSE**

```r
mse <- mean((test$charges - test$predicted)^2)
rmse <- sqrt(mse)
```

---

## Visualisations de performance

* Histogramme des résidus
* Graphique des **charges réelles vs prédites**
* Graphiques des **leviers** et **distances de Cook**

---

## Packages utilisés

| Domaine                    | Packages                       |
| -------------------------- | ------------------------------ |
| Visualisation              | `ggplot2`, `ellipse`           |
| Diagnostic du modèle       | `performance`, `lmtest`, `car` |
| Validation croisée         | `caret`                        |
| Traitement et manipulation | `zoo`, `carData`               |

Installation :

```r
install.packages(c("ggplot2", "performance", "ellipse", "caret", "lmtest", "car", "carData"))
```

---

## Résultats obtenus

* Le modèle simplifié (`model_reduced1`) offre le meilleur compromis entre **complexité** et **performance**.
* Le facteur **smoker (fumeur)** a l’effet le plus significatif sur les charges.
* Le modèle atteint une **bonne précision prédictive** selon les métriques RMSE et R².

