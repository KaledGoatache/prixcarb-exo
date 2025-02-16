library(tidyverse)
library(readxl)
library(dplyr)
setwd("/Users/kaledgoatache/Desktop/a programming/r/TD ECON R/")

prixcarb = read_xlsx("PrixCarburants.xlsx")
View(prixcarb)

prixcarb = prixcarb |> rename(id = Identifiant) 
prixcarb = prixcarb |> rename(Autom = Automate24) 

names(prixcarb)
# On utilise subset pour creer un sous ensemble avec lequel on ecrase la bdd d'origine 
# c c'est pour definir des ensembles, ou -c pour deselectionner des variables.

prixcarb = subset(prixcarb, select = c(id, Autom, Marque, SP95_E10, SP98, Gazole))
prixcarb$id = 1:nrow(prixcarb)

# Utilisation de print() pour voir la bdd sur la console ou bien head(n=#, bdd) pour afficher les # premieres obsv
prixcarb$Autom[is.na(prixcarb$Autom)] = "Non"

# EXO 2
prixcarb$Marque = gsub("Ã¨", "è", prixcarb$Marque)
prixcarb$Marque = gsub("Ã©", "é", prixcarb$Marque)

summary(prixcarb)

prixcarb$SP95_E10 = as.numeric(prixcarb$SP95_E10)
prixcarb$SP98 = as.numeric(prixcarb$SP98)
prixcarb$Gazole = as.numeric(prixcarb$Gazole)

# Sur Summary pour Gazole, la Mediane explique que 50% des "stations" vendent à ce prix affiché ou moins, 
# de même 3eme quartile explique que 75% des stations vendent au prix affiché ou moins. En se basant tjs dans 
# des probabilté après l'analyse de notre base de données.

#install.packages("psych")
library(psych)
describe(prixcarb)

# Calculer la correlation
(corr = cor(prixcarb$Gazole, prixcarb$SP98)) # Avec un coeff de 0,96 il y a forte correlation = les prix des deux augmentent simultanément
# Correlation simultanée plusieurs variables
(corr1 = cor(prixcarb[,c("Gazole", "SP98", "SP95_E10")]))

#Devoir semaine pro -> Finir EXO 4

# Exo 4 Graphique 1 ID - GAZOLE, plot ()
plot(prixcarb$id, prixcarb$Gazole, col = "blue", type = "b", main = "Prix du Gazole dans les différentes stations-services de
Tours", xlab = "Station-service par id", ylab = "Prix du Gazole")

# Graphique 2 Prix gazole/ Prix SP98
# Les prix sont corrélés à la station.
plot(prixcarb$id, prixcarb$SP98, col = "red", type = "l", lty = 1, 
     main = "Prix du Gazole et du SP98 dans les stations de Tours", 
     xlab = "Station-service par id", ylab = "Prix du carburant")

lines(prixcarb$id, prixcarb$Gazole, col = "green", type = "l", lty = 2)
legend("topleft", legend = c("Gazole", "SP98"), col = c("green", "red"), lty = c(2, 1))

# Mettre autom en tant que variable dichotomique
prixcarb$Autom <- ifelse(prixcarb$Autom == "Oui", 1, 0)

# Mettre les marques en tant que variables dichotomiques (si on a marque alors 1, else 0)
prixcarb$Marque <- ifelse(is.na(prixcarb$Marque), 0, 1)

(t_test <- t.test(prixcarb$Gazole ~ prixcarb$Marque))
# La p-value est la probabilité qu'on rejette l'hypothese nule à tort
# avec une p-value de 0.883 > 0.10 on est obligé d'accepter l'hypothèse nulle
## Conclusion: test de comparaison de moyenne -> la difference de  prix dans notre bdd ne s'explique pas par le fait
## que la même soit rattaché à une marque.

# MDL Modèle de probabilité linéaire -> MCO
# Trouver hypothése naïves- connaissances theoriques/domaines, resultats statistiques

MPL <- lm(Gazole ~ Autom + Marque, data=prixcarb)
print(MPL) #on aura plus avec summary()
# F-statistic -> Significativité globale du modèle
# Avec un p-value de 0,5009 -> notre modèle n'est pas significative
#install.packages("margins")
library(margins)
EM <- margins(MPL) # summary(EM)
## Le fait d'avoir un automate dans notre station de service ceteris paribus, augmente le prix du Gazole de 4 centimes
## cependant cet effet n'est pas signi avec une p-value de 0.2378
# Le fait qu'une station de service, ceteris paribus, affecte de +1 centime le pri du gazole, cependant le p-value est de 0,67
# alors l'effet n'est pas significatif

# On représente graphiquemetn les effets marginau obtenus. Par defaut le risque des IC = 95%
plot(EM)
# on remarque les deux intervales de confiance ->  qui passent par zero (0) c'est qui renvoie que nos variables sont pas significatives