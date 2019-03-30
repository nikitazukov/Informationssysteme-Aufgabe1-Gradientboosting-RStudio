#KOMMENTARE
# CTRL+SHIFT+C -> Kommentieren
#
#KONSOLE CLEAR
# CTRL+L
#
#VARIABLEN LÖSCHEN
# rm()

#PACKAGES
install.packages('tidyverse')
install.packages('ISLR')
install.packages('SDMTools')
install.packages('car')
install.packages('xgboost')
install.packages('GGally')

#REQUIRES
require('tidyverse')
require('ISLR')
require('SDMTools')
require('car')
require('xgboost')
require('GGally')

#LIBRARIES
library('tidyverse')
library('ISLR')
library('SDMTools')
library('car')
library('xgboost')
library('GGally')

####################################################################################################################################### Aufgabe 1.a
cc <- read.csv("C:/Users/Nikita/Desktop/Informationssysteme/creditCards.csv")
summary(cc)

# default    isStudent     balance           income     
# No :9667   No :7056   Min.   :   0.0   Min.   :  772  
# Yes: 333   Yes:2944   1st Qu.: 481.7   1st Qu.:21340  
#                       Median : 823.6   Median :34553  
#                       Mean   : 835.4   Mean   :33517  
#                       3rd Qu.:1166.3   3rd Qu.:43808  
#                       Max.   :2654.3   Max.   :73554 


cc.Student <- cc %>%
  filter(isStudent=="Yes")
summary(cc.Student)

# default    isStudent     balance           income     
# No :2817   No :   0   	Min.   :   0.0   Min.   :  772  
# Yes: 127   Yes:2944   	1st Qu.: 655.6   1st Qu.:14887  
#                         Median : 980.0   Median :17994  
#                         Mean   : 987.8   Mean   :17950  
#                         3rd Qu.:1303.9   3rd Qu.:20986  
#                         Max.   :2654.3   Max.   :33003 

cc.Arbeiter <- cc %>%
  filter(isStudent=="No")
summary(cc.Arbeiter)

# default    isStudent     balance           income     
# No :6850   No :7056   	Min.   :   0.0   Min.   : 8018  
# Yes: 206   Yes:   0   	1st Qu.: 418.2   1st Qu.:33417  
#                         Median : 759.2   Median :39893  
#                         Mean   : 771.8   Mean   :40012  
#                         3rd Qu.:1093.3   3rd Qu.:46841  
#                         Max.   :2499.0   Max.   :73554 

# Balance: Studenten im Schnitt höher
# Zahlungsverzug: Studenten 127 - Arbeiter 206


####################################################################################################################################### Aufgabe 1.b
# Zusammenhänge: Arbeiter - Studenten - Balance - Einkommen

summary(cc.Arbeiter)
# default    isStudent     balance           income     
# No :6850   No :7056   	Min.   :   0.0   Min.   : 8018  
# Yes: 206   Yes:   0   	1st Qu.: 418.2   1st Qu.:33417  
#                         Median : 759.2   Median :39893  
#                         Mean   : 771.8   Mean   :40012  
#                         3rd Qu.:1093.3   3rd Qu.:46841  
#                         Max.   :2499.0   Max.   :73554 

summary(cc.Student)

# default    isStudent     balance           income     
# No :2817   No :   0   	Min.   :   0.0   Min.   :  772  
# Yes: 127   Yes:2944   	1st Qu.: 655.6   1st Qu.:14887  
#                         Median : 980.0   Median :17994  
#                         Mean   : 987.8   Mean   :17950  
#                         3rd Qu.:1303.9   3rd Qu.:20986  
#                         Max.   :2654.3   Max.   :33003 

# Einkommen: 772 - 8018 ==> evt. irrelevant
# Teilnehmer: 1/3 Studenten - 2/3 Arbeiter ==> Zahlungsverzug: 1/3 Studenten - 2/3 Arbeiter ==> ca. ausgewogen ==> isStudent evtl. irrelevant


summary(cc.Arbeiter.Verschuldet)
# default   isStudent    balance           income     
# No :  0   No :206   Min.   : 652.4   Min.   :14664  
# Yes:206   Yes:  0   1st Qu.:1480.7   1st Qu.:34722  <== 1st Q. 1480.7
#                     Median :1709.9   Median :40281  
#                     Mean   :1678.4   Mean   :40625  
#                     3rd Qu.:1912.4   3rd Qu.:48872  
#                     Max.   :2499.0   Max.   :66466

summary(cc.Arbeiter.NichtVerschuldet)
# default    isStudent     balance           income     
# No :6850   No :6850   Min.   :   0.0   Min.   : 8018  
# Yes:   0   Yes:   0   1st Qu.: 407.8   1st Qu.:33396  
#                       Median : 742.1   Median :39886  
#                       Mean   : 744.5   Mean   :39994  
#                       3rd Qu.:1061.2   3rd Qu.:46779  <== 3rd Q. 1061.2 SKEW
#                       Max.   :2391.0   Max.   :73554


ggplot(data=cc)+geom_point(mapping = aes(x=default, y=income))

ggplot(data=cc)+geom_point(mapping = aes(x=default, y=balance))

ggplot(data=cc)+geom_point(mapping = aes(x=income, y=balance))

ggplot(data=cc)+geom_point(mapping = aes(x=balance, y=income))
ggplot(data=cc)+geom_boxplot(mapping = aes(x=default, y=balance))

# ==> idee für aufgabe 2


####################################################################################################################################### Aufgabe 2.a
#Grundidee: wir gehen davon aus, dass alle Teilnehmer nicht in Zahlungsverzug sind.


####################################################################################################################################### Aufgabe 2.b
#Grundidee: Wir sehen uns an, wer von den Teilnehmer in Zahlungsverzug geraten ist und berechnen davon den Prozentsatz.
summary(cc)
# default    isStudent     balance           income     
# No :9667   No :7056   Min.   :   0.0   Min.   :  772  
# Yes: 333   Yes:2944   1st Qu.: 481.7   1st Qu.:21340  
#                       Median : 823.6   Median :34553  
#                       Mean   : 835.4   Mean   :33517  
#                       3rd Qu.:1166.3   3rd Qu.:43808  
#                       Max.   :2654.3   Max.   :73554 

# Von 10000 sind 333 in Zahlungsverzug <- Fehlermaß


####################################################################################################################################### Aufgabe 2.c
#Grundidee: %Zahl der in Zahlungsverzug geratenen herausfinden

# 100 / Gesamtteilnehmer *  Teilnehmer im Zahlungsverzug = Fehlermaß in %

#100/ 10000 * 333 = 3.33 % 


#####################################################################    ALTERNATIVLÖSUNG 

####################################################################################################################################### Aufgabe 2.a - Ersatz
# Die Grundidee: Betrachten wir die vorhergegangene Aufgabe sind wir von y = ax +b ausgegangen. Da wir bestimmen wollen ob ein Teilnehmer in Zahlungsverzug kommt oder nicht, 
# ist unser Model ein Yes/No Model. Das bedeutet, es gibt keine Steigung wodurch mx wegfällt. Daher der Erstansatz y = b.
# 
summary(cc.Verschuldet)
# default   isStudent    balance           income     
# No :  0   No :206   Min.   : 652.4   Min.   : 9664  
# Yes:333   Yes:127   1st Qu.:1511.6   1st Qu.:19028  
#                     Median :1789.1   Median :31515  
#                     Mean   :1747.8   Mean   :32089  
#                     3rd Qu.:1988.9   3rd Qu.:43067  
#                     Max.   :2654.3   Max.   :66466 
# 
# Nach unserem Ansatz, nehmen wir den Mittelwert der Verschuldeten Teilnehmer (1747.8) und nehmen an, dass alle Teilnehmer mit einem niedrigeren Verschuldungsgrad wahrscheinlich
# nicht in Verzug kommen, und alle Teilnehmer mit einem höheren Grad, wahrscheinlich in den Verzug geraten werden.

####################################################################################################################################### Aufgabe 2.b - Ersatz
# Die Grundidee: Wir nehmen den balance-Mittelwert aus 2.1 und sagen, alle die drunter liegen sollten eigentlich nicht Verschuldet sein (richtig), jene die
# verschuldet sind, bilden einen Teil des Fehlermaßes. Die Teilnehmer die drüber liegen sollten eigentlich verschuldet sein (richtig). Sollten sie drunter 
# liegen, bilden sie den anderen Teil des Fehlermaßes.
# Wir nehmen Teil 1 und 2 zusammen und erhalten unseren komplettes Fehlermaß.

# Fürs bessere Verständnis siehe Skizze 2.2.

####################################################################################################################################### Aufgabe 2.c - Ersatz
# Die Grundidee: wir werden den angesprochenen Prozess auf die vorhandenen Daten an und schauen uns das Ergebnis an.
A.2.3.UnterMittelwert.Verschuldet <- cc.Verschuldet %>%
  filter(balance<=1747.8)
summary(A.2.3.UnterMittelwert.Verschuldet)

# default   isStudent    balance           income     
# No :  0   No :111   Min.   : 652.4   Min.   :10155  
# Yes:154   Yes: 43   1st Qu.:1329.8   1st Qu.:21591  
#                     Median :1503.2   Median :35641  
#                     Mean   :1452.8   Mean   :33611  
#                     3rd Qu.:1622.3   3rd Qu.:43744  
#                     Max.   :1743.1   Max.   :66466

# YES = 154
# 

A.2.3.UeberMittelwert.NichtVerschuldet <- cc.NichtVerschuldet %>%
   filter(balance>=1747.8)
summary(A.2.3.UeberMittelwert.NichtVerschuldet)

# default   isStudent    balance         income     
# No :180   No : 62   Min.   :1749   Min.   : 6467  
# Yes:  0   Yes:118   1st Qu.:1794   1st Qu.:17078  
#                     Median :1837   Median :21027  
#                     Mean   :1879   Mean   :25089  
#                     3rd Qu.:1931   3rd Qu.:31974  
#                     Max.   :2391   Max.   :58711

# NO = 180
# 
#
# FehlerIn% = 100 / Teilnehmerzahl * (Yes + No)
#  3.34 % = 100 / 10000 * (154+180)


####################################################################################################################################### Aufgabe 3.a

glm.fit <- glm(default ~ . , data = cc, family = binomial)
summary(glm.fit)

# Call:
#   glm(formula = default ~ ., family = binomial, data = cc)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.4691  -0.1418  -0.0557  -0.0203   3.7383  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -1.087e+01  4.923e-01 -22.080  < 2e-16 ***
#   isStudentYes -6.468e-01  2.363e-01  -2.738  0.00619 ** 
#   balance       5.737e-03  2.319e-04  24.738  < 2e-16 ***
#   income        3.033e-06  8.203e-06   0.370  0.71152    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2920.6  on 9999  degrees of freedom
# Residual deviance: 1571.5  on 9996  degrees of freedom
# AIC: 1579.5
# 
# Number of Fisher Scoring iterations: 8



Kommi # Wie man sehen kann ist die Null deviance: 2920.6 (Abweichung zum Mittelwert, gebildet aus allen Werten)
      # und die Residual deviance: 1571.5 (Abweichung zur Summe innerhalb eines Wertes, angewendet bei allen Werten)

Kommi # Ermittlung der Prognose auf die gesamten Daltensätze 

#Schritt 1: Bestimmung der Wahrschinlichkeit des gesamten Datensatzes

#Predict gibt die Prozentuelle-Wahrscheinlichkeit an, mit welcher sich der Teilnehmer in Zahlungsverzug begeben könnte
glm.probs <- predict(glm.fit, type ="response")
summary(glm.probs)
View(glm.probs)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0000103 0.0002798 0.0019662 0.0333000 0.0132244 0.9776263

#Schritt 2: Ermittlung der Row-Nummern von Default die "Yes" betragen

#Rownummer wird extrahiert für alle Defaultwerte == "Yes" -> nötig um die Rows mit den notwendigen Wahrscheinlichkeitswerten rauszusuchen
RowNummern.Default.Yes <- which(cc$default=="Yes")

#Schritt 3: Ermittlung der Wahrscheinlichkeitswerte des Zahlungsverzugs von Teilnehmern, die bereits in Zahlungsverzug geraten sind

#Extrahieren alle Wahrscheinlichkeitswerte von Teilnehmern die bereits in Zahlungsverzug sind, anhand der Rowwerte von Default die auf "Yes" waren
Zahlungsverzug.Wahrscheinlichkeiten <- glm.probs[RowNummern.Default.Yes]


#Schritt 4: Vergleich der Wahrscheinlichkeitswerte des gesamten Datensatze mit den Wahrscheinlichkeitswerten der Teilnehmer, die bereits im Zahlungsverzug sind

summary(Zahlungsverzug.Wahrscheinlichkeiten)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0008547 0.0879355 0.3071974 0.3479540 0.5708448 0.9810081 
# => Zum vergleich nehmen wir den Mean-Wert (0.3479540)  der Wahrscheinlichkeiten und wenden ihn auf den gesamten Datensatz an
# => Das bedeutet: Jeder der eine Wahrscheinlichkeit über 34.79% aufweist, wird wahrscheinlich in den Zahlungsverzug geraten

#Bestimmung des Wahrscheinlichkeitswertes
Zahlungsverzug.Wahrscheinlichkeiten.Mean <- mean(Zahlungsverzug.Wahrscheinlichkeiten)

#Der Vergleich
glm.pred <- ifelse(glm.probs >= Zahlungsverzug.Wahrscheinlichkeiten.Mean , "Yes" , "NO")
#=> Datensätze ausgewertet mit Yes und No ob der Teilnehmer in Zahlungsverzug kommt oder nicht


#Anzeige Aller Daten die in Zahlungsverzug geraten könnten, laut dem Generalized Linear Model
anzahlDatensaetze <- 10000
zaehler <- 0
for (i in 1:anzahlDatensaetze) {
  
  if (glm.pred[i] == "Yes") {
    print(i)
    zaehler <- zaehler + 1
  }
}


print(zaehler)
#=> 250
#=> Im Hinblick auf Aufgabe 2 (Fehler bei 3,33%), liegt der Fehler mit dem glm Verfahren bei ca. 2,5 %


####################################################################################################################################### Aufgabe 3.b
#Bestimmung 
#Test und Trainingsdaten erzeugen
n <- cc.InZahlen %>% nrow
indexes<-sample(n, n*0.8)
cc.train <- cc.InZahlen[indexes,]
cc.test <- cc.InZahlen[-indexes,]


cc.InZahlen <- cc

cc.InZahlen$default <- recode(cc.InZahlen$default, "'Yes'=1")
cc.InZahlen$default <- recode(cc.InZahlen$default, "'No'=0")

cc.InZahlen$default <- as.numeric(cc.InZahlen$default)

View(cc.InZahlen)

cc.InZahlen$default <- recode(cc.InZahlen$default, "2=1")
cc.InZahlen$default <- recode(cc.InZahlen$default, "1=0")


# probs.genauigkeit <- confusion.matrix( cc.InZahlen$default , glm.probs)
# probs.genauigkeit




# generalized linear model & Wahrscheinlichkeiten für Trainingsdaten erzeugen
train.glm.fit <- glm(default ~ . , data = cc.train, family = binomial)
summary(train.glm.fit)

train.glm.probs <- predict(train.glm.fit, type ="response")
summary(train.glm.probs)





# generalized linear model & Wahrscheinlichkeiten für testdaten erzeugen
test.glm.fit <- glm(default ~ . , data = cc.test, family = binomial)
summary(test.glm.fit)

test.glm.probs <- predict(test.glm.fit, type ="response")
summary(test.glm.probs)
   

#Confusionmatrix für testdaten erzeugen
confusion.matrix(cc.test$default, test.glm.probs)
#    obs
# pred    0  1
#     0 1920 64
#     1   16  0

# 100 / 2000 * (64+16) = 4%
# => Unser Fehler von 4% Hat sich im Vergleich (3.33%) um 0.67% erhöht.


####################################################################################################################################### Aufgabe 3.c
# Das Generalized Linear Model bietet uns Methoden an, um die Wahrscheinlichkeit zum Zahlungsverzug zu errechnen und anhand eines möglichst realistischen 
# Schwellwertes zu bestimmen wer in Zahlungsverzug geraten wird und wer sich darin befindet. Im gegensatz zu unserem ersten Modell, dass uns nur den 
# Fehlerwert liefern kann, jedoch ohne Vorhersagen für die anderen Teilnehmer. 



####################################################################################################################################### Aufgabe 4

#model mit trainingsdaten füttern
training.matrix <- Matrix::sparse.model.matrix(default ~ . -1, data = cc.train)
training.label <- cc.train$default
training.model <- xgboost(data = training.matrix, label=training.label, nround = 1000, Objective = "reg:")

#Fehler : 0.002677

rmse <- function(prediction, actual){
  (prediction - actual)^2 %>%
    mean %>%
      sqrt %>%
        return
}


#trainingsdatenmodel auf die testdaten anwenden
test.matrix <- Matrix::sparse.model.matrix(default ~ . -1 , data = cc.test) 
  training.model %>% predict(test.matrix) %>% rmse(cc.test$default)


#Fehler : 0.1811998
  
  
  
####################################################################################################################################### Aufgabe 5
  
  summary(glm.fit)
  
  # Call:
  #   glm(formula = default ~ ., family = binomial, data = cc)
  # 
  # Deviance Residuals: 
  #   Min       1Q   Median       3Q      Max  
  # -2.4691  -0.1418  -0.0557  -0.0203   3.7383  
  # 
  # Coefficients:
  #   Estimate Std. Error z value Pr(>|z|)    
  # (Intercept)  -1.087e+01  4.923e-01 -22.080  < 2e-16 ***
  #   isStudentYes -6.468e-01  2.363e-01  -2.738  0.00619 ** 
  #   balance       5.737e-03  2.319e-04  24.738  < 2e-16 ***
  #   income        3.033e-06  8.203e-06   0.370  0.71152    
  # ---
  #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  # 
  # (Dispersion parameter for binomial family taken to be 1)
  # 
  # Null deviance: 2920.6  on 9999  degrees of freedom
  # Residual deviance: 1571.5  on 9996  degrees of freedom
  # AIC: 1579.5
  # 
  # Number of Fisher Scoring iterations: 8
  
  # ==> Signif. codes: ** 
  # ==> Verschuldet: 1/3 Student - 2/3 Arbeiter ==> Zahlungsverzug: Student 1/3 - Arbeiter 2/3
  
  summary(cc.Balance)
  
  cc.Balance.Q3 <- cc %>%
    filter(balance>= 1166.3)
  summary(cc.Balance.Q3)
  # student => no: 1460 yes: 1040 
 
   
  cc.Balance.Q3.Q3 <- cc %>%
    filter(balance>= 1604)
  summary(cc.Balance.Q3.Q3)
  # student => no: 318 yes: 307 
  
  
  
  cc.Balance.Q3.Q3.Q3 <- cc %>%
    filter(balance>= 1923)
  summary(cc.Balance.Q3.Q3.Q3)
  # student => no: 63 yes: 94 
  
  summary(cc.Student.Verschuldet)
  #student: 127 - 333 ==> 38.13
  

   ausg<- function(b){ cc.Balance.Studentverteilung.Mittelwert <- cc %>%
      filter(balance>=b)
    # anzeige <- summary(cc.Balance.Studentverteilung.Mittelwert)
    
    isStudent_yes<-cc.Balance.Studentverteilung.Mittelwert%>%
      filter(isStudent=="Yes")
    rowanzahl_yes = nrow(isStudent_yes)
    
    isStudent_no<-cc.Balance.Studentverteilung.Mittelwert%>%
      filter(isStudent=="No")
    rowanzahl_no = nrow(isStudent_no)
    
    proz <- 100/ (rowanzahl_yes+rowanzahl_no) * rowanzahl_yes
    
    return(proz)
   }
   
   
   anfang <- 1500
   ende<-2400
   prozentzahlen<-0
  
     
     while (anfang < ende ) {
       
       anfang <-anfang+1
      prozentzahlen<- c(prozentzahlen, ausg(anfang))
      prozentzahlen
    
       
   }
prozentzahlen

