
# Data scientist : Schaphath Kouk*
# Date : 27/10/2025

                              #################################################
                              ##### PREDICTION DU RISQUE CARDIOVASCULAIRE #####
                              #################################################

"
Problématique :

Comment aider les médecins à identifier les cas à risque cardiovasculaire élevé, en réduisant les faux négatifs tout en maîtrisant le coût des examens supplémentaires ?

Objectif analytique :

Développer un modèle permettant de prédire le risque cardiovasculaire à partir d'informations cliniques.

Tâche :

Classification binaire

Caractéristiques :

Age, sexe, taille, poids, cholestérol, glucose, tabac, alcool, activité physique.

Méthodologie :

Nettoyage + recodage cohérent des variables.

modèle de machine learning.

Validation croisée et mesure de performance (Recall, Specificity, Precision, Accuracy).

Détermination du seuil optimal selon les coûts d’erreur.

Critères de succès :

Recall ≥ 90 %

Spécificité ≥ 80 %

AUC ≥ 0.85

Utilisateurs finaux :

Médecins et services de dépistage.

Impact attendu :

Prise en charge plus rapide des cas à risque.

Réduction des examens inutiles pour les patients sains.

Gain de temps pour le corps médical.

"

#------------------------------------------------------------------------------------------#

###--- Chargement des packages  
library(readr)
library(tidyverse)
library(naniar)
library(performance)
library(caret)
library(tidymodels)
library(summarytools)
library(skimr)
library(ggpubr)
library(sjPlot)
library(patchwork)
library(DataExplorer)
library(dlookr)
library(flextable)
library(ggplot2)
library(ggstatsplot)
library(questionr)
library(SmartEDA)



###--- Importation des données 
read_data <- function(path) {
  
  # importation données
  cardio <- read.csv(path, sep = ";") |> select(!id)

  cardio <- cardio |>
    mutate(
      across(c(gender, cholesterol, gluc, smoke, alco, active, cardio), as.factor)
    )

  return(cardio)
}


###--- Lecture data 
cardio <- read_data(path = "data/cardio_train.csv")
cardioCopy <- cardio # copy dataset


###--- Diagnostique data 
diagnose(cardio) |>  flextable()

###--- Graphique des valeurs manquantes
cardio |> gg_miss_var() 

"Note : Toutes des variables numériques. Aucune valeurs manquantes détectée"


#------------------------------------------------------------------------------------------#


########################################
###--- ANALYSE EXPLORATOIRE (EDA) ---###
########################################

###--- Target : table 
freq_tagert <- freq(cardio$cardio, digits = 3) |> as_tibble()
freq_tagert |> flextable()


###--- Target : freqplot 
sjPlot::set_theme(
  base = theme_gray())

plot_target <- plot_frq(cardio$cardio, 
         axis.labels = c("non", "oui"), 
         axis.title = "Présence de maladie cardiovasculaire")

plot_target

"
Note : 
  Catégories équilibrées (il y'a autant de cas positif que négatif).
  Cas négatif (50.03%) et cas positif (49.97%).
"


###--- Description features 

###--- Analyse univariée
stats <- c("mean","sd","se_mean","IQR","skewness","kurtosis", "n")
summary_num_var <- dlookr:: describe(
  cardio,statistics = stats) |> flextable()
summary_num_var

###--- histogrammes 
plot_histogram(cardio)

###--- Diagnostiques features 
diag_num_var <- cardio |> diagnose_numeric() |> flextable()
diag_num_var

###--- Diagnostique outliers 
diag_ouliers <- cardio |> diagnose_outlier() |> flextable()
diag_ouliers

###--- plot outliers 
plot_outlier(cardio)

"
Note :
Les variables ap_hi et ap_lo présentent une grande dispersion autour de la moyenne ainsi que des valeurs suspectes (valeurs négatives). 
Elles présentent également une asymétrie flagrante (skewness > 30 aussi kurtosis > 1000). 
La variable imc présente également une asymértrie non négligeable (skewness > 7 aussi kurtosis > 200). 

On observe aussi des outliers dans toutes nos variables numériques. 
Par exemple, 2000 outliers dans la variable imc et plus 4000 outliers dans la variables ap_lo.
Cependant, les estimations de la moyenne reste stable pour l'ensemble des variables si l'on supprime les outliers. 
Les graphiques des outliers montrent très bien l'effet de la suppression des outliers sur leur distribution. 

Question : On supprime les outliers ou on les impute ? 
Response : Essayons les deux et voyons l'option la plus efficae. 

"

#------------------------------------------------------------------------------------------#

#############################################
###--- OPTION 1 : IMPUTER LES OUTLIERS ---###
#############################################

"
Ici, nous precédons à l'imputation des outliers. Ensuite, nous menons une series d'analyses. 
Nous serons particulièrement attentif sur la précision des estimations en se basant sur les IC à 95%.
Nous réaliserons également des tests statisitques afin de détecter des liaisons entre nos variables.

Pour l'imputation, nous utiliserons les méthodes de médiane et de capping. 

- Imputation par capping : 
  Cette méthode, les valeurs trop basses sont remplacées par un minimum autorisé.
  Et les valeurs trop hautes sont remplacées par un maximum autorisé.

  Son aventage : Préserve la forme générale et la dispersion et moins “brutal” que de remplacer par une seule valeur (médiane).

"

###--- Simulation des imputations des valeurs outliers
"Table cardio_impute"
cardio_impute <- cardio

###--- Fonction pour corriger les outliers 
impute_outliers <- function(data, vars, k = 1.5) {
  
  data_new <- data
  
  for (v in vars) {
    if (!v %in% names(data)) {
      warning(paste("Variable", v, "introuvable dans le dataset — ignorée."))
      next
    }
    
    if (!is.numeric(data[[v]])) {
      warning(paste("Variable", v, "non numérique — ignorée."))
      next
    }
    
    out_idx <- detect_outliers(data[[v]], k)
    
    if (length(out_idx) > 0) {
      Q1 <- quantile(data[[v]], 0.25, na.rm = TRUE)
      Q3 <- quantile(data[[v]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      # Winsorisation : ramener à la limite
      lower <- Q1 - k * IQR
      upper <- Q3 + k * IQR
      
      data_new[[v]][data_new[[v]] < lower] <- lower
      data_new[[v]][data_new[[v]] > upper] <- upper
    }
  }
  
  return(data_new)
}



"Liste variables"
vars_list <- cardio_impute |> keep(is.numeric) |> names()

plot_outliers_all <- function(data, vars, method = "capping") {
  plots <- list()
  for (v in vars) {
    cat("Plotting outliers for:", v, "\n")
    p <- plot(imputate_outlier(data, !!rlang::sym(v), method = method))
    plots[[v]] <- p
  }
  return(plots)
}

"Afficher les simulations "
plot_outliers_all(data = cardio_impute, vars = vars_list)


###--- Enregistrer les imputation des un dataframe 
"Fonction pour imputation des outliers avec la méthode capping"
impute_outliers_all <- function(data, vars, method = "capping", digits = 2) {
  for (v in vars) {
    imp <- imputate_outlier(data, !!rlang::sym(v), method = method, no_attrs = TRUE)
    data <- data |> dplyr::mutate(!!v := round(imp, digits))
  }
  return(data)
}

"Enregistrer les modification dans cardio_impute"
cardio_impute <- impute_outliers_all(data = cardio_impute, vars = vars_list)


###--- Vérification 
diagnose_outlier(cardio_impute) |> flextable()

###--- Nouvelle distribution des features 
plot_histogram(cardio_impute)

###--- Normalité 
plot_normality(cardio_impute)

###--- Test de normalité 
table_normality <- normality(cardio) 
table_normality <- table_normality |>
  mutate(p_value = round(p_value, 3))
table_normality |> flextable()

###--- Création de la variable hypertension 
calcul_hypertension <- function(data,
                                col_systolique = "ap_hi",
                                col_diastolique = "ap_lo",
                                guide = c("ESC", "ACC")) {
  guide <- match.arg(guide)
  
 "Seuils selon le guide choisi"
  if (guide == "ESC") {
    seuil_sbp <- 140
    seuil_dbp <- 90
  } else { # Guide "ACC"
    seuil_sbp <- 130
    seuil_dbp <- 80
  }

  # Calcul des indicateurs
  new_data <- data %>%
    mutate(
      SBP = .data[[col_systolique]],  # Pression systolique
      DBP = .data[[col_diastolique]], # Pression diastolique

      # Variable binaire : 1 si hypertendu, 0 sinon
      hypertendu = (SBP >= seuil_sbp) | (DBP >= seuil_dbp),
      hypertendu = ifelse(hypertendu == TRUE, 1, 0)
    ) %>% select(-SBP, -DBP)  # On retire les colonnes temporaires

  return(new_data)
}

###--- Calcul de la variable hypertension
cardio_impute <- calcul_hypertension(data = cardio_impute, guide = "ESC")
cardio_impute$hypertendu <- as.factor(cardio_impute$hypertendu)



#------------------------------------------------------------------------------------------#

################################
###--- FEATURES VS TARGET ---###
################################

"Question_1 : Est-ce que l'âge permet de différencier les patient à fort risque cardio vasculaire des autres ?"
ggplot(cardio_impute, aes(x = age, color = cardio)) +
  geom_density() +
  facet_wrap(~cardio)

"Note : Le grpahique des densités, montre qu'il pourrait y avoir un lien entre l'âge et le risque cardivasculaire."


"Question_2 : Est-ce que les femmes les plus agées sont plus à risuqe par rapport aux hommes ?"
ggplot(cardio_impute, aes(x = age, color = cardio)) +
  geom_density() +
  facet_wrap(~gender)

"Note : Ici, nous pouvons constater que les femmes âgée de plus de 50 ans présentent un risque plus élévé."


"Question_3 : Est-ce que l'hypertension est-elle liée au risque cardiovasculaire ?"
ggplot(cardio_impute, aes(x = hypertendu, fill = hypertendu)) +
  geom_bar() +
  facet_wrap(~cardio)
 
"Note : Le graphique montre une différence seleon que le patient soir hypertendu ou non."


######################
###--- BOXPLOTS ---###
######################

#--- Variables numériques VS target 
plot_boxplot(cardio_impute, by = "cardio")


################################
###--- TESTS STATISTIQUES ---###
################################

#--- Age VS target  
ggbetweenstats(
  data = cardio_impute, 
  x = cardio, 
  y = age, 
  type = "np"
)


#--- ap_hi VS target  
ggbetweenstats(
  data = cardio_impute, 
  x = cardio, 
  y = ap_hi, 
  type = "np"
)


#--- ap_lo VS target  
ggbetweenstats(
  data = cardio_impute, 
  x = cardio, 
  y = ap_lo, 
  type = "np"
)


###--- height 
ggbetweenstats(
  data = cardio_impute, 
  x = cardio, 
  y = height, 
  type = "np"
)

###--- weight
ggbetweenstats(
  data = cardio_impute, 
  x = cardio, 
  y = weight, 
  type = "np"
)

###--- imc
ggbetweenstats(
  data = cardio_impute, 
  x = cardio, 
  y = imc, 
  type = "np"
)


####################################
###--- ANALYSE DE CORRELATION ---###
####################################

#--- correlation plot
plot_correlate(cardio, method = "spearman")


#------------------------------------------------------------------------------------------#

##############################################
###--- VARIABLES QUALITATIVES VS TARGET ---###
##############################################

#--- Toutes les variables catégorielles
cardio_impute |> 
  select(-cardio) |> 
  plot_bar()

#--- By Target 
plot_bar(cardio_impute, by = "cardio")


#############################
###--- BIVARIATE PLOTS ---###
#############################

ExpCatViz(
  cardio_impute |> 
    select(cardio, cholesterol, gluc, active, hypertendu),
    target = "cardio")



#--- bivariate plots
ExpCatViz(
  cardio_impute |> 
    select(cardio, gender, smoke, alco),
    target = "cardio")



###########################
###--- TESTS DE CHI2 ---###
###########################

#--- Cholesterol VS cardio
ggbarstats(data = cardio_impute, x = cardio, y = cholesterol, 
           label = "both")


#--- Gluc VS target 
ggbarstats(data = cardio_impute , x = cardio, y = gluc, 
           label = "both")


#--- Activ VS target 
ggbarstats(data = cardio_impute, x = cardio, y = active, 
           label = "both")

#--- Activ VS target 
ggbarstats(data = cardio_impute, x = cardio, y = hypertendu, 
           label = "both")


#-------------------------------------------------------------------------------------#

"
CONCLUSION : 

L’objectif de ce projet est de prédire le risque cardiovasculaire chez les nouveaux patients.
L’objectif de cette série d’analyses est de mieux comprendre les variables disponibles. 
Nous avons imputer les valeurs outliers par la méthode capping. 
Les variables imc et hypertendu ont été créées. 

L’analyse s’est déroulée en suivant la nature des variables :

1. Variables quantitatives

Ces variables avaient une distriution iniale très asymétrique à cause des outliers. 
Nous avons donc procédé à une imputation des outliers afin de corriger la distribution de ces variables.
Ensuite, nous avons appliqué des tests non paramétriques pour la comparaison des moyennes (notamment le test de Mann-Whitney).
Les variables quantitatives les plus marquantes sont la pression artérielle systolique et diastolique, 
suivies respectivement par l’âge et l’indice de masse corporelle (IMC).


2. Variables qualitatives

Nous avons crée la variable hypertendu (1 si hypertendu sinon 0). 
Nous avons appliqué le test de chi2 pour les variables catégorielles. 
La variable la plus associée au risque cardiovasculaire est la variable hypertendu et la variable cholestérol. 


Les variables à considérer : 
  - la pression systolique 
  - La pression diastolique, 
  - Age 
  - imc
  - hypertendu
  - cholestérol
  
"

###--- Enregistrer la table cardio_impute
saveRDS(cardio_impute, "cardio_impute.rds")
write.csv(cardio_impute, "cardio_impute.csv", row.names = FALSE)







