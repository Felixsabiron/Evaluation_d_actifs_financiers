# Évaluation d’actifs financiers avec R

🇬🇧 [English version](README.md)

Ce projet a été réalisé dans le cadre du cours **Analyse des actifs financiers** du **Master 1 ECAP**.

Son objectif était d’analyser et de comparer les performances de **10 actifs financiers**, comprenant **9 clubs de football cotés en bourse** ainsi que **Solana (SOL-USD)** utilisé comme actif de référence, à l’aide de méthodes de gestion de portefeuille et d’analyse du risque financier sous **R**.

## Présentation du projet

L’analyse repose sur des données de marché historiques provenant de **Yahoo Finance** sur la période allant du **01/12/2023 au 30/11/2024**.

Le projet porte notamment sur :
- l’analyse de l’évolution des prix
- le calcul des rendements
- l’analyse statistique descriptive des rendements
- l’évaluation du risque et de la volatilité
- l’analyse des covariances et des corrélations
- la mesure de la performance des actifs et des portefeuilles
- la construction et l’optimisation de portefeuilles

## Principales tâches réalisées

### Préparation des données
- Importation et structuration de données provenant de plusieurs fichiers Excel
- Harmonisation des noms de variables entre les différents jeux de données
- Fusion de plusieurs séries temporelles financières dans une base unique
- Traitement des valeurs manquantes et préparation des séries de rendements

### Analyse exploratoire
- Visualisation de l’évolution des prix des actifs
- Visualisation de la dynamique des rendements
- Création de boxplots et d’histogrammes des rendements
- Construction d’indices de prix normalisés afin de comparer les trajectoires des actifs

### Analyse statistique
- Calcul de :
  - rendement moyen
  - variance
  - écart-type
  - asymétrie
  - kurtosis
- Estimation des matrices de covariance et de corrélation entre les actifs

### Mesures de performance et de risque
Le projet mobilise plusieurs indicateurs de performance et de risque, notamment :
- Ratio de Sharpe
- Ratio de Sortino
- Ratio de Treynor
- Alpha de Jensen
- Coefficient de variation
- Ratio d’information

### Analyse de portefeuille
- Construction d’un portefeuille équipondéré
- Comparaison des actifs selon une approche **risque-rendement**
- Construction d’une **frontière efficiente** pour une sélection d’actifs
- Calcul de :
  - portefeuille de variance minimale
  - portefeuille tangent

## Outils et technologies
- **R**
- **data.table**
- **dplyr**
- **ggplot2**
- **PerformanceAnalytics**
- **FactoMineR**
- **corrplot**
- **readxl**
- **xts**
- **fPortfolio**

## Compétences mobilisées
- Nettoyage et transformation de données financières
- Manipulation de séries temporelles sous R
- Analyse statistique des rendements financiers
- Évaluation du risque et de la performance
- Visualisation de données
- Construction et optimisation de portefeuilles

## Structure du dépôt

- `Code_du_Projet.R` — script R principal contenant l’ensemble de l’analyse
- `README.md` — présentation du projet en anglais
- `README_FR.md` — présentation du projet en français
- `Evaluation_Actifs_Financiers.Rproj` — fichier du projet R

## Rapport académique

Le rapport académique complet est disponible en **français**.

**Accéder au rapport complet :** [Cliquer ici](https://drive.google.com/drive/u/0/folders/11D0kcX7RXt6IrpOiD8jGu5PV2u3A5Ilp)

## Remarque

Ce dépôt présente de manière synthétique la méthodologie, les analyses réalisées et les principales techniques mobilisées dans le cadre du projet.
