### Projet R 

#Importation des librairies : 

library(fPortfolio)
library(ggplot2) 
library(scales)
library(ggrepel)
library(extrafont);loadfonts() 
library(corrplot)
library(data.table)
library(dplyr)
library(pspearman)
library(PerformanceAnalytics)
library(car)
library(EnvStats)
library(factoextra)
library(FactoMineR)
library(gridExtra)
library(lmtest)
library(AER)
library(tseries)
library(FinCal)
library(readxl)
library(tidyr)
library(naniar)
library(gridExtra)


#Importation des 10 bases de données

set.seed(19)

JUVE_MI <- data.table(read_excel("~/Documents/S1 ecap/Evaluation des actifs financiers/Dossier/Données/base_1.xlsx"),ticker="JUVE_MI")
SSL_MI <- data.table(read_excel("~/Documents/S1 ecap/Evaluation des actifs financiers/Dossier/Données/base_2.xlsx"),ticker="SSL_MI")
XXT_SG <- data.table(read_excel("~/Documents/S1 ecap/Evaluation des actifs financiers/Dossier/Données/base_3.xlsx"),ticker="XXT_SG")
CCP_L <- data.table(read_excel("~/Documents/S1 ecap/Evaluation des actifs financiers/Dossier/Données/base_4.xlsx"),ticker="CCP_L")
BVB_MU <- data.table(read_excel("~/Documents/S1 ecap/Evaluation des actifs financiers/Dossier/Données/base_5.xlsx"),ticker="BVB_MU")
SCP_LS <- data.table(read_excel("~/Documents/S1 ecap/Evaluation des actifs financiers/Dossier/Données/base_6.xlsx"),ticker="SCP_LS")
AAB_CO <- data.table(read_excel("~/Documents/S1 ecap/Evaluation des actifs financiers/Dossier/Données/base_7.xlsx"),ticker="AAB_CO")
GSRAY_IS <- data.table(read_excel("~/Documents/S1 ecap/Evaluation des actifs financiers/Dossier/Données/base_8.xlsx"),ticker="GSRAY_IS")
AJAX_AS <- data.table(read_excel("~/Documents/S1 ecap/Evaluation des actifs financiers/Dossier/Données/base_9.xlsx"),ticker="AJAX_AS")
SOL_USD   <- data.table(read_excel("~/Documents/S1 ecap/Evaluation des actifs financiers/Dossier/Données/base_10.xlsx"),ticker="SOL_USD")


#Visualisation des différentes bases
View(JUVE_MI)
View(SSL_MI)
View(XXT_SG)
View(CCP_L)
View(BVB_MU)
View(SCP_LS)
View(AAB_CO)
View(GSRAY_IS) 
View(AJAX_AS)               
View(SOL_USD)               

#Vérification
str(JUVE_MI)
str(SSL_MI)
str(XXT_SG)
str(CCP_L)
str(BVB_MU)
str(SCP_LS)
str(AAB_CO)
str(GSRAY_IS)
str(AJAX_AS)               
str(SOL_USD)   

# On renomme les colonnes pour qu'elles portent toutes le mm nom
colnames(JUVE_MI) <- c("Date", "Open", "High","Low","Close","Volume","Adjusted","ticker")
colnames(SSL_MI) <-  c("Date", "Open", "High","Low","Close","Volume","Adjusted","ticker")
colnames(XXT_SG) <-  c("Date", "Open", "High","Low","Close","Volume","Adjusted","ticker")
colnames(CCP_L) <-  c("Date", "Open", "High","Low","Close","Volume","Adjusted","ticker")
colnames(BVB_MU) <-  c("Date", "Open", "High","Low","Close","Volume","Adjusted","ticker")
colnames(SCP_LS) <-  c("Date", "Open", "High","Low","Close","Volume","Adjusted","ticker")
colnames(AAB_CO) <-  c("Date", "Open", "High","Low","Close","Volume","Adjusted","ticker")
colnames(GSRAY_IS) <- c("Date", "Open", "High","Low","Close","Volume","Adjusted","ticker")
colnames(AJAX_AS) <-  c("Date", "Open", "High","Low","Close","Volume","Adjusted","ticker")
colnames(SOL_USD) <-  c("Date", "Open", "High","Low","Close","Volume","Adjusted","ticker")

# On regroupe les bases entre elles 
base <- rbind(JUVE_MI, SSL_MI, XXT_SG, CCP_L, BVB_MU, SCP_LS, AAB_CO, GSRAY_IS, AJAX_AS,SOL_USD)
str(base)
View(base)

# On calcule le rendement de chaque actif 

JUVE_MI[, rendements := Adjusted / shift(Adjusted, 1) - 1]
SSL_MI[, rendements := Adjusted / shift(Adjusted, 1) - 1]
XXT_SG[, rendements := Adjusted / shift(Adjusted, 1) - 1]
CCP_L[, rendements := Adjusted / shift(Adjusted, 1) - 1]
BVB_MU[, rendements := Adjusted / shift(Adjusted, 1) - 1]
SCP_LS[, rendements := Adjusted / shift(Adjusted, 1) - 1]
AAB_CO[, rendements := Adjusted / shift(Adjusted, 1) - 1]
GSRAY_IS[, rendements := Adjusted / shift(Adjusted, 1) - 1]
AJAX_AS[, rendements := Adjusted / shift(Adjusted, 1) - 1]
SOL_USD[, rendements := Adjusted / shift(Adjusted, 1) - 1]


#-------------------- Exercice 1 --------------------


#---------- Question 1 ----------

# Adj.Close = prix sans biais des dividendes

#---- 1.1 prix ---- black grey3

 
 # Vecteur des diff fonds
 vecteur_colors <- c(
  "JUVE_MI" = "blue",
  "SSL_MI" = "red",
  "XXT_SG" = "green",
  "CCP_L" = "orange",
  "BVB_MU" = "purple",
  "SCP_LS" = "pink",
  "AAB_CO" = "cyan",
  "GSRAY_IS" = "grey3",
  "AJAX_AS" = "brown",  
  "SOL_USD" = "black"
 )
 head(base$ticker)
 str(base)
 # Ici il y l'évolution de tt les titres ds le mm grah
 
 ggplot(base, 
        aes(x = Date, y = Adjusted, color = ticker)) +
  geom_line() +
  ggtitle("Evolution des prix") +
  scale_color_manual(
   values = vecteur_colors,
   name = "Différents fonds") +
  labs(
   title = "Évolution des prix des actifs", 
   x = "Date",
   y = "Cours de l'actif",
   caption = "Data source: Yahoo Finance",
   subtitle = "du 01/12/23 au 30/11/24") +
  theme_classic()+
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 9),
   plot.caption = element_text(color = "azure4", face ="plain")
  )
 
 # mm graph sans SOL_USD et sans CCP_L
 
 base_filtered <- base %>%
  filter(!ticker %in% c("SOL_USD", "CCP_L","AAB_CO"))
 
 ggplot(base_filtered, 
        aes(x = Date, y = Adjusted, color = ticker)) +
  geom_line() +
  ggtitle("Évolution des prix des actifs") +
  scale_color_manual(
   values = vecteur_colors,
   name = "Différents fonds") +
  labs(
   title = "Évolution des prix des actifs", 
   x = "Date",
   y = "Cours de l'actif",
   caption = "Source : Yahoo Finance",
   subtitle = "du 01/12/23 au 30/11/24") +
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 9),
   plot.caption = element_text(color = "azure4", face = "plain")
  )
 
 # Le graph de chaque titre affiché ensemble  
 ggplot(base, aes(x = Date, y = Adjusted, color = ticker)) +
  geom_line() +
  ggtitle("Evolution des prix") +
  scale_color_manual(
   values = vecteur_colors,
   name = "Différents fonds"
  ) +
  labs(
   title = "Évolution des prix des actifs", 
   x = "Date",
   y = "Cours de l'actif",
   caption = "Data source: Yahoo Finance",
   subtitle = "du 01/12/23 au 30/11/24"
  ) +
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 9),
   plot.caption = element_text(color = "azure4", face = "plain")
  ) +
  facet_wrap(~ ticker, scales = "free_y")  # Crée un graphique par ticker
 
 ## Evolution des prix et des rendements de chaque actif individuellement
 
 # JUVE_MI 
 
 plot_prix_JUVE_MI <- ggplot(JUVE_MI, aes(x = Date, y = Adjusted, color = "JUVE_MI")) +
  geom_line(linewidth = 1.2) +  # Utilisation de linewidth au lieu de size
  labs( 
   title = "Évolution du prix de l'actif JUVE_MI",  
   subtitle = "du 01/12/23 au 30/11/24",
   x = "Date",
   y = "Cours de l'actif",
   color = "Actif", # Titre de la légende
   caption = "Source : Yahoo Finance"
  ) +
  scale_color_manual(values = c("JUVE_MI" = "darkgreen")) + 
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 10),
   plot.caption = element_text(color = "black", face = "italic"),
   legend.title = element_text(size = 12, face = "bold"),
   legend.text = element_text(size = 10)
  )
 
 plot_rdmt_JUVE_MI <- ggplot(JUVE_MI, aes(x = Date, y = rendements, color = "JUVE_MI")) +
  geom_line(linewidth = 0.5) +  # Utilisation de linewidth au lieu de size
  geom_hline(aes(yintercept = mean(JUVE_MI$rendements, na.rm = TRUE)), color = "black", linetype = "dashed")+
  labs( 
   title = "Évolution du rendement de l'actif JUVE_MI",  
   subtitle = "du 01/12/23 au 30/11/24",
   x = "Date",
   y = "Cours de l'actif",
   color = "Actif", # Titre de la légende
   caption = "Source : Yahoo Finance"
  ) +
  scale_color_manual(values = c("JUVE_MI" = "darkgreen")) + 
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 10),
   plot.caption = element_text(color = "black", face = "italic"),
   legend.title = element_text(size = 12, face = "bold"),
   legend.text = element_text(size = 10)
  )
 
 # Prix et rendement de l'actif JUVE_MI
 grid.arrange(plot_rdmt_JUVE_MI, plot_prix_JUVE_MI, ncol = 2)
 
 # SSL_MI 
 
 plot_prix_SSL_MI <- ggplot(SSL_MI, aes(x = Date, y = Adjusted, color = "SSL_MI")) +
  geom_line(linewidth = 1.2) +  # Utilisation de linewidth au lieu de size
  labs( 
   title = "Évolution du prix de l'actif SSL_MI",  
   subtitle = "du 01/12/23 au 30/11/24",
   x = "Date",
   y = "Cours de l'actif",
   color = "Actif", # Titre de la légende
   caption = "Source : Yahoo Finance"
  ) +
  scale_color_manual(values = c("SSL_MI" = "darkgreen")) + 
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 10),
   plot.caption = element_text(color = "black", face = "italic"),
   legend.title = element_text(size = 12, face = "bold"),
   legend.text = element_text(size = 10)
  )
 
 plot_rdmt_SSL_MI <- ggplot(SSL_MI, aes(x = Date, y = rendements, color = "SSL_MI")) +
  geom_line(linewidth = 0.5) +  # Utilisation de linewidth au lieu de size
  geom_hline(aes(yintercept = mean(SSL_MI$rendements, na.rm = TRUE)), color = "black", linetype = "dashed")+
  labs( 
   title = "Évolution du rendement de l'actif SSL_MI",  
   subtitle = "du 01/12/23 au 30/11/24",
   x = "Date",
   y = "Cours de l'actif",
   color = "Actif", # Titre de la légende
   caption = "Source : Yahoo Finance"
  ) +
  scale_color_manual(values = c("SSL_MI" = "darkgreen")) + 
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 10),
   plot.caption = element_text(color = "black", face = "italic"),
   legend.title = element_text(size = 12, face = "bold"),
   legend.text = element_text(size = 10)
  )
 
 # Prix et rendement de l'actif SSL_MI
 grid.arrange(plot_rdmt_SSL_MI, plot_prix_SSL_MI, ncol = 2)
 
 #XXT_SG
 
 plot_prix_XXT_SG <- ggplot(XXT_SG, aes(x = Date, y = Adjusted, color = "XXT_SG")) +
  geom_line(linewidth = 1.2) +  # Utilisation de linewidth au lieu de size
  labs( 
   title = "Évolution du prix de l'actif XXT_SG",  
   subtitle = "du 01/12/23 au 30/11/24",
   x = "Date",
   y = "Cours de l'actif",
   color = "Actif", # Titre de la légende
   caption = "Source : Yahoo Finance"
  ) +
  scale_color_manual(values = c("XXT_SG" = "darkgreen")) + 
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 10),
   plot.caption = element_text(color = "black", face = "italic"),
   legend.title = element_text(size = 12, face = "bold"),
   legend.text = element_text(size = 10)
  )
 
 plot_rdmt_XXT_SG <- ggplot(XXT_SG, aes(x = Date, y = rendements, color = "XXT_SG")) +
  geom_line(linewidth = 0.5) +  # Utilisation de linewidth au lieu de size
  geom_hline(aes(yintercept = mean(XXT_SG$rendements, na.rm = TRUE)), color = "black", linetype = "dashed")+
  labs( 
   title = "Évolution du rendement de l'actif XXT_SG",  
   subtitle = "du 01/12/23 au 30/11/24",
   x = "Date",
   y = "Cours de l'actif",
   color = "Actif", # Titre de la légende
   caption = "Source : Yahoo Finance"
  ) +
  scale_color_manual(values = c("XXT_SG" = "darkgreen")) + 
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 10),
   plot.caption = element_text(color = "black", face = "italic"),
   legend.title = element_text(size = 12, face = "bold"),
   legend.text = element_text(size = 10)
  )
 
 # Prix et rendement de l'actif XXT_SG
 grid.arrange(plot_rdmt_XXT_SG, plot_prix_XXT_SG, ncol = 2)
 
 #CCP_L
 
 plot_prix_CCP_L <- ggplot(CCP_L, aes(x = Date, y = Adjusted, color = "CCP_L")) +
  geom_line(linewidth = 1.2) +  # Utilisation de linewidth au lieu de size
  labs( 
   title = "Évolution du prix de l'actif CCP_L",  
   subtitle = "du 01/12/23 au 30/11/24",
   x = "Date",
   y = "Cours de l'actif",
   color = "Actif", # Titre de la légende
   caption = "Source : Yahoo Finance"
  ) +
  scale_color_manual(values = c("CCP_L" = "darkgreen")) + 
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 10),
   plot.caption = element_text(color = "black", face = "italic"),
   legend.title = element_text(size = 12, face = "bold"),
   legend.text = element_text(size = 10)
  )
 
 plot_rdmt_CCP_L <- ggplot(CCP_L, aes(x = Date, y = rendements, color = "CCP_L")) +
  geom_line(linewidth = 0.5) +  # Utilisation de linewidth au lieu de size
  geom_hline(aes(yintercept = mean(CCP_L$rendements, na.rm = TRUE)), color = "black", linetype = "dashed")+
  labs( 
   title = "Évolution du rendement de l'actif CCP_L",  
   subtitle = "du 01/12/23 au 30/11/24",
   x = "Date",
   y = "Cours de l'actif",
   color = "Actif", # Titre de la légende
   caption = "Source : Yahoo Finance"
  ) +
  scale_color_manual(values = c("CCP_L" = "darkgreen")) + 
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 10),
   plot.caption = element_text(color = "black", face = "italic"),
   legend.title = element_text(size = 12, face = "bold"),
   legend.text = element_text(size = 10)
  )
 
 # Prix et rendement de l'actif CCP_L
 grid.arrange(plot_rdmt_CCP_L, plot_prix_CCP_L, ncol = 2)
 
 #BVB_MU
 
 plot_prix_BVB_MU <- ggplot(BVB_MU, aes(x = Date, y = Adjusted, color = "BVB_MU")) +
  geom_line(linewidth = 1.2) +  # Utilisation de linewidth au lieu de size
  labs( 
   title = "Évolution du prix de l'actif BVB_MU",  
   subtitle = "du 01/12/23 au 30/11/24",
   x = "Date",
   y = "Cours de l'actif",
   color = "Actif", # Titre de la légende
   caption = "Source : Yahoo Finance"
  ) +
  scale_color_manual(values = c("BVB_MU" = "darkgreen")) + 
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 10),
   plot.caption = element_text(color = "black", face = "italic"),
   legend.title = element_text(size = 12, face = "bold"),
   legend.text = element_text(size = 10)
  )
 
 plot_rdmt_BVB_MU <- ggplot(BVB_MU, aes(x = Date, y = rendements, color = "BVB_MU")) +
  geom_line(linewidth = 0.5) +  # Utilisation de linewidth au lieu de size
  geom_hline(aes(yintercept = mean(BVB_MU$rendements, na.rm = TRUE)), color = "black", linetype = "dashed")+
  labs( 
   title = "Évolution du rendement de l'actif BVB_MU",  
   subtitle = "du 01/12/23 au 30/11/24",
   x = "Date",
   y = "Cours de l'actif",
   color = "Actif", # Titre de la légende
   caption = "Source : Yahoo Finance"
  ) +
  scale_color_manual(values = c("BVB_MU" = "darkgreen")) + 
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 10),
   plot.caption = element_text(color = "black", face = "italic"),
   legend.title = element_text(size = 12, face = "bold"),
   legend.text = element_text(size = 10)
  )
 
 # Prix et rendement de l'actif BVB_MU
 grid.arrange(plot_rdmt_BVB_MU, plot_prix_BVB_MU, ncol = 2)
 
 #SCP_LS
 
 plot_prix_SCP_LS <- ggplot(SCP_LS, aes(x = Date, y = Adjusted, color = "SCP_LS")) +
  geom_line(linewidth = 1.2) +  # Utilisation de linewidth au lieu de size
  labs( 
   title = "Évolution du prix de l'actif SCP_LS",  
   subtitle = "du 01/12/23 au 30/11/24",
   x = "Date",
   y = "Cours de l'actif",
   color = "Actif", # Titre de la légende
   caption = "Source : Yahoo Finance"
  ) +
  scale_color_manual(values = c("SCP_LS" = "darkgreen")) + 
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 10),
   plot.caption = element_text(color = "black", face = "italic"),
   legend.title = element_text(size = 12, face = "bold"),
   legend.text = element_text(size = 10)
  )
 
 plot_rdmt_SCP_LS <- ggplot(SCP_LS, aes(x = Date, y = rendements, color = "SCP_LS")) +
  geom_line(linewidth = 0.5) +  # Utilisation de linewidth au lieu de size
  geom_hline(aes(yintercept = mean(SCP_LS$rendements, na.rm = TRUE)), color = "black", linetype = "dashed")+
  labs( 
   title = "Évolution du rendement de l'actif SCP_LS",  
   subtitle = "du 01/12/23 au 30/11/24",
   x = "Date",
   y = "Cours de l'actif",
   color = "Actif", # Titre de la légende
   caption = "Source : Yahoo Finance"
  ) +
  scale_color_manual(values = c("SCP_LS" = "darkgreen")) + 
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 10),
   plot.caption = element_text(color = "black", face = "italic"),
   legend.title = element_text(size = 12, face = "bold"),
   legend.text = element_text(size = 10)
  )
 
 # Prix et rendement de l'actif SCP_LS
 grid.arrange(plot_rdmt_SCP_LS, plot_prix_SCP_LS, ncol = 2)
 
 #AAB_CO
 
 plot_prix_AAB_CO <- ggplot(AAB_CO, aes(x = Date, y = Adjusted, color = "AAB_CO")) +
  geom_line(linewidth = 1.2) +  # Utilisation de linewidth au lieu de size
  labs( 
   title = "Évolution du prix de l'actif AAB_CO",  
   subtitle = "du 01/12/23 au 30/11/24",
   x = "Date",
   y = "Cours de l'actif",
   color = "Actif", # Titre de la légende
   caption = "Source : Yahoo Finance"
  ) +
  scale_color_manual(values = c("AAB_CO" = "darkgreen")) + 
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 10),
   plot.caption = element_text(color = "black", face = "italic"),
   legend.title = element_text(size = 12, face = "bold"),
   legend.text = element_text(size = 10)
  )
 
 plot_rdmt_AAB_CO <- ggplot(AAB_CO, aes(x = Date, y = rendements, color = "AAB_CO")) +
  geom_line(linewidth = 0.5) +  # Utilisation de linewidth au lieu de size
  geom_hline(aes(yintercept = mean(AAB_CO$rendements, na.rm = TRUE)), color = "black", linetype = "dashed")+
  labs( 
   title = "Évolution du rendement de l'actif AAB_CO",  
   subtitle = "du 01/12/23 au 30/11/24",
   x = "Date",
   y = "Cours de l'actif",
   color = "Actif", # Titre de la légende
   caption = "Source : Yahoo Finance"
  ) +
  scale_color_manual(values = c("AAB_CO" = "darkgreen")) + 
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 10),
   plot.caption = element_text(color = "black", face = "italic"),
   legend.title = element_text(size = 12, face = "bold"),
   legend.text = element_text(size = 10)
  )
 
 # Prix et rendement de l'actif AAB_CO
 grid.arrange(plot_rdmt_AAB_CO, plot_prix_AAB_CO, ncol = 2)
 
 #GSRAY_IS
 
 plot_prix_GSRAY_IS <- ggplot(GSRAY_IS, aes(x = Date, y = Adjusted, color = "GSRAY_IS")) +
  geom_line(linewidth = 1.2) +  # Utilisation de linewidth au lieu de size
  labs( 
   title = "Évolution du prix de l'actif GSRAY_IS",  
   subtitle = "du 01/12/23 au 30/11/24",
   x = "Date",
   y = "Cours de l'actif",
   color = "Actif", # Titre de la légende
   caption = "Source : Yahoo Finance"
  ) +
  scale_color_manual(values = c("GSRAY_IS" = "darkgreen")) + 
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 10),
   plot.caption = element_text(color = "black", face = "italic"),
   legend.title = element_text(size = 12, face = "bold"),
   legend.text = element_text(size = 10)
  )
 
 plot_rdmt_GSRAY_IS <- ggplot(GSRAY_IS, aes(x = Date, y = rendements, color = "GSRAY_IS")) +
  geom_line(linewidth = 0.5) +  # Utilisation de linewidth au lieu de size
  geom_hline(aes(yintercept = mean(GSRAY_IS$rendements, na.rm = TRUE)), color = "black", linetype = "dashed")+
  labs( 
   title = "Évolution du rendement de l'actif GSRAY_IS",  
   subtitle = "du 01/12/23 au 30/11/24",
   x = "Date",
   y = "Cours de l'actif",
   color = "Actif", # Titre de la légende
   caption = "Source : Yahoo Finance"
  ) +
  scale_color_manual(values = c("GSRAY_IS" = "darkgreen")) + 
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 10),
   plot.caption = element_text(color = "black", face = "italic"),
   legend.title = element_text(size = 12, face = "bold"),
   legend.text = element_text(size = 10)
  )
 
 # Prix et rendement de l'actif GSRAY_IS
 grid.arrange(plot_rdmt_GSRAY_IS, plot_prix_GSRAY_IS, ncol = 2)
 
 #AJAX_AS
 
 plot_prix_AJAX_AS <- ggplot(AJAX_AS, aes(x = Date, y = Adjusted, color = "AJAX_AS")) +
  geom_line(linewidth = 1.2) +  # Utilisation de linewidth au lieu de size
  labs( 
   title = "Évolution du prix de l'actif AJAX_AS",  
   subtitle = "du 01/12/23 au 30/11/24",
   x = "Date",
   y = "Cours de l'actif",
   color = "Actif", # Titre de la légende
   caption = "Source : Yahoo Finance"
  ) +
  scale_color_manual(values = c("AJAX_AS" = "darkgreen")) + 
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 10),
   plot.caption = element_text(color = "black", face = "italic"),
   legend.title = element_text(size = 12, face = "bold"),
   legend.text = element_text(size = 10)
  )
 
 plot_rdmt_AJAX_AS <- ggplot(AJAX_AS, aes(x = Date, y = rendements, color = "AJAX_AS")) +
  geom_line(linewidth = 0.5) +  # Utilisation de linewidth au lieu de size
  geom_hline(aes(yintercept = mean(AJAX_AS$rendements, na.rm = TRUE)), color = "black", linetype = "dashed")+
  labs( 
   title = "Évolution du rendement de l'actif AJAX_AS",  
   subtitle = "du 01/12/23 au 30/11/24",
   x = "Date",
   y = "Cours de l'actif",
   color = "Actif", # Titre de la légende
   caption = "Source : Yahoo Finance"
  ) +
  scale_color_manual(values = c("AJAX_AS" = "darkgreen")) + 
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 10),
   plot.caption = element_text(color = "black", face = "italic"),
   legend.title = element_text(size = 12, face = "bold"),
   legend.text = element_text(size = 10)
  )
 
 # Prix et rendement de l'actif AJAX_AS
 grid.arrange(plot_rdmt_AJAX_AS, plot_prix_AJAX_AS, ncol = 2)
 
 #SOL_USD
 
 plot_prix_SOL_USD <- ggplot(SOL_USD, aes(x = Date, y = Adjusted, color = "SOL_USD")) +
  geom_line(linewidth = 1.2) +  # Utilisation de linewidth au lieu de size
  labs( 
   title = "Évolution du prix de l'actif SOL_USD",  
   subtitle = "du 01/12/23 au 30/11/24",
   x = "Date",
   y = "Cours de l'actif",
   color = "Actif", # Titre de la légende
   caption = "Source : Yahoo Finance"
  ) +
  scale_color_manual(values = c("SOL_USD" = "darkgreen")) + 
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 10),
   plot.caption = element_text(color = "black", face = "italic"),
   legend.title = element_text(size = 12, face = "bold"),
   legend.text = element_text(size = 10)
  )
 
 plot_rdmt_SOL_USD <- ggplot(SOL_USD, aes(x = Date, y = rendements, color = "SOL_USD")) +
  geom_line(linewidth = 0.5) +  # Utilisation de linewidth au lieu de size
  geom_hline(aes(yintercept = mean(SOL_USD$rendements, na.rm = TRUE)), color = "black", linetype = "dashed")+
  labs( 
   title = "Évolution du rendement de l'actif SOL_USD",  
   subtitle = "du 01/12/23 au 30/11/24",
   x = "Date",
   y = "Cours de l'actif",
   color = "Actif", # Titre de la légende
   caption = "Source : Yahoo Finance"
  ) +
  scale_color_manual(values = c("SOL_USD" = "darkgreen")) + 
  theme_classic() +
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 10),
   plot.caption = element_text(color = "black", face = "italic"),
   legend.title = element_text(size = 12, face = "bold"),
   legend.text = element_text(size = 10)
  )
 
 # Prix et rendement de l'actif SOL_USD
 grid.arrange(plot_rdmt_SOL_USD, plot_prix_SOL_USD, ncol = 2)
 
 
 #---- 1.2 rendements ----
 
 base[, rendements := Adjusted / shift(Adjusted, 1) - 1, by = ticker]
 
 ggplot(base, 
        aes(x = Date, y = rendements, color = ticker)) +
  geom_line() +
  ggtitle("Evolution des rendements") +
  scale_color_manual(
   values = vecteur_colors,
   name = "Différents fonds") +
  labs(
   title = "Évolution des rendements des actifs", 
   x = "Date",
   y = "rendements",
   caption = "Data source: Yahoo Finance",
   subtitle = "du 01/12/23 au 30/11/24") +
  theme_classic()+
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 9),
   plot.caption = element_text(color = "azure4", face ="plain")
  )
 
 ##Question 2 + 3
 
 #---------- Question 4 ----------
 
 
 par(mfrow = c(2,3))
 
 
 JUVE_MI[, rendements := Adjusted / shift(Adjusted, 1) - 1]
 SSL_MI[, rendements := Adjusted / shift(Adjusted, 1) - 1]
 XXT_SG[, rendements := Adjusted / shift(Adjusted, 1) - 1]
 CCP_L[, rendements := Adjusted / shift(Adjusted, 1) - 1]
 BVB_MU[, rendements := Adjusted / shift(Adjusted, 1) - 1]
 SCP_LS[, rendements := Adjusted / shift(Adjusted, 1) - 1]
 AAB_CO[, rendements := Adjusted / shift(Adjusted, 1) - 1]
 GSRAY_IS[, rendements := Adjusted / shift(Adjusted, 1) - 1]
 AJAX_AS[, rendements := Adjusted / shift(Adjusted, 1) - 1]
 SOL_USD[, rendements := Adjusted / shift(Adjusted, 1) - 1]
 
 JUVE_MI <- JUVE_MI[-1, ]
 SSL_MI <- SSL_MI[-1, ]
 XXT_SG <- XXT_SG[-1, ]
 CCP_L <- CCP_L[-1, ]
 BVB_MU <- BVB_MU[-1, ]
 SCP_LS <- SCP_LS[-1, ]
 AAB_CO <- AAB_CO[-1, ]
 GSRAY_IS <- GSRAY_IS[-1, ]
 AJAX_AS <- AJAX_AS[-1, ]
 SOL_USD <- SOL_USD[-1, ]
 View(SOL_USD)
 boxplot(JUVE_MI$rendements,ylab="JUVE_MI", col="darkgreen")
 boxplot(SSL_MI$rendements,ylab="SSL_MI",col="darkgreen")
 boxplot(XXT_SG$rendements,ylab="XXT_SG",col="darkgreen")
 boxplot(CCP_L$rendements,ylab="CCP_L",col="darkgreen")
 boxplot(BVB_MU$rendements,ylab="BVB_MU",col="darkgreen")
 boxplot(SCP_LS$rendements,ylab="SCP_LS",col="darkgreen")
 boxplot(AAB_CO$rendements,ylab="AAB_CO",col="darkgreen")
 boxplot(GSRAY_IS$rendements,ylab="GSRAY_IS",col="darkgreen")
 boxplot(AJAX_AS$rendements,ylab="AJAX_AS",col="darkgreen")
 boxplot(SOL_USD$rendements,ylab="SOL_USD",col="darkgreen")
 
 
 ggplot(JUVE_MI, aes(x = rendements)) +
  geom_histogram(binwidth = 0.01, fill = "darkgreen", color = "black", alpha = 0.7) +
  labs(
   title = "Histogramme des rendements",
   x = "Rendement",
   y = "Fréquence",
   caption = "Source : Vos données"
  ) +
  theme_minimal() +
  theme(
   plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
   axis.title = element_text(size = 12)
  )
 
 
 ggplot(base, aes(x = rendements, fill = ticker)) + 
  geom_histogram(binwidth = 0.005, color = "darkgreen", alpha = 0.7, position = "identity") + 
  scale_fill_manual(values = vecteur_colors, guide = "none") +  # Suppression de la légende
  labs(
   title = "Histogramme des rendements des actifs", 
   x = "Rendements",
   y = "Fréquence",
   caption = "Data source: Yahoo Finance",
   subtitle = "du 01/12/23 au 30/11/24"
  ) + 
  theme_classic() + 
  theme(
   plot.title = element_text(color = "black", size = 20, face = "bold"),
   plot.subtitle = element_text(color = "grey3", size = 9),
   plot.caption = element_text(color = "azure4", face ="plain")
  ) +
  facet_wrap(~ ticker, scales = "free_y")  # Crée un histogramme par ticker
 
 #---------- Question 5 ----------
 
 # Supprimer les lignes avec NA dans rendements
 base_clean <- base[!is.na(rendements), ]
 
 # Vérifiez la nouvelle table
 table(base_clean$ticker)
 
 
 sum(is.na(base_clean$rendements))  # Nombre total de NA dans rendements
 table(base$ticker, is.na(base_clean$rendements))  # Nombre de NA par ticker
 
 #tapply(X, INDEX, FUN)
 
 moyenne=tapply(base_clean$rendements,base_clean$ticker,mean)
 variance=tapply(base_clean$rendements,base_clean$ticker,var)
 ecarttype=tapply(base_clean$rendements,base_clean$ticker,sd)
 skewness=tapply(base_clean$rendements,base_clean$ticker,skewness)
 kurtosis=tapply(base_clean$rendements,base_clean$ticker,kurtosis)
 
 
 moyenne
 variance
 ecarttype
 skewness
 kurtosis
 
 #---------- Question 6 ----------
 
 R<-tapply(base_clean$rendements,base_clean$ticker,mean)
 O<-tapply(base_clean$rendements,base_clean$ticker,sd)
 r=0.005
 
 #Ratio de Sharpe 
 S=(R-r)/O
 S
 
 #---------- Question 7 ----------
 
 # Création de la colonne "index des prix normalisés"
 
 JUVE_MI<-mutate(JUVE_MI, idx_price := Adjusted/Adjusted[1])
 SSL_MI <-mutate(SSL_MI, idx_price := Adjusted/Adjusted[1])
 XXT_SG <-mutate(XXT_SG, idx_price := Adjusted/Adjusted[1])
 CCP_L<-mutate(CCP_L, idx_price := Adjusted/Adjusted[1])
 BVB_MU<-mutate(BVB_MU, idx_price := Adjusted/Adjusted[1])
 SCP_LS<-mutate(SCP_LS, idx_price := Adjusted/Adjusted[1])
 AAB_CO<-mutate(AAB_CO, idx_price := Adjusted/Adjusted[1])
 GSRAY_IS<-mutate(GSRAY_IS, idx_price := Adjusted/Adjusted[1])
 AJAX_AS<-mutate(AJAX_AS, idx_price := Adjusted/Adjusted[1])
 
 #Suppression des NA
 JUVE_MI <- na.omit(JUVE_MI)
 SSL_MI <- na.omit(SSL_MI)
 XXT_SG <- na.omit(XXT_SG)
 CCP_L <- na.omit(CCP_L)
 BVB_MU <- na.omit(BVB_MU)
 SCP_LS <- na.omit(SCP_LS)
 AAB_CO <- na.omit(AAB_CO)
 GSRAY_IS <- na.omit(GSRAY_IS)
 AJAX_AS <- na.omit(AJAX_AS)
 
 #Fusion pour avoir toutes les observations en commun
 # merge pr combiner :)
 View(JUVE_MI)
 View(basebis)
 
 basebis = merge(JUVE_MI, SSL_MI, by = "Date")
 names(basebis)[2]="Open_JUVE_MI"
 names(basebis)[3]="High_JUVE_MI"
 names(basebis)[4]="Low_JUVE_MI"
 names(basebis)[5]="Close_JUVE_MI"
 names(basebis)[6]="Volume_JUVE_MI"
 names(basebis)[7]="Adjusted_JUVE_MI"
 names(basebis)[8]="ticker_JUVE_MI"
 names(basebis)[9]="rendements_JUVE_MI"
 names(basebis)[10]="idx_price_JUVE_MI"
 names(basebis)[11]="Open_SSL_MI"
 names(basebis)[12]="High_SSL_MI"
 names(basebis)[13]="Low_SSL_MI"
 names(basebis)[14]="Close_SSL_MI"
 names(basebis)[15]="Volume_SSL_MI"
 names(basebis)[16]="Adjusted_SSL_MI"
 names(basebis)[17]="ticker_SSL_MI"
 names(basebis)[18]="rendements_SSL_MI"
 names(basebis)[19]="idx_price_SSL_MI"
 basebis2 = merge(basebis, XXT_SG, by = "Date")
 names(basebis2)[20]="Open_XXT_SG"
 names(basebis2)[21]="High_XXT_SG"
 names(basebis2)[22]="Low_XXT_SG"
 names(basebis2)[23]="Close_XXT_SG"
 names(basebis2)[24]="Volume_XXT_SG"
 names(basebis2)[25]="Adjusted_XXT_SG"
 names(basebis2)[26]="ticker_XXT_SG"
 names(basebis2)[27]="rendements_XXT_SG"
 names(basebis2)[28]="idx_price_XXT_SG"
 basebis3 = merge(basebis2, CCP_L, by = "Date")
 names(basebis3)[29]="Open_CCP_L"
 names(basebis3)[30]="High_CCP_L"
 names(basebis3)[31]="Low_CCP_L"
 names(basebis3)[32]="Close_CCP_L"
 names(basebis3)[33]="Volume_CCP_L"
 names(basebis3)[34]="Adjusted_CCP_L"
 names(basebis3)[35]="ticker_CCP_L"
 names(basebis3)[36]="rendements_CCP_L"
 names(basebis3)[37]="idx_price_CCP_L"
 basebis4 = merge(basebis3, BVB_MU, by = "Date")
 names(basebis4)[38]="Open_BVB_MU"
 names(basebis4)[39]="High_BVB_MU"
 names(basebis4)[40]="Low_BVB_MU"
 names(basebis4)[41]="Close_BVB_MU"
 names(basebis4)[42]="Volume_BVB_MU"
 names(basebis4)[43]="Adjusted_BVB_MU"
 names(basebis4)[44]="ticker_BVB_MU"
 names(basebis4)[45]="rendements_BVB_MU"
 names(basebis4)[46]="idx_price_BVB_MU"
 basebis5 = merge(basebis4, SCP_LS, by = "Date")
 names(basebis5)[47]="Open_SCP_LS"
 names(basebis5)[48]="High_SCP_LS"
 names(basebis5)[49]="Low_SCP_LS"
 names(basebis5)[50]="Close_SCP_LS"
 names(basebis5)[51]="Volume_SCP_LS"
 names(basebis5)[52]="Adjusted_SCP_LS"
 names(basebis5)[53]="ticker_SCP_LS"
 names(basebis5)[54]="rendements_SCP_LS"
 names(basebis5)[55]="idx_price_SCP_LS"
 basebis6 = merge(basebis5, AAB_CO, by = "Date")
 names(basebis6)[56]="Open_AAB_CO"
 names(basebis6)[57]="High_AAB_CO"
 names(basebis6)[58]="Low_AAB_CO"
 names(basebis6)[59]="Close_AAB_CO"
 names(basebis6)[60]="Volume_AAB_CO"
 names(basebis6)[61]="Adjusted_AAB_CO"
 names(basebis6)[62]="ticker_AAB_CO"
 names(basebis6)[63]="rendements_AAB_CO"
 names(basebis6)[64]="idx_price_AAB_CO"
 basebis7 = merge(basebis6, GSRAY_IS, by = "Date")
 names(basebis7)[65]="Open_GSRAY_IS"
 names(basebis7)[66]="High_GSRAY_IS"
 names(basebis7)[67]="Low_GSRAY_IS"
 names(basebis7)[68]="Close_GSRAY_IS"
 names(basebis7)[69]="Volume_GSRAY_IS"
 names(basebis7)[70]="Adjusted_GSRAY_IS"
 names(basebis7)[71]="ticker_GSRAY_IS"
 names(basebis7)[72]="rendements_GSRAY_IS"
 names(basebis7)[73]="idx_price_GSRAY_IS"
 basebis8 = merge(basebis7, AJAX_AS, by = "Date")
 names(basebis8)[74]="Open_AJAX_AS"
 names(basebis8)[75]="High_AJAX_AS"
 names(basebis8)[76]="Low_AJAX_AS"
 names(basebis8)[77]="Close_AJAX_AS"
 names(basebis8)[78]="Volume_AJAX_AS"
 names(basebis8)[79]="Adjusted_AJAX_AS"
 names(basebis8)[80]="ticker_AJAX_AS"
 names(basebis8)[81]="rendements_AJAX_AS"
 names(basebis8)[82]="idx_price_AJAX_AS"
 basebis9 = merge(basebis8, SOL_USD, by = "Date")
 names(basebis9)[83]="Open_SOL_USD"
 names(basebis9)[84]="High_SOL_USD"
 names(basebis9)[85]="Low_SOL_USD"
 names(basebis9)[86]="Close_SOL_USD"
 names(basebis9)[87]="Volume_SOL_USD"
 names(basebis9)[88]="Adjusted_SOL_USD"
 names(basebis9)[89]="ticker_SOL_USD"
 names(basebis9)[90]="rendements_SOL_USD"
 names(basebis9)[91]="idx_price_SOL_USD"
 
 
 View(basebis9)
 
 # Je fais une nouvelle base avec uniquement les rendements 
 
 
 basebisrendement <- xts(basebis9[,c(9,18,27,36,45,54,63,72,81)], order.by=as.Date(basebis9$Date))
 names(basebisrendement) <- c("JUVE_MI", "SSL_MI", "XXT_SG","CCP_L","BVB_MU","SCP_LS", "AAB_CO", "GSRAY_IS", "AJAX_AS")
 basebisrendement <- basebisrendement[-1,]
 
 View(basebisrendement)
 
 #Matrice des variances-covariances
 
 cov(na.omit(basebisrendement),method = "pearson")
 matrice <- cov(na.omit(basebisrendement),method = "pearson")
 matrice
 
 #---------- Question 8 ----------
 
 
 #Matrice de corrélation
 par(mfrow = c(1,1))
 corr<-cor(na.omit(basebisrendement))
 corrplot(corr, method="number",type="upper",tl.col = "black",
          col = colorRampPalette(c("skyblue", "blue", "navy"))(100))
 
 
 
 #---------- Question 9 ----------
 
 #cf dossier écrit
 
 
 
 #---------- Question 10 ----------
 
 
 #Mesure de performance
 #xts -> séries temporelles
 #ici on prend la 4e colonne du data set qui rpz les rendements
 #order.by=as.Date(ACCA$Date) -> On ordonne en fct des dates
 
 
 JUVE_MIrdmt <- xts(JUVE_MI[,9], order.by=as.Date(JUVE_MI$Date))
 SSL_MIrdmt <- xts(SSL_MI[,9], order.by=as.Date(SSL_MI$Date))
 XXT_SGrdmt <- xts(XXT_SG[,9], order.by=as.Date(XXT_SG$Date))
 CCP_Lrdmt <- xts(CCP_L[,9], order.by=as.Date(CCP_L$Date))
 BVB_MUrdmt <- xts(BVB_MU[,9], order.by=as.Date(BVB_MU$Date))
 SCP_LSrdmt <- xts(SCP_LS[,9], order.by=as.Date(SCP_LS$Date))
 AAB_COrdmt <- xts(AAB_CO[,9], order.by=as.Date(AAB_CO$Date))
 GSRAY_ISrdmt <- xts(GSRAY_IS[,9], order.by=as.Date(GSRAY_IS$Date))
 AJAX_ASrdmt <- xts(AJAX_AS[,9], order.by=as.Date(AJAX_AS$Date))
 SOL_USDrdmt <- xts(SOL_USD[,9], order.by=as.Date(SOL_USD$Date))
 
 Names<-c("JUVE_MI", "SSL_MI", "XXT_SG","CCP_L","BVB_MU","SCP_LS","AAB_CO","GSRAY_IS","AJAX_AS","SOL_USD")
 
 #Coefficient de variation
 
 CV1 <- sd(JUVE_MIrdmt)/mean(JUVE_MIrdmt)
 CV2 <- sd(SSL_MIrdmt)/mean(SSL_MIrdmt)
 CV3 <- sd(XXT_SGrdmt)/mean(XXT_SGrdmt)
 CV4 <- sd(CCP_Lrdmt)/mean(CCP_Lrdmt)
 CV5 <- sd(BVB_MUrdmt)/mean(BVB_MUrdmt)
 CV6 <- sd(SCP_LSrdmt)/mean(SCP_LSrdmt)
 CV7 <- sd(AAB_COrdmt)/mean(AAB_COrdmt)
 CV8 <- sd(GSRAY_ISrdmt)/mean(GSRAY_ISrdmt)
 CV9 <- sd(AJAX_ASrdmt)/mean(AJAX_ASrdmt)
 CV10 <- sd(SOL_USDrdmt)/mean(SOL_USDrdmt)
 
 CV<-c(CV1,CV2,CV3,CV4,CV5,CV6, CV7,CV8,CV9,CV10)
 CVfinal<-rbind(Names,CV)
 CVfinal
 
 #Names "SOL_USD":CV="9.39562097361627" le plus failbe coef de var
 
 #Ratio de Jensen
 
 A_Jensen1<-SFM.jensenAlpha(JUVE_MIrdmt,SOL_USDrdmt)
 A_Jensen2<-round(SFM.jensenAlpha(SSL_MIrdmt,SOL_USDrdmt),5)
 A_Jensen3<-round(SFM.jensenAlpha(XXT_SGrdmt,SOL_USDrdmt),5)
 A_Jensen4<-round(SFM.jensenAlpha(CCP_Lrdmt,SOL_USDrdmt),5)
 A_Jensen5<-round(SFM.jensenAlpha(BVB_MUrdmt,SOL_USDrdmt),5)
 A_Jensen6<-round(SFM.jensenAlpha(SCP_LSrdmt,SOL_USDrdmt),5)
 A_Jensen7<-round(SFM.jensenAlpha(AAB_COrdmt,SOL_USDrdmt),5)
 A_Jensen8<-round(SFM.jensenAlpha(GSRAY_ISrdmt,SOL_USDrdmt),5)
 A_Jensen9<-round(SFM.jensenAlpha(AJAX_ASrdmt,SOL_USDrdmt),5)
 A_Jensen10<-round(SFM.jensenAlpha(SOL_USDrdmt,SOL_USDrdmt),5)
 Alpha_Jensen<-c(A_Jensen1,A_Jensen2,A_Jensen3,A_Jensen4,A_Jensen5,A_Jensen6, A_Jensen7,A_Jensen8,A_Jensen9,A_Jensen10)
 Names<-c("JUVE_MI", "SSL_MI", "XXT_SG","CCP_L","BVB_MU","SCP_LS","AAB_CO","GSRAY_IS","AJAX_AS","SOL_USD")
 Jensen<-rbind(Names,Alpha_Jensen)
 Jensen
 
 #Ratio de Treynor 
 
 Treynor1<-round(TreynorRatio(JUVE_MIrdmt, SOL_USDrdmt),5)
 Treynor2<-round(TreynorRatio(SSL_MIrdmt, SOL_USDrdmt),5)
 Treynor3<-round(TreynorRatio(XXT_SGrdmt, SOL_USDrdmt),5)
 Treynor4<-round(TreynorRatio(CCP_Lrdmt, SOL_USDrdmt),5)
 Treynor5<-round(TreynorRatio(BVB_MUrdmt, SOL_USDrdmt),5)
 Treynor6<-round(TreynorRatio(SCP_LSrdmt, SOL_USDrdmt),5)
 Treynor7<-round(TreynorRatio(AAB_COrdmt, SOL_USDrdmt),5)
 Treynor8<-round(TreynorRatio(GSRAY_ISrdmt, SOL_USDrdmt),5)
 Treynor9<-round(TreynorRatio(AJAX_ASrdmt, SOL_USDrdmt),5)
 Treynor10<-round(TreynorRatio(SOL_USDrdmt, SOL_USDrdmt),5)
 Treynor<-c(Treynor1,Treynor2,Treynor3,Treynor4,Treynor5,Treynor6,Treynor7,Treynor8,Treynor9,Treynor10)
 Treynor<-rbind(Names,Treynor)
 Treynor
 
 
 #Ratio de Sortino 
 
 Sortino1 <- round(SortinoRatio(JUVE_MIrdmt,MAR = 0),5)
 Sortino2 <- round(SortinoRatio(SSL_MIrdmt,MAR = 0),5) 
 Sortino3 <- round(SortinoRatio(XXT_SGrdmt,MAR = 0),5)
 Sortino4 <- round(SortinoRatio(CCP_Lrdmt,MAR = 0),5)
 Sortino5 <- round(SortinoRatio(BVB_MUrdmt,MAR = 0),5)
 Sortino6 <- round(SortinoRatio(SCP_LSrdmt,MAR = 0),5)
 Sortino7 <- round(SortinoRatio(AAB_COrdmt,MAR = 0),5)
 Sortino8 <- round(SortinoRatio(GSRAY_ISrdmt,MAR = 0),5)
 Sortino9 <- round(SortinoRatio(AJAX_ASrdmt,MAR = 0),5)
 Sortino10 <- round(SortinoRatio(SOL_USDrdmt,MAR = 0),5)
 Sortino<-c(Sortino1,Sortino2,Sortino3,Sortino4,Sortino5,Sortino6,Sortino7,Sortino8,Sortino9,Sortino10) 
 Sortino<-rbind(Names,Sortino) 
 Sortino 
 
 #Ratio de Sharpe 
 S=(R-r)/O
 S
 
 #Ratio de Roy 
 JUVE_MI1 <- SharpeRatio(JUVE_MIrdmt,mean(SOL_USDrdmt))
 SSL_MI2 <- SharpeRatio(SSL_MIrdmt,mean(SOL_USDrdmt))
 XXT_SG3 <- SharpeRatio(XXT_SGrdmt,mean(SOL_USDrdmt))
 CCP_L4 <- SharpeRatio(CCP_Lrdmt,mean(SOL_USDrdmt))
 BVB_MU5 <- SharpeRatio(BVB_MUrdmt,mean(SOL_USDrdmt))
 SCP_LS6 <- SharpeRatio(SCP_LSrdmt,mean(SOL_USDrdmt))
 AAB_CO7 <- SharpeRatio(AAB_COrdmt,mean(SOL_USDrdmt))
 GSRAY_IS8 <- SharpeRatio(GSRAY_ISrdmt,mean(SOL_USDrdmt))
 AJAX_AS9 <- SharpeRatio(AJAX_ASrdmt,mean(SOL_USDrdmt))
 SOL_USD10 <- SharpeRatio(SOL_USDrdmt,mean(SOL_USDrdmt))
 
 JUVE_MI1
 SSL_MI2
 XXT_SG3
 CCP_L4
 BVB_MU5
 SCP_LS6
 AAB_CO7
 GSRAY_IS8
 AJAX_AS9
 SOL_USD10
 
 #Ratio de l'information
 RatioInfo <- R/O
 RatioInfo
 
 
 #-------------------- Exercice 2 --------------------
 
 
 #---------- Question 1 ----------
 
 
 View(Rdmts_equi)
 Rdmts_equi<-Return.portfolio(basebisrendement)
 Portefeuille_equi <- data.table(Rdmts_equi,ticker="PORTEFEUILLE")
 names(Portefeuille_equi) <- c("rdmt","ticker")
 mean(Portefeuille_equi$rdmt)
 sd(Portefeuille_equi$rdmt)
 
 liste<-rbind(basebis,Portefeuille_equi,fill=T)
 
 tab <- liste[!is.na(rdmt), .(ticker, rdmt)]
 
 
 
 tab <- tab[, .(Er = round(mean(rdmt), 6),
                sd = round(sd(rdmt), 6)),
            by = "ticker"]
 
 tab
 View(tab)
 #Représentation de nos différentes séries dans un plan moyenne-variance
 
 plot_moyvar = ggplot(tab, aes(x = sd, y = Er, color = ticker)) +
  geom_point(size = 4) +
  theme_light() + ggtitle("Echange risque-rendement") +
  xlab("Volatilité") + ylab("Rendement attendu") +
  scale_y_continuous(label = percent) +
  scale_x_continuous(label = percent)
 
 plot_moyvar = plot_moyvar + labs(title = "Echange risque-rendement")
 
 plot_moyvar = plot_moyvar + theme(
  plot.title = element_text(color = "black", size = 12, face = "bold"),
  plot.caption = element_text(color = "grey", face ="plain")
 )
 
 plot_moyvar
 
 ##
 
 #---------- Question 2 ----------
 
 ##Frontière d'efficience pour les 3 séries JUVE_MI SSL_MI XXT_SG
 
 JUVE_MIrdmt <- xts(JUVE_MI[,9], order.by=as.Date(JUVE_MI$Date))
 SSL_MIrdmt <- xts(SSL_MI[,9], order.by=as.Date(SSL_MI$Date))
 XXT_SGrdmt <- xts(XXT_SG[,9], order.by=as.Date(XXT_SG$Date))
 CCP_Lrdmt <- xts(CCP_L[,9], order.by=as.Date(CCP_L$Date))
 BVB_MUrdmt <- xts(BVB_MU[,9], order.by=as.Date(BVB_MU$Date))
 SCP_LSrdmt <- xts(SCP_LS[,9], order.by=as.Date(SCP_LS$Date))
 AAB_COrdmt <- xts(AAB_CO[,9], order.by=as.Date(AAB_CO$Date))
 GSRAY_ISrdmt <- xts(GSRAY_IS[,9], order.by=as.Date(GSRAY_IS$Date))
 AJAX_ASrdmt <- xts(AJAX_AS[,9], order.by=as.Date(AJAX_AS$Date))
 SOL_USDrdmt <- xts(SOL_USD[,9], order.by=as.Date(SOL_USD$Date))
 
 data=data.table(JUVE_MIrdmt,SSL_MIrdmt,XXT_SGrdmt)
 names(data) <- c("x","y","z")
 View(data)
 
 
 # Esperance de rentabilité
 Ex <- mean(data$x)
 Ey <- mean(data$y)
 Ez <- mean(data$z)
 
 # Ecart-type = risque
 Sdx <- sd(data$x)
 Sdy <- sd(data$y)
 Sdz <- sd(data$z)
 
 # Covariance 
 Covxy <- cov(data$x, data$y)
 Covxz <- cov(data$x, data$z)
 Covyz <- cov(data$y, data$z)
 
 # Création d'un portefeuille des poids
 xpoids <- seq(from = 0, to = 1, length.out = 1000)
 
 # Création d'une table avec les 3 actifs
 Actifs3bis <- data.table(wx = rep(xpoids, each = length(xpoids)),
                          wy = rep(xpoids, length(xpoids)))
 Actifs3bis[, wz := 1 - wx - wy]
 
 # Calcul des esperance de rentabilité et de la volatilité pour les 1000 portefeuille possibles
 Actifs3bis[, ':=' (Ep = wx * Ex + wy * Ey + wz * Ez,
                    Sdp = sqrt(wx^2 * Sdx^2 +
                                wy^2 * Sdy^2 +
                                wz^2 * Sdz^2 +
                                2 * wx * wy * Covxy +
                                2 * wx * wz * Covxz +
                                2 * wy * wz * Covyz))]
 
 # On garde seulement les poids superieur ou egale ? 0 
 Actifs3bis <- Actifs3bis[wx >= 0 & wy >= 0 & wz >= 0]
 Actifs3bis
 
 # Graphique de la frontière d'efficience des portefeuilles
 plot = ggplot() +
  geom_point(data = Actifs3bis, aes(x = Sdp, y = Ep, color = wx - wz)) +
  geom_point(data = data.table(Sd = c(Sdx, Sdy, Sdz), E = c(Ex, Ey, Ez)),
             aes(x = Sd, y = E), color = "black", size = 3, shape = 18) +
  theme_light() + ggtitle("Frontiere d'efficience du portefeuille compose de ALAGR, MLAAH, VFINX") +
  xlab("Volatilité") + ylab("Esperance de rentabilité") +
  scale_y_continuous(label = percent, limits = c(0.001, max(Actifs3bis$Ep) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0.001, max(Actifs3bis$Sdp) * 1.2)) +
  scale_color_gradientn(colors = c("orange", "lightgoldenrodyellow", "red"),
                        name = expression(omega[x] - omega[z]), labels = percent)
 
 plot = plot  + labs(title = "Frontiere d'efficience du portefeuille",
                     subtitle = "composé des actifs JUVE_MI, SSL_MI, XXT_SG",
                     caption = "Data source: Yahoo Finance") 
 
 plot = plot + theme(
  plot.title = element_text(color = "black", size = 12, face = "bold"),
  plot.subtitle = element_text(color = "grey3", size = 9),
  plot.caption = element_text(color = "azure4", face ="plain")
 )
 
 plot
 
 ## Portefeuille de variance minimale 
 assetSymbols <- c('JUVE_MI','SSL_MI','XXT_SG')
 assetReturns <- data
 assetReturns <- data.frame(assetReturns)
 mu <- colMeans(assetReturns) 
 cov.mat <- cov(assetReturns) 
 
 getMinVariancePortfolio <- function(mu,covMat,assetSymbols) {
  U <- rep(1, length(mu)) 
  O <- solve(covMat)    
  w <- O%*%U /as.numeric(t(U)%*%O%*% U)
  Risk <- sqrt(t(w) %*% covMat %*% w)
  ExpReturn <- t(w) %*% mu
  Weights <- `names<-`(round(w, 5), assetSymbols)
  list(Weights = t(Weights),
       ExpReturn = round(as.numeric(ExpReturn), 5),
       Risk = round(as.numeric(Risk), 5))
 }
 
 Pf_VM <- getMinVariancePortfolio(mu, cov.mat,assetSymbols)
 Pf_VM
 
 ## Portefeuille tangent
 
 assetsNames <- c('JUVE_MIrdmt','SSL_MIrdmt','XXT_SGrdmt')
 E = c(Ex,Ey,Ez)
 
 r.free = 2/(100*360)
 cov.mat <- cov(assetReturns) 
 
 E <- as.vector(E)
 cov.mat <- as.matrix(cov.mat)
 cov.mat.inv <- solve(cov.mat)
 
 w.t <- cov.mat.inv %*% (E - r.free)
 w.t <- as.vector(w.t/sum(w.t))
 
 names(w.t) <- assetsNames
 E.t <- crossprod(w.t,E)
 Sd.t <- sqrt(t(w.t) %*% cov.mat %*% w.t)
 PTangent <- list("Weights" = w.t,
                  "ExpReturn" = as.vector(E.t),
                  "Risk" = as.vector(Sd.t))
 PTangent 
 
 