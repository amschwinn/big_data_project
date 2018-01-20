##############################################
# Data Mining for Big Data Project
#
# MLDM M2
#
# Subject: Analysis of complaints and customer
# survey results Groupama
#
# Austin Schwinn, Joe Renner, 
# Dimitris Tsolakidis, & Oussama Bouldjedri
#
# January, 2018
##############################################


library(readr)
library(hashmap)

#Specify directory
dir <- 'Programming/BD_PROJECT/big_data_project_confidential/'

#Load files
B_Actions_rattachees_demandes <- read_csv2(paste(dir,"BASE_Actions_rattachees_demandes.csv",sep = ''))
B_Avantages_clients <- read_csv2(paste(dir,"BASE_Avantages_clients.csv",sep = ''))
B_Demandes_clients_hors <- read_csv2(paste(dir,"BASE_Demandes_clients_hors_reclamations.csv",sep = ''))
B_Donnes_clients <- read_csv2(paste(dir,"BASE_Donnees_Clients.csv",sep = ''))
B_Reclamations_clients <- read_csv2(paste(dir,"BASE_Reclamations_clients.csv",sep = ''))
B_Structure_Commerciale <- read_csv2(paste(dir,"BASE_Structure_Commerciale.csv",sep = ''))

S_AUTO_BDG <- read_csv2(paste(dir,"SATISFACTION_AUTO_BDG_2015_2016_2017.csv",sep = ''))
S_AUTO_CLASSIQUE <- read_csv2(paste(dir,"SATISFACTION_AUTO_CLASSIQUE_2015_2016_2017.csv",sep = ''))
S_AUTO_CLASSIQUE_TMA <- read_csv2(paste(dir,"SATISFACTION_AUTO_CLASSIQUE_TMA_2016_2017.csv",sep = ''))
S_AUTOPRESTO <- read_csv2(paste(dir,"SATISFACTION_AUTOPRESTO_2015_2016_2017.csv",sep = ''))
S_AUTO_TMA_BDG <- read_csv2(paste(dir,"SATISFACTION_AUTO_TMA_BDG_2016_2017.csv",sep = ''))
S_DAB <- read_csv2(paste(dir,"SATISFACTION_DAB_2015_2016_2107.csv",sep = ''))
S_DEMANDE <- read_csv2(paste(dir,"SATISFACTION_DEMANDE_2015_2016_2017.csv",sep = ''))
S_DENTAIRE_NON_PARTENAIRE <- read_csv2(paste(dir,"SATISFACTION_DENTAIRE_NON_PARTENAIRE_2015_2016_2017.csv",sep = ''))
S_DENTAIRE_PARTENAIRE <- read_csv2(paste(dir,"SATISFACTION_DENTAIRE_PARTENAIRE_2015_2016_2017.csv",sep = ''))
S_MODIFICATION <- read_csv2(paste(dir,"SATISFACTION_MODIFICATION_2015_2016_2017.csv",sep = ''))
S_OPTIQUE_NON_PARTENAIRE <- read_csv2(paste(dir,"SATISFACTION_OPTIQUE_NON_PARTENAIRE_2015_2016_2017.csv",sep = ''))
S_OPTIQUE_PARTENAIR <- read_csv2(paste(dir,"SATISFACTION_OPTIQUE_PARTENAIRE_2015_2016_2017.csv",sep = ''))
S_RECLAMATION <- read_csv2(paste(dir,"SATISFACTION_RECLAMATION_2015_2016_2107.csv",sep = ''))
S_RESILIATION <- read_csv2(paste(dir,"SATISFACTION_RESILIATION_2015_2016_2017.csv",sep = ''))
S_SOUSCRIPTION <- read_csv2(paste(dir,"SATISFACTION_SOUSCRIPTION_2015_2016_2017.csv",sep = ''))
S_SUIVI_PROACTIF <- read_csv2(paste(dir,"SATISFACTION_SUIVI_PROACTIF_2016_2017.csv",sep = ''))

#Create two lists
complaints <- list(B_Actions_rattachees_demandes,B_Avantages_clients,B_Demandes_clients_hors,B_Donnes_clients,
                B_Reclamations_clients,B_Structure_Commerciale)

surveys <- list(S_AUTO_BDG,S_AUTO_CLASSIQUE,S_AUTO_CLASSIQUE_TMA,S_AUTOPRESTO,S_AUTO_TMA_BDG,S_DAB,S_DEMANDE,
             S_DENTAIRE_NON_PARTENAIRE,S_DENTAIRE_PARTENAIRE,S_MODIFICATION,S_OPTIQUE_NON_PARTENAIRE,
             S_OPTIQUE_PARTENAIR,S_RECLAMATION,S_RESILIATION,S_SOUSCRIPTION,S_SUIVI_PROACTIF)


Average_Satisfaction <- hashmap(c(0),c(0))  #  (key = user id, value = average satisfaction)
num_surveys <- hashmap(c(0),c(0))           # (key = user id, value = num of surveys processed)
Average_Satisfaction$clear()
num_surveys$clear()

for (table in surveys){
  print(table)
  x <- nrow(table)
  for (i in seq.int(from=2, to=x)){
    key <- as.numeric(table[i,'Meta_donnee 3'])
    if (Average_Satisfaction$has_key(key)){
      avg <- Average_Satisfaction[[key]]
      num <- num_surveys[[key]]
      new_avg <- ((avg * num) + as.numeric(table[i,2])) / (num + 1)
      Average_Satisfaction[[key]] <- new_avg
      num_surveys[[key]] <- num + 1
    }else{
      Average_Satisfaction[[key]] <- as.numeric(table[i,2])
      num_surveys[[key]] <- 1
    }
  }
}

print(Average_Satisfaction[[601283]])

