##############################################
# Data Mining for Big Data Project
#
# MLDM M2
#
# Subject: Q2) Understand why the customers are not satisfied
# Start with apriori to look for frequent itemsets of customer
# information to complaint type.
# Secondly, we will do text analysis for each of the complaint 
# types and look for descriptive words for those complains
#
# Austin Schwinn, Joe Renner, 
# Dimitris Tsolakidis, & Oussama Bouldjedri
#
# January, 2018
##############################################


library(tidyr)
library(dplyr)
library(plyr)
library(readr)
library(arules)
library(rpart)
setwd('D:/GD/MLDM/Big Data/Project/git_repo')
#############################################
# Q 3.1)
#############################################

####
# Load relevant files with the base_survey.R script
# We will use the following dataframes from that script:
# - B_Donnes_clients
# - B_Structure_Commerciale
# - B_Reclamations_clients
####

#Select the survey for this iteration
survey_num <- 11

#Load files
dir <- 'D:/GD/MLDM/Big Data/Project/big_data_project_confidential/'

B_Donnes_clients <- read_csv2(paste(dir,"BASE_Donnees_Clients.csv",sep = ''))
B_Reclamations_clients <- read_csv2(paste(dir,"BASE_Reclamations_clients.csv",sep = ''))
B_Structure_Commerciale <- read_csv2(paste(dir,"BASE_Structure_Commerciale.csv",sep = ''))
B_Actions_rattachees_demandes <- read_csv2(paste(dir,"BASE_Actions_rattachees_demandes.csv",sep = ''))
B_Avantages_clients <- read_csv2(paste(dir,"BASE_Avantages_clients.csv",sep = ''))


S_AUTO_BDG <- data.frame(read_csv2(paste(dir,"SATISFACTION_AUTO_BDG_2015_2016_2017.csv",sep = '')))
S_AUTO_CLASSIQUE <- data.frame(read_csv2(paste(dir,"SATISFACTION_AUTO_CLASSIQUE_2015_2016_2017.csv",sep = '')))
S_AUTO_CLASSIQUE_TMA <- data.frame(read_csv2(paste(dir,"SATISFACTION_AUTO_CLASSIQUE_TMA_2016_2017.csv",sep = '')))
S_AUTOPRESTO <- data.frame(read_csv2(paste(dir,"SATISFACTION_AUTOPRESTO_2015_2016_2017.csv",sep = '')))
S_AUTO_TMA_BDG <- data.frame(read_csv2(paste(dir,"SATISFACTION_AUTO_TMA_BDG_2016_2017.csv",sep = '')))
S_DAB <- data.frame(read_csv2(paste(dir,"SATISFACTION_DAB_2015_2016_2107.csv",sep = '')))
S_DEMANDE <- data.frame(read_csv2(paste(dir,"SATISFACTION_DEMANDE_2015_2016_2017.csv",sep = '')))
S_DENTAIRE_NON_PARTENAIRE <- data.frame(read_csv2(paste(dir,"SATISFACTION_DENTAIRE_NON_PARTENAIRE_2015_2016_2017.csv",sep = '')))
S_DENTAIRE_PARTENAIRE <- data.frame(read_csv2(paste(dir,"SATISFACTION_DENTAIRE_PARTENAIRE_2015_2016_2017.csv",sep = '')))
S_MODIFICATION <- data.frame(read_csv2(paste(dir,"SATISFACTION_MODIFICATION_2015_2016_2017.csv",sep = '')))
S_OPTIQUE_NON_PARTENAIRE <- data.frame(read_csv2(paste(dir,"SATISFACTION_OPTIQUE_NON_PARTENAIRE_2015_2016_2017.csv",sep = '')))
S_OPTIQUE_PARTENAIR <- data.frame(read_csv2(paste(dir,"SATISFACTION_OPTIQUE_PARTENAIRE_2015_2016_2017.csv",sep = '')))
S_RECLAMATION <- data.frame(read_csv2(paste(dir,"SATISFACTION_RECLAMATION_2015_2016_2107.csv",sep = '')))
S_RESILIATION <- data.frame(read_csv2(paste(dir,"SATISFACTION_RESILIATION_2015_2016_2017.csv",sep = '')))
S_SOUSCRIPTION <-data.frame( read_csv2(paste(dir,"SATISFACTION_SOUSCRIPTION_2015_2016_2017.csv",sep = '')))
S_SUIVI_PROACTIF <- data.frame(read_csv2(paste(dir,"SATISFACTION_SUIVI_PROACTIF_2016_2017.csv",sep = '')))


surveys <- list(S_AUTO_BDG, S_AUTO_CLASSIQUE, S_AUTO_CLASSIQUE_TMA, S_AUTOPRESTO,
                S_AUTO_TMA_BDG, S_DAB, S_DEMANDE, S_DENTAIRE_NON_PARTENAIRE, 
                S_DENTAIRE_PARTENAIRE, S_MODIFICATION, S_OPTIQUE_NON_PARTENAIRE,
                S_OPTIQUE_PARTENAIR, S_RECLAMATION,
                S_RESILIATION, S_SOUSCRIPTION, S_SUIVI_PROACTIF)

survey_names <- list('S_AUTO_BDG',' S_AUTO_CLASSIQUE',' S_AUTO_CLASSIQUE_TMA',' S_AUTOPRESTO','
                     S_AUTO_TMA_BDG',' S_DAB',' S_DEMANDE',' S_DENTAIRE_NON_PARTENAIRE',' 
                     S_DENTAIRE_PARTENAIRE',' S_MODIFICATION',' S_OPTIQUE_NON_PARTENAIRE','
                     S_OPTIQUE_PARTENAIR',' S_RECLAMATION','
                     S_RESILIATION',' S_SOUSCRIPTION',' S_SUIVI_PROACTIF')


#Combine all revelant information we will need for apriori
tot_info <- B_Donnes_clients[,c('ID_GRC','TRANCHE_AGE','SEXE','NATURE_PERSONNE',
                                'MARCHE_PSO','TYPOLOGIE','CD_COMMERCIAL_CHARGE')]

#Join with employee information to get region, agency, etc info
#align column names
colnames(B_Structure_Commerciale)[1] <- 'CD_COMMERCIAL_CHARGE'
#filter non useful examples
B_Structure_Commerciale <- B_Structure_Commerciale[
  B_Structure_Commerciale$SECTEUR_COMMERCIAL != 'HORS RESEAU',]
#only keep cols we care about
B_Structure_Commerciale <- B_Structure_Commerciale[,-2]
#Join together
tot_info <- merge(x=tot_info, y=B_Structure_Commerciale, by='CD_COMMERCIAL_CHARGE',all.x=TRUE)

#Join with actual complaint information
tot_info <- merge(x=tot_info, y=B_Reclamations_clients[,c('ID_GRC','TYPE','SOUS_TYPE')],
                  by='ID_GRC', all=TRUE)

#Filter out obvservations that don't have specified type
tot_info <- drop_na(tot_info, 'TYPE')

#join with advantage given to customer
B_Avantages_clients <- B_Avantages_clients[,c('ID_GRC','CODE_AVG','STATUT')]
colnames(B_Avantages_clients) <- c('ID_GRC','advantage','adv_consume')
tot_info <- merge(x=tot_info, y=B_Avantages_clients, by='ID_GRC', all.x=TRUE)

#join action information
B_Actions_rattachees_demandes <- B_Actions_rattachees_demandes[,c('ID_GRC','TYPE','SOUS_TYPE')]
colnames(B_Actions_rattachees_demandes) <- c('ID_GRC','action_type','action_sub_type')
tot_info <- merge(x=tot_info, y=B_Actions_rattachees_demandes, by='ID_GRC', all.x=TRUE)


#Split survey scores into ranks
survey <- surveys[[survey_num]][2:nrow(surveys[[survey_num]]),c('Q1','Meta_donnee.3')]
survey_name <- survey_names[[survey_num]]
survey[,'Q2'] <- survey$Q1
survey <- drop_na(survey,'Q1')
for(i in 1:nrow(survey)){
  check <- i
  if(survey$Q1[i] >= 7){
    survey$Q2[i] = 'High'
  } else {
    if((survey$Q1[i] >= 4) & (survey$Q1[i] < 7)){
      survey$Q2[i] = 'Med'
    }else{
      survey$Q2[i] = 'Low'
    }}
}
survey <- survey[,c('Q2','Meta_donnee.3')]
colnames(survey) <- c('Survey_rank','ID_GRC')



#Join survey score
tot_info <- merge(x=tot_info, y=survey, by='ID_GRC', all=TRUE )


#Label data in columns so easier to understand apriori results
tot_info$TRANCHE_AGE <- sapply(tot_info$TRANCHE_AGE,function(x)paste('Age:',x),simplify=TRUE)
tot_info$SEXE<- sapply(tot_info$SEXE,function(x)paste('Sex:',x),simplify=TRUE)
tot_info$MARCHE_PSO <- sapply(tot_info$MARCHE_PSO,function(x)paste('Activity:',x),simplify=TRUE)
tot_info$TYPOLOGIE <- sapply(tot_info$TYPOLOGIE,function(x)paste('Area:',x),simplify=TRUE)
tot_info$NATURE_PERSONNE <- sapply(tot_info$NATURE_PERSONNE,function(x)paste('Nature:',x),simplify=TRUE)
tot_info$TYPE <- sapply(tot_info$TYPE,function(x)paste('Complaint Type:',x),simplify=TRUE)
tot_info$SOUS_TYPE <- sapply(tot_info$SOUS_TYPE,function(x)paste('Complaint Sub_Type:',x),simplify=TRUE)
tot_info$advantage <- sapply(tot_info$advantage,function(x)paste('Advantage:',x),simplify=TRUE)
tot_info$adv_consume <- sapply(tot_info$adv_consume,function(x)paste('Consume Advantage:',x),simplify=TRUE)
tot_info$action_type <- sapply(tot_info$action_type,function(x)paste('Action Type:',x),simplify=TRUE)
tot_info$action_sub_type <- sapply(tot_info$action_sub_type,function(x)paste('Action Sub Type:',x),simplify=TRUE)
tot_info$Survey_rank <- sapply(tot_info$Survey_rank,function(x)paste('Survey Response:',x),simplify=TRUE)


#Combine into 2 col matrix of ID and attribute
apr_ready <- tot_info[,c('ID_GRC','TRANCHE_AGE')]
for(i in 4:length(colnames(tot_info))){
  new_col <- colnames(tot_info)[i]
  colnames(apr_ready) <- c('ID_GRC',new_col)
  apr_ready <- rbind(apr_ready,tot_info[,c('ID_GRC',new_col)])
}
colnames(apr_ready) <- c('id','att')

#Remove any remaining irrelevant examples
apr_ready[grepl('NULL', apr_ready$att),] = NA
apr_ready[grepl(': NA', apr_ready$att),] = NA
apr_ready <- drop_na(apr_ready,'id')
apr_ready <- drop_na(apr_ready,'att')


#Convert to basket format for apriori
#sort
apr_ready <- apr_ready[order(apr_ready$id),]
#numeric id
apr_ready$id <- as.numeric(apr_ready$id)
#Remove duplicates
apr_ready <- distinct(apr_ready)
#Concate attributes by cust id
apr_ready <- ddply(apr_ready,c('id'),
                   function(apr_ready)paste(apr_ready$att,
                                            collapse = ','))

#Remove id column
apr_ready$id <- NULL
colnames(apr_ready) <- c('itemList')


#Save to csv
csv_name <- paste(survey_name,'apriori_table.csv',sep='_')
write.csv(apr_ready,csv_name,row.names = FALSE)

######################################
# **** IMPORTANT *****
# Must open csv file and replace every , with ","
# and then save before the next step
######################################

#Load apr_ready df into transactions table
apr_ready <- read.transactions(file=csv_name,
                               rm.duplicates = TRUE, format='basket',
                               sep=',', cols = NULL)



#apr_ready@itemInfo$labels <- gsub("\"","",apr_ready@itemInfo$labels)
survey_rules <- apriori(apr_ready,parameter = list(sup = 0.01,conf = 0.5,target="rules"))
survey_rules <- as(survey_rules,"data.frame")
View(survey_rules)

rules_name <- paste(survey_name,'rule.csv',sep='_')
write.csv(survey_rules,rules_name)

#############################################
# Q 3.2)
#############################################
#Load files
dir <- 'D:/GD/MLDM/Big Data/Project/big_data_project_confidential/'

B_Donnes_clients <- read_csv2(paste(dir,"BASE_Donnees_Clients.csv",sep = ''))

S_AUTO_BDG <- read_csv2(paste(dir,"SATISFACTION_AUTO_BDG_2015_2016_2017.csv", sep= ''))
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

survey <- rbind(S_RECLAMATION[2:nrow(S_RECLAMATION),c('Q1','Meta_donnee 3')],
                S_AUTO_BDG[2:nrow(S_AUTO_BDG),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_AUTO_CLASSIQUE[2:nrow(S_AUTO_CLASSIQUE),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_AUTO_CLASSIQUE_TMA[2:nrow(S_AUTO_CLASSIQUE_TMA),c('Q1','Meta_donnee 3')])
survey <- rbind(survey, S_AUTOPRESTO[2:nrow(S_AUTOPRESTO),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_AUTO_TMA_BDG[2:nrow(S_AUTO_TMA_BDG),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_DAB[2:nrow(S_DAB),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_DEMANDE[2:nrow(S_DEMANDE),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_DENTAIRE_NON_PARTENAIRE[2:nrow(S_DENTAIRE_NON_PARTENAIRE),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_DENTAIRE_PARTENAIRE[2:nrow(S_DENTAIRE_PARTENAIRE),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_MODIFICATION[2:nrow(S_MODIFICATION),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_OPTIQUE_NON_PARTENAIRE[2:nrow(S_OPTIQUE_NON_PARTENAIRE),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_SOUSCRIPTION[2:nrow(S_SOUSCRIPTION),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_SUIVI_PROACTIF[2:nrow(S_SUIVI_PROACTIF),c('Q1','Meta_donnee 3')])

#Split survey scores into ranks
colnames(survey) <- c('Survey_rank','ID_GRC')

#Combine all revelant information we will need for apriori
tot_info <- B_Donnes_clients[,c('ID_GRC', 'MT_IARD_2015','MT_IARD_2016',
                                'MT_IARD_2017')]
tot_info <- merge(x=tot_info, y=survey, by='ID_GRC', all=TRUE )

tot_info <- tot_info[complete.cases(tot_info), ]
tot_info$ID_GRC <- NULL
tot_info$MT_IARD_2015 <- as.numeric(tot_info$MT_IARD_2015)
tot_info$MT_IARD_2016 <- as.numeric(tot_info$MT_IARD_2016)
tot_info$MT_IARD_2017 <- as.numeric(tot_info$MT_IARD_2017)
tot_info$Survey_rank <- as.numeric(tot_info$Survey_rank)
#Plot data
plot(tot_info)

#Look at correlation
cor(tot_info)
class(tot_info$MT_IARD_2015)

#Run linear regression
income_reg <- lm(Survey_rank~.,data=tot_info)
summary(income_reg)
plot(income_reg)

#############################################
# Q 3.3)
#############################################
#Load files
dir <- 'D:/GD/MLDM/Big Data/Project/big_data_project_confidential/'

B_Donnes_clients <- read_csv2(paste(dir,"BASE_Donnees_Clients.csv",sep = ''))
B_Avantages_clients <- read_csv2(paste(dir,"BASE_Avantages_clients.csv",sep = ''))

S_AUTO_BDG <- read_csv2(paste(dir,"SATISFACTION_AUTO_BDG_2015_2016_2017.csv", sep= ''))
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

survey <- rbind(S_RECLAMATION[2:nrow(S_RECLAMATION),c('Q1','Meta_donnee 3')],
                S_AUTO_BDG[2:nrow(S_AUTO_BDG),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_AUTO_CLASSIQUE[2:nrow(S_AUTO_CLASSIQUE),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_AUTO_CLASSIQUE_TMA[2:nrow(S_AUTO_CLASSIQUE_TMA),c('Q1','Meta_donnee 3')])
survey <- rbind(survey, S_AUTOPRESTO[2:nrow(S_AUTOPRESTO),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_AUTO_TMA_BDG[2:nrow(S_AUTO_TMA_BDG),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_DAB[2:nrow(S_DAB),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_DEMANDE[2:nrow(S_DEMANDE),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_DENTAIRE_NON_PARTENAIRE[2:nrow(S_DENTAIRE_NON_PARTENAIRE),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_DENTAIRE_PARTENAIRE[2:nrow(S_DENTAIRE_PARTENAIRE),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_MODIFICATION[2:nrow(S_MODIFICATION),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_OPTIQUE_NON_PARTENAIRE[2:nrow(S_OPTIQUE_NON_PARTENAIRE),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_SOUSCRIPTION[2:nrow(S_SOUSCRIPTION),c('Q1','Meta_donnee 3')])
survey <- rbind(survey,S_SUIVI_PROACTIF[2:nrow(S_SUIVI_PROACTIF),c('Q1','Meta_donnee 3')])

#Split survey scores into ranks
survey[,'Q2'] <- survey$Q1
survey <- drop_na(survey,'Q1')
for(i in 1:nrow(survey)){
  check <- i
  if(survey$Q1[i] >= 7){
    survey$Q2[i] = 'High'
  } else {
    if((survey$Q1[i] >= 4) & (survey$Q1[i] < 7)){
      survey$Q2[i] = 'Med'
    }else{
      survey$Q2[i] = 'Low'
    }}
}
survey <- survey[,c('Q1','Meta_donnee 3')]
colnames(survey) <- c('Survey_rank','ID_GRC')


#Combine advantage into
B_Avantages_clients <- B_Avantages_clients[,c('ID_GRC','CODE_AVG')]
colnames(B_Avantages_clients) <- c('ID_GRC','advantage')
tot_info <- merge(x=B_Avantages_clients, y=survey, by='ID_GRC', all=TRUE)
tot_info$ID_GRC <- NULL

#Runa decision tree to look from advantages to survey reults
adv_tree <- rpart(Survey_rank ~ .,
                  method="class", data=tot_info)
# display the results
printcp(adv_tree) 
# visualize cross-validation results
plotcp(adv_tree) 
# detailed summary of splits
summary(adv_tree) 

# plot tree
plot(adv_tree, uniform=TRUE,
     main="Advantage vs Satisfaction Decision Tree")
text(adv_tree, use.n=TRUE, all=TRUE, cex=.8)



income_info <- B_Donnes_clients[,c('ID_GRC', 'MT_IARD_2015','MT_IARD_2016',
                                   'MT_IARD_2017')]
income_info <- merge(x=B_Avantages_clients, y=income_info, by='ID_GRC', all=TRUE )

income_info$ID_GRC <- NULL
income_info$MT_IARD_2015 <- as.numeric(income_info$MT_IARD_2015)
income_info$MT_IARD_2016 <- as.numeric(income_info$MT_IARD_2016)
income_info$MT_IARD_2017 <- as.numeric(income_info$MT_IARD_2017)
#income_info$Survey_rank <- as.numeric(income_info$Survey_rank)
income_info <- income_info[complete.cases(income_info), ]


#Runa decision tree to look from advantages to income
adv_tree <- rpart(advantage ~ .,
                  method="class", data=tot_info)
# display the results
printcp(adv_tree) 
# visualize cross-validation results
plotcp(adv_tree) 
# detailed summary of splits
summary(adv_tree) 

# plot tree
plot(adv_tree, uniform=TRUE,
     main="Advantage vs Satisfaction Decision Tree")
text(adv_tree, use.n=TRUE, all=TRUE, cex=.8)

