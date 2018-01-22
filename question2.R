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
library(plyr)
library(dplyr)
library(readr)
library(arules)

#############################################
# Q 2.1)
#############################################

####
# Load relevant files with the base_survey.R script
# We will use the following dataframes from that script:
# - B_Donnes_clients
# - B_Structure_Commerciale
# - B_Reclamations_clients
####

#Load files
#You have to change the path if you use it in your own machine.

dir <- 'C:/Users/Ts0/Documents/big_data_project_confidential/'

B_Donnes_clients <- read_csv2(paste(dir,"BASE_Donnees_Clients.csv", sep= ''))
B_Reclamations_clients <- read_csv2(paste(dir,"BASE_Reclamations_clients.csv",sep = ''))
B_Structure_Commerciale <- read_csv2(paste(dir,"BASE_Structure_Commerciale.csv",sep = ''))
B_Actions_rattachees_demandes <- read_csv2(paste(dir,"BASE_Actions_rattachees_demandes.csv",sep = ''))
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
survey <- survey[,c('Q2','Meta_donnee 3')]
colnames(survey) <- c('Survey_rank','ID_GRC')


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

#Join survey score
tot_info <- merge(x=tot_info, y=survey, by='ID_GRC', all.x=TRUE )


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
d <- data.frame(lapply(apr_ready, function(x) {gsub(',', '","', x)}))

#Save to csv
write.csv2(d,'apriori_table.csv',row.names = FALSE)

######
# **** IMPORTANT *****
# Must open csv file and replace every , with ","
# and then save before the next step
# For example use Notepad++, use find-replace command and replace (,) with (",")

#BUT, for convenience we provide the 'apriori_table1.csv' file which is in an
#appropriate format to load.

#We tried to fix it by coding exlusively in R, but instead of replacing with (",")
#it was replaced by ("",""). 
#E.g.: #apr_ready <- data.frame(lapply(apr_ready, function(x) {gsub(',', '","', x)}))

######

#Load apr_ready df into transactions table
apr_ready <- read.transactions(file="apriori_table1.csv",
                               rm.duplicates = TRUE, format="basket",
                               sep=",", cols = NULL)



#apr_ready <- data.frame(lapply(apr_ready, function(x) {gsub(',', '","', x)}))

#sapply(apr_ready@itemInfo$labels, sub, pattern="", replacement=",")
#apr_ready@itemInfo$labels <- gsub("\"","",apr_ready@itemInfo$labels)

complaint_rules <- apriori(apr_ready,parameter = list(supp = 0.01,conf = 0.5, target="rules"))
complaint_rules <- as(complaint_rules,"data.frame")
View(complaint_rules)


#############################################
# Q 2.2
#############################################
library(NLP)
library(tm)
library(XML)
library(SnowballC)
library(slam)
library(ggplot2)
library(wordcloud)
library(cluster)

#Load complaints into corpus
complaints <- Corpus(VectorSource(B_Reclamations_clients$COMMENTAIRE_DEMANDE))

#Check encoding
for (cmp in 1:length(complaints)) {
  utf8::utf8_format(complaints[[cmp]][["content"]])
  Encoding(complaints[[cmp]][["content"]]) <- "UTF-8"
}

#Remove whitespace
complaints <- tm_map(complaints,stripWhitespace)
writeLines(as.character(complaints[[1]]))

#Eliminate stop words
complaints <- tm_map(complaints,removeWords,stopwords("french"))
writeLines(as.character(complaints[[1]]))

#Stem to roots words
#complaints <- tm_map(complaints,stemDocument)
#writeLines(as.character(complaints[[1]]))

#Convert all to lower case
complaints <- tm_map(complaints, content_transformer(tolower))
writeLines(as.character(complaints[[1]]))

#remove numbers
complaints <- tm_map(complaints, removeNumbers)
writeLines(as.character(complaints[[1]]))

#remove punctuation
complaints <- tm_map(complaints, removePunctuation)
writeLines(as.character(complaints[[1]]))

#Inspect term frequency
dtm <- DocumentTermMatrix(complaints)
inspect(dtm)

#Remove sparce terms
dtm <- removeSparseTerms(dtm,0.99)
#inspect(dtm)

#Transform from term doc matrix to plain text matrix
dtm_mat <- as.matrix(dtm)


#Text Data Mining
freq <- colSums(dtm_mat)

#index size
length(freq)

#list most frequent terms by ordering by frequenc
ord <- order(freq)

#Least frequent terms
freq[head(ord)]

#most frequent terms
freq[tail(ord)]

#Plotting word frequencies
freq <- sort(colSums(dtm_mat), decreasing = TRUE)
head(freq,14)

wf <- data.frame(word=names(freq),freq=freq)
head(wf)

barplot(freq)

#make word cloud
wordcloud(names(freq),freq,max.words=15)

#Frequent itemsets and associations
findFreqTerms(dtm,5)

#Find words having a correlation rate higher of equal to .5
c_names <- colnames(dtm)
c_values <- rep(0.5, length(c_names))
findAssocs(dtm,c_names,c_values)

#Clustering of terms
#heirachial clustering
d   <- dist(t(dtm), method="euclidean")
fit <- hclust(d=d,method="ward.D2")
fit
plot(fit, hang=-1)
