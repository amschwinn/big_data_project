

library(hashmap)
Average_Satisfaction <- hashmap(c(0),c(0))  #  (key = user id, value = average satisfaction)
num_surveys <- hashmap(c(0),c(0))           # (key = user id, value = num of surveys processed)
Average_Satisfaction$clear()
num_surveys$clear()

x <- nrow(SATISFACTION_AUTO_BDG_2015_2016_2017)
for (i in seq.int(from=2, to=x)){
  key <- as.numeric(SATISFACTION_AUTO_BDG_2015_2016_2017[i,'Meta_donnee 3'])
  if (Average_Satisfaction$has_key(key)){
    avg <- Average_Satisfaction[[key]]
    num <- num_surveys[[key]]
    new_avg <- ((avg * num) + as.numeric(SATISFACTION_AUTO_BDG_2015_2016_2017[i,1])) / (num + 1)
    Average_Satisfaction[[key]] <- new_avg
    num_surveys[[key]] <- num + 1
  }else{
    Average_Satisfaction[[key]] <- as.numeric(SATISFACTION_AUTO_BDG_2015_2016_2017[i,1])
    num_surveys[[key]] <- 1
  }
}


