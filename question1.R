
# Create hashmap (key = user id, value = average satisfaction)

SATISFACTION_AUTO_BDG_2015_2016_2017[1,'Meta_donnee 3']

library(hashmap)
H <- hashmap(c(0),c(0))
H$clear()
H$empty()

x <- nrow(SATISFACTION_AUTO_BDG_2015_2016_2017)
for (i in seq.int(from=2, to=x)){
  print(SATISFACTION_AUTO_BDG_2015_2016_2017[i,'Meta_donnee 3'])
  #if (H$has_key(table[i,'Meta_donnee 3'])){
      
  #}else{
      
  #}
    
}

