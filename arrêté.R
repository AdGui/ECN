library(plyr)
library(ggplot2)
library(magrittr)

BDD <- read.csv(paste0(getwd(), "/data/2010.csv"), header=FALSE, sep=";")

#rang
rang <- c()
for(i in 1:length(BDD[,1])){
  rang[i] <- substr(BDD[i,1], start=1 , stop=gregexpr(pattern="[[:upper:]]",BDD[i,1])[[1]][1][1])
}
rang <- gsub(x=rang, pattern="[[:upper:]]", replacement="")
rang <- as.numeric(rang)

#sexe
sexe <- c()
for(i in 1:length(BDD[,1])){
  sexe[i] <- gregexpr(pattern="[M]\\.", BDD[i,1])[[1]][1]
}
sexe <- unlist(sexe)
sexe <- as.character(sexe)
sexe[which(sexe=="-1")] <- "f"
sexe[-which(sexe=="f")] <- "m"


#nom
nom <- c()
for(i in 1:length(BDD[,1])){
  nom[i] <- substr(BDD[i,1], start=gregexpr(pattern="[[:upper:]]",BDD[i,1])[[1]][1] , stop=gregexpr(pattern="\\(",BDD[i,1])[[1]][1])
}
nom <- gsub(x=nom, pattern="\\(", replacement="")
for(i in 1:length(BDD[,1])){
  nom[i] <- substr(nom[i], start=gregexpr(pattern="[[:space:]]",nom[i])[[1]][1]+1 , stop=nchar(nom[i],type="bytes")-1)
}

#prenom
prenom <- c()
for(i in 1:length(BDD[,1])){
  prenom[i] <- substr(BDD[i,1], start=gregexpr(pattern="\\(",BDD[i,1])[[1]][1]+1 , stop=gregexpr(pattern="\\)",BDD[i,1])[[1]][1]-1)
}

#ddn
ddn <- c()
for(i in 1:length(BDD[,1])){
  ddn[i] <- substr(BDD[i,1], start=gregexpr(pattern="\\)\\,",BDD[i,1])[[1]][1] + 3 , stop=gregexpr(pattern="[0-9]\\,",BDD[i,1])[[1]][1])
}
ddn <- gsub(x=ddn, pattern=".*[[:space:]][l][e][[:space:]]",replacement="")
ddj <- gsub(x=ddn, pattern="[[:space:]][a-z].*", replacement="")
ddj <- gsub(x=ddj, pattern="[a-z]*", replacement="")
dda <- gsub(x=ddn, pattern="[[:digit:]]*[[:space:]]", replacement="")
dda <- gsub(x=dda, pattern="[a-zé]", replacement="")
for(i in 1:length(dda)){dda[i] <- substr(dda[i], start=nchar(dda[i])-3,stop=nchar(dda[i]))}
ddm <- gsub(x=ddn, pattern="[[:digit:]]*", replacement="")
ddm <- gsub(x=ddm, pattern="[e][r]",replacement="")
ddm <- as.factor(ddm)
m <- c("08","04","12","02","01","07","06","05","03","11","10","09")
ddm <- mapvalues(ddm, from=levels(ddm), to=m)

df <- data.frame(attr(summary(as.factor(ddm)),"names"),summary(as.factor(ddm)))
ggplot()+geom_histogram(data=df,aes(x=df[,1], y=df[,2]),stat="identity")



#SubDis
subdis <- c()
BDD[,1] <- as.character(BDD[,1])
for(i in 1:length(BDD[,1])){
  subdis[i] <- substr(BDD[i,1], start=gregexpr(pattern="\\)\\,.*",BDD[i,1])[[1]][1] , stop=nchar(BDD[i,1],type="bytes"))
}
subdis <- gsub(x=subdis, pattern="\\)\\,.*\\,",replacement="")
sub <- gsub(x=subdis, pattern=".*[[:space:]][à][[:space:]]",replacement="")
sub <- gsub(x=sub, pattern=".*[[:space:]][e][n][[:space:]]",replacement="")
sub <- gsub(x=sub, pattern=".*[[:space:]][a][u][x][[:space:]]",replacement="")
sub <- gsub(x=sub, pattern="\\.",replacement="")
dis <- gsub(x=subdis, pattern="[[:space:]][à][[:space:]].*",replacement="")
dis <- gsub(x=dis, pattern="[[:space:]][e][n][[:space:]].*",replacement="")
dis <- gsub(x=dis, pattern="[[:space:]][a][u][x][[:space:]].*",replacement="")

df.dum <- data.frame(nom,prenom,sexe,ddj,ddm,dda,rang,sub,dis)

write.csv(df.dum, file="BDD2010")

#### corrections ####
#### 2011
#BDD[,1] <- as.character(BDD[,1])
#BDD[6851,1]
#BDD[3003,1] 
#BDD[,1] <- as.factor(BDD[,1])

#### 2010
#ddn : start=gregexpr(pattern="\\)\\,",BDD[i,1])[[1]][1] ## + 3 ##
#BDD[,1] <- BDD[,1] %>% as.character
#BDD[242,1]
#BDD[476,1] 
#BDD[957,1] 
#BDD[1907,1] 
#BDD[1908,1] 
#BDD[5515,1] 
#BDD[3836,1] 
#BDD[5217,1] 
#BDD[4528,1] 
#BDD[,1] <- BDD[,1] %>% as.factor

