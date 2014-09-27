library(shiny)
library(RCurl)
library(XML)
library(plyr)
library(ggplot2)
library(stringr)

Offre_data <- read.csv("./data/Offre_data.csv",fileEncoding = "UTF-8")

shinyServer(function(input,output){

  row.names(Offre_data) <- Offre_data[,1]
  Offre_data <- Offre_data[,-1]
  
  dataset <- reactive({
    if(input$Choix.BDD=="simulations2014"){
      con <- file("./data/ECN2014_simulations.prn",encoding="UTF-8")
    }
    if(input$Choix.BDD=="affectations2014"){
      con <- file("./data/ECN2014_affectations.prn",encoding="UTF-8")
    }
    ECN_data <- read.csv(con, sep=",")
    colnames(ECN_data)[10] <-"Désir (non officiel)" 
    ECN_data <- ECN_data[,-1]
    ECN_data
  })
  
#  dataset <- reactive({
#    input$runButton
#    ECN_data <- isolate({
#      ECN <- getURL("https://www.cngsante.fr/chiron2014/celine/listing.html", ssl.verifypeer = FALSE, .encoding="UTF-8")
#      ECN_data.dum <- readHTMLTable(ECN,stringsAsFactors = FALSE)
#      ECN_data <- ECN_data.dum[[1]]
#      rm(ECN_data.dum)
#      rm(ECN)
#      colnames(ECN_data) <- ECN_data[9,]
#      colnames(ECN_data)[5] <-"Voeu"
#      colnames(ECN_data)[9] <-"Désir (non officiel)" 
#      ECN_data <- ECN_data[-c(1:9),]
#      ECN_data <- ECN_data[-which(ECN_data[,1]=="testeur"),]
#      ECN_data <- ECN_data[-which(ECN_data[,1]=="déclassé"),]
#      ECN_data[,4] <- gsub(pattern=" \\)",replacement="",gsub(pattern="\\d* \\( ",replacement="",ECN_data[,4]))
#      ECN_data
#    })
#    #pre<-read.csv("./data/prechoix.csv",sep=";",encoding="UTF-8")
#    #row.names(pre) <- pre[,1]
#    #pre <- pre[,-1]
#    #ECN_data <- pre
#    return(ECN_data)
#  })
  
#  actu <- reactive({
#    ECN <- getURL("https://www.cngsante.fr/chiron2014/celine/listing.html", ssl.verifypeer = FALSE, .encoding="UTF-8")
#    input$runButton
#    date <- isolate({
#      date <- substr(substr(ECN, start=8374, stop=8400),start=14,stop=20)
#      h <- substr(substr(ECN, start=8374, stop=8400),start=1,stop=5)
#      dateh <- paste(h, date)
#      return(dateh)
#    })
#    return(date)
#  })

  
#  output$date <- renderText({
#    dateh <- actu()
#    date <- substr(dateh, start=7,stop=13)
#    return(date)
#  })
  
  output$sim1 <- renderText({
    ECN_data <- dataset()
    ECN_data.dum<-ECN_data[-which(as.factor(ECN_data[,3])=="CESP"),]
    ECN_data.dum<-ECN_data.dum[-which(as.factor(ECN_data.dum[,1])=="ESSA"),]
    ECN_data.dum<-ECN_data.dum[-which(as.factor(ECN_data.dum[,1])=="invalidé"),]
    str <- substr(as.factor(ECN_data.dum[,8]),start=1,stop=2)
    nbr.t <- length(which(str=="Di"))+length(which(str=="ca"))+length(which(str=="ma"))+length(which(str=="Sp"))
    texte1 <- paste("Sur les", length(ECN_data[,1]),
      "étudiants ayant passé les ECN :",
      nbr.t,
      "sont passés par les simulations classiques,",
      length(ECN_data[which(as.factor(ECN_data[,3])=="CESP"),1]),
      "sont passés par les simulations CESP,",
      length(ECN_data[which(as.factor(ECN_data[,1])=="ESSA"),1]),
      "étaient des étudiants des armées et",
      length(ECN_data[which(as.factor(ECN_data[,1])=="invalidé"),1]),
      "ont redoublé")
    return(texte1)
  })
  
##  output$sim2 <- renderText({
#    ECN_data <- dataset()
#    ECN_data.dum<-ECN_data[-which(as.factor(ECN_data[,3])=="CESP"),]
#    ECN_data.dum<-ECN_data.dum[-which(as.factor(ECN_data.dum[,1])=="ESSA"),]
#    ECN_data.dum<-ECN_data.dum[-which(as.factor(ECN_data.dum[,1])=="invalidé"),]
#    str <- substr(as.factor(ECN_data.dum[,8]),start=1,stop=2)
#    nbr.t <- length(which(str=="Di"))+length(which(str=="ca"))+length(which(str=="ma"))+length(which(str=="Sp"))
#    aff <- length(which(as.factor(ECN_data.dum[,1])=="affecté"))
#    aff.p <- round(aff/nbr.t,2)*100
#    sim <- (length(which(str=="Di"))+length(which(str=="Sp")))-aff
#    sim.p <- round(sim/nbr.t,2)*100
#    rie <- length(which(str=="ca"))
#    rie.p <- round(rie/nbr.t,2)*100
#    mal <- length(which(str=="ma"))
#    mal.p <- round(mal/nbr.t,2)*100
#    texte2 <- paste("Avancement des choix :", aff,"affectés (",aff.p,"%),",sim,"simulés (",sim.p,"%),", mal ,"non simulés malgré un ou plusieurs voeux (", mal.p,"%),", rie ,"non simulés car aucun choix (", rie.p,"%).")
#    return(texte2)
#  })
  
#  output$h <- renderText({
#     dateh <- actu()
#     h <- substr(dateh, start=1,stop=5)
#     return(h)
#  })
  
  output$table <- renderDataTable({
    ECN_data <- dataset()
    ville.vec<-as.numeric(substr(ECN_data[,11],start=2,stop=4))
    spe.vec<-as.numeric(substr(ECN_data[,11],start=5,stop=7))
    ECN_data[,8] <- as.factor(ECN_data[,8])
    ECN_data[,7] <- as.factor(ECN_data[,7])
    ECN_data[,4] <- as.numeric(ECN_data[,4])
    if(input$Ville != 0){
      if(input$Spe != 0){
        ECN_comb<-ECN_data[which(ville.vec==input$Ville & spe.vec==input$Spe),]
        if(length(ECN_comb[,1])==0){ 
          df<-data.frame(v1="pas de candidat pour cette combinaison")
        } else {
          df<-ECN_comb
        }
      }
    }        
    if(input$Ville != 0){
      if(input$Spe == 0){
        ECN_comb<-ECN_data[which(ville.vec==input$Ville),]
        if(length(ECN_comb[,1])==0){ 
          df<-data.frame(v1="pas de candidat pour cette combinaison")
        } else {
          df<-ECN_comb
        }
      }
    }
    if(input$Ville == 0){
      if(input$Spe != 0){
        ECN_comb<-ECN_data[which(spe.vec==input$Spe),]
        if(length(ECN_comb[,1])==0){ 
          df<-data.frame(v1="pas de candidat pour cette combinaison")
        } else {
          df<-ECN_comb
        }
      }
    }
    if(input$Ville == 0){
      if(input$Spe == 0){
        ECN_comb<-ECN_data
        if(length(ECN_comb[,1])==0){ 
          df<-data.frame(v1="pas de candidat pour cette combinaison")
        } else {
          df<-ECN_comb
          
        }
      }
    }
    if(length(df[1]) != 1){
      colnames(df) <- colnames(ECN_data)
      rownames(df) <- 1:length(df[,1])
    } else {}
    df
  })
  
  output$plot_spe <- renderPlot({
    ECN_data <- dataset()
    xlab_spe <- c("AR","Bi","GyM","GyO","MT","MG","Psy","Ped","SP","ChOr","ChG","ChN","Opht","ORL","An","Ca","De","En","Ga","GM","He","MI","MN","MPR","Neu","Nep","Onc","Pne","Rad","Rhu")
    ECN_data.dum <- ECN_data[-which(ECN_data[,11]==""), ]
    ECN_data.dum[,8] <- as.character(ECN_data.dum[,8])
    ECN_data.dum[,8] <- as.factor(ECN_data.dum[,8])
    ECN_data.dum[,4] <- as.numeric(ECN_data.dum[,4])
    ECN_data.dum[,8] <- mapvalues(ECN_data.dum[,8],from=levels(ECN_data.dum[,8]),to=xlab_spe)
    df_order <- ddply(ECN_data.dum, .(Discipline), summarize, 
                      median=median(Etudiant,na.rm=T),
                      mean=mean(Etudiant,na.rm=T),
                      max=max(Etudiant,na.rm=T),
                      min=min(Etudiant,na.rm=T),
                      TQuart=quantile(Etudiant, 0.75,na.rm=T),
                      PQuart=quantile(Etudiant, 0.25,na.rm=T)
    )
    
    if(input$meth.order=="median"){
      ECN_data.dum[,8] <- factor(ECN_data.dum[,8], levels = xlab_spe[order(df_order$median)])
    }
    if(input$meth.order=="mean"){
      ECN_data.dum[,8] <- factor(ECN_data.dum[,8], levels = xlab_spe[order(df_order$mean)])
    }
    if(input$meth.order=="max"){
      ECN_data.dum[,8] <- factor(ECN_data.dum[,8], levels = xlab_spe[order(df_order$max)])
    }
    if(input$meth.order=="min"){
      ECN_data.dum[,8] <- factor(ECN_data.dum[,8], levels = xlab_spe[order(df_order$min)])
    }
    if(input$meth.order=="TQ"){
      ECN_data.dum[,8] <- factor(ECN_data.dum[,8], levels = xlab_spe[order(df_order$TQuart)])
    }
    if(input$meth.order=="PQ"){
      ECN_data.dum[,8] <- factor(ECN_data.dum[,8], levels = xlab_spe[order(df_order$PQuart)])
    }
    
    plot <- ggplot(ECN_data.dum, aes(x=Discipline,y=Etudiant))+
      geom_boxplot()+
      ylab("Classement")+
      xlab("Disciplines")
    print(plot)
  })
  
  output$plot_ville <- renderPlot({
    ECN_data <- dataset()
    xlab_ville <- c("AixM","Ami","Ang","AntG","Bes","Bord","Bre","Cae","CleF","Dij","Gre","IDF","Lil","Lim","Ly","Mon","Nanc","Nant","Nic","OceI","Poi","Rei","Ren","Rou","StE","Stra","Toul","Tour")
    ECN_data.dum <- ECN_data[-which(ECN_data[,11]==""), ]
    ECN_data.dum[,7] <- as.character(ECN_data.dum[,7])
    ECN_data.dum[,7] <- as.factor(ECN_data.dum[,7])
    ECN_data.dum[,4] <- as.numeric(ECN_data.dum[,4])
    ECN_data.dum[,7] <- mapvalues(ECN_data.dum[,7],from=levels(ECN_data.dum[,7]),to=xlab_ville)
    df_order <- ddply(ECN_data.dum, .(Subdivision), summarize, 
                      median=median(Etudiant,na.rm=T),
                      mean=mean(Etudiant,na.rm=T),
                      max=max(Etudiant,na.rm=T),
                      min=min(Etudiant,na.rm=T),
                      TQuart=quantile(Etudiant, 0.75,na.rm=T),
                      PQuart=quantile(Etudiant, 0.25,na.rm=T)
    )
    
    if(input$meth.order=="median"){
      ECN_data.dum[,7] <- factor(ECN_data.dum[,7], levels = xlab_ville[order(df_order$median)])
    }
    if(input$meth.order=="mean"){
      ECN_data.dum[,7] <- factor(ECN_data.dum[,7], levels = xlab_ville[order(df_order$mean)])
    }
    if(input$meth.order=="max"){
      ECN_data.dum[,7] <- factor(ECN_data.dum[,7], levels = xlab_ville[order(df_order$max)])
    }
    if(input$meth.order=="TQ"){
      ECN_data.dum[,7] <- factor(ECN_data.dum[,7], levels = xlab_ville[order(df_order$TQuart)])
    }
    if(input$meth.order=="min"){
      ECN_data.dum[,7] <- factor(ECN_data.dum[,7], levels = xlab_ville[order(df_order$min)])
    }
    if(input$meth.order=="TQ"){
      ECN_data.dum[,7] <- factor(ECN_data.dum[,7], levels = xlab_ville[order(df_order$TQuart)])
    }
    if(input$meth.order=="PQ"){
      ECN_data.dum[,7] <- factor(ECN_data.dum[,7], levels = xlab_ville[order(df_order$PQuart)])
    }
    plot <- ggplot(ECN_data.dum, aes(x=Subdivision,y=Etudiant))+
      geom_boxplot()+
      ylab("Classement")+
      xlab("Subdivisions")
    print(plot)
  })
  
  output$plot_sel_spe <- renderPlot({
    ECN_data <- dataset()
    if(input$Restcand == "oui"){
        min <- as.numeric(input$Rang.min)
        max <- as.numeric(input$Rang.max)
        ECN_data.dum <- ECN_data[min:max,]
    } else {
        min <- 1
        max <- length(ECN_data[,1])
        ECN_data.dum <- ECN_data
    }
    spe.vec <- as.numeric(substr(ECN_data[,11],start=5,stop=7))
    ville.vec <- as.numeric(substr(ECN_data[,11],start=2,stop=4))
    df_offre.comp <- data.frame(ECN_data.dum[min:max,4],spe.vec[min:max],ville.vec[min:max])

    Offre_vec_ville <- c(015,020,018,016,017,019,022,024,023,021,038,025,026,027,029,032,028,030,033,031,035,034,036,037,042,040,039,041,000)
    Offre_vec_spe <- c(11,4,3,5,9,6,2,10,7,12,13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 32, 29, 30, 31, 0)
    xlab_ville <- c("AixM","Ami","Ang","AntG","Bes","Bord","Bre","Cae","CleF","Dij","Gre","IDF","Lil","Lim","Ly","Mon","Nanc","Nant","Nic","OceI","Poi","Rei","Ren","Rou","StE","Stra","Toul","Tour")
    indic_ville <- c(034,021,028,041,016,037,029,022,038,017,025,015,023,039,026,035,018,030,036,042,031,019,032,024,027,020,040,033)
    xlab_spe <- c("AR","Bi","GyM","GyO","MT","MG","Psy","Ped","SP","ChOr","ChG","ChN","Opht","ORL","An","Ca","De","En","Ga","GM","He","MI","MN","MPR","Neu","Nep","Onc","Pne","Rad","Rhu")
    indic_spe <- c(004,003,005,009,006,011,010,002,007,032,028,029,030,031,012,013,014,015,016,017,018,019,020,021,023,022,024,025,026,027)
  
    if(input$Choix.indic=="abs"){
      ECN_data.dum[,8] <- as.factor(ECN_data.dum[,8])
      ECN_data.dum[,4] <- as.numeric(ECN_data.dum[,4])
      if(input$Ville == 0) {ECN_data.dum <- ECN_data.dum[-which(ECN_data.dum[,11]==""), ]} else {ECN_data.dum <- ECN_data.dum[which(ville.vec == input$Ville),]}
      ECN_data.dum[,8] <- mapvalues(ECN_data.dum[,8],from=levels(as.factor(as.character(ECN_data[-which(ECN_data[,11]==""), ][,8]))),to=xlab_spe)
      df_order.dum <- ddply(ECN_data.dum, .(Discipline), summarize, median=length(Etudiant))
      vec0 <- xlab_spe[-which(xlab_spe %in% df_order.dum$Discipline)]
      df_order <- data.frame(Discipline = c(as.character(df_order.dum$Discipline),vec0), Etudiant=c(df_order.dum[,2],rep(0,length(vec0))))
      df_order <- df_order[order(df_order[,2],decreasing=TRUE),]
      df_order[,1] <- factor(df_order[,1], levels = df_order[,1])
      df_order$Etudiant.p <- df_order$Etudiant+max(df_order$Etudiant)*0.03
      plot <- ggplot(df_order,aes(x=Discipline))+geom_histogram(aes(y=Etudiant),stat="identity")+xlab("Disciplines")+ylab("Postes pourvus")
      plot <- plot + geom_text(aes(y=Etudiant.p,label=Etudiant),color="black")
    }
    
    if(input$Choix.indic=="pourcent"){
      ECN_data.dum <- ECN_data.dum[-which(ECN_data.dum[,11]==""), ]
      ECN_data.dum[,8] <- as.factor(ECN_data.dum[,8])
      ECN_data.dum[,4] <- as.numeric(ECN_data.dum[,4])
      Pourvu <- c()
      Pourvu <- as.data.frame(Pourvu)
      for(i in Offre_vec_spe){
        for(j in Offre_vec_ville){
          poste.pourvu <- length(which(df_offre.comp[which(df_offre.comp[,2]==i),][,3]==j))
          if (length(poste.pourvu)==0){ 
            Pourvu[which(Offre_vec_ville==j),which(Offre_vec_spe==i)] <- 0
          } else { 
            Pourvu[which(Offre_vec_ville==j),which(Offre_vec_spe==i)] <- poste.pourvu
          }
        }
      }
      Pourvu[,dim(Pourvu)[2]] <- 0
      for(i in 1:(dim(Pourvu)[1]-1)){
        Pourvu[i,dim(Pourvu)[2]] <- sum(Pourvu[i,1:(dim(Pourvu)[2]-1)])
      }
      for(i in 1:(dim(Pourvu)[2])){
        Pourvu[dim(Pourvu)[1],i] <- sum(Pourvu[1:(dim(Pourvu)[1]-1),i])
      }
      #Pourcentage
      Pourcent_Pourvu <- c()
      Pourcent_Pourvu <- as.data.frame(Pourcent_Pourvu)
      for( i in 1:length(Pourvu[,1])){
        for( j in 1:length(Pourvu[1,])){
          Pourcent_Pourvu[i,j] <- Pourvu[i,j]/as.numeric(Offre_data[i,j])
        }
      }
      
      #Arrangements des données
      indic_spe <- c(004,003,005,009,006,011,010,002,007,032,028,029,030,031,012,013,014,015,016,017,018,019,020,021,023,022,024,025,026,027)
      xlab_spe <- c("AR","Bi","GyM","GyO","MT","MG","Psy","Ped","SP","ChOr","ChG","ChN","Opht","ORL","An","Ca","De","En","Ga","GM","He","MI","MN","MPR","Neu","Nep","Onc","Pne","Rad","Rhu")
      
      #xlab_spe[which(indic_spe==Offre_vec_spe[1])]
      for(i in 1:length(indic_spe)){
        colnames(Pourcent_Pourvu)[i] <- xlab_spe[which(indic_spe==Offre_vec_spe[i])]
      }
      colnames(Pourcent_Pourvu)[length(Pourcent_Pourvu[1,])] <-"Tot"
      
      xlab_ville <- c("AixM","Ami","Ang","AntG","Bes","Bord","Bre","Cae","CleF","Dij","Gre","IDF","Lil","Lim","Ly","Mon","Nanc","Nant","Nic","OceI","Poi","Rei","Ren","Rou","StE","Stra","Toul","Tour")
      indic_ville <- c(034,021,028,041,016,037,029,022,038,017,025,015,023,039,026,035,018,030,036,042,031,019,032,024,027,020,040,033)
      
      #xlab_ville[which(indic_ville==Offre_vec_ville[1])]
      for(i in 1:length(indic_ville)){
        rownames(Pourcent_Pourvu)[i] <- xlab_ville[which(indic_ville==Offre_vec_ville[i])]
      }
      rownames(Pourcent_Pourvu)[length(Pourcent_Pourvu[,1])] <-"Tot"

      df_order <- data.frame(Discipline=colnames(Pourcent_Pourvu), Pourcentage=unlist(Pourcent_Pourvu[which(Offre_vec_ville == input$Ville),,drop=TRUE])*100)
      df_order <- df_order[order(df_order[,2],decreasing=TRUE),]
      df_order[,1] <- factor(df_order[,1], levels = df_order[,1])
      if(length(which(is.na(df_order[,2])))==0) {
      } else {
        df_order <- df_order[-which(is.na(df_order[,2])),]
      }
      col <- rep("black", length(df_order[,1]))
      col[which(df_order[,1]=="Tot")] <- "red"
      df_order$Pourcentage.p <- df_order$Pourcentage+max(df_order$Pourcentage)*0.03
      plot <- ggplot(df_order,aes(x=Discipline))+geom_histogram(aes(y=Pourcentage),stat="identity",fill=col, col="white")+xlab("Disciplines")
      plot <- plot + geom_text(aes(y=Pourcentage.p,label=paste0(round(Pourcentage,1),"%")),color="black")
    }
    
    if(input$Choix.indic=="offre"){
      datad <- unlist(Offre_data[which(Offre_vec_ville==input$Ville),-length(Offre_data[1,]),drop=TRUE])
      datad[which(is.na(datad))] <- 0
      df <- data.frame(Spe=Offre_vec_spe[-length(Offre_vec_spe)], Offre=datad)
      df <- df[order(df[,2],decreasing=TRUE),]
      df$Offre.p <- df$Offre + max(df$Offre)*0.03
      df$xlab_spe <- NA
      for(i in 1:30){
        df$xlab_spe[i] <- xlab_spe[which(indic_spe==df$Spe[i])]
      }
      df[,4] <- factor(df[,4], levels = df[,4])
      xh <- df[1,2]
      tot <- unlist(Offre_data[which(Offre_vec_ville==input$Ville),length(Offre_data[1,]),drop=TRUE])
      tot <- paste("Total : ", as.character(tot))
      dftot <- data.frame(tot=tot, xh=xh)
      plot <- ggplot(df,aes(x=xlab_spe))+
        geom_histogram(aes(y=Offre),stat="identity")+
        geom_text(aes(y=Offre.p,label=Offre),size=5,color="black")+
        geom_text(data=dftot, aes(x=27, y=xh,label=tot),size= 6)+
        xlab("Disciplines")
    }
    if(input$Ville==0){
      plot <- plot + ggtitle(as.character("Toutes les subdivisions"))
    } else {
      titre <- as.character(ECN_data[which(ville.vec==input$Ville),7][1])
      plot <- plot + ggtitle(titre)
    }
    print(plot)
  })
  
  output$plot_sel_ville <- renderPlot({
    ECN_data <- dataset()
    if( input$Restcand=="oui"){
      min <- as.numeric(input$Rang.min)
      max <- as.numeric(input$Rang.max)
      ECN_data.dum <- ECN_data[min:max,]
    } else {
      min <- 1
      max <- length(ECN_data[,1])
      ECN_data.dum <- ECN_data
    }
    spe.vec<-as.numeric(substr(ECN_data[,11],start=5,stop=7))
    ville.vec<-as.numeric(substr(ECN_data[,11],start=2,stop=4))
    df_offre.comp <- data.frame(ECN_data.dum[min:max,4],spe.vec[min:max],ville.vec[min:max])

    Offre_vec_ville <- c(015,020,018,016,017,019,022,024,023,021,038,025,026,027,029,032,028,030,033,031,035,034,036,037,042,040,039,041,000)
    Offre_vec_spe <- c(11,4,3,5,9,6,2,10,7,12,13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 32, 29, 30, 31, 0)
    xlab_ville <- c("AixM","Ami","Ang","AntG","Bes","Bord","Bre","Cae","CleF","Dij","Gre","IDF","Lil","Lim","Ly","Mon","Nanc","Nant","Nic","OceI","Poi","Rei","Ren","Rou","StE","Stra","Toul","Tour")
    indic_ville <- c(034,021,028,041,016,037,029,022,038,017,025,015,023,039,026,035,018,030,036,042,031,019,032,024,027,020,040,033)
    xlab_spe <- c("AR","Bi","GyM","GyO","MT","MG","Psy","Ped","SP","ChOr","ChG","ChN","Opht","ORL","An","Ca","De","En","Ga","GM","He","MI","MN","MPR","Neu","Nep","Onc","Pne","Rad","Rhu")
    indic_spe <- c(004,003,005,009,006,011,010,002,007,032,028,029,030,031,012,013,014,015,016,017,018,019,020,021,023,022,024,025,026,027)
    
    if(input$Choix.indic=="abs"){
      ECN_data.dum[,7] <- as.factor(ECN_data.dum[,7])
      ECN_data.dum[,4] <- as.numeric(ECN_data.dum[,4])
      if(input$Spe == 0) {ECN_data.dum <- ECN_data.dum[-which(ECN_data.dum[,11]==""), ]} else {ECN_data.dum <- ECN_data.dum[which(spe.vec[min:max] == input$Spe),]}
      ECN_data.dum[,7] <- mapvalues(ECN_data.dum[,7],from=levels(as.factor(as.character(ECN_data[-which(ECN_data[,11]==""), ][,7]))),to=xlab_ville)
      df_order.dum <- ddply(ECN_data.dum, .(Subdivision), summarize, median=length(Etudiant))
      vec0 <- xlab_ville[-which(xlab_ville %in% df_order.dum$Subdivision)]
      df_order <- data.frame(Subdivision = c(as.character(df_order.dum$Subdivision),vec0), Etudiant=c(df_order.dum[,2],rep(0,length(vec0))))
      df_order <- df_order[order(df_order[,2],decreasing=TRUE),]
      df_order[,1] <- factor(df_order[,1], levels = df_order[,1])
      df_order$Etudiant.p <- df_order$Etudiant+max(df_order$Etudiant)*0.03
      plot <- ggplot(df_order,aes(x=Subdivision))+geom_histogram(aes(y=Etudiant),stat="identity")+xlab("Subdivisions")+ylab("Postes pourvus")
      plot <- plot + geom_text(aes(y=Etudiant.p,label=Etudiant),color="black")
    }
    
    if(input$Choix.indic=="pourcent"){
      ECN_data.dum <- ECN_data.dum[-which(ECN_data.dum[,11]==""), ]
      ECN_data.dum[,7] <- as.factor(ECN_data.dum[,7])
      ECN_data.dum[,4] <- as.numeric(ECN_data.dum[,4])
      Pourvu <- c()
      Pourvu <- as.data.frame(Pourvu)
      for(i in Offre_vec_spe){
        for(j in Offre_vec_ville){
          poste.pourvu <- length(which(df_offre.comp[which(df_offre.comp[,2]==i),][,3]==j))
          if (length(poste.pourvu)==0){ 
            Pourvu[which(Offre_vec_ville==j),which(Offre_vec_spe==i)] <- 0
          } else { 
            Pourvu[which(Offre_vec_ville==j),which(Offre_vec_spe==i)] <- poste.pourvu
          }
        }
      }
      Pourvu[,dim(Pourvu)[2]] <- 0
      for(i in 1:(dim(Pourvu)[1]-1)){
        Pourvu[i,dim(Pourvu)[2]] <- sum(Pourvu[i,1:(dim(Pourvu)[2]-1)])
      }
      for(i in 1:(dim(Pourvu)[2])){
        Pourvu[dim(Pourvu)[1],i] <- sum(Pourvu[1:(dim(Pourvu)[1]-1),i])
      }
      #Pourcentage
      Pourcent_Pourvu <- c()
      Pourcent_Pourvu <- as.data.frame(Pourcent_Pourvu)
      for( i in 1:length(Pourvu[,1])){
        for( j in 1:length(Pourvu[1,])){
          Pourcent_Pourvu[i,j] <- Pourvu[i,j]/as.numeric(Offre_data[i,j])
        }
      }
      
      #Arrangements des données
      indic_spe <- c(004,003,005,009,006,011,010,002,007,032,028,029,030,031,012,013,014,015,016,017,018,019,020,021,023,022,024,025,026,027)
      xlab_spe <- c("AR","Bi","GyM","GyO","MT","MG","Psy","Ped","SP","ChOr","ChG","ChN","Opht","ORL","An","Ca","De","En","Ga","GM","He","MI","MN","MPR","Neu","Nep","Onc","Pne","Rad","Rhu")
      
      #xlab_spe[which(indic_spe==Offre_vec_spe[1])]
      for(i in 1:length(indic_spe)){
        colnames(Pourcent_Pourvu)[i] <- xlab_spe[which(indic_spe==Offre_vec_spe[i])]
      }
      colnames(Pourcent_Pourvu)[length(Pourcent_Pourvu[1,])] <-"Tot"
      
      xlab_ville <- c("AixM","Ami","Ang","AntG","Bes","Bord","Bre","Cae","CleF","Dij","Gre","IDF","Lil","Lim","Ly","Mon","Nanc","Nant","Nic","OceI","Poi","Rei","Ren","Rou","StE","Stra","Toul","Tour")
      indic_ville <- c(034,021,028,041,016,037,029,022,038,017,025,015,023,039,026,035,018,030,036,042,031,019,032,024,027,020,040,033)
      
      #xlab_ville[which(indic_ville==Offre_vec_ville[1])]
      for(i in 1:length(indic_ville)){
        rownames(Pourcent_Pourvu)[i] <- xlab_ville[which(indic_ville==Offre_vec_ville[i])]
      }
      rownames(Pourcent_Pourvu)[length(Pourcent_Pourvu[,1])] <-"Tot"
      df_order<-data.frame(Subdivision=rownames(Pourcent_Pourvu), Pourcentage=unlist(Pourcent_Pourvu[,which(Offre_vec_spe == input$Spe),drop=TRUE])*100)
      df_order <- df_order[order(df_order[,2],decreasing=TRUE),]
      df_order[,1] <- factor(df_order[,1], levels = df_order[,1])
      if(length(which(is.na(df_order[,2])))==0) {
      } else {
        df_order <- df_order[-which(is.na(df_order[,2])),]
      }
      col <- rep("black", length(df_order[,1]))
      col[which(df_order[,1]=="Tot")] <- "red"
      df_order$Pourcentage.p <- df_order$Pourcentage+max(df_order$Pourcentage)*0.03
      plot <- ggplot(df_order,aes(x=Subdivision))+geom_histogram(aes(y=Pourcentage),stat="identity", fill=col, col="white")+xlab("Subdivisions")
      plot <- plot + geom_text(aes(y=Pourcentage.p,label=paste0(round(Pourcentage,1),"%")),color="black")
    }
    
    if(input$Choix.indic=="offre"){
      datad <- unlist(Offre_data[-length(Offre_data[,1]),which(Offre_vec_spe==input$Spe),drop=TRUE])
      datad[which(is.na(datad))] <- 0
      df <- data.frame(Subdivision=Offre_vec_ville[-length(Offre_vec_ville)], Offre=datad)
      df <- df[order(df[,2],decreasing=TRUE),]
      df$Offre.p <- df$Offre + max(df$Offre)*0.03
      df$xlab_ville <- NA
      for(i in 1:28){
        df$xlab_ville[i] <- xlab_ville[which(indic_ville==df$Subdivision[i])]
      }
      df[,4] <- factor(df[,4], levels = df[,4])
      xh <- df[1,2]
      tot <- Offre_data[length(Offre_data[,1]),which(Offre_vec_spe==input$Spe)]
      tot <- paste("Total : ", as.character(tot))
      dftot <- data.frame(tot=tot, xh=xh)
      plot <- ggplot(df,aes(x=xlab_ville))+
        geom_histogram(aes(y=Offre),stat="identity")+
        geom_text(aes(y=Offre.p,label=Offre),size=5,color="black")+
        geom_text(data=dftot, aes(x=25, y=xh,label=tot),size= 6)+
        xlab("Subdivisions")
    }
    
    if(input$Spe==0){
      plot <- plot + ggtitle(as.character("Toutes les spécialités"))
    } else {
      titre <- as.character(ECN_data[which(spe.vec==input$Spe),8][1])
      plot <- plot + ggtitle(titre)
    }
    print(plot)
  })
  
  output$plot_attr_spe <- renderPlot({
    ECN_data <- dataset()
    Offre_vec_ville <- c(015,020,018,016,017,019,022,024,023,021,038,025,026,027,029,032,028,030,033,031,035,034,036,037,042,040,039,041,000)
    Offre_vec_spe <- c(11,4,3,5,9,6,2,10,7,12,13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 32, 29, 30, 31, 0)
    xlab_ville <- c("AixM","Ami","Ang","AntG","Bes","Bord","Bre","Cae","CleF","Dij","Gre","IDF","Lil","Lim","Ly","Mon","Nanc","Nant","Nic","OceI","Poi","Rei","Ren","Rou","StE","Stra","Toul","Tour")
    indic_ville <- c(034,021,028,041,016,037,029,022,038,017,025,015,023,039,026,035,018,030,036,042,031,019,032,024,027,020,040,033)
    spe.vec<-as.numeric(substr(ECN_data[,11],start=5,stop=7))
    ville.vec<-as.numeric(substr(ECN_data[,11],start=2,stop=4))
    ECN_data.dum <- ECN_data
    df_offre.comp <- data.frame(as.numeric(as.character(ECN_data.dum[,4])),spe.vec,ville.vec)
    Attracti<-c()
    nbr.poste <- c()
    if(input$Spe==000){
      for(i in indic_ville){
        nbr.poste <- as.numeric(Offre_data[which(Offre_vec_ville==i),31])
        nbr.poste.t <- as.numeric(Offre_data[29,31])
        if(is.na(nbr.poste)){Attracti[which(indic_ville==i)] <- NA
        } else {
          offre.attr <- df_offre.comp[-which(ECN_data[,11]==""),1]
          if((nbr.poste.t - length(offre.attr)) >= nbr.poste){
            SXmax <- 8305*nbr.poste
          } else {
            SXmax <- sum(c(offre.attr[(length(offre.attr) - (nbr.poste - (nbr.poste.t - length(offre.attr)))):length(offre.attr)] ,rep(8305, (nbr.poste.t - length(offre.attr)))))
          }
          offre.attr <- offre.attr[1:nbr.poste]
          SXmin <- sum(offre.attr)
          x <- df_offre.comp[which(df_offre.comp[,3]==i),1]
          if(length(x) != length(offre.attr)){
            x <- c(x,rep(8305,length(offre.attr) - length(x)))
          }
          Sx <- sum(x)
          Attracti[which(indic_ville==i)] <- (Sx - SXmin)/(SXmax-SXmin)
        }
      }
    } else {
      if(input$Choix.calc=="glob"){
        for(i in indic_ville){
          nbr.poste <- as.numeric(Offre_data[which(Offre_vec_ville==i),which(Offre_vec_spe==input$Spe)])
          nbr.poste.t <- as.numeric(Offre_data[29,which(Offre_vec_spe==input$Spe)])
          if(is.na(nbr.poste)){ Attracti[which(indic_ville==i)] <- NA
          } else {
            offre.attr <- df_offre.comp[which(df_offre.comp[,2]==input$Spe),1]
            if((nbr.poste.t - length(offre.attr)) >= nbr.poste){
              SXmax <- 8305*nbr.poste
            } else {
              SXmax <- sum(c(offre.attr[(length(offre.attr) - (nbr.poste - (nbr.poste.t - length(offre.attr)))):length(offre.attr)] ,rep(8305, (nbr.poste.t - length(offre.attr)))))
            }
            offre.attr <- offre.attr[1:nbr.poste]
            SXmin <- sum(offre.attr)
            df_dum <- df_offre.comp[which(df_offre.comp[,3]==i),]
            x <- df_dum[which(df_dum[,2]==input$Spe),1]
            rm(df_dum)
            if(length(x) != length(offre.attr)){
              x <- c(x,rep(8305,length(offre.attr) - length(x)))
            }
            Sx <- sum(x)
            Attracti[which(indic_ville==i)] <- (Sx - SXmin)/(SXmax-SXmin)
          }
        }
      }
      if(input$Choix.calc=="sel"){
        for(i in indic_ville){
          nbr.poste <- as.numeric(Offre_data[which(Offre_vec_ville==i),which(Offre_vec_spe==input$Spe)])
          nbr.poste.t <- as.numeric(Offre_data[29,which(Offre_vec_spe==input$Spe)])
          if(is.na(nbr.poste)){ Attracti[which(indic_ville==i)] <- NA
          } else {
            offre.attr <- df_offre.comp[which(df_offre.comp[,2]==input$Spe),1]
            offre.attr <- c(1:length(offre.attr))
            if((nbr.poste.t - length(offre.attr)) >= nbr.poste){
              SXmax <- (nbr.poste.t + 1) * nbr.poste
            } else {
              SXmax <- sum(c(offre.attr[(length(offre.attr) - (nbr.poste - (nbr.poste.t - length(offre.attr)))):length(offre.attr)] ,rep((nbr.poste.t + 1), (nbr.poste.t - length(offre.attr)))))
            }
            offre.attr <- offre.attr[1:nbr.poste]
            SXmin  <- sum(offre.attr)
            df_dum <- df_offre.comp[which(df_offre.comp[,3]==i),]
            x <- df_dum[which(df_dum[,2]==input$Spe),1]
            if(length(x)==0){ 
              x <- NULL
            } else { 
              x <- which(df_offre.comp[which(df_offre.comp[,2]==input$Spe),1] %in% x)
            }
            if(length(x) != length(offre.attr)){
              x <- c(x,rep((nbr.poste.t + 1),length(offre.attr) - length(x)))
            }
            Sx <- sum(x)
            Attracti[which(indic_ville==i)] <- (Sx - SXmin)/(SXmax - SXmin)
          }
        }
      }    
    }
    df_order <- data.frame(indic_ville,xlab_ville,Attr=100-Attracti*100)
    if(length(which(is.na(df_order[,3]))) != 0){
      df_order <- df_order[-which(is.na(df_order[,3])),]
    } else {}
    df_order <- df_order[order(df_order[,3],decreasing=TRUE),]
    df_order[,2] <- factor(df_order[,2], levels = df_order[,2])
    df_order$Attr.p <- df_order$Attr + max(df_order$Attr)*0.03
    plot <- ggplot(df_order,aes(x=xlab_ville))+geom_histogram(aes(y=Attr),stat="identity")+xlab("Subdivision")+ylab("Attractivité")
    plot <- plot + geom_text(aes(y=Attr.p,label=round(Attr,1)),color="black")
    if(input$Spe==0){
      plot <- plot + ggtitle(as.character("Toutes les spécialités"))
    } else {
      titre <- as.character(ECN_data[which(spe.vec==input$Spe),8][1])
      plot <- plot + ggtitle(titre)
    }
    print(plot)
  })
  
  output$plot_attr_ville <- renderPlot({
    ECN_data <- dataset()
    Offre_vec_ville <- c(015,020,018,016,017,019,022,024,023,021,038,025,026,027,029,032,028,030,033,031,035,034,036,037,042,040,039,041,000)
    Offre_vec_spe <- c(11,4,3,5,9,6,2,10,7,12,13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 32, 29, 30, 31, 0)
    xlab_ville <- c("AixM","Ami","Ang","AntG","Bes","Bord","Bre","Cae","CleF","Dij","Gre","IDF","Lil","Lim","Ly","Mon","Nanc","Nant","Nic","OceI","Poi","Rei","Ren","Rou","StE","Stra","Toul","Tour")
    indic_ville <- c(034,021,028,041,016,037,029,022,038,017,025,015,023,039,026,035,018,030,036,042,031,019,032,024,027,020,040,033)
    xlab_spe <- c("AR","Bi","GyM","GyO","MT","MG","Psy","Ped","SP","ChOr","ChG","ChN","Opht","ORL","An","Ca","De","En","Ga","GM","He","MI","MN","MPR","Neu","Nep","Onc","Pne","Rad","Rhu")
    indic_spe <- c(004,003,005,009,006,011,010,002,007,032,028,029,030,031,012,013,014,015,016,017,018,019,020,021,023,022,024,025,026,027)
    spe.vec<-as.numeric(substr(ECN_data[,11],start=5,stop=7))
    ville.vec<-as.numeric(substr(ECN_data[,11],start=2,stop=4))
    ECN_data.dum <- ECN_data
    df_offre.comp <- data.frame(as.numeric(as.character(ECN_data.dum[,4])),spe.vec,ville.vec)
    Attracti<-c()
    nbr.poste <- c()
    if(input$Ville==000){
      for(i in indic_spe){
        nbr.poste <- as.numeric(Offre_data[29,which(Offre_vec_spe==i)])
        nbr.poste.t <- as.numeric(Offre_data[29,31])
        if(is.na(nbr.poste)){Attracti[which(indic_spe==i)] <- NA
        } else {
          offre.attr <- df_offre.comp[-which(ECN_data[,11]==""),1]
          if((nbr.poste.t - length(offre.attr)) >= nbr.poste){
            SXmax <- 8305*nbr.poste
          } else {
            SXmax <- sum(c(offre.attr[(length(offre.attr) - (nbr.poste - (nbr.poste.t - length(offre.attr)))):length(offre.attr)] ,rep(8305, (nbr.poste.t - length(offre.attr)))))
          }
          offre.attr <- offre.attr[1:nbr.poste]
          SXmin <- sum(offre.attr)
          x <- df_offre.comp[which(df_offre.comp[,2]==i),1]
          if(length(x) != length(offre.attr)){
            x <- c(x,rep(8305, length(offre.attr) - length(x)))
          }
          Sx <- sum(x)
          Attracti[which(indic_spe==i)] <- (Sx - SXmin)/(SXmax-SXmin)
        }
      }
    } else {
      if(input$Choix.calc=="glob"){
        for(i in indic_spe){
          nbr.poste <- as.numeric(Offre_data[which(Offre_vec_ville == input$Ville),which(Offre_vec_spe==i)])
          nbr.poste.t <- as.numeric(Offre_data[which(Offre_vec_ville==input$Ville),31])
          if(is.na(nbr.poste)){ Attracti[which(indic_spe==i)] <- NA
          } else {
            offre.attr <- df_offre.comp[which(df_offre.comp[,3] == input$Ville),1]
            if((nbr.poste.t - length(offre.attr)) >= nbr.poste){
              SXmax <- 8305*nbr.poste
            } else {
              SXmax <- sum(c(offre.attr[(length(offre.attr) - (nbr.poste - (nbr.poste.t - length(offre.attr)))):length(offre.attr)] ,rep(8305, (nbr.poste.t - length(offre.attr)))))
            }
            offre.attr <- offre.attr[1:nbr.poste]
            SXmin <- sum(offre.attr)
            df_dum <- df_offre.comp[which(df_offre.comp[,3] == input$Ville),]
            x <- df_dum[which(df_dum[,2]==i),1]
            if(length(x) != length(offre.attr)){
              x <- c(x,rep(8305,length(offre.attr) - length(x)))
            }
            Sx <- sum(x)
            Attracti[which(indic_spe==i)] <- (Sx - SXmin)/(SXmax-SXmin)
          }
        }
      }
      if(input$Choix.calc=="sel"){
        for(i in indic_spe){
          nbr.poste <- as.numeric(Offre_data[which(Offre_vec_ville == input$Ville),which(Offre_vec_spe==i)])
          nbr.poste.t <- as.numeric(Offre_data[which(Offre_vec_ville==input$Ville),31])
          if(is.na(nbr.poste)){ Attracti[which(indic_spe==i)] <- NA
          } else {
            offre.attr <- df_offre.comp[which(df_offre.comp[,3] == input$Ville),1]
            offre.attr <- c(1:length(offre.attr))
            if((nbr.poste.t - length(offre.attr)) >= nbr.poste){
              SXmax <- (nbr.poste.t + 1) * nbr.poste
            } else {
              SXmax <- sum(c(offre.attr[(length(offre.attr) - (nbr.poste - (nbr.poste.t - length(offre.attr)))):length(offre.attr)] ,rep((nbr.poste.t + 1), (nbr.poste.t - length(offre.attr)))))
            }
            offre.attr <- offre.attr[1:nbr.poste]
            SXmin <- sum(offre.attr)
            df_dum <- df_offre.comp[which(df_offre.comp[,3] == input$Ville),]
            x<- df_dum[which(df_dum[,2]==i),1]
            if(length(x)==0){ 
              x <- NULL
            } else { 
              x <- which(df_offre.comp[which(df_offre.comp[,3]==input$Ville),1] %in% x)
            }
            if(length(x) != length(offre.attr)){
              x <- c(x,rep((nbr.poste.t + 1),length(offre.attr) - length(x)))
            }
            Sx <- sum(x)
            Attracti[which(indic_spe==i)] <- (Sx - SXmin)/(SXmax-SXmin)
          }
        }
      }
    }
    df_order <- data.frame(indic_spe,xlab_spe,Attr=100-Attracti*100)
    if(length(which(is.na(df_order[,3]))) != 0){
      df_order <- df_order[-which(is.na(df_order[,3])),]
    } else {}
    df_order <- df_order[order(df_order[,3],decreasing=TRUE),]
    df_order[,2] <- factor(df_order[,2], levels = df_order[,2])
    df_order$Attr.p <- df_order$Attr + max(df_order$Attr)*0.03
    plot <- ggplot(df_order,aes(x=xlab_spe))+geom_histogram(aes(y=Attr),stat="identity")+xlab("Disciplines")+ylab("Attractivité")
    plot <- plot + geom_text(aes(y=Attr.p,label=round(Attr,1)),color="black")
    if(input$Ville==0){
      plot <- plot + ggtitle(as.character("Toutes les subdivisions"))
    } else {
      titre <- as.character(ECN_data[which(ville.vec==input$Ville),7][1])
      plot <- plot + ggtitle(titre)
    }
    print(plot)
  })
  
  output$agreg_spe <- renderDataTable({
    ECN_data <- dataset()
    xlab_spe <- c("AR","Bi","GyM","GyO","MT","MG","Psy","Ped","SP","ChOr","ChG","ChN","Opht","ORL","An","Ca","De","En","Ga","GM","He","MI","MN","MPR","Neu","Nep","Onc","Pne","Rad","Rhu")
    ECN_data.dum <- ECN_data[-which(ECN_data[,11]==""), ]
    ECN_data.dum[,8] <- as.character(ECN_data.dum[,8])
    ECN_data.dum[,8] <- as.factor(ECN_data.dum[,8])    
    ECN_data.dum[,4] <- as.numeric(ECN_data.dum[,4])
    df_corr_spe <- data.frame(levels(ECN_data.dum[,8]),xlab_spe)
    ECN_data.dum[,8] <- mapvalues(ECN_data.dum[,8],from=levels(ECN_data.dum[,8]),to=xlab_spe)
    df_order <- ddply(ECN_data.dum, .(Discipline), summarize, 
      median=median(Etudiant,na.rm=T),
      mean=mean(Etudiant,na.rm=T),
      max=max(Etudiant,na.rm=T),
      min=min(Etudiant,na.rm=T),
      TQuart=quantile(Etudiant, 0.75,na.rm=T),
      PQuart=quantile(Etudiant, 0.25,na.rm=T)
    )
    df_order
  })
  
  output$agreg_ville <- renderDataTable({
    ECN_data <- dataset()
    xlab_ville <- c("AixM","Ami","Ang","AntG","Bes","Bord","Bre","Cae","CleF","Dij","Gre","IDF","Lil","Lim","Ly","Mon","Nanc","Nant","Nic","OceI","Poi","Rei","Ren","Rou","StE","Stra","Toul","Tour")
    ECN_data.dum <- ECN_data[-which(ECN_data[,11]==""), ]
    ECN_data.dum[,7] <- as.character(ECN_data.dum[,7])
    ECN_data.dum[,7] <- as.factor(ECN_data.dum[,7])
    ECN_data.dum[,4] <- as.numeric(ECN_data.dum[,4])
    df_corr_ville <- data.frame(levels(ECN_data.dum[,7]),xlab_ville)
    ECN_data.dum[,7] <- mapvalues(ECN_data.dum[,7],from=levels(ECN_data.dum[,7]),to=xlab_ville)
    df_order <- ddply(ECN_data.dum, .(Subdivision), summarize, 
      median=median(Etudiant,na.rm=T),
      mean=mean(Etudiant,na.rm=T),
      max=max(Etudiant,na.rm=T),
      min=min(Etudiant,na.rm=T),
      TQuart=quantile(Etudiant, 0.75,na.rm=T),
      PQuart=quantile(Etudiant, 0.25,na.rm=T)
    )
    df_order
  })
  
  output$legende_spe <- renderTable({
    ECN_data <- dataset()
    xlab_spe <- c("AR","Bi","GyM","GyO","MT","MG","Psy","Ped","SP","ChOr","ChG","ChN","Opht","ORL","An","Ca","De","En","Ga","GM","He","MI","MN","MPR","Neu","Nep","Onc","Pne","Rad","Rhu")
    ECN_data.dum <- ECN_data[-which(ECN_data[,11]==""), ]
    ECN_data.dum[,8] <- as.factor(as.character(ECN_data.dum[,8]))
    ECN_data.dum[,4] <- as.numeric(ECN_data.dum[,4])
    df_corr_spe <- data.frame(Discipline=levels(ECN_data.dum[,8]),Abbréviation=xlab_spe)
    df_corr_spe
  })
  
  output$legende_ville <- renderTable({
    ECN_data <- dataset()
    xlab_ville <- c("AixM","Ami","Ang","AntG","Bes","Bord","Bre","Cae","CleF","Dij","Gre","IDF","Lil","Lim","Ly","Mon","Nanc","Nant","Nic","OceI","Poi","Rei","Ren","Rou","StE","Stra","Toul","Tour")
    ECN_data.dum <- ECN_data[-which(ECN_data[,11]==""), ]
    ECN_data.dum[,7] <- as.factor(as.character(ECN_data.dum[,7]))
    ECN_data.dum[,4] <- as.numeric(ECN_data.dum[,4])
    df_corr_ville <- data.frame(Subdivision=levels(ECN_data.dum[,7]),Abbréviation=xlab_ville)
    df_corr_ville
  })
})