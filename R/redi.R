#' redi
#'
#' calcul les differents indicateurs de charge
#'
#' @param base jeu de données comprenant les variables "Date","Athlete" et les variables de charge.
#' @param N1 nombre de jours à prendre en compte pour la charge chronique pour l'EWMA
#' @param N2 nombre de jours à prendre en compte pour la charge aigue pour l'EWMA
#' @param lambda1 lambda pour la charge aigue pour le REDI
#' @param lambda2 lambda pour la charge chronique pour le REDI
#' @param indic les variables à prendre en compte pour calculer les différents indicateurs
#' @param fun si plusieurs sessions le même jour, comment aggreger les données
#' @param formatdate format des dates
#' @param replace me rappelle plus
#' @param keep pas encore en place
#' @param FORSTER calcul les indicateurs de FOSTER
#' @param rpeload variable indiquant la rpeSession si FOSTER=TRUE



#' @return un data.frame avec les différents indicateurs de charge
#' @export
#' @examples NULL
redi = function(base , N1=8,N2=28, lambda1=2/(8+1),lambda2=2/(28+1),indic,fun,formatdate="%d/%m/%Y",replace=F,keep,FOSTER=F,rpeload=NULL){

  base$Date=as.Date(base$Date,format=formatdate)
  tab=data.frame(nom=unique(base$Athlete),
                 ID=unique(base$Athlete),
                 jour=seq(min(base$Date), max(base$Date), by = "day"))

  cond=c()
  for (i in 1:length(indic)){
    cond[i]=!all(is.na(base[,indic[i]]))
  }

  if(all(cond)){

    t=list()
    for(i in 1:length(indic)){
      fonction=get(fun[i])
      t[[i]]=aggregate(as.formula(paste(indic[i],"~ Date")),data=base,FUN=function(x){fonction(x,na.rm=TRUE)})
    }

    RPE=Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Date", all = TRUE),t)


    # RPE=aggregate(as.formula(paste(indic,"~ Date")),data=base,FUN=function(x){fun(x,na.rm = TRUE)})
    RPE$Date=as.Date(RPE$Date,formatdate)

    tab=merge(tab,RPE,by.x="jour",by.y="Date",all.x=TRUE)
    # tab[,indic]=RPE[,indic][match(tab$jour,RPE$Date)]
    tab$Statut=base$Statut[match(tab$jour,base$Date)]

    tab[paste0("ewma1_",indic)]=NA
    tab[paste0("ewma2_",indic)]=NA

    for (i in 1:length(indic)){
      tab[1,paste0("ewma1_",indic[i])]=tab[1,indic[i]]
      tab[1,paste0("ewma2_",indic[i])]=tab[1,indic[i]]
    }


    tab[paste0("redi1_",indic)]=NA
    tab[paste0("redi2_",indic)]=NA

    for (i in 1:length(indic)){
      tab[1,paste0("redi1_",indic[i])]=tab[1,indic[i]]
      tab[1,paste0("redi2_",indic[i])]=tab[1,indic[i]]
    }


    for (i in 1:length(indic)){
      tab[paste0(indic[i],"2")]=tab[paste0(indic[i])]
      tab[paste0(indic[i],"2")][is.na(tab[paste0(indic[i],"2")])]=0
    }

    tab$IM=NA
    tab$contrainte=NA

    if(nrow(tab)>1){
      for (i in 2:nrow(tab)){

        tab[i,paste0("ewma1_",indic)]=sapply(indic,function(x){
          (tab[,x][i]*(2/(N1+1)))+((1-(2/(N1+1)))*tab[i-1,paste0("ewma1_",x)])
        })

        tab[i,paste0("ewma2_",indic)]=sapply(indic,function(x){
          (tab[,x][i]*(2/(N2+1)))+((1-(2/(N2+1)))*tab[i-1,paste0("ewma2_",x)])
        })

        ##Calcul REDI
        if(replace==F){
          indicateur=indic
        }else{
          if(replace==T){
            indicateur=paste0(indic,"2")
          }
        }

        temp=tab[1:i,][indicateur]

        for (z in 1:length(indicateur)){
          temp2=temp[,indicateur[z]]

          alpha1=c()
          alpha2=c()

          for (j in 1:length(temp2)){
            alpha1[j]=exp(- lambda1 * j)
          }

          for (j in 1:length(temp2)){
            alpha2[j]=exp(- lambda2 * j)
          }


          alpha1=rev(alpha1)
          alpha2=rev(alpha2)
          alpha1[is.na(temp2)]=0
          alpha2[is.na(temp2)]=0
          temp2[is.na(temp2)]=0


          S1=sum(alpha1*temp2)
          S2=sum(alpha1)
          tab[i,paste0("redi1_",indic[z])]=S1/S2

          S1=sum(alpha2*temp2)
          S2=sum(alpha2)
          tab[i,paste0("redi2_",indic[z])]=S1/S2
        }

        if(FOSTER==T){
          if(i>=7){
            tab$IM[i]=mean(tab[,paste0(rpeload,"2")][c(i:(i-6))])/sd(tab[,paste0(rpeload,"2")][c(i:(i-6))])
            tab$contrainte[i]=mean(tab[,paste0(rpeload,"2")][c(i:(i-6))])*tab$IM[i]
          }
        }
      }
    }

    if(FOSTER==T){
      # tab$fitness=tab[,paste0(rpeload,"2")]-tab$contrainte
      tab$fitness=tab[,paste0("redi2_",rpeload)]-tab$contrainte
    }

    for (i in 1:length(indic)){
      tab[,paste0("ratioredi_",indic[i])]=tab[,paste0("redi2_",indic[i])]/tab[,paste0("redi1_",indic[i])]
    }

    for (i in 1:length(indic)){
      tab[,paste0("ratioewma_",indic[i])]=tab[,paste0("ewma1_",indic[i])]/tab[,paste0("redi2_",indic[i])]
    }

    tab$ACWR=NA

    # if(nrow(tab)>28){
    #   for (i in 28:nrow(tab)){
    #     tab$ACWR[i]=mean(tab$rpeLoad[c(i:(i-7))])/mean(tab$rpeLoad[c(i:(i-28))])
    #   }}

    return(tab)
  }
}
