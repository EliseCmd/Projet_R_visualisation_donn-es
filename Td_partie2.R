#Partie 2
#A)
#1)

#library(tidyverse)
data<-read.csv("2023_01_01_09_50_23.csv")
head(data)

distance<-max(data$km)
distance
puissance_moyenne<-mean(data$watts)
puissance_moyenne
vitesse_moyenne<-mean(data$kph)
vitesse_moyenne
fc_moy<-mean(data$hr)
fc_moy
#deniv_positif<-sum(diff(data$alt)>0)
deniv_positif #582

#tableau <- data.frame(distance = data$distance,puissance = data$puissance, vitesse = data$vitesse)

#TSS = durée de la séance en secondes*NP*IF)/(FTP*3600)*100

#NP est la puissance normalisée: cherche à estimer l'intensité de la séance si l'individu avait pédalé de la même façon pdt tte la séance
#IF est le facteur d'intensité: NP/FTP avec FTP: estimation de la plus haute puissance qu'on est capable sur 1h 
duree_seance<-max(data$secs)

#fonction modifiée pour calculer la Puissance Normalisée (NP) avec un argument n
calculate_normalized_power<-function(power_output,n=30){
  #Etape 1 : Calcul de la puissance mobile moyenne sur n secondes
  rolling_mean_power<-zoo::rollmean(power_output,k=n,fill=NA,align="center")
  
  #Rempalcer les NA initiaux et finaux par les valeurs disponibles les plus proches
  rolling_mean_power[is.na(rolling_mean_power)]<-power_output[is.na(rolling_mean_power)]
  
  #Etape 2: Ele vation à la puissance 4
  power_4<-rolling_mean_power^4
  
  #Etape 3 : calcul de la moyenne
  avg_power_4<-mean(power_4,na.rm=TRUE)
  
  #Etape 4 : Extraction de la racine quatrième
  normalized_power<-avg_power_4^(1/4)
  
  return(normalized_power)
}
power_output<-data$watts
n=30
Normalized_Power_abs=round(calculate_normalized_power(power_output,n),0)
Normalized_Power_abs

library(zoo)
FTP<-max(rollmean(data$watts,20*60))
FTP
IF<-Normalized_Power_abs/FTP
IF

TSS<-(duree_seance*Normalized_Power_abs*IF)/(FTP*3600)*100
TSS
travail_total<-round(sum(data$watts)/100,0)
df<-data.frame(Distance_km=distance,Vitesse_moyenne=vitesse_moyenne,Puissance_moyenne=puissance_moyenne,Puissance_normalisée=Normalized_Power_abs,Durée=duree_seance,FC_moyenne=fc_moy)
print(df)

dossier<-"datas_td_monitoring/cyclist1_datas"
fichiers <- list.files(path = dossier, pattern = "\\.csv$", full.names = TRUE)

# Initialiser une liste pour stocker les résultats
resultats <- list()


df_final <- data.frame(Distance_km = numeric(),
                       Vitesse_moyenne = numeric(),
                       Puissance_moyenne = numeric(),
                       Puissance_normalisée = numeric(),
                       Durée = numeric(),
                       FC_moyenne = numeric(),
                       Travail_total = numeric(),
                       p20min=numeric(),
                       Annee = numeric(),
                       Mois = character(),
                       Jour = character(),
                       semaine=character(),
                       stringsAsFactors = FALSE)

# Boucle pour lire chaque fichier et effectuer le traitement
for (fichier in fichiers) {
  data<-read.csv(fichier)
  nom_fichier <- basename(fichier) # Récupère uniquement le nom du fichier sans le chemin
  date_str <- substr(nom_fichier, 1, 10) # Extraire "2023_01_01"
  date <- ymd(date_str) # Convertit en objet Date avec lubridate
  
  # Extraire l'année, le mois et le jour à partir de l'objet Date
  annee <- year(date)
  mois <- month(date, label = TRUE, abbr = FALSE) # Nom complet du mois
  jour <- wday(date, label = TRUE, abbr = FALSE) # Nom complet du jour
  semaine<- isoweek(date)
  
  distance<-max(data$km)
  distance
  puissance_moyenne<-mean(data$watts)
  puissance_moyenne
  vitesse_moyenne<-mean(data$kph)
  vitesse_moyenne
  fc_moy<-mean(data$hr)
  fc_moy
  travail_total<-round(sum(data$watts)/1000)
  
  #deniv_positif<-sum(diff(data$alt)>0)
  deniv_positif #582
  
  duree_seance<-max(data$secs)
  
  
  #fonction modifiée pour calculer la Puissance Normalisée (NP) avec un argument n
  calculate_normalized_power<-function(power_output,n=30){
    #Etape 1 : Calcul de la puissance mobile moyenne sur n secondes
    rolling_mean_power<-zoo::rollmean(power_output,k=n,fill=NA,align="center")
    
    #Rempalcer les NA initiaux et finaux par les valeurs disponibles les plus proches
    rolling_mean_power[is.na(rolling_mean_power)]<-power_output[is.na(rolling_mean_power)]
    
    #Etape 2: Ele vation à la puissance 4
    power_4<-rolling_mean_power^4
    
    #Etape 3 : calcul de la moyenne
    avg_power_4<-mean(power_4,na.rm=TRUE)
    
    #Etape 4 : Extraction de la racine quatrième
    normalized_power<-avg_power_4^(1/4)
    
    return(normalized_power)
  }
  power_output<-data$watts
  n=30
  Normalized_Power_abs=round(calculate_normalized_power(power_output,n),0)
  Normalized_Power_abs
  
  FTP<-max(rollmean(data$watts,20*60))
  
  df <- data.frame(Distance_km = distance,
                   Vitesse_moyenne = vitesse_moyenne,
                   Puissance_moyenne = puissance_moyenne,
                   Puissance_normalisée = Normalized_Power_abs,
                   Durée = duree_seance,
                   FC_moyenne = fc_moy,
                   Travail_total = travail_total,
                   p20min=FTP,
                   Annee = annee,
                   Mois = mois,
                   Jour = jour,
                   semaine=semaine)
  
  # Ajouter la ligne au data.frame final
  df_final <- rbind(df_final, df)
}
head(df_final)
df_semaine<-df_final|>group_by(semaine)|>summarise( Distance_km = sum(Distance_km, na.rm = TRUE),
                                                    Vitesse_moyenne = mean(Vitesse_moyenne, na.rm = TRUE),
                                                    Puissance_moyenne = mean(Puissance_moyenne, na.rm = TRUE),
                                                    Puissance_normalisée = mean(Puissance_normalisée, na.rm = TRUE),
                                                    Durée = sum(Durée, na.rm = TRUE),
                                                    FC_moyenne = mean(FC_moyenne, na.rm = TRUE),
                                                    Travail_total = sum(Travail_total, na.rm = TRUE),
                                                    p20min = mean(p20min, na.rm = TRUE))

#IF=NP/FTP -> p20min*0.95
#FTP=max(data$p20min)
FTP<-(max(df_final$p20min))*0.95
FTP

df_final$IF <- df_final$Puissance_normalisée / FTP
df_final$TSS <- (df_final$Durée * df_final$Puissance_normalisée * df_final$IF) / (FTP * 3600) * 100

library(tidyverse)
ggplot(df_semaine)+aes(x=semaine,y=Travail_total)+geom_col(fill = "skyblue")+geom_smooth(color = "blue", se = FALSE) +  # Ajoute une courbe lissée
  labs(x = "Semaine", y = "Travail Total", title = "Travail Total par Semaine") +
  theme_minimal()

