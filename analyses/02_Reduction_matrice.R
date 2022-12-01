# setwd("~/Documents/M2 BEST/Stage/Analyse photo/Analyse_2022/Data/Pour NMDS")
# data=read.table(file = "Data_sans_UNAV-NR-OROS.csv", header=TRUE, sep=";", dec=",")
# #On ne selectionne que les ARMS issues de la campagne RUNA
# data=subset(data, data$Nom_ARMS=="RUNA")
#On créé une matrice Site/Espèce
#matrix=data[,20:ncol(data)]
#On retire toutes les espèce dont la somme des abondance est zero 
#(les espèces qui n'ont pas été recensé dans cette campagne)
#matrix=matrix[ , colSums(matrix) != 0]

View(matrix)
vec.2=c()
U.2=NULL

#On calcule pour chaque espèce le pourcentage de 0

for (i in 1:ncol(matrix)) {
  
  
  U.2=((length(which(matrix[,i]==0)))*100)/432
  vec.2[i]=U.2
  
}

#On tri ces espèces en fonction de leurs pourcentage de 0 (ordre croissant)
vec.2.sort=sort(vec.2)

#On peut refaire une matrice ordonnée selon ce tri

matrix=rbind(matrix,vec.2)
matrix.2=matrix[,order(matrix[433,], decreasing = FALSE)] 
matrix.2=matrix.2[-433,]


#On fait un graph comme dans Souissi et al. 2001
plot(vec.2.sort, type = "o", xlab = "rang des espèces", ylab="Pourcentage de 0 (%)")
abline(90,0)
abline(v=22)

#the percentage of zeroes in the 135 sites for different species are sorted 
#in ascending order. The threshold level of 95 % is considered.

#Contribution of each selected species in the total abundance.
#The 38 species selected in the step (A) are ranked according their contribution.

matrix.2.red=matrix.2[,1:26]

matrix.2.red=matrix.2.red[-433,]
rownames(matrix.2.red)=data$Image_name
#matrix.2.red=cbind(data[,1:19],matrix.2.red)

write.table(matrix.2.red, file="Data_red_1%.csv", sep=";",dec="," )


#On ajoute une ligne qui fait la somme des abondances de chaque espèce
matrix.2.red[433,]=colSums(matrix.2.red)
#On range les espèces de notre matrice par ordre décroissant d'abondance
#(les espèces les plus abondante aparaissent en premier dans la matrice)
matrix.ord=matrix.2.red[,order(matrix.2.red[433,], decreasing = TRUE)] 
#On créé un vecteur qui est composé des sommes de chaque colonnes 
#(toujours ranger dans l'ordre décroissant)
AbSum=colSums(matrix.2.red[-433,])

AbSum.s <- sort(AbSum, decreasing = TRUE)
#On créé un objet qui prends simplement la valeur 
#de la somme de toutes les abondances de notre matrice 
#toute ligne et toute colonne confondue
S=sum(AbSum.s)

#Pour i allant de 1 (l'espece la plus abondante)
#au nombre total d'espèces (dans l'ordre de leurs abondance), 
#on calcul la contribution à l'abondance total (S) de chaque espèce.
#Cela donne la valeur U  (%).

vec=c()
U=NULL

for (i in 1:length(AbSum.s)) {
  U=(AbSum.s[i]*100)/S
  vec[i]=U
} 

#On en fait un data.frame avec les noms des espèces
data.vec=as.data.frame(vec)
row.names(data.vec)=names(AbSum.s)
#On fait un graph de la contribution a l'abondance total de chaque espèce 
#en fonction de leurs rang (la premiere etant la plus abondante 
#et la derniere la moins abondante)
plot(data.vec$vec, type ="o", xlab = "rang des espèces", ylab = "contribution à l'abondance des espèces (%)")
#Trace une droite qui permet d'identifier toutes les espèces qui ont une 
#contribution inferieur à 0,05% (on peut choisir nptq seuil)
abline(0.05, 0)
#Il s'avere que ces 38 especes contribuent toutes au moins a hauteur de 5%
#de l'abondance totale (S), on les garde toutes 
#Obtenir la matrice qui correspond à ces 61 espèces
matrix.reduite=matrix.ord[-433,c(1:62)]
View(matrix.reduite)

####################

data$Nouveau.Nom
data.red=cbind(data$Nouveau.Nom,matrix.2.red)

View(data.red)

tab=NULL
U=NULL
ncol(data.red)

data.red$`data$Nouveau.Nom`

for (i in 2:ncol(data.red)) {
  U=tapply(data.red[,i], data.red$`data$Nouveau.Nom`, mean)
  tab=cbind(tab,U)
  print(i)
}

tab

N=colnames(data.red)
N=N[2:27]
colnames(tab)=N
View(tab)

tab=as.data.frame(tab)

setwd("~/Documents/M2 BEST/Stage/Analyse photo/Analyse_2022/Data/Matrice_Reduite_Souissi/V3")
write.table(tab, file="ARMS_pool_red(10%).csv", sep=";",dec="," )







