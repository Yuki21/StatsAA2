# Apprentissage par problème
# Dylan, Marylie, Olivier, Léo, Loïc B, Célia

# Protocole de simulation

data <- c(1,2.1,2.8,3.7,5,3,6,6.5,7.2,8.2,9.5)

# On cherche un modèle de donées :
# On envisage les lois qu'on connait :
# Les données ne suivent à priori pas une loi exponentielle, car la durée max serait alors infinie,
# ce qui ne correspond pas à la réalité physique. De même pour la loi normale.
# Le modèle pourrait être une loi uniforme, qui est bornée. Les valeurs semblent également au premier abord correspondre.
# On trace le Qplot de ces données :

plot(sort(data)[1:(length(data)-1)], qunif(seq(1:(length(data)-1))/length(data)))

# Les données ne permettent pas de donner un réponse certaines, car l'échantillon est trop petit. 
# En prenat du recul sur la question, et en reflechissant les données d'un réseau de bus, on peut se dire que si Marius et Jeannette
# arrivent de manière irégulière à l'arret de bus, la loi normale semble corespondre. 

# On cherche maintenant les paramètres de cet loi.
# On choisis la borne min a = 0, car ils peuvent ne pas attendre le bus du tout. 
# On doit donc déterminer la borne sup, T, correspondant à la période entre le passage de deux bus.

# Marius propose 10
mean(data) * 2 
# Jeannette propose 9,5
max(data)

# Protocole de simulation
# On propose de tirer des échantillons aléatoires avec un T fixé, puis de calculer avec chacune des deux méthodes
# (celle de Marius et celle de Jeannette) une approximation de T, et enfin de constater quelle méthode donne le meilleur résultat. 

app <- function() {
  # uniQQplot(x)
  # expQQplot(x)
  # normQQplot(x)
  # 
  
  resM=0
  resJ=0
  moyM=0
  moyJ=0
  b=15
  for(i in 1:1000) {
    x1 = runif(10,0,b)
    marius=(sum(x1)/length(x1))*2
    jeannette=max(x1)
    moyM=moyM+marius
    moyJ=moyJ+jeannette
    resM=resM+abs(b-marius)
    resJ=resJ+abs(b-jeannette)
  }
  print(resJ)
  print(resM)
  print(moyJ/1000)
  print(moyM/1000)
}

# On remarque que avec cette méthode, Marius est meilleur en moyenne, mais moins bon en dispersion. 


#On veut maintenant formaliser le problème
# On pose Theta l'estimation du parametre T
# Theta = f(data)
E(X) = (a+b)/2
E(TETAMarius)= 2somme(i=1 => )
estimateur de marius, convergeant, variance en 1/n

