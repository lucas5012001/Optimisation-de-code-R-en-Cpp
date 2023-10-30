rm(list=ls())
library(Rcpp)
library(RcppArmadillo)
library(profvis)
set.seed(1234)
source("MainKmeans.R")
sourceCpp("R_cpp.cpp")
x = simuledata(1000,15)
x=x[[1]]
plot(x)

##TP4 test du code c++ vs le code R
a=single_kmeans_cpp(x,matrix(c(1,2,3,4),ncol = 2))
b=singleKmeans(x,matrix(c(1,2,3,4),ncol = 2))
all(a[[1]]+1==b[[1]])    #On ajoute 1 car le vecteur z commence à 0 en c++ et 1 en R
abs(sum(a[[2]]-b[[2]]))
abs(a[[3]]-b[[3]])       #Les quantités sont presque egales, la petite difference vient
                         #du nombre de chiffres significatifs. 

a=mainKmeans_optimise(x,4,100)
b=mainKmeans(x,4,100)
checkpartition(a,b)
abs(sum(a[[2]]-b[[2]]))
abs(a[[3]]-b[[3]])

#mesure du temps entre la méthode R et la méthode optimisée en c++
q = profvis({
  mainKmeans_optimise(x,4,300)
  mainKmeans(x,4,300)
})
htmlwidgets::saveWidget(q, "profile_c++_R.html")
#Les resultats sont impressionnants: la fonction utilisant le c++ met
#239 fois moins de temps que la fonction R


##TP4 test du code c++ vs le code c++ en parallele
c=mainKmeans_optimise(x,4,100)
d=mainKmeans_optimise_et_parallele(x,4,100)
checkpartition(c,d)
abs(sum(c[[2]]-d[[2]]))
abs(c[[3]]-d[[3]])

#mesure du temps entre la méthode optimisée en c++ et la méthode c++ et parallèle
h = profvis({
  mainKmeans_optimise(x,4,300)
  mainKmeans_optimise_et_parallele(x,4,300)
})
htmlwidgets::saveWidget(h, "profile_c++_c++parallele.html")
#On peut conclure qu'il n'est pas rentable de paralleliser ici étant donné
#la rapidité d'execution du code c++. Cela pourrait changer avec des matrices x
#de plus grande taille.