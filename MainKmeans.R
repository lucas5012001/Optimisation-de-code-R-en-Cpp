library(Rcpp)
library(RcppArmadillo)
library(parallel)
source("toolsFunctions.R")
sourceCpp("R_cpp.cpp")

singleKmeans = function(x,centers){
  k = nrow(centers)
  n = nrow(x)
  zprec=rep(0,n)
  zactu=calcul_z(matrice_de_distances(x,centers))
  while(!all(zprec==zactu)){
    centers=mise_a_jour_centres(x,zactu,centers)
    zprec=zactu
    zactu=calcul_z(matrice_de_distances(x,centers))
  }
  inertie_intra = inertie_intra_classes(x,zactu,centers)
  return(list(zactu,centers,inertie_intra))
}

mainKmeans = function(x,nbgroupes,nbinits){
  inits = giveStarting(x,nbgroupes,nbinits)
  resultats=lapply(inits,singleKmeans,x=x)
  pos_min_inertie=1
  min_inertie = resultats[[pos_min_inertie]][[3]]
  for(i in 1:nbinits){
    if(resultats[[i]][[3]]<min_inertie){
      pos_min_inertie=i
      min_inertie = resultats[[pos_min_inertie]][[3]]
    }
  }
  return(resultats[[pos_min_inertie]])
}

mainKmeans_optimise = function(x,nbgroupes,nbinits){
  inits = giveStarting(x,nbgroupes,nbinits)
  return(main_kmeans_cpp(x,nbgroupes,inits))
}

mainKmeans_optimise_et_parallele = function(data,nbgroupes,nbinits){
  inits = giveStarting(data,nbgroupes,nbinits)
  coeurs = detectCores()
  grappe = makeCluster(coeurs-1)  
  clusterEvalQ(cl = grappe, {require(Rcpp);sourceCpp("R_cpp.cpp");})
  resultats=parLapply(grappe,inits,function(c){return(single_kmeans_cpp(data,c))})
  stopCluster(grappe)
  return(selection_resultat_parallele(resultats))
}