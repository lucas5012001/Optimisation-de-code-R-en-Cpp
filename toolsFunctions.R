simuledata = function(n,delta){
  resultat=matrix(NA,nrow = n, ncol = 2)
  z=matrix(NA,nrow = n,ncol = 1)
  for(i in 1:n){
    zi=sample(c(1:4),1,1/4)
    if(zi==1){
      xi1=rnorm(1,delta,1)
      xi2=rnorm(1,delta,1)
    }
    else if(zi==2){
      xi1=rnorm(1,delta,1)
      xi2=rnorm(1,-delta,1)
    }
    else if(zi==3){
      xi1=rnorm(1,-delta,1)
      xi2=rnorm(1,delta,1)
    }
    else{
      xi1=rnorm(1,-delta,1)
      xi2=rnorm(1,-delta,1)
    }
    resultat[i,]=c(xi1,xi2)
    z[i,]=c(zi)
  }
  return(list(resultat,z))
}

giveStarting = function(x,nbgroupes,nbinits){
  n=length(x[,1])
  liste_centres=as.list(rep(NA,nbinits))
  liste_centres=lapply(liste_centres,function(i,x,n,nbgroupes){return(x[sample(c(1:n),nbgroupes,1/n),])},x=x,n=n,nbgroupes=nbgroupes)
  return(liste_centres)
}

matrice_de_distances=function(x,centers){
  k=length(centers[,1])
  n=length(x[,1])
  matrice=matrix(NA,nrow =  n,ncol =  k)
  for(j in 1:k){
    matrice[,j]=colSums(apply(x,1,function(vect){return(vect-centers[j,])})**2)
  }
  return(matrice)
}

calcul_z = function(matrice_de_distance){
  return(apply(matrice_de_distance,1,which.min))
}

mise_a_jour_centres = function(x,z,centers){
  n=length(x[,1])
  k=length(centers[,1])
  for(k in 1:k){
    vecteur_encodage = matrix(0,nrow = n, ncol = 1)
    vecteur_encodage[which(z==k),1]=1
    taille_groupe = sum(vecteur_encodage)
    if(taille_groupe>0){
      centers[k,] = (1/taille_groupe)*(t(x)%*%vecteur_encodage)
      }
    
  }
  return(centers)
}

inertie_intra_classes = function(x,z,centers){
  inertie=0
  for(i in 1:length(x[,1])){
    inertie=inertie+sum((x[i,]-centers[z[i],])**2)
  }
  return(inertie)
}

checkpartition = function(a,b){
  b0=b
  n=length(a[[1]])
  k=length(a[[2]][,1])
  p=length(a[[2]][1,])
  if(length(b0[[2]][,1])!=k || length(b0[[1]])!=n || length(b0[[2]][1,])!=p){return(FALSE)}
  else{
    for(i in 1:n){
      if(a[[1]][i]!=b0[[1]][i]){
        ancienne_val=b0[[1]][i]
        for(i in 1:n){
          if(b0[[1]][i]==ancienne_val){
            b0[[1]][i]=a[[1]][i]
          }
        }
      }
    }
  }
  test=sum(abs(b0[[1]]-a[[1]]))
  return(test<1)
}