# include <RcppArmadillo.h>
#include <Rcpp.h>
#include <math.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace std;
using namespace arma;



// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
bool test_egalite(mat& matrice1, mat& matrice2){
  int n = matrice1.n_rows;
  int p = matrice1.n_cols;
  if(matrice2.n_rows!=matrice1.n_rows or matrice2.n_cols!=matrice1.n_cols){
    return false;
  }
  else{
    for(int i = 0; i<n; i++){
      for(int j = 0; j<p; j++){
        if(matrice1(i,j)!=matrice2(i,j)){
          return false;
        }
      }
    }
  }
  return true;
}

// [[Rcpp::export]]
mat matrice_de_distances_cpp(mat& matrice, mat& centers){
  int k = centers.n_rows;
  int n = matrice.n_rows;
  int p = matrice.n_cols;
  mat matrice_de_dist = mat(n,k);
  for(int i  = 0 ; i< n; i++){
    for(int j = 0; j<k; j++){
      double elem=0.0;
      for(int l = 0; l<p; l++){
        elem+=pow(matrice(i,l)-centers(j,l),2);
      }
      matrice_de_dist(i,j)=elem;
    }
  }
  return matrice_de_dist;
}

// [[Rcpp::export]]
mat calcul_z_cpp(mat& matrice_de_dist){
  int n = matrice_de_dist.n_rows;
  int k = matrice_de_dist.n_cols;
  mat z = mat(n,1);
  for(int i = 0; i<n; i++){
    int indice_minimum = 0;
    for(int j = 0; j<k; j++){
      if(matrice_de_dist(i,j)<matrice_de_dist(i,indice_minimum)){
        indice_minimum = j;
      }
    }
    z(i,0)=indice_minimum;
  }
  return z;
}

// [[Rcpp::export]]
mat mise_a_jour_des_centres_cpp(mat& matrice, mat& z, int nombre_de_groupes){
  int n = matrice.n_rows;
  int k = nombre_de_groupes;
  int p = matrice.n_cols;
  mat newcenters=mat(k,p);
  for(int num_groupe=0; num_groupe<k; num_groupe++){
    int effectif = 0;
    for(int i = 0; i<n; i++){
      if(z(i,0)==num_groupe){
        effectif+=1;
        for(int num_var=0; num_var<p; num_var++){
          newcenters(num_groupe,num_var)+=matrice(i,num_var);
        }
      }
    }
    for(int num_var=0; num_var<p; num_var++){
      newcenters(num_groupe,num_var)=newcenters(num_groupe,num_var)/effectif;
    }
  }
  return newcenters;
}

// [[Rcpp::export]]
double inertie_intra_classes_cpp(mat& matrice, mat& z, mat& centers){
  int n = matrice.n_rows;
  int p = matrice.n_cols;
  double inertie = 0.0;
  for(int i = 0; i<n; i++){
    for(int j= 0; j<p; j++){
      inertie+=pow(matrice(i,j)-centers(z(i,0),j),2);
    }
  }
  return inertie;
}

// [[Rcpp::export]]
List single_kmeans_cpp(mat& matrice,mat centers){
  int k=centers.n_rows;
  int n=matrice.n_rows;
  mat zprec = mat(n,1);
  mat mat_dist=matrice_de_distances_cpp(matrice,centers);
  mat zactu = calcul_z_cpp(mat_dist);
  while(!test_egalite(zprec,zactu)){
    centers = mise_a_jour_des_centres_cpp(matrice,zactu,k);
    zprec=zactu;
    mat_dist = matrice_de_distances_cpp(matrice,centers);
    zactu=calcul_z_cpp(mat_dist);
  }
  double inertie_intra = inertie_intra_classes_cpp(matrice,zactu,centers);
  List l = List::create(zactu,centers,inertie_intra);
  return l;
}

// [[Rcpp::export]]
List main_kmeans_cpp(mat& matrice,int nbgroupes,List& inits){
  int nbinits = inits.size();
  List resultats = List(nbinits);
  for(int i = 0; i<nbinits ; i++){
    resultats[i]=single_kmeans_cpp(matrice,inits[i]);
  }
  int pos_min_inertie = 0;
  List result = resultats[pos_min_inertie];
  double min_inertie = result[2];
  
  for(int i = 0;i<nbinits;i++){
    result = resultats[i];
    if(result[2]<min_inertie){
      pos_min_inertie=i;
      min_inertie = result[2];
    }
  }
  return resultats[pos_min_inertie];
}

// [[Rcpp::export]]
List selection_resultat_parallele(List& resultats){
  int nbinits = resultats.size();
  int pos_min_inertie = 0;
  List result = resultats[pos_min_inertie];
  double min_inertie = result[2];
  
  for(int i = 0;i<nbinits;i++){
    result = resultats[i];
    if(result[2]<min_inertie){
      pos_min_inertie=i;
      min_inertie = result[2];
    }
  }
  return resultats[pos_min_inertie];
}
// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

*/

