#alright, simulations for the fMRI thing

#so important simulations. comparing ravikumar on graphs with no misclassification
#and graphs with misclassification.

#step 1, create networks... scale free?
#step 2, generate Ising Data with those networks
#step 3, fit IsingFit on those networks!

#Okay, cool. 

#Step 1. can do Erdos Renyi Model and scale-free. It is possibe that scale-free is hard

#scalefree network simulation: http://gdangelo.web.cs.unibo.it/pool/ricerca/simutools2009/gda-simutools-2009.pdf

#here might be better, in terms of goiing fast. the above link is too general
#http://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=5349196

#f[i] denotes an indicator of the node v[i] in the case of adding a new node
#0 < w < sum k (where k is the number of neighbors of each node), w is randomly generated
#g[i] = w-k[i]
#f[i] = 1 if g[i] < 0, 0 if g[i] >= 0

RXGENscale_free = function(n, gamma) {
  
}


#Emergence of Scaling in Random Networks, Barabasi, Albert

BAGenScale_free = function(n, gamma) {
  
  
}



#perhaps we can just use igraph
install.packages("igraph")
library(igraph)

#the function sample_pa() generates scale-free graphs according to Barabasi-Albert model
g = sample_pa(1000)
