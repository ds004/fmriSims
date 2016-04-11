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
#n is number of nodes
g = sample_pa(n)
#prints vertices
V(g)
#prints edges
E(g)
#prints degree of each vertex
degree(g)
#get adjacency matrix
Adj = get.adjacency(g, sparse = FALSE)
Adj[upper.tri(Adj)] = t(Adj)[upper.tri(Adj)]

newgraph = graph.adjacency(Adj, mode = "undirected")
#plot in a way I can look at
#example plots http://igraph.org/r/doc/plot.common.html
plot(newgraph, vertex.size = 3, vertex.label = NA)


#okay, that's my scale-free graph. now to do Erdos Renyi 
#p is probability of an edge
#n is number of nodes
#set p so that expected number of edges is = n.
p = 1/((n-1)/2) 
#undirected -> n*(n-1)/2 possible edges. 
elist = rbinom(n*(n-1)/2, 1, p)
Adj = matrix(0, nrow = n, ncol = n)
Adj[lower.tri(Adj)] = elist
Adj[upper.tri(Adj)] = t(Adj)[upper.tri(Adj)]
newgraph = graph.adjacency(Adj, mode = "undirected")
plot(newgraph, vertex.size=3, vertex.label=NA)



#STEP 2
#okay, given adjacency matrix, how to simulate from Ising
#IsingSampler package seems to be good.
library(IsingSampler)
n = 101
graph = Adj
thresholds = rep(1, 100)
t = proc.time()[3]
Okay = IsingSampler(n, graph, thresholds)
proc.time()[3] -t

#questions, what is threshold, what is beta?
#threshold is, yea, individual parameters.  beta is fine
#need to have thing taking -1 and 1 as states
thresholds = rep(0, 100)
sim = IsingSampler(n, graph, thresholds, responses = c(-1L, 1L))
sim2 = sim == TRUE


#STEP 3
#using IsingFit and see how we do
library(IsingFit)
#IsingFit does number of obs * number of var
Fit = IsingFit(sim)
Fit2 = IsingFit(sim2)
#note that we DO have to change responses to 1 and 0


#STEP 4
#compare graphs.
#ways to compare graphs. 
#1. # of correct edges
#2. proportion of correct edges
#3. compare over changing misclassification.

fitAdj = Fit2$weiadj
#note we have 101 edges and there are 105 true edges! That's good.
sum(fitAdj != 0)/2; sum(Adj)/2

#from weighted adjacency, get adjacency
fitAdj = fitAdj!=0 



