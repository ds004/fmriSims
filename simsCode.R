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
n = 100
p = 1/((n-1)/2) 
#undirected -> n*(n-1)/2 possible edges. 
elist = rbinom(n*(n-1)/2, 1, p)
Adj = matrix(0, nrow = n, ncol = n)
Adj[lower.tri(Adj)] = elist
Adj[upper.tri(Adj)] = t(Adj)[upper.tri(Adj)]
newgraph = graph.adjacency(Adj, mode = "undirected")
plot(newgraph, vertex.size=3, vertex.label=NA)


#function getting Erdos Renyi graph
ErdRen = function(n) {
	p = 1/((n-1)/2) 
#undirected -> n*(n-1)/2 possible edges. 
	elist = rbinom(n*(n-1)/2, 1, p)
	Adj = matrix(0, nrow = n, ncol = n)
	Adj[lower.tri(Adj)] = elist
	Adj[upper.tri(Adj)] = t(Adj)[upper.tri(Adj)]
	Adj
}

#function getting scalefree graph
ScaFre = function(n) {
	g = sample_pa(n)
	Adj = get.adjacency(g, sparse = FALSE)
	Adj[upper.tri(Adj)] = t(Adj)[upper.tri(Adj)]
	Adj
}


#STEP 2
#okay, given adjacency matrix, how to simulate from Ising
#IsingSampler package seems to be good.
library(IsingSampler)
nsim = 101
graph = Adj
#thresholds = rep(1, 100)
#t = proc.time()[3]
#Okay = IsingSampler(nsim, graph, thresholds)
#proc.time()[3] -t

#questions, what is threshold, what is beta?
#threshold is, yea, individual parameters.  beta is fine
#need to have thing taking -1 and 1 as states
thresholds = rep(0, 100)
t = proc.time()[3]
sim = IsingSampler(nsim, graph, thresholds, responses = c(-1L, 1L))
proc.time()[3]-t
sim = sim == TRUE



#STEP 3
#using IsingFit and see how we do
library(IsingFit)
#IsingFit does number of obs * number of var
#Fit = IsingFit(sim)
Fit = IsingFit(sim)
#note that we DO have to change responses to 1 and 0


#STEP 4
#compare graphs.
#ways to compare graphs. 
#1. # of correct edges
#2. proportion of correct edges
#3. compare over changing misclassification.

fitAdj = Fit$weiadj
#note we have 101 edges and there are 105 true edges! That's good.
sum(fitAdj != 0)/2; sum(Adj)/2

#from weighted adjacency, get adjacency
fitAdj = fitAdj!=0 


#given True and Fitted adjacency matrix, calculate proportion of equal edges
propEqal = function(TrueAdj, fitAdj) {
	sum(TrueAdj == fitAdj)/(dim(TrueAdj)[1]^2)
} 

#calculate proportion of correct non zero edges
NonZero = function(TrueAdj, fitAdj) {
	sum(TrueAdj + fitAdj == 2)/sum(TrueAdj)
}

#proportion of correct zero edges
Zero = function(TrueAdj, fitAdj) {
	sum(TrueAdj + fitAdj == 0)/sum(TrueAdj == 0)
}


#Sims
#1. choose probability of misclassification for each node to be 1-> 10%
#2. simulate 50 times at each misclassification level
#3. do ±2 standard deviation errors bars. draw lines between means and error bars
#redo for scale free graph

misclass = seq(0, 10, by = 1)
out1 = out2 = matrix(NA, nrow = 50, ncol = 11)
thresholds = rep(0, 100)
nsim = 500
#loop over each miclassification thing
for (i in misclass) {
#simulate 50 times
t = proc.time()[3]
	for (k in 1:50) {
		TrueAdj = ErdRen(100)
		sim = IsingSampler(nsim, TrueAdj, thresholds, responses = c(-1L, 1L))
		
		sim = sim==TRUE
		sim = misClass(sim, i)
		Fit = IsingFit(sim, plot = FALSE, progressbar=FALSE)
		fitAdj = Fit$weiadj
		fitAdj = fitAdj!=0 
		out1[k, i+1] = Zero(TrueAdj, fitAdj)
		out2[k, i+1] = NonZero(TrueAdj, fitAdj)
	}
print(proc.time()[3] - t)
}

save(out1, file = 'out1')
save(out2, file = 'out2')

misclass = seq(0, 10, by = 1)
out3 = out4 = matrix(NA, nrow = 50, ncol = 11)
thresholds = rep(0, 100)
nsim = 500
#loop over each miclassification thing
for (i in misclass) {
#simulate 50 times
t = proc.time()[3]
	for (k in 1:50) {
		TrueAdj = ScaFre(100)
		sim = IsingSampler(nsim, TrueAdj, thresholds, responses = c(-1L, 1L))
		
		sim = sim==TRUE
		sim = misClass(sim, i)
		Fit = IsingFit(sim, plot = FALSE, progressbar=FALSE)
		fitAdj = Fit$weiadj
		fitAdj = fitAdj!=0 
		out3[k, i+1] = Zero(TrueAdj, fitAdj)
		out4[k, i+1] = NonZero(TrueAdj, fitAdj)
	}
print(proc.time()[3] - t)
}

save(out3, file = 'out3')
save(out4, file = 'out4')


#misclassifier function, given simulations, randomly 
#misclassify observations
misClass = function(sim, prop) {
	total = prod(dim(sim))
	missed = sample(1:total, rbinom(1, total, prop))
	sim[missed] = !sim[missed]
	sim
}

okay


