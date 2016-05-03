library(igraph)
library(IsingFit)
library(IsingSampler)

load("trueNet")
load("msim")
load("Fit")


#EM algorithm on areas of the graph


#Step 1. Small groups or something. small amount of nodes. maybe 1 misclassified edge. 

#Step 2. Write out the Q function, and how to maximize it. 


#Okay, so let's see. 

#Generate true network
#Use ScaFre from simsCode.R

trueNet = ScaFre(50)
#save(trueNet, file = 'trueNet')
graph = graph.adjacency(trueNet, mode = "undirected")
plot(graph, vertex.size = 3, vertex.label = NA)
#okay now using that, generate some Ising data. 
#Use IsingSampler function from simsCode.R

nsim = 200
thresholds = rep(0, 50)
sim = IsingSampler(nsim, trueNet, thresholds, responses = c(-1L, 1L)) == TRUE
#misclassify 3 connected nodes 10% of the time. 
#choose 3 nodes that are somewhat highly connected. 
#choose nodes 16, 30, and 44
MISS = c(16, 30, 44)
msim = sim
m16 = sample(1:200,20); m30 = sample(1:200, 20); m44 = sample(1:200, 20)
msim[m16,16]= !msim[m16, 16]
msim[m30, 30] = !msim[m30,30]
msim[m44, 44] = !msim[m44, 44] 
#save(msim, file = "msim")

#the neighborhoods we're interested in are
neighborhood(graph, order = 1, nodes = c(16, 30, 44))

#initial fit
Fit = IsingFit(msim)

#save(Fit, file = "Fit")
fitAdj = Fit$weiadj != 0
graphFit = graph.adjacency(fitAdj, mode = "undirected")

#true M^N is 
tMN = unique(unlist(neighborhood(graph, order = 2, nodes = c(16, 30, 44))))
#fittedM^N is
fMN = unique(unlist(neighborhood(graphFit, order = 2, nodes = c(16, 30, 44))))

#label graphFit
colnames(fitAdj) = 1:50
rownames(fitAdj) = 1:50



#Lines 66-75 don't need to be run
edges = get.edgelist(graphFit)
newEdge = matrix(NA, ncol = 2)[-1,]
for (i in 1:dim(edges)[1]) {
	if ((edges[i,1] %in% fMN) | (edges[i,2] %in% fMN)) newEdge = rbind(newEdge, edges[i,])
}

subNet = graph_from_edgelist(newEdge, directed = FALSE)
K = max(c(newEdge))
if (K < 50) add_vertices(subNet, 50-K)
subAdj = get.adjacency(subNet)

#the lambda we use is 
lam = mean(Fit$lambda.values[fMN])

#now for each node, we need to calculate all the weights

#need to do this given weights

#we update these parameters in an outer loop.
edgePar = Fit$weiadj
nodePar = Fit$thresholds
CLASS = fMN[which(!(fMN %in% MISS))]

partFn = rep(1, 200)
#calculate w_ZM from equation (5)
size = (2^(length(MISS))-1)
w_ZM = matrix(rep(1, size*200), nrow = 200, ncol = 2^(length(MISS)))
W_i = Z_i = array(0, dim = c(length(fMN), 2^(length(MISS)), 200))
X_i = array(0, dim = c(50, 2^(length(MISS)), 200))

for (n in 1:200) {
#calculate the partition function for the conditional
#start partFn and z at 1 since when all obs are 0, A(z_c, z_m) = 1
	obs = msim[n,]
	for (z in 0:(2^(length(MISS))-1)) {
	  zm = as.numeric(intToBits(z))[1:3]
	  newObs = obs
	  newObs[MISS] = zm
	  if (z != 0) {
		  Azczm = assocFunc(zm, MISS, CLASS, edgePar, nodePar, newObs)
		  partFn[n] = partFn[n] + Azczm
		  w_ZM[n, (z+1)] = Azczm
	  }
		for (m in 1:length(fMN)) {
		  newZ = z+1
		  resp = fMN[m]
		  Odds = exp(nodePar[resp] + edgePar[resp,]%*%newObs)
		  fitprob = Odds/(1+Odds)
		  W_i[m, newZ, n] = fitprob*(1-fitprob)*w_ZM[n, newZ]
		  Z_i[m,newZ, n] = edgePar[resp,]%*%newObs + (newObs[m]-fitprob)/
		    (fitprob*(1-fitprob))
		}
	X_i[,newZ, n] = newObs
	}
	w_ZM[n,] = w_ZM[n,]/partFn[n]
}




#Given misclassified z nodes, their vertex numbers, vertex numbers for classified nodes, edge parameters, and node parameters, calculate A(z_c, z_m)
assocFunc <- function(z_m, MISS, CLASS, edge, node, obs) {
	p1 = z_m%*%node[MISS]
	p2 = 0
	for (m in 1:length(MISS)) {
		lnodes = c(MISS, CLASS)[which(c(MISS, CLASS) > MISS[m])]
		if (length(lnodes) > 0)p2 = p2 + z_m[m]*edge[m,lnodes]%*%obs[lnodes]
	}
	exp(p1 + p2)
}






#okay, so in the above calculation of the partFn 
#MISS, CLASS, edgePar, nodePar, msim, 
#edgePar and nodePar change each time.

#need to get weights for the weighted LASSO, and need observations and design matrix
#for each of z_i and w_i, we need the estimate probability for that 


#so we have Z_i, W_i, and X_i
#since we only want to fit on certain nodes, we'll make sure to only fit nodes
#on neighbors U fMN

#THE FINAL FIT
#done at each node
EMweiadj = EMadj = matrix(rep(0, length(fMN)^2), nrow = length(fMN))
EMnod = rep(0, length(fMN))
for (nf in 1:length(fMN)) {
nodeFit = fMN[nf]
MNnodeFit = nf
vars = unique(c(fMN, unlist(neighborhood(graphFit, order = 1, nodes = 
                                           nodeFit))))
vars = vars[vars!= nodeFit]
nodeFitnodeLasso = glmnet(x = matrix(X_i[vars,,], nrow = 1600, byrow = TRUE), 
                          y = c(Z_i[MNnodeFit,,]), family = "gaussian", 
                          weights = c(W_i[MNnodeFit,,]), lambda = lam)

EMnod[nf] = nodeFitnodeLasso$a0
EMedges = nodeFitnodeLasso$beta@i+1
origVars = vars[EMedges]
LassVars = EMedges[which(origVars %in% fMN)]
fMNVar = which(fMN %in% origVars)
EMweiadj[nf,fMNVar] = nodeFitnodeLasso$beta@x[LassVars]
EMadj[nf,fMNVar] = 1
}

#get AND adjacaency matrix
EMadj = EMadj * t(EMadj)
EMweiadj = (EMweiadj + t(EMweiadj))/2
EMweiadj[!EMadj] = 0




#alright, now that that area of the graph has been updated
#note that we're losing neighbors
#change the tuning parameter to 0.03666 (avg tuning parameter in fit over
#smaller network)














