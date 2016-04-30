#EM algorithm on areas of the graph


#Step 1. Small groups or something. small amount of nodes. maybe 1 misclassified edge. 

#Step 2. Write out the Q function, and how to maximize it. 


#Okay, so let's see. 

#Generate true network
#Use ScaFre from simsCode.R

trueNet = ScaFre(50)

#okay now using that, generate some Ising data. 
#Use IsingSampler function from simsCode.R

nsim = 100
thresholds = rep(0, 50)
sim = IsingSampler(nsim, trueNet, thresholds, responses = c(-1L, 1L)) == TRUE

#misclassify 3 connected nodes 10% of the time. 


#initial fit