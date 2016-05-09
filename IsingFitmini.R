IsingFitmini <- function (x, family = "binomial", AND = TRUE, gamma = 0.25, plot = TRUE, 
    progressbar = TRUE, lowerbound.lambda = NA, fMN, ...) 
{
    t0 <- Sys.time()
    xx <- x
    if (family != "binomial") 
        stop("This procedure is currently only supported for binary (family='binomial') data")
    NodesToAnalyze <- apply(x, 2, sd, na.rm = TRUE) != 0
    names(NodesToAnalyze) <- colnames(x)
    if (!any(NodesToAnalyze)) 
        stop("No variance in dataset")
    if (any(!NodesToAnalyze)) {
        warning(paste("Nodes without variance:", paste(colnames(x)[!NodesToAnalyze], 
            collapse = ", ")))
    }
    x <- as.matrix(x)
    allthemeans <- colMeans(x)
    x <- x[, NodesToAnalyze, drop = FALSE]
    nvar <- ncol(x)
    p <- rep(0, nvar)
    intercepts <- betas <- lambdas <- list(vector, nvar)
    nlambdas <- rep(0, nvar)
    for (i in 1:nvar) {
    	if (i %in% fMN) {
    		vars = unique(c(fMN, unlist(neighborhood(graphFit, order = 1, nodes = i))))
			vars = vars[vars!= i]
			p[i] = length(vars)
        a <- glmnet(x[, vars], x[, i], family = family)
        intercepts[[i]] <- a$a0
        betas[[i]] <- a$beta
        lambdas[[i]] <- a$lambda
        nlambdas[i] <- length(lambdas[[i]])
        }
    }
    if (progressbar == TRUE) 
        pb <- txtProgressBar(max = nvar, style = 3)
    P <- logl <- sumlogl <- J <- matrix(0, max(nlambdas), nvar)
    for (i in 1:nvar) {
    	if (i %in% fMN) J[1:ncol(betas[[i]]), i] <- colSums(betas[[i]] != 0)
    }
    logl_M <- P_M <- array(0, dim = c(nrow(x), max(nlambdas), 
        nvar))
    N <- nrow(x)
    for (i in 1:nvar) {
    	if (i %in% fMN) {
        betas.ii <- as.matrix(betas[[i]])
        int.ii <- intercepts[[i]]
        y <- matrix(0, nrow = N, ncol = ncol(betas.ii))
        xi <- x[, -i]
        NB <- nrow(betas.ii)
        for (bb in 1:NB) {
            y <- y + betas.ii[rep(bb, N), ] * xi[, bb]
        }
        y <- matrix(int.ii, nrow = N, ncol = ncol(y), byrow = TRUE) + 
            y
        n_NA <- max(nlambdas) - ncol(y)
        if (n_NA > 0) {
            for (vv in 1:n_NA) {
                y <- cbind(y, NA)
            }
        }
        P_M[, , i] <- exp(y * x[, i])/(1 + exp(y))
        logl_M[, , i] <- log(P_M[, , i])
        if (progressbar == TRUE) 
            setTxtProgressBar(pb, i)
            }
    }
    logl_Msum <- colSums(logl_M, 1, na.rm = FALSE)
    if (progressbar == TRUE) 
        close(pb)
    sumlogl <- logl_Msum
    sumlogl[sumlogl == 0] = NA
    penalty <- J * log(nrow(x)) + 2 * gamma * sweep(J, 2, log(p), '*')
    EBIC <- -2 * sumlogl + penalty
    lambda.mat <- matrix(NA, nrow(EBIC), ncol(EBIC))
    for (i in 1:nvar) {
    	if (i %in% fMN) {
        lambda.mat[, i] <- c(lambdas[[i]], rep(NA, nrow(EBIC) - 
            length(lambdas[[i]])))
            }
    }
    if (!is.na(lowerbound.lambda)) {
        EBIC <- EBIC/(lambda.mat >= lowerbound.lambda) * 1
    }
    lambda.opt <- apply(EBIC, 2, which.min)
    lambda.out = rep(0, nvar)
    for (i in 1:nvar) {
    	if (i %in% fMN) {
    		lambda.out[i] = lambda.mat[lambda.opt[[i]], i]
    		}
    }
    lambda.out
    }