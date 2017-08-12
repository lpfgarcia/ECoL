# R Code
# Measures of Linearity
# L. P. F. Garcia A. C. Lorena and M. de Souto 2016
# The set of Linearity and Non Linearity Measures

# These measures are described in:
#@article{ho2002complexity,
#  title={Complexity measures of supervised classification problems},
#  author={Ho, Tin Kam and Basu, Mitra},
#  journal={IEEE transactions on pattern analysis and machine intelligence},
#  volume={24},
#  number={3},
#  pages={289--300},
#  year={2002},
#  publisher={IEEE}
#}

# Aplying SMO algorithm to the data frame, with linear Kernel
smo <- function(data) {
	# ??? Why not scale = TRUE??
	svm(class ~ ., data, scale=FALSE, kernel="linear")
}

# Auxiliary function for computing the diagonal of hyperretangle containing all data
# ??? Please see if it is correct (takes two vectors: with maximumns and minimuns per column. Then computes their euclidean distance.
hyperretangle <- function(data) {
       data <- data[-data$class] # removing the labels
       # taking the maximums per column
       max <- colMax(data)
       # taking the minimums per column
       min <- colMin(data)
       return(sqrt(sum((max-min)^2))) # size of the diagonal
}

# Minimized sum of error distance by linear programming (L1)
l1 <- function(model, data) {

	aux = mapply(function(m, d) {
		prd = predict(m, d, decision.values=TRUE) # predictions for the examples
		err = rownames(d[prd != d$class,]) # taking the erroneous instances
		dst = attr(prd, "decision.values")[err,] # ??? You should take 1-y_i *pred(x_i)
		sum(abs(dst))/(nrow(d)*hyperretangle(d)) # other normalization factor, diagonal of hyperretangle containing all examples 
	}, m=model, d=data)

	aux = max(aux) # maximum between all pairwise sub-problems???
	return(aux)
}

# Error rate of a classifier
error <- function(pred, class) {
	1 - sum(diag(table(class, pred)))/sum(table(class, pred))
}

# Error rate of linear classifier by LP (L2)
l2 <- function(model, data) {

	aux = mapply(function(m, d) {
		pred = predict(m, d)
		error(pred, d$class)
	}, m=model, d=data)

	return(mean(aux)) # ??? why in L1 you take a maximum and here you take an average??
}

# Randomly interpolating elements from the same class 
interpolation <- function(data) {

	aux = sample(levels(data$class), 1)
	aux = filter(data, class == aux) %>% sample_n(., 2)

	for(i in 1:(ncol(data)-1)) {
		if(is.numeric(data[,i])) { # it is applicable to numerical data only
			rnd = runif(1)
			aux[1,i] = aux[1,i]*rnd + aux[2,i]*(1-rnd) # random interpolation
		}
		else{
			rnd = runif(1) # for nominal values, chooses one of them randomly to be in the new example
			if(rnd > 0.5){
			   aux[1,i] = aux[2,i]
			}
		}
	}

	return(aux[1,])
}

# Generating interpolated dataset
generate <- function(data) {

	tmp = do.call("rbind",
		lapply(1:nrow(data), # ??? It will contain the same number of examples as the original dataset?
			function(i) {
				interpolation(data)
		})
	)

	return(tmp)
}

# Nonlinearity of linear classifier by LP (L3)
l3 <- function(model, data) {

	aux = mapply(function(m, d) {
		tmp = generate(d)
		pred = predict(m, tmp) 
		error(pred, tmp$class) # taking the error rate of the linear predictor on interpolated data
	}, m=model, d=data)

	return(mean(aux)) # ??? Why here you take the average and in L1 you take the maximum value??
}

# Function for calling the linearity measures
linearity <- function(data) {

	data = ovo(data) # they are applicable to binary problems only, therefore the multiclass problem has to be decomposed previously
	model = lapply(data, smo) # one model for each OVO subproblem

	aux = lapply(LINEARITY, 
		function(i) {
			do.call(i, list(model, data))
	})

	aux = unlist(aux)
	names(aux) = LINEARITY
	return(aux)
}

