classify <- function(S,z){
	x = sign( S %*% as.vector(z))
	return(x)
}

delta_cp <- function(S,z,y){
  n = dim(S)[1]
	d = dim(S)[2]
	cal_y = classify(S,z)
	sum <- vector(mode="numeric", length=d)
	for (i in 1:n) {
		if(cal_y[i] != y[i]){
			sum = sum - y[i] * S[i,]
		}
	}
	#x = sign( S %*% as.vector(z))
	return(sum)
}

cp <- function(S,z,y){
	n = dim(S)[1]
	cal_y = classify(S,z)
	sum = 0
	for (i in 1:n) {
		if(cal_y[i] != y[i]){
			sum = sum + abs(z %*% S[i,])
		}
	}
	#x = sign( S %*% as.vector(z))
	return(sum)
}

perceptrain <- function(S,y){
	#z <- vector(mode="numeric", length=dim(S)[2])
  z = rep(1,dim(S)[2])
	Z_history = z
	k = 1
	repeat{
	  z = z - (1/k)*delta_cp(S,z,y)
	  k = k + 1
	  Z_history = rbind(Z_history, as.vector(z))
	  if(cp(S,z,y)==0){
	    break
	  }
	}
	return(list(z=z,Z_history=Z_history))
}

check <- function(){
  w <- runif(3, -1, 1)
  fdata = fakedata(w,100)
  res = perceptrain(fdata$S,fdata$y)$z
  sum = 0
  
  fdata1 = fakedata(w,100)
  b = classify(fdata1$S,res)
  a = fdata1$y
  for(i in 1:dim(b)[1]){
    if(a[i]!=b[i]){
      sum = sum+1
    }
  }
  return(sum)
}

scatter_plot <- function(data,tit){
  neg_x = {}
  neg_y = {}
  pos_x = {}
  pos_y = {}
  for(i in 1:dim(data$S)[1]){
    if(data$y[i] == 1){
      pos_x = c(pos_x,data$S[i,1])
      pos_y = c(pos_y,data$S[i,2])
    }
    else{
      neg_x = c(neg_x,data$S[i,1])
      neg_y = c(neg_y,data$S[i,2])
    }
  }
  xmax = max(max(pos_x),max(neg_x))
  xmin = min(min(pos_x),min(neg_x))
  ymax = max(max(pos_y),max(neg_y))
  ymin = min(min(pos_y),min(neg_y))
  plot(pos_x,pos_y,main = tit,
       xlim = c(xmin,xmax),ylim = c(ymin,ymax),xlab = "",ylab = "",pch = 1,cex=0.8)
  par(new = "T")
  plot(neg_x,neg_y,main = "",
       xlim = c(xmin,xmax),ylim = c(ymin,ymax),xlab = "",ylab = "",pch = 17,cex=0.8)
}

proceptron_train_plot <- function(test_data,z){
  scatter_plot(test_data,"Scatter plot for data and classifier")
  abline(-z[3]/z[2],-z[1]/z[2])
}

proceptron_traj_plot <- function(training_data,z){
  scatter_plot(training_data,"Convergence trajactory")
  colfunc <- colorRampPalette(c("gray", "black"))
  c = colfunc(nrow(z$Z_history))
  for(i in 1:dim(z$Z_history)[1] ){
    zh = z$Z_history[i,]
    abline(-zh[3]/zh[2],-zh[1]/zh[2],col = c[i])
  }
}