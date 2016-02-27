mydata = read.table("uspsdata/uspsdata.txt")
mydata_y = read.table("uspsdata/uspscl.txt")
index = runif(nrow(mydata))<0.2
validation_data = mydata[index,]
validation_y = mydata_y[index,]
train_data = mydata[!index,]
train_y = mydata_y[!index,]
#linear_svm <- svm(train_data ,train_y , type = "C-classification", cost = 0.01,kernel = "linear",gamma = 0)
#res <- predict(linear_svm,validation_data)

err <- function(res,validation_y){
  n = length(res)
  sum = 0
  for (i in 1:n) {
    if(res[i] != validation_y[i]){
      sum = sum + 1
    }
  }
  return(sum)
}

cross_valid <- function(train_data,train_y){
  cost = c(seq(0.001,0.2,0.001),seq(0.2,1,0.01))
  gamm = c(0.0001,0.001,0.01,0.1)
  err_arr_l = rep(0,length(cost))
  err_arr_rbf = rbind(rep(0,length(cost)),rep(0,length(cost)),rep(0,length(cost)),rep(0,length(cost)))
  sel = runif(nrow(train_data))<0.2
  stand_by = train_data[sel,]
  stand_by_y = train_y[sel]
  n = length(stand_by_y)
  t_data = train_data[!sel,]
  t_y = train_y[!sel]
  for(i in 1:length(cost)){
    l_svm <- svm(t_data ,t_y , type = "C-classification", cost = cost[i] ,kernel = "linear",gamma = 0)
    res <- predict(l_svm,stand_by)
    err_arr_l[i] = err(res,stand_by_y)/n
    for(j in 1:length(gamm)){
      rbf_svm <- svm(t_data ,t_y , type = "C-classification", cost = cost[i] ,kernel = "radial",gamma = gamm[j])
      res <- predict(rbf_svm,stand_by)
      err_arr_rbf[j,i] = err(res,stand_by_y)/n
    }
  }
  return(list(cost=cost,gamm = gamm, err_arr_l=err_arr_l, err_arr_rbf = err_arr_rbf))
}
ans = cross_valid(train_data ,train_y )
col = c("red","blue","black","yellow")
#plot(ans$cost,ans$err_arr_l,x_lim=c(0.001,2),y_lim=c(0,0.05),type="l")
for(g in 1:4){
  plot(ans$cost,ans$err_arr_rbf[g,],xlim=c(0.001,1),ylim=c(0,1),type="l",col = col[g])
  par(new = "T")
}
legend(0.7,0.95,c("0.0001","0.001","0.01","0.1"),lty=c(1,1,1,1),lwd=c(2.5,2.5,2.5,2.5),col=col)

linear_cost =  ans$cost[which(ans$err_arr_l == min(ans$err_arr_l))[1] ]
rbf_ind = which(ans$err_arr_rbf == min(ans$err_arr_rbf) , arr.ind = TRUE )[1,]
rbf_cost = ans$cost[rbf_ind[2]]
rbf_gam = ans$gamm[rbf_ind[1]]

linear_svm <- svm(train_data ,train_y , type = "C-classification", cost = linear_cost ,kernel = "linear",gamma = 0)
res <- predict(linear_svm,validation_data)
sprintf("The error rate of linear_svm on test data is %f, with linear_cost %f",
        err(res,validation_y)/length(validation_y),linear_cost)

rbf_svm <- svm(train_data ,train_y , type = "C-classification", cost = rbf_cost ,kernel = "radial",gamma = rbf_gam)
res <- predict(rbf_svm,validation_data)
sprintf("The error rate of radial_svm on test data is %f, with rbf_cost %f and gamma %f",
        err(res,validation_y)/length(validation_y),rbf_cost,rbf_gam)

