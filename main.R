mean.test = function(mydata,alpha=0.05,mu,sigma=NULL,method='unknown'){
  #mydata must be a matrix of n*p.
  #mu must be vector 1*p and sigma must be matrix p*p.
  X_bar = apply(mydata,2,mean)
  n = nrow(mydata)
  p = ncol(mydata)
  
  if (method=='unknown'){
    S = var(mydata)
    A = S*(n-1)
    T_square = n*(n-1)*t(X_bar-mu)%*%solve(A)%*%(X_bar-mu)
    F = (n-p)/(n-1)/p*T_square
    p_value = 1-pf(F,p,n-p)
    statistics = F
    }
  if (method=='known'){
    if (sigma==NULL){
      'error'
      break
    }
    T_square = n*t(X_bar-mu)%*%solve(sigma)%*%(X_bar-mu)
    p_value = 1-pchisq(T_square,p)
    statistics = T_square
  }
  show = list()
  show[['p_value']] = p_value
  show[['statistics']] = statistics
  if (p_value<=alpha){
    hypothesis = 'reject'
  }
  if (p_value>alpha){
    hypothesis = 'do not reject'
  }
  
  show[['hypothesis']] = hypothesis
  return(show)
}
