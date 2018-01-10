
# selfStart method for exp3P model (formula as y = a *exp(b*x)+ c)
SSexp3P<-selfStart(
  function(predictor,a,b,c){a*exp(b*predictor)+c},
  function(mCall,LHS, data)
  {
    xy <- sortedXyData(mCall[["predictor"]],LHS, data)
    y=xy[,"y"]
    x=xy[,"x"]
    adjy=y-min(y)+1
    xadjy=data.frame(x,adjy)

    lmFit <- lm(log(adjy) ~ x)
    coefs <- coef(lmFit)
    get.b <- coefs[2]   #slope

    nlsFit<-nls(adjy~cbind(1+exp(b*x),exp(b*x)),
                start = list(b=get.b),data = xadjy,algorithm = "plinear",
                nls.control(maxiter = 5000000,minFactor = 10^(-10)))

    coef<-coef(nlsFit)
    b<-coef[1]
    c<-coef[2]+min(y)-1
    a<-coef[3]+coef[2]

    value <- c(a,b,c)
    names(value) <- mCall[c("a","b","c")]
    value
  },c("a","b","c"))

  # getInitial(y~SSexp3P(x,a,b,c),data = z)
