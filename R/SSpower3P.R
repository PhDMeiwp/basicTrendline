
# selfStart method for power3P model (formula as y = a *x^b+ c)
SSpower3P<-selfStart(
  function(predictor,a,b,c){a*predictor^b+c},
  function(mCall,LHS, data)
    {
      xy <- sortedXyData(mCall[["predictor"]],LHS, data)
      y=xy[,"y"]
      x=xy[,"x"]

    if (min(x)>0){

      adjy=y-min(y)+1
      xadjy=data.frame(x,adjy)

      lmFit <- lm(log(adjy) ~ log(x)) # both x and adjy values should be greater than 0.
      coefs <- coef(lmFit)
      get.b <- coefs[2]   #slope

      nlsFit<-nls(adjy~cbind(1+x^b,x^b),
                  start = list(b=get.b),data = xadjy,algorithm = "plinear",
                  nls.control(maxiter = 5000000,minFactor = 10^(-10)))

      coef<-coef(nlsFit)
      b<-coef[1]
      c<-coef[2]+min(y)-1
      a<-coef[3]+coef[2]

      value <- c(a,b,c)
      names(value) <- mCall[c("a","b","c")]
      value

      }else{stop("
>>Try to use other selfStart functions.
Because the 'SSpower3P' function need ALL x values greater than 0.")
        }
    },c("a","b","c"))

    # getInitial(y~SSpower3P(x,a,b,c),data = xy)
