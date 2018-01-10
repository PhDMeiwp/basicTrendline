
trendline <- function(x,y,model="line2P", plot=TRUE, linecolor="red", lty=1, lwd=1, summary=TRUE, ePos="topleft", eDigit=5, eSize=1)
{
  model=model
  if (plot==TRUE){
  plot(x, y)
  }else{}

  OK <- complete.cases(x, y)
  x <- x[OK]
  y <- y[OK]
  z<-data.frame(x,y)

# 1) model="line2P"
if (model== c("line2P"))
  {
  formula = 'y = a*x + b'

  fit<- lm(y~x)
  sum.line2P <- summary(fit)

  if (summary==TRUE){
  print(sum.line2P,digits=eDigit)
  }else{}

  coeff<-sum.line2P$coefficients
  a<-coeff[2,1]   # slope
  b<-coeff[1,1]   # intercept
  aa = abs(a)
  bb = abs(b)
  n<-length(x)
  pval <- coeff[2,4]   # p-value of parameter "a", indicates the p-value of whole model.
  pval<-unname(pval)
  r2 <- sum.line2P$r.squared
  adjr2<- sum.line2P$adj.r.squared

  param <- vector('expression',2)

  a = format(a, digits = eDigit)
  b = format(b, digits = eDigit)
  aa = format(aa, digits = eDigit)
  bb = format(bb, digits = eDigit)
  r2 = format(r2, digits = eDigit)
  adjr2 = format(adjr2, digits = eDigit)
  pval = format(pval, digits = eDigit)

  if (aa==1){aa=c("")}

if (a>0)
{
  if (b>=0)
  {param[1] <- substitute(expression(italic("y") == aa~italic("x") + bb))[2]
  }else{param[1] <- substitute(expression(italic("y") == aa~italic("x") - bb))[2]
  }
}else{
  if (b>=0)
  {param[1] <- substitute(expression(italic("y") == -aa~italic("x") + bb))[2]
  }else{param[1] <- substitute(expression(italic("y") == -aa~italic("x") - bb))[2]
  }
}

  param[2] <- substitute(expression(italic(R)^2 == r2*","~~italic(p) == pval))[2]

  a=as.numeric(a)
  b=as.numeric(b)
  param.out<- c(list("a"=a,"b"=b)) # for return values

  xx<-seq(min(x),max(x),len=10000)

  yfit<-predict(fit,data.frame(x=xx))   # definite x using xx

  lines(xx,yfit,lty=lty,col=linecolor,lwd=lwd)  # fit lines using data xx and yfit
 }

# 2) model="line3P"
  if (model== c("line3P"))
  {
    formula = 'y = a*x^2 + b*x + c'

    fit<-lm(y~I(x^2)+x)

    sum.line3P <- summary(fit)

    if (summary==TRUE){
    print(sum.line3P,digits=eDigit)
    }else{}

    coeff<-coef(sum.line3P)
    a<-coeff[2,1] # slope of x.square
    b<-coeff[3,1] # slope of x
    c<-coeff[1,1] # intercept c
    aa = abs(a)
    bb = abs(b)
    cc = abs(c)

    n<-length(x)
    r2<-sum.line3P$r.squared
    adjr2 <- sum.line3P$adj.r.squared

    fstat<-sum.line3P$fstatistic
    pval<-pf(fstat[1], fstat[2], fstat[3], lower.tail=FALSE) #p-value of whole model.
    pval<-unname(pval)

    param <- vector('expression',2)

    a = format(a, digits = eDigit)
    b = format(b, digits = eDigit)
    c = format(c, digits = eDigit)
    aa = format(aa, digits = eDigit)
    bb = format(bb, digits = eDigit)
    cc = format(cc, digits = eDigit)
    r2 = format(r2, digits = eDigit)
    adjr2 = format(adjr2, digits = eDigit)
    pval = format(pval, digits = eDigit)

    if (aa==1){aa=c("")}
    if (bb==1){bb=c("")}

  if (a>0)
  {
    if (b>=0)
    {
      if(c>=0)
      {param[1] <- substitute(expression(italic("y") == aa~italic("x")^2 + bb~italic("x") +cc))[2]
      }else{param[1] <- substitute(expression(italic("y") == aa~italic("x")^2 + bb~italic("x") -cc))[2]
      }
    }else{
      if(c>=0)
      {param[1] <- substitute(expression(italic("y") == aa~italic("x")^2 - bb~italic("x") +cc))[2]
      }else{param[1] <- substitute(expression(italic("y") == aa~italic("x")^2 - bb~italic("x") -cc))[2]
      }
    }

  }else{
    if (b>=0)
    {
      if(c>=0)
      {param[1] <- substitute(expression(italic("y") == -aa~italic("x")^2 + bb~italic("x") +cc))[2]
      }else{param[1] <- substitute(expression(italic("y") == -aa~italic("x")^2 + bb~italic("x") -cc))[2]
      }
    }else{
      if(c>=0)
      {param[1] <- substitute(expression(italic("y") == -aa~italic("x")^2 - bb~italic("x") +cc))[2]
      }else{param[1] <- substitute(expression(italic("y") == -aa~italic("x")^2 - bb~italic("x") -cc))[2]
      }
    }

  }

    param[2] <- substitute(expression(italic(R)^2 == r2*","~~italic(p) == pval))[2]

    a=as.numeric(a)
    b=as.numeric(b)
    c=as.numeric(c)
    param.out<- c(list("a"=a,"b"=b,"c"=c))

    xx<-seq(min(x),max(x),len=10000)
    xx.square=xx^2

    yfit<-predict(fit,data.frame(x=xx,x.square=xx.square))   # definite x using xx

    lines(xx,yfit,lty=lty,col=linecolor,lwd=lwd)  # fit lines using data xx and yfit

  }

# 3) model="log2P"
if (model== c("log2P"))
  {
  formula = 'y = a*ln(x) + b'

  yadj<-y-min(y) #adjust

  if (min(x)>0)
  {
    if (summary==TRUE){
      fit0<-lm(y~log(x))
    sum.log0<-summary(fit0)
    print(sum.log0, digits = eDigit)
    }else{}

    fit<-lm(yadj~log(x))  # adjusted y used
    sum.log<-summary(fit)
    a<-sum.log$coefficients[2,1]  # slope
    b<-sum.log$coefficients[1,1]  # intercept
    b=b+min(y)  #re-adjust
    aa = abs(a)
    bb = abs(b)

    fstat<-sum.log$fstatistic
    pval<-pf(fstat[1], fstat[2], fstat[3], lower.tail=FALSE) #p-value of whole model.
    pval<-unname(pval)

    n<-length(x)
    r2<-sum.log$r.squared
    adjr2 <- sum.log$adj.r.squared

    param <- vector('expression',2)

    a = format(a, digits = eDigit)
    b = format(b, digits = eDigit)
    aa = format(aa, digits = eDigit)
    bb = format(bb, digits = eDigit)
    r2 = format(r2, digits = eDigit)
    adjr2 = format(adjr2, digits = eDigit)
    pval= format(pval, digits = eDigit)

  if (aa==1){aa=c("")}

  if (a>0)
  {
    if (b>=0)
    {
      param[1] <- substitute(expression(italic("y") == aa~"ln(x)" + bb))[2]
    }else{
      param[1] <- substitute(expression(italic("y") == aa~"ln(x)" - bb))[2]
    }

  }else{
    if (b>=0)
    {
      param[1] <- substitute(expression(italic("y") == -aa~"ln(x)" + bb))[2]
    }else{
      param[1] <- substitute(expression(italic("y") == -aa~"ln(x)" - bb))[2]
    }
  }

    param[2] <- substitute(expression(italic(R)^2 == r2*","~~italic(p) == pval))[2]

    a=as.numeric(a)
    b=as.numeric(b)
    param.out<- c(list("a"=a,"b"=b))

    xx<-seq(min(x),max(x),len=10000)

    yfit<-predict(fit,data.frame(x=xx))   # definite x using xx

    yfit<-yfit+min(y)  # re-adjust

    lines(xx,yfit,lty=lty,col=linecolor,lwd=lwd)  # fit lines using data xx and yfit

 }else{
    stop("
'log2P' model need ALL x values greater than 0. Try other models.")
  }
  }

# 4) model="exp3P"
  if (model== c("exp3P"))
  {
    formula = 'y = a*exp(b*x) + c'

    yadj<-y-min(y)+1
    zzz<-data.frame(x,yadj)

    n=length(x)
    k = 3     # k means the count numbers of parameters(i.e., 'a', 'b' and 'c' in this case)

# use selfStart function 'SSexp3P' for y = a *exp(b*x)+ c
# fit model
    fit<-nls(yadj~SSexp3P(x,a,b,c),data=zzz) # use 'yadj', in case of extreme high y-values with low range, such as y= c(600002,600014,600018,600019,600020).
    sum.exp3P <- summary(fit)   # Get the exact value of each parameter.

    ### calculate the F-statistic and p-value for model
    ss.res<-sum((residuals(fit))^2) # Residual Sum of Squares, DF= n-k
    ss.reg<-sum(y^2)-ss.res                # Regression Sum of Squares, DF= k in this case
    # ss.total.uncor<-sum(y^2)             # Uncorrected Total Sum of Squares, DF=n
    # ss.total.cor<-sum((y-mean(y))^2)     # Corrected Total Sum of Squares, DF=n-1

    dfE= n-k  # degrees of freedom for Error (or Residuals)
    dfR= k-1    # degrees of freedom for Regression
    Fval=(ss.reg/3)/(ss.res/dfE)
    pval=pf(Fval,dfR,dfE,lower.tail = F)
    pval<-unname(pval)

    RSE<-sum.exp3P$sigma  # Residual standard error, type ?summary.nls in R for more detials.
    SSE<-(RSE^2)*(n-1)      # Sum of Squares for Error, not equal to 'ss.res'.

    adjr2 <- 1-SSE/((var(y))*(n-1))
    r2<-1-(1-adjr2)*((n-k)/(n-1))

    if (summary==TRUE){
    ### Start print step by step
    #re-adjust the output of coefficients.
    coeffadj = coef(sum.exp3P)
    ab.param<-coeffadj[1:2,]
    # re-adjust the Estimate value of parameter c
    c.param<-coeffadj[3,]
    c.p1<-c.param[1]
    c.p1 = c.p1 + min(y)-1  # re-adjust 'Estimate' value
    c.se<-c.param[2]  # Std.Error value
    c.tval<-c.p1/c.se #re-adjust 't-value'
    c.pval<-2 * pt(abs(c.tval), n-k, lower.tail = FALSE) #re-adjust 'p-value'
    c<-c(c.p1,c.se,c.tval,c.pval) # re-adjust
    coeff.re.adj<- rbind(ab.param,c)

    # print
    cat("\nNonlinear regression model\n")
      cat("\nFormula: y = a*exp(b*x) + c","\n")
    df <- sum.exp3P$df
    rdf <- df[2L]
    cat("\nParameters:\n")
    printCoefmat(coeff.re.adj, digits = eDigit)
    cat("\nResidual standard error:",
        format(sum.exp3P$sigma, digits = eDigit), "on", rdf, "degrees of freedom","\n")

    convInfo = fit$convInfo
    iterations<-convInfo$finIter
    tolerance<-convInfo$finTol

    cat("\nNumber of iterations to convergence:",
        format(iterations, digits = eDigit))
    cat("\nAchieved convergence tolerance:",format(tolerance, digits = eDigit),"\n")

    cat("\nMultiple R-squared:",
        format(r2, digits = eDigit), ", Adjusted R-squared: ",
        format(adjr2, digits = eDigit))
    cat("\nF-statistic:",
        format(Fval, digits = eDigit), "on", dfR, "and", dfE, "DF, ", "p-value:", format(pval, digits = eDigit), "\n")
    ### finished print
    }else{}

    coeff<-sum.exp3P$coefficients
    a<-coeff[1,1]
    b<-coeff[2,1]
    c<-coeff[3,1]+min(y)-1
    aa= abs(a)
    bb= abs(b)
    cc= abs(c)

    param <- vector('expression',2)

    a = format(a, digits = eDigit)
    b = format(b, digits = eDigit)
    c = format(c, digits = eDigit)
    aa = format(aa, digits = eDigit)
    bb = format(bb, digits = eDigit)
    cc = format(cc, digits = eDigit)
    r2 = format(r2, digits = eDigit)
    adjr2 = format(adjr2, digits = eDigit)
    pval= format(pval, digits = eDigit)

 if (aa==1){aa=c("")}
 if (bb==1){bb=c("")}

 if (a>=0)
   {
    if (b>=0)
    {
      if (c>=0){
      param[1] <- substitute(expression(italic("y") == aa~"exp"(bb~italic("x"))~+cc))[2]
      }else{
      param[1] <- substitute(expression(italic("y") == aa~"exp"(bb~italic("x"))~-cc))[2]
      }
    }else{
      if (c>=0){
        param[1] <- substitute(expression(italic("y") == aa~"exp"(bb~italic("x"))~+cc))[2]
      }else{
        param[1] <- substitute(expression(italic("y") == aa~"exp"(bb~italic("x"))~-cc))[2]
      }
    }

 }else{
   if (b>=0)
   {
     if (c>=0){
       param[1] <- substitute(expression(italic("y") == -aa~"exp"(bb~italic("x"))~+cc))[2]
     }else{
       param[1] <- substitute(expression(italic("y") == -aa~"exp"(bb~italic("x"))~-cc))[2]
     }
   }else{
     if (c>=0){
       param[1] <- substitute(expression(italic("y") == -aa~"exp"(bb~italic("x"))~+cc))[2]
     }else{
       param[1] <- substitute(expression(italic("y") == -aa~"exp"(bb~italic("x"))~-cc))[2]
     }
   }
}
    param[2] <- substitute(expression(italic(R)^2 == r2*","~~italic(p) == pval))[2]

    a=as.numeric(a)
    b=as.numeric(b)
    c=as.numeric(c)
    param.out<- c(list("a"=a,"b"=b,"c"=c))

    xx<-seq(min(x),max(x),len=10000)

    yfit<-predict(fit,data.frame(x=xx))   # definite x using xx

    yfit=yfit+min(y)-1

    lines(xx,yfit,lty=lty,col=linecolor,lwd=lwd)  # fit lines using data xx and yfit
  }

 # 5) model="power3P"
if (model== c("power3P"))
    {
    formula = 'y = a*x^b + c'

    yadj<-y-min(y)+1
    zzz<-data.frame(x,yadj)

    n<-length(x)
    k =  3  # k means the count numbers of parameters (i.e., a, b and c in this case)

    if (min(x)>0){
# use selfStart function 'SSpower3P' for y = a *x^b+ c
# trendline model
      fit<-nls(yadj~SSpower3P(x,a,b,c),data=zzz)  # use 'yadj', in case of extreme high y-values with low range.
      sum.power3P <- summary(fit)

      ### calculate the F-statistic and p-value for model
      ss.res<-sum((residuals(fit))^2) # Residual Sum of Squares, DF= n-k
      ss.reg<-sum(y^2)-ss.res                # Regression Sum of Squares, DF= k in this case
      # ss.total.uncor<-sum(y^2)             # Uncorrected Total Sum of Squares, DF=n
      # ss.total.cor<-sum((y-mean(y))^2)     # Corrected Total Sum of Squares, DF=n-1

      dfE= n-k  # degrees of freedom for Error (or Residuals)
      dfR= k-1    # degrees of freedom for Regression
      Fval=(ss.reg/3)/(ss.res/dfE)
      pval=pf(Fval,dfR,dfE,lower.tail = F)
      pval<-unname(pval)

      RSE<-sum.power3P$sigma  # Residual standard error, type ?summary.nls in R for more detials.
      SSE<-(RSE^2)*(n-1)      # Sum of Squares for Error, not equal to 'ss.res'.

      adjr2 <- 1-SSE/((var(y))*(n-1))
      r2<-1-(1-adjr2)*((n-k)/(n-1))

      if (summary==TRUE){

        ### Start print step by step
        #re-adjust the output of coefficients.
        coeffadj = coef(sum.power3P)
        ab.param<-coeffadj[1:2,]
        # re-adjust the 'Estimate\ value of parameter 'c'
        c.param<-coeffadj[3,]
        c.p1<-c.param[1]
        c.p1 = c.p1 + min(y)-1  # re-adjust
        c.se<-c.param[2]  # Std.Error value
        c.tval<-c.p1/c.se #re-adjust 't-value'
        c.pval<-2 * pt(abs(c.tval), n-k, lower.tail = FALSE) #re-adjust 'p-value'
        c<-c(c.p1,c.se,c.tval,c.pval) # re-adjust
        coeff.re.adj<- rbind(ab.param,c)

        # print
        cat("\nNonlinear regression model\n")
          cat("\nFormula:  y = a*x^b + c","\n")
        df <- sum.power3P$df
        rdf <- df[2L]
        cat("\nParameters:\n")
        printCoefmat(coeff.re.adj, digits = eDigit)
        cat("\nResidual standard error:",
            format(sum.power3P$sigma, digits = eDigit), "on", rdf, "degrees of freedom","\n")

        convInfo = fit$convInfo
        iterations<-convInfo$finIter
        tolerance<-convInfo$finTol

        cat("\nNumber of iterations to convergence:",
            format(iterations, digits = eDigit))
        cat("\nAchieved convergence tolerance:",format(tolerance, digits = eDigit),"\n")

        cat("\nMultiple R-squared:",
            format(r2, digits = eDigit), ", Adjusted R-squared: ",
            format(adjr2, digits = eDigit))
        cat("\nF-statistic:",
            format(Fval, digits = eDigit), "on", dfR, "and", dfE, "DF, ", "p-value:", format(pval, digits = eDigit), "\n")
      ### finished print
        }else{}

      coeff<-sum.power3P$coefficients
      a<-coeff[1,1]
      b<-coeff[2,1]
      c<-coeff[3,1]
      c<-c+min(y)-1  #re-adjust
      aa<-abs(a)
      cc<-abs(c)

      param <- vector('expression',2)

      a = format(a, digits = eDigit)
      b = format(b, digits = eDigit)
      c = format(c, digits = eDigit)
      aa = format(aa, digits = eDigit)
      cc = format(cc, digits = eDigit)
      r2 = format(r2, digits = eDigit)
      adjr2 = format(adjr2, digits = eDigit)
      pval = format(pval, digits = eDigit)

 if (aa==1){aa=c("")}

  if (a>=0)
   {
    if (c>=0){
        param[1] <- substitute(expression(italic("y") == aa~italic("x")^b ~ + cc))[2]
        }else{
        param[1] <- substitute(expression(italic("y") == aa~italic("x")^b ~ - cc))[2]
        }

  }else{
    if (c>=0){
      param[1] <- substitute(expression(italic("y") == -aa~italic("x")^b ~ + cc))[2]
    }else{
      param[1] <- substitute(expression(italic("y") == -aa~italic("x")^b ~ - cc))[2]
    }
  }

      param[2] <- substitute(expression(italic(R)^2 == r2*","~~italic(p) == pval))[2]

      a=as.numeric(a)
      b=as.numeric(b)
      c=as.numeric(c)
      param.out<- c(list("a"=a,"b"=b,"c"=c))

      xx<-seq(min(x),max(x),len=10000)

      yfit<-predict(fit,data.frame(x=xx))   # definite x using xx

      yfit=yfit+min(y)-1

      lines(xx,yfit,lty=lty,col=linecolor,lwd=lwd)  # fit lines using data xx and yfit
    }else{
    stop("
'power3P' model need ALL x values greater than 0. Try other models.")
  }

# 6) beyond the  built-in models.

}else{
  Check<-c("line2P","line3P","log2P","exp3P","power3P")
  if (!model %in% Check)
  stop("
\"model\" should be one of c(\"lin2P\",\"line3P\",\"log2P\",\"exp3P\",\"power3P\".")
}

  if (ePos==c("none")){}else{
  legend(ePos, inset=-0.01,legend = param, cex = eSize,bty = 'n')
  }

  r2=as.numeric(r2)
  adjr2=as.numeric(adjr2)
  pval=as.numeric(pval)
  AIC = as.numeric(format(AIC(fit), digits = eDigit))
  BIC = as.numeric(format(BIC(fit), digits = eDigit))

  if (summary==TRUE){
    ##print AIC and BIC
    cat("AIC:", AIC, ", BIC: ", BIC, "\n")
  }else{}

  invisible(list(formula=formula, parameter=param.out, R.squared=r2, adj.R.squared=adjr2, p.value = pval, AIC=AIC, BIC=BIC))
}
