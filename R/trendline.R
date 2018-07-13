#' Add Trendline and Show Equation to Plot
#'
#' Plot and show both regression line and equation as simple as possible,
#' by using different models built in the 'trendline()' function. The function includes the following models in the latest version:
#' "line2P" (formula as: y=a*x+b), "line3P" (y=a*x^2+b*x+c), "log2P" (y=a*ln(x)+b), "exp2P" (y=a*exp(b*x)),"exp3P" (y=a*exp(b*x)+c), "power2P" (y=a*x^b), and "power3P" (y=a*x^b+c).
#' Besides, the summarized results of each fitted model are also output by default.
#'
#' @param x,y  the x and y arguments provide the x and y coordinates for the plot. Any reasonable way of defining the coordinates is acceptable.
#' @param model select which model to fit. Default is "line2P". The "model" should be one of c("line2P", "line3P", "log2P", "exp2P", "exp3P", "power2P", "power3P"), their formulas are as follows:\cr "line2P": y=a*x+b \cr "line3P": y=a*x^2+b*x+c \cr "log2P": y=a*ln(x)+b \cr "exp2P": y=a*exp(b*x) \cr "exp3P": y=a*exp(b*x)+c \cr "power2P": y=a*x^b \cr "power3P": y=a*x^b+c
#' @param linecolor color of regression line.
#' @param Pvalue.corrected if P-value corrected or not, the vlaue is one of c("TRUE", "FALSE").
#' @param lty line type. lty can be specified using either text c("blank","solid","dashed","dotted","dotdash","longdash","twodash") or number c(0, 1, 2, 3, 4, 5, 6). Note that lty = "solid" is identical to lty=1.
#' @param lwd line width. Default is 1.
#' @param summary summarizing the model fits. Default is TRUE.
#' @param ePos equation position, such as one of c("none","bottomright","bottom","bottomleft","left","topleft","top","topright","right","center").
#' @param eDigit the numbers of digits for equation parameters. Default is 5.
#' @param eSize  font size in percentage of equation. Default is 1.
#' @param plot draw a scatter plot automatically, the vlaue is one of c("TRUE", "FALSE").
#' @param ... additional parameters to \code{\link[graphics]{plot}},such as type, main, sub, xlab, ylab, col.

#' @import graphics
#' @import stats
#' @export
#' @details The linear models (line2P, line3P, log2P) in this package are estimated by \code{\link[stats]{lm}} function, \cr while the nonlinear models (exp2P, exp3P, power2P, power3P) are estimated by \code{\link[stats]{nls}} function (i.e., least-squares method).\cr\cr The argument 'Pvalue.corrected' is workful for non-linear regression only.\cr\cr If "Pvalue.corrected = TRUE", the P-vlaue is calculated by using "Residual Sum of Squares" and "Corrected Total Sum of Squares (i.e. sum((y-mean(y))^2))".\cr If "Pvalue.corrected = TRUE", the P-vlaue is calculated by using "Residual Sum of Squares" and "Uncorrected Total Sum of Squares (i.e. sum(y^2))".

#' @return NULL
#' @examples
#' library(basicTrendline)
#' x1<-1:5
#' x2<- -2:2
#' x3<- c(101,105,140,200,660)
#' x4<- -5:-1
#' x5<- c(1,30,90,180,360)
#'
#' y1<-c(2,14,18,19,20)        # increasing convex trend
#' y2<- c(-2,-14,-18,-19,-20)  # decreasing concave trend
#' y3<-c(2,4,16,38,89)         # increasing concave trend
#' y4<-c(-2,-4,-16,-38,-89)    # decreasing convex trend
#' y5<- c(600002,600014,600018,600019,600020) # high y values with low range.
#'
#' trendline(x1,y1,model="line2P",summary=TRUE,eDigit=10)
#' trendline(x2,y2,model="line3P",summary=FALSE,ePos="topright")
#' trendline(x3,y3,model="log2P",linecolor="blue")
#' trendline(x4,y4,model="exp3P",eSize=0.7) #change the font size of equation.
#' trendline(x5,y5,model="power3P")
#'
#' ## Not run
#' plot(x1,y1,main="Different regression lines in one plot")
#' library(basicTrendline)
#' trendline(x1,y1,model="line2P",plot=FALSE,ePos="none",linecolor="red")
#' trendline(x1,y1,model="log2P",plot=FALSE,ePos="none",linecolor="blue",lty=2)
#' trendline(x1,y1,model="exp3P",plot=FALSE,ePos="none",linecolor="black",lty=3)
#' legend("bottomright",c("line2P","log2P","exp3P"), lty=c(1,2,3),col=c("red","blue","black"))
#' ## END (Not run)
#'
#' @author Weiping Mei, Guangchuang Yu
#' @seealso  \code{\link{trendline}}, \code{\link{SSexp3P}}, \code{\link{SSpower3P}}, \code{\link[stats]{nls}}, \code{\link[stats]{selfStart}}

trendline <- function(x, y, model="line2P", Pvalue.corrected=TRUE, linecolor="red", lty=1, lwd=1, summary=TRUE, ePos="topleft", eDigit=5, eSize=1, plot=TRUE, ...)
{
  model=model
  if (plot==TRUE){
  plot(x, y,...)
  }else{}

  OK <- complete.cases(x, y)
  x <- x[OK]
  y <- y[OK]
  z<-data.frame(x,y)

  return <- trendline_summary(x=x, y=y, model=model, Pvalue.corrected=Pvalue.corrected,summary = FALSE,eDigit = 22)
  a = return$parameter$a
  b = return$parameter$b
  if (is.null(return$parameter$c)==FALSE){
    c = return$parameter$c
  }else{}
  pval <- return$p.value
  pval<-unname(pval)
  r2 <- return$R.squared
  adjr2<- return$adj.R.squared

  xx<-seq(min(x),max(x),len=100)

# 1) model="line2P"
if (model== c("line2P"))
  {
  Pvalue.corrected=TRUE
  yfit<- a*xx + b

  if (summary==TRUE){
    trendline_summary(x,y,"line2P", eDigit=eDigit)
  }else{}

  aa = abs(a)
  bb = abs(b)

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

  lines(xx,yfit,lty=lty,col=linecolor,lwd=lwd)  # fit lines using data xx and yfit
 }

# 2) model="line3P"
  if (model== c("line3P"))
  {
    Pvalue.corrected=TRUE
    yfit <- a*xx^2 + b*xx + c

    if (summary==TRUE){
    trendline_summary(x,y,"line3P", eDigit=eDigit)
    }else{}


    aa = abs(a)
    bb = abs(b)
    cc = abs(c)

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

    param[2] <- substitute(expression(italic(R)^2 == r2*","~~italic(p) == pval*"            "))[2]

    lines(xx, yfit, lty=lty, col=linecolor, lwd=lwd)  # fit lines using data xx and yfit

  }

# 3) model="log2P"
if (model== c("log2P"))
  {
  Pvalue.corrected=TRUE

  if (summary==TRUE){
    trendline_summary(x,y,"log2P", eDigit=eDigit)
  }else{}

  if (min(x)>0)
  {
  yfit<- a*log(xx) + b

    aa = abs(a)
    bb = abs(b)

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

    lines(xx, yfit, lty=lty, col=linecolor, lwd=lwd)  # fit lines using data xx and yfit

 }else{
    stop("
'log2P' model need ALL x values greater than 0. Try other models.")
 }
}

# 4.2) model="exp2P"
  if (model== c("exp2P"))
  {
    yfit = a*exp(b*xx)

    if (summary==TRUE){
      trendline_summary(x, y, "exp2P", Pvalue.corrected=Pvalue.corrected, eDigit=eDigit)
    }else{}

    aa= abs(a)
    bb= abs(b)
    param <- vector('expression',2)

    a = format(a, digits = eDigit)
    b = format(b, digits = eDigit)
    aa = format(aa, digits = eDigit)
    bb = format(bb, digits = eDigit)
    r2 = format(r2, digits = eDigit)
    adjr2 = format(adjr2, digits = eDigit)
    pval= format(pval, digits = eDigit)

    if (aa==1){aa=c("")}
    if (bb==1){bb=c("")}

    if (a>=0)
    {
      if (b>=0)
      {
          param[1] <- substitute(expression(italic("y") == aa~"e"^{bb~italic("x")}))[2]
      }else{
          param[1] <- substitute(expression(italic("y") == aa~"e"^{bb~italic("x")}))[2]
      }

    }else{
      if (b>=0)
      {
          param[1] <- substitute(expression(italic("y") == -aa~"e"^{bb~italic("x")}))[2]
      }else{
          param[1] <- substitute(expression(italic("y") == -aa~"e"^{bb~italic("x")}))[2]
      }
    }

    param[2] <- substitute(expression(italic(R)^2 == r2*","~~italic(p) == pval))[2]

    lines(xx,yfit,lty=lty,col=linecolor,lwd=lwd)  # fit lines using data xx and yfit
  }


# 4.3) model="exp3P"
  if (model== c("exp3P"))
  {
    yfit = a*exp(b*xx) + c

    if (summary==TRUE){
      trendline_summary(x, y, "exp3P", Pvalue.corrected=Pvalue.corrected, eDigit=eDigit)
        }else{}

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
      param[1] <- substitute(expression(italic("y") == aa~"e"^{bb~italic("x")}~+cc))[2]
      }else{
      param[1] <- substitute(expression(italic("y") == aa~"e"^{bb~italic("x")}~-cc))[2]
      }
    }else{
      if (c>=0){
        param[1] <- substitute(expression(italic("y") == aa~"e"^{bb~italic("x")}~+cc))[2]
      }else{
        param[1] <- substitute(expression(italic("y") == aa~"e"^{bb~italic("x")}~-cc))[2]
      }
    }

 }else{
   if (b>=0)
   {
     if (c>=0){
       param[1] <- substitute(expression(italic("y") == -aa~"e"^{bb~italic("x")}~+cc))[2]
     }else{
       param[1] <- substitute(expression(italic("y") == -aa~"e"^{bb~italic("x")}~-cc))[2]
     }
   }else{
     if (c>=0){
       param[1] <- substitute(expression(italic("y") == -aa~"e"^{bb~italic("x")}~+cc))[2]
     }else{
       param[1] <- substitute(expression(italic("y") == -aa~"e"^{bb~italic("x")}~-cc))[2]
     }
   }
}
    param[2] <- substitute(expression(italic(R)^2 == r2*","~~italic(p) == pval))[2]

    lines(xx,yfit,lty=lty,col=linecolor,lwd=lwd)  # fit lines using data xx and yfit
  }


# 5.2) model="power2P"
if (model== c("power2P"))
  {
    if (summary==TRUE){
      trendline_summary(x, y, "power2P", Pvalue.corrected=Pvalue.corrected, eDigit=eDigit)
    }else{}

    if (min(x)>0){
    yfit = a*xx^b

      aa<-abs(a)

      param <- vector('expression',2)

      a = format(a, digits = eDigit)
      b = format(b, digits = eDigit)
      aa = format(aa, digits = eDigit)
      r2 = format(r2, digits = eDigit)
      adjr2 = format(adjr2, digits = eDigit)
      pval = format(pval, digits = eDigit)

      if (aa==1){aa=c("")}

      if (a>=0)
      {
        param[1] <- substitute(expression(italic("y") == aa~italic("x")^b))[2]
      }else{
        param[1] <- substitute(expression(italic("y") == -aa~italic("x")^b))[2]
      }
      param[2] <- substitute(expression(italic(R)^2 == r2*","~~italic(p) == pval))[2]

      lines(xx, yfit, lty=lty, col=linecolor, lwd=lwd)  # fit lines using data xx and yfit
    }else{
      stop("
           'power2P' model need ALL x values greater than 0. Try other models.")
    }
}


 # 5.3) model="power3P"
if (model== c("power3P"))
    {
    if (summary==TRUE){
      trendline_summary(x,y,"power3P", Pvalue.corrected=Pvalue.corrected, eDigit=eDigit)
    }else{}

    if (min(x)>0){
      yfit = a*xx^b + c
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

      lines(xx,yfit,lty=lty,col=linecolor,lwd=lwd)  # fit lines using data xx and yfit
    }else{
    stop("
'power3P' model need ALL x values greater than 0. Try other models.")
  }

# 100) beyond the  built-in models.

}else{
  Check<-c("line2P","line3P","log2P","exp2P","exp3P","power2P","power3P")
  if (!model %in% Check)
  stop("
\"model\" should be one of c(\"lin2P\",\"line3P\",\"log2P\",\"exp2P\",\"exp3P\",\"power2P\",\"power3P\".")
}

  if (ePos==c("none")){}else{
  legend(ePos, inset=0,legend = param, cex = eSize,bty = 'n')
  }
}
