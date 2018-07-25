#' Add Trendline and Show Equation to Plot
#'
#' Plot, draw regression line and confidence interval, and show regression equation, R-square and P-value,  as simple as possible,
#' by using different models built in the 'trendline()' function. The function includes the following models in the latest version:
#' "line2P" (formula as: y=a*x+b), "line3P" (y=a*x^2+b*x+c), "log2P" (y=a*ln(x)+b), "exp2P" (y=a*exp(b*x)),"exp3P" (y=a*exp(b*x)+c), "power2P" (y=a*x^b), and "power3P" (y=a*x^b+c).
#' Besides, the summarized result of each fitted model is also output by default.
#'
#' @param x,y  the x and y arguments provide the x and y coordinates for the plot. Any reasonable way of defining the coordinates is acceptable.
#' @param model select which model to fit. Default is "line2P". The "model" should be one of c("line2P", "line3P", "log2P", "exp2P", "exp3P", "power2P", "power3P"), their formulas are as follows:\cr "line2P": y=a*x+b \cr "line3P": y=a*x^2+b*x+c \cr "log2P": y=a*ln(x)+b \cr "exp2P": y=a*exp(b*x) \cr "exp3P": y=a*exp(b*x)+c \cr "power2P": y=a*x^b \cr "power3P": y=a*x^b+c
#' @param Pvalue.corrected if P-value corrected or not, the value is one of c("TRUE", "FALSE").
#' @param linecolor color of regression line.
#' @param lty line type. lty can be specified using either text c("blank","solid","dashed","dotted","dotdash","longdash","twodash") or number c(0, 1, 2, 3, 4, 5, 6). Note that lty = "solid" is identical to lty=1.
#' @param lwd line width. Default is 1.
#' @param show.equation whether to show the regression equation, the value is one of c("TRUE", "FALSE").
#' @param show.Rpvalue whether to show the R-square and P-value, the value is one of c("TRUE", "FALSE").
#' @param Rname to specify the character of R-square, the value is one of c(o, 1), corresponding to c(R^2, r^2).
#' @param Pname  to specify the character of P-value, the value is one of c(o, 1), corresponding to c(P, p).
#' @param CI.fill fill the confidance interval? (TRUE by default, see 'CI.level' to control)
#' @param CI.level level of confidence interval to use (0.95 by default)
#' @param CI.alpha alpha value of fill color of confidence interval.
#' @param CI.color line or fill color of confidence interval.
#' @param CI.lty line type of confidence interval.
#' @param CI.lwd line width of confidence interval.
#' @param summary summarizing the model fits. Default is TRUE.
#' @param text.col the color used for the legend text.
#' @param ePos.x,ePos.y equation position. Default are: ePos.x = min(x), ePos.y = max(y). If no need to show equation, set ePos.x = NA.
#' @param eDigit the numbers of digits for equation parameters. Default is 5.
#' @param eSize  font size in percentage of equation. Default is 1.
#' @param xlab,ylab labels of x- and y-axis.
#' @param las style of axis labels. (0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical)
#' @param ... additional parameters to \code{\link[graphics]{plot}}, such as type, main, sub, pch, col.
#' @import graphics
#' @import scales
#' @import stats
#' @import investr
#' @export
#' @details The linear models (line2P, line3P, log2P) in this package are estimated by \code{\link[stats]{lm}} function, \cr while the nonlinear models (exp2P, exp3P, power2P, power3P) are estimated by \code{\link[stats]{nls}} function (i.e., least-squares method).\cr\cr The argument 'Pvalue.corrected' is workful for non-linear regression only.\cr\cr If "Pvalue.corrected = TRUE", the P-value is calculated by using "Residual Sum of Squares" and "Corrected Total Sum of Squares (i.e. sum((y-mean(y))^2))".\cr If "Pvalue.corrected = TRUE", the P-value is calculated by using "Residual Sum of Squares" and "Uncorrected Total Sum of Squares (i.e. sum(y^2))".
#' @note
#' Confidence intervals for nonlinear regression (i.e., objects of class
#' \code{nls}) are based on the linear approximation described in Bates & Watts (2007) and Greenwell & Schubert-Kabban (2014).
#'
#' @references
#' Bates, D. M., and Watts, D. G. (2007)
#' \emph{Nonlinear Regression Analysis and its Applications}. Wiley.
#'
#' Greenwell B. M., and Schubert-Kabban, C. M. (2014)
#' \emph{investr: An R Package for Inverse Estimation}. The R Journal, 6(1), 90-100.
#' @return NULL
#' @examples
#' library(basicTrendline)
#' x1 <- 1:5
#' x2 <- -2:2
#' x3 <- c(101,105,140,200,660)
#' x4 <- -5:-1
#'
#' y1 <- c(2,14,18,19,20)       # increasing convex  trend
#' y2 <- c(-2,-14,-18,-19,-20)  # decreasing concave trend
#' y3 <- c(2,4,16,38,89)        # increasing concave trend
#' y4 <- c(-2,-4,-16,-38,-89)   # decreasing convex  trend
#'
#' # [case 1] default (plot, regression line, confidence interval)
#' trendline(x1, y1, model="line2P", summary=TRUE, eDigit=10)
#' # [case 2]  'eSize' is to change the font size of equation.
#' trendline(x2, y2, model="line3P", summary=FALSE, linecolor="red", eSize=1.4)
#' # [case 3]  lines of confidenc interval only (i.e. not fill)
#' trendline(x3, y3, model="log2P", CI.fill = FALSE, CI.color = "black", CI.lty = 2)
#' # [case 4]  trendliine only (i.e. without confidence interval)
#' trendline(x4, y4, model="exp3P", ePos.x= -2, ePos.y = -50, CI.color = NA)
#'
#' # [case 5]  show regression equation only
#' trendline(x1, y1, model="line2P", show.equation = TRUE, show.Rpvalue = FALSE)
#' # [case 6]  specify the name of parameters (R^2 or r^2; P or p) in regression equation
#' trendline(x1, y1, model="line2P", Rname=1, Pname = 1)
#' # [case 7]  don't show equation
#' trendline(x1, y1, model="line2P", ePos.x = NA)
#'
#' @author Weiping Mei, Guangchuang Yu
#' @seealso  \code{\link{trendline}}, \code{\link{SSexp3P}}, \code{\link{SSpower3P}}, \code{\link[stats]{nls}}, \code{\link[stats]{selfStart}}, \code{\link[investr]{plotFit}}

trendline <- function(x, y, model="line2P", Pvalue.corrected = TRUE,
                      linecolor = "blue", lty = 1, lwd = 1,
                      show.equation = TRUE, show.Rpvalue = TRUE,
                      Rname = 0, Pname = 0,
                      summary = TRUE,
                      ePos.x = min(x), ePos.y = max(y), text.col="black", eDigit = 5, eSize = 1,
                      CI.fill = TRUE, CI.level = 0.95, CI.color = "grey", CI.alpha = 1, CI.lty = 1, CI.lwd = 1,
                      las = 1, xlab=NULL, ylab=NULL, ...)
{
  model=model
  if(is.null(xlab))  xlab = deparse(substitute(x)) else xlab = xlab
  if(is.null(ylab))  ylab = deparse(substitute(y)) else ylab = ylab
  if(Rname==0)            Rname = "R"         else Rname = "r"
  if(Pname==0)            Pname = "P"         else Pname = "p"

  OK <- complete.cases(x, y)
  x <- x[OK]
  y <- y[OK]
  z<-data.frame(x,y)

  return <- trendline_summary(x=x, y=y, model=model, Pvalue.corrected=Pvalue.corrected, summary = FALSE, eDigit = eDigit)
  a = return$parameter$a
  b = return$parameter$b
  if (is.null(return$parameter$c)==FALSE){
    c = return$parameter$c
  }else{}
  if (return$p.value >= 0.0001){
    pval <- return$p.value
    pval <- paste("=" , unname(pval))
    }else{
    pval <- "< 0.0001"
  }
  r2   <- return$R.squared
  adjr2<- return$adj.R.squared


# 1) model="line2P"
if (model== c("line2P"))
  {  Pvalue.corrected=TRUE
  formula = 'y = a*x + b'
  fitting <- lm(y~x)

  if (summary==TRUE){
    trendline_summary(x,y,"line2P", Pvalue.corrected=Pvalue.corrected, eDigit=eDigit)
  }else{}

  aa = abs(a)
  bb = abs(b)
  aa = format(aa, digits = eDigit)
  bb = format(bb, digits = eDigit)

  param <- vector('expression',2)
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

  param[2] <- substitute(expression(italic(Rname)^2 == r2*","~~italic(Pname)~~pval))[2]

 }

# 2) model="line3P"
  if (model== c("line3P"))
  {    Pvalue.corrected=TRUE
  formula = 'y = a*x^2 + b*x + c'
    fitting <- lm(y~I(x^2)+x)

    if (summary==TRUE){
    trendline_summary(x,y,"line3P", Pvalue.corrected=Pvalue.corrected, eDigit=eDigit)
    }else{}


    aa = abs(a)
    bb = abs(b)
    cc = abs(c)
    aa = format(aa, digits = eDigit)
    bb = format(bb, digits = eDigit)
    cc = format(cc, digits = eDigit)

    param <- vector('expression',2)

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

    param[2] <- substitute(expression(italic(Rname)^2 == r2*","~~italic(Pname)~~pval*"            "))[2]

  }

# 3) model="log2P"
if (model== c("log2P"))
  {
  Pvalue.corrected=TRUE
  formula = 'y = a*ln(x) + b'
  if (summary==TRUE){
    trendline_summary(x,y,"log2P", Pvalue.corrected=Pvalue.corrected, eDigit=eDigit)
  }else{}

  if (min(x)>0)
  {
  fitting <- lm(y~log(x))

    aa = abs(a)
    bb = abs(b)

    param <- vector('expression',2)

    aa = format(aa, digits = eDigit)
    bb = format(bb, digits = eDigit)

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

    param[2] <- substitute(expression(italic(Rname)^2 == r2*","~~italic(Pname)~~pval))[2]

 }else{
    stop("
'log2P' model need ALL x values greater than 0. Try other models.")
 }
}

# 4.2) model="exp2P"
  if (model== "exp2P")
  {
    formula = 'y = a*exp(b*x)'
    fitting <- nls(y~SSexp2P(x,a,b),data=z)

    if (summary==TRUE){
      trendline_summary(x, y, "exp2P", Pvalue.corrected=Pvalue.corrected, eDigit=eDigit)
    }else{}

    aa= abs(a)
    bb= abs(b)
    param <- vector('expression',2)

    aa = format(aa, digits = eDigit)
    bb = format(bb, digits = eDigit)

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

    param[2] <- substitute(expression(italic(Rname)^2 == r2*","~~italic(Pname)~~pval))[2]

  }


# 4.3) model="exp3P"
  if (model== "exp3P")
  {
    formula = 'y = a*exp(b*x) + c'
    fitting <- nls(y~SSexp3P(x,a,b,c),data=z)

    if (summary==TRUE){
      trendline_summary(x, y, "exp3P", Pvalue.corrected=Pvalue.corrected, eDigit=eDigit)
        }else{}

    aa= abs(a)
    bb= abs(b)
    cc= abs(c)

    param <- vector('expression',2)

    aa = format(aa, digits = eDigit)
    bb = format(bb, digits = eDigit)
    cc = format(cc, digits = eDigit)

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
    param[2] <- substitute(expression(italic(Rname)^2 == r2*","~~italic(Pname)~~pval))[2]
  }


# 5.2) model="power2P"
if (model== "power2P")
  {formula = 'y = a*x^b'

    if (summary==TRUE){
      trendline_summary(x, y, "power2P", Pvalue.corrected=Pvalue.corrected, eDigit=eDigit)
    }else{}

    if (min(x)>0){
      fitting <- nls(y~SSpower2P(x,a,b),data=z)

      aa<-abs(a)

      param <- vector('expression',2)

      aa = format(aa, digits = eDigit)

      if (aa==1){aa=c("")}

      if (a>=0)
      {
        param[1] <- substitute(expression(italic("y") == aa~italic("x")^b))[2]
      }else{
        param[1] <- substitute(expression(italic("y") == -aa~italic("x")^b))[2]
      }
      param[2] <- substitute(expression(italic(Rname)^2 == r2*","~~italic(Pname)~~pval))[2]

    }else{
      stop("
           'power2P' model need ALL x values greater than 0. Try other models.")
    }
}


 # 5.3) model="power3P"
if (model== "power3P")
    {formula = 'y = a*x^b + c'

    if (summary==TRUE){
      trendline_summary(x,y,"power3P", Pvalue.corrected=Pvalue.corrected, eDigit=eDigit)
    }else{}

    if (min(x)>0){
      fitting <- nls(y~SSpower3P(x,a,b,c),data=z)

      aa<-abs(a)
      cc<-abs(c)

      param <- vector('expression',2)

      aa = format(aa, digits = eDigit)
      cc = format(cc, digits = eDigit)

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
      param[2] <- substitute(expression(italic(Rname)^2 == r2*","~~italic(Pname)~~pval))[2]

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

### plot and draw trendline
  if (requireNamespace(c("investr", "scales"), quietly = TRUE)){
  investr::plotFit(fitting, interval = "confidence", level = CI.level, data=z,
                   shade = CI.fill,
                   col.fit = linecolor, lty.fit = lty, lwd.fit = lwd,
                   col.conf = scales::alpha(CI.color, alpha = CI.alpha),lty.conf = CI.lty,  lwd.conf = CI.lwd,
                   las = las, xlab = xlab, ylab = ylab, ...)
  }

### show legend
  if (show.equation == TRUE) param[1] = param[1]  else param[1]=NULL
  if (show.Rpvalue  == TRUE) param[2] = param[2]  else param[2]=NULL

  if(!is.null(ePos.x & ePos.y)){
    ePos.x = ePos.x
    ePos.y = ePos.y
  legend(x = ePos.x, y = ePos.y, text.col = text.col, legend = param, cex = eSize, bty = 'n')
  }

}
