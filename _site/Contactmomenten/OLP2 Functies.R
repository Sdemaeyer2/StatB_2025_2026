## Patch rond is.R in base
is.R <- function(...) {
  invalid <- function(x) {
    is.null(x) || length(x) == 0 || all(is.na(x))
  }
  TRUE  # altijd TRUE
}

  

freqtabel <- function(X){
  Table <- data.frame( table(X) )
  Table$Percentage <- prop.table( Table$Freq )*100
  Table$CumulativeN <- cumsum(Table$Freq)
  Table$CumulativePerc <-  cumsum( Table$Percentage )
  Table
}

reikwijdte <- function(X){max(X, na.rm=TRUE)-min(X, na.rm=TRUE)} 

interkwartiel <- function(X){quantile(X,c(.75), na.rm=TRUE)-quantile(X, c(.25), na.rm=TRUE)} 

gemabsafw <- function(X){mean(abs(X-mean(X,na.rm=TRUE)),na.rm=TRUE)} 

zscore <- function(X){(X-mean(X,na.rm=TRUE))/sd(X, na.rm=TRUE)} 

standaardfout<-function(X){sd(X,na.rm=TRUE)/sqrt(length(X))} 

betr.interval = function(data, conf.level = 0.95) { 
	z = qnorm((1 - conf.level)/2, lower.tail = FALSE) 	
	xbar = mean(data,na.rm=TRUE) 
	sdx = sqrt(var(data,na.rm=TRUE)/length(data))
	c(xbar - z * sdx, xbar + z * sdx)
	}

betr.interval.Var1= function(data, conf.level = 0.95) { 
	halfalpha=(1-conf.level)/2
	df=length(data)-1
	v=var(data,na.rm=TRUE)
	boven = v*df/qchisq(halfalpha,df)
	onder = v*df/qchisq(1-halfalpha,df)	
	c(onder, boven)
	}

betr.interval.Var2= function(x,n, conf.level = 0.95) { 
 	halfalpha=(1-conf.level)/2
 	df=n-1
 	v=x
 	boven = v*df/qchisq(halfalpha,df)
 	onder = v*df/qchisq(1-halfalpha,df)	
 	c(onder, boven)
 	}

st.fout.Skew= function(X) { 
 	sqrt(((6*length(X)*(length(X)-1))/((length(X)-2)*((length(X))+1)*(length(X)+3))))
 	}

scheefheid=
function (x, na.rm = FALSE) 
{
    if (is.matrix(x)) 
        apply(x, 2, scheefheid, na.rm = na.rm)
    else if (is.vector(x)) {
	if (na.rm) x <- x[!is.na(x)] 
	n <- length(x)
     (sum((x-mean(x))^3)/n)/(sum((x-mean(x))^2)/n)^(3/2)
	}
    else if (is.data.frame(x)) 
        sapply(x, scheefheid, na.rm = na.rm)
    else scheefheid(as.vector(x), na.rm = na.rm)
}

betr.interval.Skew=function(X,conf.level=0.95) { 
 	onder=scheefheid(X,na.rm=TRUE)-(qnorm(conf.level+((1-conf.level)/2))*st.fout.Skew(X))
 	boven=scheefheid(X,na.rm=TRUE)+(qnorm(conf.level+((1-conf.level)/2))*st.fout.Skew(X))
 	c(onder,boven)
 	}

kurtosisb <-
function (x, na.rm = FALSE) 
{
    if (is.matrix(x)) 
        apply(x, 2, kurtosisb, na.rm = na.rm)
    else if (is.vector(x)) {
	if (na.rm) x <- x[!is.na(x)] 
	n <- length(x)
	n*sum( (x-mean(x))^4 )/(sum( (x-mean(x))^2 )^2)
	}
    else if (is.data.frame(x)) 
        sapply(x, kurtosisb, na.rm = na.rm)
    else kurtosisb(as.vector(x), na.rm = na.rm)
}


st.fout.Kurt= function(X) { 
 	2*st.fout.Skew(X)*sqrt(((length(X)^2)-1)/((length(X)-3)*(length(X)+5)))
 	}


betr.interval.Kurt=function(X,conf.level=0.95) { 
 	onder=kurtosisb(X,na.rm=TRUE)-(qnorm(conf.level+((1-conf.level)/2))*st.fout.Kurt(X))
 	boven=kurtosisb(X,na.rm=TRUE)+(qnorm(conf.level+((1-conf.level)/2))*st.fout.Kurt(X))
 	c(onder,boven)
 	}

varP=function(X) { 
 	verschil=X-mean(X,na.rm=TRUE)
 	verschilkwadraat=verschil^2
 	kwadratensom=sum(verschilkwadraat)
 	kwadratensom/length(X)
 	}

sdP=function(X) { 
 	sqrt(varP(X))
 	}


stfoutproportie=function(p,n) { 
 	sqrt((p*(1-p))/n)
 	}

betr.interval.Prop=function(p,n,conf.level=0.95) { 
 	onder=p-(qnorm(conf.level+((1-conf.level)/2))*stfoutproportie(p,n))
 	boven=p+(qnorm(conf.level+((1-conf.level)/2))*stfoutproportie(p,n))
 	c(onder,boven)
 	}

## hfdst kruistabellen

cv.test <- function(x) {
	CV<-sqrt(chisq.test(x)$statistic/(sum(x) * min(dim(x) - 1 )))
	Phi<-sqrt(chisq.test(x)$statistic/(sum(x)))
	print.noquote("Cramer's V & Phi (bij 2x2 tabel):")
     print(c(as.numeric(CV),as.numeric(Phi)))
    print.noquote("Kolommen: ") 
    print(ncol(x))
    print.noquote("Rijen: ") 
    print(nrow(x))
	}


chi.noncentral.conf<-function (chival, df, conf, prec = 1e-05) 
{
	result <- NA
	ulim <- 1 - (1 - conf)/2
	lc <- c(0.001, chival/2, chival)
	while (pchisq(chival, df, lc[1]) < ulim) {
		if (pchisq(chival, df) < ulim) {
			result <- (c(0, pchisq(chival, df)))
			break
		}
		lc <- c(lc[1]/4, lc[1], lc[3])
	}
	diff <- 1
	if (all(is.na(result))) {
		while (diff > prec) {
			if (pchisq(chival, df, lc[2]) < ulim) 
				lc <- c(lc[1], (lc[1] + lc[2])/2, lc[2])
			else lc <- c(lc[2], (lc[2] + lc[3])/2, lc[3])
			diff <- abs(pchisq(chival, df, lc[2]) - ulim)
			ucdf <- pchisq(chival, df, lc[2])
		}
		result <- c(lc[2], ucdf)
	}
	uc <- c(chival, 2 * chival, 3 * chival)
	llim <- (1 - conf)/2

	while (pchisq(chival, df, uc[1]) < llim && uc>prec) {
		uc <- c(uc[1]/4, uc[1], uc[3])
	}
	while (pchisq(chival, df, uc[3]) > llim) {
		uc <- c(uc[1], uc[3], uc[3] + chival)
	}
	diff <- 1
	count<-0
	while (diff > prec) {
		if (pchisq(chival, df, uc[2]) < llim) 
			uc <- c(uc[1], (uc[1] + uc[2])/2, uc[2])
		else uc <- c(uc[2], (uc[2] + uc[3])/2, uc[3])
		diff <- abs(pchisq(chival, df, uc[2]) - llim)
		lcdf <- pchisq(chival, df, uc[2])
		count<-count+1
		if(count>1000){
			warning("Convergence not reached in chi.noncentral.conf .")
			uc[2]<-Inf
			lcdf<-0
			break
		}
	}
	result <- rbind(result, c(uc[2], lcdf))
	rownames(result) <- c("Lower", "Upper")
	colnames(result) <- c("Non-Central", "%")
	result
}


chi.kwadraat.test <- function (x, y = NULL, conservative = FALSE, cramers.v.conf=.95, simulate.p.value = FALSE, B = 10000) 
{

	cramers.v<-function(chival,df,n,k,conf){
		non.cent<-chi.noncentral.conf(chival,df,conf)
		V<-sqrt(chival/(n*(k-1)))
		low<-sqrt(non.cent[1,1]/(n*(k-1)))
		hi<-sqrt(non.cent[2,1]/(n*(k-1)))
		hi<-min(hi,1)
		result<-c(V,low,hi)
		names(result)<-c("Cramer's V",(1-conf)/2,1-(1-conf)/2)
		result
	}
	ESTIMATE<-NA
	CONF.INT<-c(NA,NA)
    DNAME <- deparse(substitute(x))
    if (is.data.frame(x)) 
        x <- as.matrix(x)
    if (is.matrix(x)) {
        if (min(dim(x)) == 1) 
            x <- as.vector(x)
    }
    if (!is.matrix(x) && !is.null(y)) {
        if (length(x) != length(y)) 
            stop("'x' and 'y' must have the same length")
        DNAME <- c(DNAME, deparse(substitute(y)))
        OK <- complete.cases(x, y)
        x <- factor(x[OK])
        y <- factor(y[OK])
        if ((nlevels(x) < 2) || (nlevels(y) < 2)) 
            stop("'x' and 'y' must have at least 2 levels")
        x <- table(x, y)
        names(dimnames(x)) <- DNAME
        DNAME <- paste(DNAME, collapse = " and ")
    }
    if (any(x < 0) || any(is.na(x))) 
        stop("all entries of 'x' must be nonnegative and finite")
    if ((n <- sum(x)) == 0) 
        stop("at least one entry of 'x' must be positive")
    if (simulate.p.value) {
        setMETH <- function() METHOD <<- paste(METHOD, "with simulated",if(conservative) "(mid)" else "(conservative)", "p-value\n\t (based on", 
            B, "replicates)")
        almost.1 <- 1 - 64 * .Machine$double.eps
		just.over.1 <- 1 + 64 * .Machine$double.eps
    }
    if (is.matrix(x)) {
        METHOD <- "Pearson's Chi-squared test"
        nr <- nrow(x)
        nc <- ncol(x)
        sr <- rowSums(x)
        sc <- colSums(x)
        E <- outer(sr, sc, "*")/n
        dimnames(E) <- dimnames(x)
        if (simulate.p.value && all(sr > 0) && all(sc > 0)) {
            setMETH()
            tmp <- .C("chisqsim", as.integer(nr), as.integer(nc), 
                as.integer(sr), as.integer(sc), as.integer(n), 
                as.integer(B), as.double(E), integer(nr * nc), 
                double(n + 1), integer(nc), results = double(B),PACKAGE="stats")
            STATISTIC <- sum(sort((x - E)^2/E, decreasing = TRUE))
            PARAMETER <- (nr - 1) * (nc - 1)
            PVAL <- (1 + sum(tmp$results >= almost.1 * STATISTIC))/(B + 
                1)
			if(!conservative)
				PVAL<-PVAL - .5* (sum( (tmp$results <=just.over.1*STATISTIC) & (tmp$results>=almost.1*STATISTIC))/(B+1))
			
        }
        else {
            if (simulate.p.value) 
                warning("cannot compute simulated p-value with zero marginals")
            if (conservative && nrow(x) == 2 && ncol(x) == 2) {
                YATES <- 0.5
                METHOD <- paste(METHOD, "with Yates' continuity correction")
            }else if(conservative && !(nrow(x) == 2 && ncol(x) == 2)){
				warning("Conservative Yates correction only applies to 2X2 tables.")
				return(structure(list(estimate=NA,conf.int=NA,statistic = NA, parameter = NA, 
									  p.value = NA, method = paste(METHOD, "with Yates' continuity correction"), 
									  data.name = DNAME, observed = x, 
									  expected = E, residuals = (x - E)/sqrt(E)), class = "htest"))
            }else YATES <- 0
            STATISTIC <- sum((abs(x - E) - YATES)^2/E)
            PARAMETER <- (nr - 1) * (nc - 1)
            PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
			if(!conservative){
				cramer<-cramers.v(STATISTIC,PARAMETER,n,min(nr,nc),cramers.v.conf)
				ESTIMATE<-cramer[1]
				CONF.INT<-cramer[2:3]
				attr(CONF.INT, "conf.level")<-cramers.v.conf
			}
        }
    }
    else {
        stop("Could not create table from x and y")
    }
    names(STATISTIC) <- "X-squared"
    names(PARAMETER) <- "df"
    #if (any(E < 5) && is.finite(PARAMETER)) 
    #    warning("Chi-squared approximation may be incorrect")
    structure(list(estimate=ESTIMATE,conf.int=CONF.INT,statistic = STATISTIC, parameter = PARAMETER, 
        p.value = PVAL, method = METHOD, data.name = DNAME, observed = x, 
        expected = E, residuals = (x - E)/sqrt(E)), class = "htest")
}

kruistabel.kolom<- function(y,x) {
	addmargins(prop.table(addmargins(table(y,x),2),2)*100,1)
	}

kruistabel.rij<- function(y,x) {
	addmargins(prop.table(addmargins(table(y,x),1),1)*100,2)
	}

## hfdst ttest

d=function(Y,X){
	data<-na.omit(data.frame(Y,X))
	laagste<-data$X[rank(data$X)==rank(data$X)[order(rank(data$X))][2]][1] 
	hoogste<-data$X[rank(data$X)==rank(data$X)[order(rank(data$X),decreasing = TRUE)][2]][1] 
	teller=mean(data$Y [which (data$X==laagste)], na.rm=TRUE)- mean(data$Y [which (data$X==hoogste)], na.rm=TRUE)
	noemer=(sd(data$Y [which (data$X==laagste)], na.rm=TRUE) + sd(data$Y [which (data$X==hoogste)], na.rm=TRUE))/2
	teller/noemer
	}

dpaired=function(Y,X){
	teller=Y
	noemer=sqrt(X)
	teller/noemer
	}

## $Id: plotmeans.R 1365 2009-11-12 15:38:53Z warnes $

invalid <- function(x)
  {
    if( missing(x) || is.null(x) || length(x)==0 )
      return(TRUE)
    if(is.list(x))
      return(all(sapply(x,invalid)))
    else if(is.vector(x))
      return(all(is.na(x)))
    else
      return(FALSE)
  }

errorbar  <- function (formula, data = NULL, subset, na.action,
                        bars=TRUE, p=0.95,
                        minsd=0, minbar=NULL, maxbar=NULL,
                        xlab=names(mf)[2], ylab=names(mf)[1],
                        mean.labels=FALSE, ci.label=FALSE, n.label=TRUE,
                        digits=getOption("digits"), col="black",
                        barwidth=1,
                        barcol="blue",
                        connect=FALSE,
                        ccol=col,
                        legends=names(means),
                        xaxt,
                        use.t = TRUE,
                        ...)
{
  is.R <- get("is.R")
  if(is.null(is.R)) is.R <- function(x) FALSE

  if(!is.R())
    {
      if(col=="black")
        col <- 1
      if(barcol=="blue")
        barcol <- 2
    }

  if (invalid(formula) || (length(formula) != 3))
    stop("formula missing or incorrect")
  if (invalid(na.action))
    na.action <- options("na.action")
  m <- match.call(expand.dots = FALSE)
  if(is.R())
    {
      if (is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    }
  else
    {
      if (is.matrix(eval(m$data, FALSE)))
        m$data <- as.data.frame(data)
    }
  m$... <- m$bars <- m$barcol <- m$p <- NULL
  m$minsd <- m$minbar <- m$maxbar <- NULL
  m$xlab <- m$ylab <-  NULL
  m$col  <- m$barwidth  <- NULL
  m$digits  <- m$mean.labels  <- m$ci.label  <- m$n.label <- NULL
  m$connect  <- m$ccol  <-  m$legends <- m$labels<- NULL
  m$xaxt <- m$use.t <- NULL
  m[[1]] <- as.name("model.frame")
  mf <- eval(m, parent.frame())
  response <- attr(attr(mf, "terms"), "response")

  ## drop unused levels in factors!!!
  
  wFact <- which(attr(attr(mf, "terms"),"dataClasses") == "factor")
  for(i in wFact)
    mf[,i] <- factor(mf[,i])
  
  means  <-  sapply(split(mf[[response]], mf[[-response]]), mean, na.rm=TRUE)
  ns     <-  sapply(sapply(split(mf[[response]], mf[[-response]]), na.omit,
                           simplify=FALSE), length )
  xlim  <-  c(0.5, length(means)+0.5)


  
  if(!bars)
    {
      plot( means, ..., col=col, xlim=xlim)
    }
  else
    {

      myvar  <-  function(x) var(x[!is.na(x)])
      vars <- sapply(split(mf[[response]], mf[[-response]]), myvar)

      ## apply minimum variance specified by minsd^2
      vars <- ifelse( vars < (minsd^2), (minsd^2), vars)

      if(use.t)
        ci.width  <- qt( (1+p)/2, ns-1 ) * sqrt(vars/ns)
      else
        ci.width  <- qnorm( (1+p)/2 ) * sqrt(vars/ns)

      if(length(mean.labels)==1)
        {
          if (mean.labels==TRUE)
            mean.labels  <-  format( round(means, digits=digits ))
          else if (mean.labels==FALSE)
            mean.labels  <- NULL
        }

      plotCI(x=1:length(means), y=means, uiw=ci.width, xaxt="n",
             xlab=xlab, ylab=ylab, labels=mean.labels, col=col, xlim=xlim,
             lwd=barwidth, barcol=barcol, minbar=minbar, maxbar=maxbar, ... )
      if(invalid(xaxt) || xaxt!="n")
        axis(1, at = 1:length(means), labels = legends)

      if(ci.label)
        {
          ci.lower <- means-ci.width
          ci.upper <- means+ci.width

          if(!invalid(minbar))
            ci.lower <- ifelse(ci.lower < minbar, minbar, ci.lower)
          if(!invalid(maxbar))
            ci.upper <- ifelse(ci.upper > maxbar, maxbar, ci.upper)

          labels.lower <- paste( " \n", format(round(ci.lower, digits=digits)),
                                sep="")
          labels.upper <- paste( format(round(ci.upper, digits=digits)), "\n ",
                                sep="")

          text(x=1:length(means),y=ci.lower, labels=labels.lower, col=col)
          text(x=1:length(means),y=ci.upper, labels=labels.upper, col=col)
        }

    }


  if(n.label)
    if(is.R())
      text(x=1:length(means),y=par("usr")[3],
           labels=paste("n=",ns,"\n",sep=""))
    else
      {
        axisadj <- (par("usr")[4] - (par("usr")[3]) )/75
        text(x=1:length(means),y=par("usr")[3] + axisadj,
             labels=paste("n=",ns,"\n",sep=""))
      }

  if(!invalid(connect) & !identical(connect,FALSE))
    {
      if(is.list(connect))
        {
          if(length(ccol)==1)
            ccol  <-  rep(ccol, length(connect) )

          for(which in 1:length(connect))
            lines(x=connect[[which]],y=means[connect[[which]]],col=ccol[which])
        }
      else
        lines(means, ..., col=ccol)
    }



}

# $Id: plotCI.R 1318 2009-05-08 21:56:38Z warnes $


plotCI <- function (x,
                    y = NULL,
                    uiw,
                    liw = uiw,
                    ui,
                    li,

                    err='y',
                    ylim=NULL,
                    xlim=NULL,
                    type="p",

                    col=par("col"),
                    barcol=col,
                    pt.bg = par("bg"),

                    sfrac = 0.01,
                    gap=1,

                    lwd=par("lwd"),
                    lty=par("lty"),

                    labels=FALSE,

                    add=FALSE,
                    xlab,
                    ylab,

                    minbar,
                    maxbar,
                    ...
                    )
{

  if (is.list(x)) {
    y <- x$y
    x <- x$x
  }

  if(invalid(xlab))
    xlab <- deparse(substitute(x))

  if(invalid(ylab))
    {
      if(is.null(y))
        {
          xlab  <- ""
          ylab <- deparse(substitute(x))
        }
      else
        ylab <- deparse(substitute(y))
    }

  if (is.null(y)) {
    if (is.null(x))
      stop("both x and y NULL")
    y <- as.numeric(x)
    x <- seq(along = x)
  }


  if(err=="y")
    z  <- y
  else
    z  <- x

  if(invalid(uiw))
    uiw <- NA
  if(invalid(liw))
    liw <- NA
  
  
  if(invalid(ui))
    ui <- z + uiw
  if(invalid(li))
    li <- z - liw

  if(!invalid(minbar))
    li <- ifelse( li < minbar, minbar, li)

  if(!invalid(maxbar))
    ui <- ifelse( ui > maxbar, maxbar, ui)

   if(err=="y")
     {
       if(is.null(ylim))
         ylim <- range(c(y, ui, li), na.rm=TRUE)
       if(is.null(xlim) && !is.R() )
         xlim <- range( x, na.rm=TRUE)
     }
   else if(err=="x")
     {
       if(is.null(xlim))
         xlim <- range(c(x, ui, li), na.rm=TRUE)
       if(is.null(ylim) && !is.R() )
         ylim <- range( x, na.rm=TRUE)
     }

  if(!add)
    {
      if(invalid(labels) || labels==FALSE )
        plot(x, y, ylim = ylim, xlim=xlim, col=col,
             xlab=xlab, ylab=ylab, ...)
      else
        {
          plot(x, y, ylim = ylim, xlim=xlim, col=col, type="n",
               xlab=xlab, ylab=ylab,  ...)
          text(x, y, label=labels, col=col, ... )
        }
    }
  if(is.R())
    myarrows <- function(...) arrows(...)
  else
    myarrows <- function(x1,y1,x2,y2,angle,code,length,...)
      {
        segments(x1,y1,x2,y2,open=TRUE,...)
        if(code==1)
          segments(x1-length/2,y1,x1+length/2,y1,...)
        else
          segments(x2-length/2,y2,x2+length/2,y2,...)
      }

  if(err=="y")
    {
      if(gap!=FALSE)
        gap <- strheight("O") * gap
      smidge <- par("fin")[1] * sfrac


      # draw upper bar
      if(!is.null(li))
          myarrows(x , li, x, pmax(y-gap,li), col=barcol, lwd=lwd,
                 lty=lty, angle=90, length=smidge, code=1)
      # draw lower bar
      if(!is.null(ui))
          myarrows(x , ui, x, pmin(y+gap,ui), col=barcol,
                 lwd=lwd, lty=lty, angle=90, length=smidge, code=1)
    }
  else
    {
      if(gap!=FALSE)
        gap <- strwidth("O") * gap
      smidge <- par("fin")[2] * sfrac

      # draw left bar
      if(!is.null(li))
        myarrows(li, y, pmax(x-gap,li), y, col=barcol, lwd=lwd,
                 lty=lty, angle=90, length=smidge, code=1)
      if(!is.null(ui))
        myarrows(ui, y, pmin(x+gap,ui), y, col=barcol, lwd=lwd,
                 lty=lty, angle=90, length=smidge, code=1)

    }

  ## _now_ draw the points (to avoid having lines drawn 'through' points)
  points(x, y, col = col, lwd = lwd, bg = pt.bg, type = type, ...)

  invisible(list(x = x, y = y))
}


errorbar2=function(Y,X,xlab="Meting",ylab="Score"){
	x<-c(rep(1,length(Y)), rep(2,length(X)))
	y<-c(Y,X)
	errorbar(y~x,xlab=xlab,ylab=ylab)
}

# hoofdstuk ANOVA
TSS=function(Y){
	grandmean<-mean(Y,na.rm=TRUE)
	afwijking<-(Y-grandmean)^2
	TSS<-sum(afwijking)
	TSS/1
	}

SSB=function(Y, X, A, B, C){
	meanlaagste=mean(Y [which (X==1)], na.rm=TRUE)
	meanmiddelste=mean(Y [which (X==2)], na.rm=TRUE)
	meanhoogste=mean(Y [which (X==3)], na.rm=TRUE)
	grandmean<-mean(Y,na.rm=TRUE)	
	groepA=((meanlaagste-grandmean)^2)*A
	groepB=((meanmiddelste-grandmean)^2)*B
	groepC=((meanhoogste-grandmean)^2)*C
	SSB<-sum(groepA, groepB, groepC)
	SSB/1
	}

eta.square=function(Y, X){
	data<-na.omit(data.frame(Y,X))
	grandmean<-mean(data$Y,na.rm=TRUE)
	afwijking<-(data$Y-grandmean)^2
	TSS<-sum(afwijking)
	ESS<-sum(residuals(aov(data$Y~data$X))^2)
	SSB<-TSS-ESS
	SSB/TSS
}

vcov.default <- function(object, ...){
	stop(paste("there is no vcov() method for models of class",
					paste(class(object), collapse=", ")))
}

has.intercept.matrix <- function (model, ...) {
	"(Intercept)" %in% colnames(model)
}


makeHypothesis <- function(cnames, hypothesis, rhs = NULL){
	parseTerms <- function(terms){
		component <- gsub("^[-\\ 0-9\\.]+", "", terms)
		component <- gsub(" ", "", component, fixed=TRUE)
		component
	}
	stripchars <- function(x) {
		x <- gsub(" ", "", x, fixed = TRUE)
		x <- gsub("*", "", x, fixed = TRUE)
		x <- gsub("-", "+-", x, fixed = TRUE)
		x <- strsplit(x, "+", fixed = TRUE)[[1]]
		x <- x[x!=""]
		x
	}
	char2num <- function(x) {
		x[x == ""] <- "1"
		x[x == "-"] <- "-1"
		as.numeric(x)
	}
	constants <- function(x, y) { 
		with.coef <- unique(unlist(sapply(y,
								function(z) which(z == parseTerms(x)))))
		if (length(with.coef) > 0) x <- x[-with.coef]
		x <- if (is.null(x)) 0 else sum(as.numeric(x))
		if (any(is.na(x)))
			stop('The hypothesis "', hypothesis,
					'" is not well formed: contains bad coefficient/variable names.')
		x
	}
	coefvector <- function(x, y) {
		rv <- gsub(" ", "", x, fixed=TRUE) ==
				parseTerms(y)
		if (!any(rv)) return(0)
		if (sum(rv) > 1) stop('The hypothesis "', hypothesis,
					'" is not well formed.')
		rv <- sum(char2num(unlist(strsplit(y[rv], x, fixed=TRUE))))
		if (is.na(rv))
			stop('The hypothesis "', hypothesis,
					'" is not well formed: contains non-numeric coefficients.')
		rv
	}
	
	rhs <- rep(rhs, length.out = length(hypothesis))
	if (length(hypothesis) > 1)
		return(rbind(Recall(cnames, hypothesis[1], rhs[1]),
						Recall(cnames, hypothesis[-1], rhs[-1])))
	
	cnames_symb <- sapply(c("@", "#", "~"), function(x) length(grep(x, cnames)) < 1)
	
	if(any(cnames_symb)) {
		cnames_symb <- head(c("@", "#", "~")[cnames_symb], 1)
		cnames_symb <- paste(cnames_symb, seq_along(cnames), cnames_symb, sep = "")
		hypothesis_symb <- hypothesis
		for(i in order(nchar(cnames), decreasing = TRUE))
			hypothesis_symb <- gsub(cnames[i], cnames_symb[i], hypothesis_symb, fixed = TRUE)
	} else {
		stop('The hypothesis "', hypothesis,
				'" is not well formed: contains non-standard coefficient names.')
	}
	
	lhs <- strsplit(hypothesis_symb, "=", fixed=TRUE)[[1]] 
	if (is.null(rhs)) {
		if (length(lhs) < 2) rhs <- "0"
		else if (length(lhs) == 2) {
			rhs <- lhs[2]
			lhs <- lhs[1]
		}
		else stop('The hypothesis "', hypothesis,
					'" is not well formed: contains more than one = sign.')
	}
	else {
		if (length(lhs) < 2) as.character(rhs)
		else stop('The hypothesis "', hypothesis,
					'" is not well formed: contains a = sign although rhs was specified.')
	}
	lhs <- stripchars(lhs)
	rhs <- stripchars(rhs)
	rval <- sapply(cnames_symb, coefvector, y = lhs) - sapply(cnames_symb, coefvector, y = rhs) 
	rval <- c(rval, constants(rhs, cnames_symb) - constants(lhs, cnames_symb)) 
	names(rval) <- c(cnames, "*rhs*")
	rval
}

printHypothesis <- function(L, rhs, cnames){
	hyp <- rep("", nrow(L))
	for (i in 1:nrow(L)){
		sel <- L[i,] != 0
		h <- L[i, sel]
		h <- ifelse(h < 0, as.character(h), paste("+", h, sep=""))
		nms <- cnames[sel]
		h <- paste(h, nms)
		h <- gsub("-1", "-", h)
		h <- gsub("+1", "+", h, fixed=TRUE)
		h <- gsub("-", " - ", h)
		h <- gsub("+", "  + ", h, fixed=TRUE)
		h <- paste(h, collapse="")
		h <- gsub("  ", " ", h, fixed=TRUE)
		h <- sub("^\\ \\+", "", h)
		h <- sub("^\\ ", "", h)
		h <- sub("^-\\ ", "-", h)
		hyp[i] <- paste(h, "=", rhs[i])
	}
	hyp
}

linearHypothesis <- function (model, ...)
	UseMethod("linearHypothesis")

lht <- function (model, ...)
	UseMethod("linearHypothesis")

linearHypothesis.default <- function(model, hypothesis.matrix, rhs=NULL,
		test=c("Chisq", "F"), vcov.=NULL, singular.ok=FALSE, verbose=FALSE, ...){
	df <- df.residual(model)
	if (is.null(df)) df <- Inf ## if no residual df available
	V <- if (is.null(vcov.)) vcov(model)
			else if (is.function(vcov.)) vcov.(model) else vcov.
	b <- coef(model)
	if (any(aliased <- is.na(b)) && !singular.ok)
		stop("there are aliased coefficients in the model")
	b <- b[!aliased]
	if (is.null(b)) stop(paste("there is no coef() method for models of class",
						paste(class(model), collapse=", ")))
	if (is.character(hypothesis.matrix)) {
		L <- makeHypothesis(names(b), hypothesis.matrix, rhs)
		if (is.null(dim(L))) L <- t(L)
		rhs <- L[, NCOL(L)]
		L <- L[, -NCOL(L), drop = FALSE]
		rownames(L) <- hypothesis.matrix
	}
	else {
		L <- if (is.null(dim(hypothesis.matrix))) t(hypothesis.matrix)
				else hypothesis.matrix
		if (is.null(rhs)) rhs <- rep(0, nrow(L))
	}
	q <- NROW(L)
	if (verbose){
		cat("\nHypothesis matrix:\n")
		print(L)
		cat("\nRight-hand-side vector:\n")
		print(rhs)
		cat("\nEstimated linear function (hypothesis.matrix %*% coef - rhs)\n")
		print(drop(L %*% b - rhs))
		cat("\n")
	}
	SSH <- as.vector(t(L %*% b - rhs) %*% solve(L %*% V %*% t(L)) %*% (L %*% b - rhs))
	test <- match.arg(test)
	if (!(is.finite(df) && df > 0)) test <- "Chisq"
	name <- try(formula(model), silent = TRUE)
	if (inherits(name, "try-error")) name <- substitute(model)
	title <- "Linear hypothesis test\n\nHypothesis:"
	topnote <- paste("Model 1: restricted model","\n", "Model 2: ", 
			paste(deparse(name), collapse = "\n"), sep = "")
	note <- if (is.null(vcov.)) ""
			else "\nNote: Coefficient covariance matrix supplied.\n"
	rval <- matrix(rep(NA, 8), ncol = 4)
	colnames(rval) <- c("Res.Df", "Df", test, paste("Pr(>", test, ")", sep = ""))
	rownames(rval) <- 1:2
	rval[,1] <- c(df+q, df)
	if (test == "F") {
		f <- SSH/q
		p <- pf(f, q, df, lower.tail = FALSE)
		rval[2, 2:4] <- c(q, f, p)
	}
	else {
		p <- pchisq(SSH, q, lower.tail = FALSE)
		rval[2, 2:4] <- c(q, SSH, p)
	}
	if (!(is.finite(df) && df > 0)) rval <- rval[,-1]
	structure(as.data.frame(rval),
			heading = c(title, printHypothesis(L, rhs, names(b)), "", topnote, note),
			class = c("anova", "data.frame"))
}

linearHypothesis.glm <- function(model, ...)
	linearHypothesis.default(model, ...)

linearHypothesis.lm <- function(model, hypothesis.matrix, rhs=NULL,
		test=c("F", "Chisq"), vcov.=NULL,
		white.adjust=c(FALSE, TRUE, "hc3", "hc0", "hc1", "hc2", "hc4"),
		singular.ok=FALSE, ...){
	if (!singular.ok && is.aliased(model))
		stop("there are aliased coefficients in the model.")
	test <- match.arg(test)
	white.adjust <- as.character(white.adjust)
	white.adjust <- match.arg(white.adjust)
	if (white.adjust != "FALSE"){
		if (white.adjust == "TRUE") white.adjust <- "hc3"
		vcov. <- hccm(model, type=white.adjust)
	}
	rval <- linearHypothesis.default(model, hypothesis.matrix, rhs = rhs,
			test = test, vcov. = vcov., singular.ok=singular.ok, ...)
	if (is.null(vcov.)) {
		rval2 <- matrix(rep(NA, 4), ncol = 2)
		colnames(rval2) <- c("RSS", "Sum of Sq")
		SSH <- rval[2,test]
		if (test == "F") SSH <- SSH * abs(rval[2, "Df"])
		df <- rval[2, "Res.Df"]
		error.SS <- deviance(model)
		rval2[,1] <- c(error.SS + SSH * error.SS/df, error.SS)
		rval2[2,2] <- abs(diff(rval2[,1]))
		rval2 <- cbind(rval, rval2)[,c(1, 5, 2, 6, 3, 4)]
		class(rval2) <- c("anova", "data.frame")
		attr(rval2, "heading") <- attr(rval, "heading")
		rval <- rval2
	}
	rval
}


check.imatrix <- function(X, terms){ 
# check block orthogonality of within-subjects model matrix
	XX <- crossprod(X)
	if (missing(terms)) terms <- attr(X, "assign")
	for (term in unique(terms)){
		subs <- term == terms
		XX[subs, subs] <- 0
	}
	if (any(abs(XX) > sqrt(.Machine$double.eps)))
		stop("Terms in the intra-subject model matrix are not orthogonal.")
}

linearHypothesis.mlm <- function(model, hypothesis.matrix, rhs=NULL, SSPE, V,
		test, idata, icontrasts=c("contr.sum", "contr.poly"), idesign, iterms,
		check.imatrix=TRUE, P=NULL, title="", verbose=FALSE, ...){
	if (missing(test)) test <- c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")
	test <- match.arg(test, c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"),
			several.ok=TRUE)
	df.residual <- df.residual(model)
	wts <- if (!is.null(model$weights)) model$weights else rep(1,nrow(model.matrix(model)))
	# V = (X'WX)^{-1}
	if (missing (V)) V <- solve(wcrossprod(model.matrix(model), w=wts))
	B <- coef(model)
	if (is.character(hypothesis.matrix)) {
		L <- makeHypothesis(rownames(B), hypothesis.matrix, rhs)
		if (is.null(dim(L))) L <- t(L)
		L <- L[, -NCOL(L), drop = FALSE]
		rownames(L) <- hypothesis.matrix
	}
	else {
		L <- if (is.null(dim(hypothesis.matrix))) t(hypothesis.matrix)
				else hypothesis.matrix
	}
	# SSPE = E'WE
	if (missing(SSPE)) SSPE <- wcrossprod(residuals(model),w=wts)
	if (missing(idata)) idata <- NULL
	if (missing(idesign)) idesign <- NULL
	if (!is.null(idata)){
		for (i in 1:length(idata)){
			if (is.null(attr(idata[,i], "contrasts"))){
				contrasts(idata[,i]) <- if (is.ordered(idata[,i])) icontrasts[2]
						else icontrasts[1]
			}
		}
		if (is.null(idesign)) stop("idesign (intra-subject design) missing.")
		X.design <- model.matrix(idesign, data=idata)
		if (check.imatrix) check.imatrix(X.design)
		intercept <- has.intercept(X.design)
		term.names <- term.names(idesign)
		if (intercept) term.names <- c("(Intercept)", term.names)
		which.terms <- match(iterms, term.names)
		if (any(nas <- is.na(which.terms))){
			if (sum(nas) == 1)
				stop('The term "', iterms[nas],'" is not in the intrasubject design.')
			else stop("The following terms are not in the intrasubject design: ",
						paste(iterms[nas], collapse=", "), ".")
		}
		select <- apply(outer(which.terms, attr(X.design, "assign") + intercept, "=="),
				2, any)
		P <- X.design[, select, drop=FALSE]
	}
	if (!is.null(P)){
		rownames(P) <- colnames(B)
		SSPE <- t(P) %*% SSPE %*% P
		B <- B %*% P
	}
	rank <- sum(eigen(SSPE, only.values=TRUE)$values >= sqrt(.Machine$double.eps))
	if (rank < ncol(SSPE))
		stop("The error SSP matrix is apparently of deficient rank = ",
				rank, " < ", ncol(SSPE))
	r <- ncol(B)
	if (is.null(rhs)) rhs <- matrix(0, nrow(L), r)
	rownames(rhs) <- rownames(L)
	colnames(rhs) <- colnames(B)
	q <- NROW(L)
	if (verbose){
		cat("\nHypothesis matrix:\n")
		print(L)
		cat("\nRight-hand-side matrix:\n")
		print(rhs)
		cat("\nEstimated linear function (hypothesis.matrix %*% coef - rhs):\n")
		print(drop(L %*% B - rhs))
		cat("\n")
	}
	SSPH <- t(L %*% B - rhs) %*% solve(L %*% V %*% t(L)) %*% (L %*% B - rhs)
	rval <- list(SSPH=SSPH, SSPE=SSPE, df=q, r=r, df.residual=df.residual, P=P,
			title=title, test=test)
	class(rval) <- "linearHypothesis.mlm"
	rval
}


#linearHypothesis.mlm <- function(model, hypothesis.matrix, rhs=NULL, SSPE, V,
#   test, idata, icontrasts=c("contr.sum", "contr.poly"), idesign, iterms,
#   check.imatrix=TRUE, P=NULL, title="", verbose=FALSE, ...){
#   if (missing(test)) test <- c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")
#   test <- match.arg(test, c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"),
#       several.ok=TRUE)
#   df.residual <- df.residual(model)
#   if (missing (V)) V <- solve(crossprod(model.matrix(model)))
#   B <- coef(model)
#   if (is.character(hypothesis.matrix)) {
#       L <- makeHypothesis(rownames(B), hypothesis.matrix, rhs)
#       if (is.null(dim(L))) L <- t(L)
#       L <- L[, -NCOL(L), drop = FALSE]
#       rownames(L) <- hypothesis.matrix
#   }
#   else {
#       L <- if (is.null(dim(hypothesis.matrix))) t(hypothesis.matrix)
#           else hypothesis.matrix
#   }
#   if (missing(SSPE)) SSPE <- crossprod(residuals(model))
#   if (missing(idata)) idata <- NULL
#   if (missing(idesign)) idesign <- NULL
#   if (!is.null(idata)){
#       for (i in 1:length(idata)){
#           if (is.null(attr(idata[,i], "contrasts"))){
#               contrasts(idata[,i]) <- if (is.ordered(idata[,i])) icontrasts[2]
#                   else icontrasts[1]
#           }
#       }
#       if (is.null(idesign)) stop("idesign (intra-subject design) missing.")
#       X.design <- model.matrix(idesign, data=idata)
#       if (check.imatrix) check.imatrix(X.design)
#       intercept <- has.intercept(X.design)
#       term.names <- term.names(idesign)
#       if (intercept) term.names <- c("(Intercept)", term.names)
#       which.terms <- match(iterms, term.names)
#       if (any(nas <- is.na(which.terms))){
#           if (sum(nas) == 1)
#               stop('The term "', iterms[nas],'" is not in the intrasubject design.')
#           else stop("The following terms are not in the intrasubject design: ",
#                   paste(iterms[nas], collapse=", "), ".")
#       }
#       select <- apply(outer(which.terms, attr(X.design, "assign") + intercept, "=="),
#           2, any)
#       P <- X.design[, select, drop=FALSE]
#   }
#   if (!is.null(P)){
#       rownames(P) <- colnames(B)
#       SSPE <- t(P) %*% SSPE %*% P
#       B <- B %*% P
#   }
#   rank <- sum(eigen(SSPE, only.values=TRUE)$values >= sqrt(.Machine$double.eps))
#   if (rank < ncol(SSPE))
#       stop("The error SSP matrix is apparently of deficient rank = ",
#           rank, " < ", ncol(SSPE))
#   r <- ncol(B)
#   if (is.null(rhs)) rhs <- matrix(0, nrow(L), r)
#   rownames(rhs) <- rownames(L)
#   colnames(rhs) <- colnames(B)
#   q <- NROW(L)
#   if (verbose){
#       cat("\nHypothesis matrix:\n")
#       print(L)
#       cat("\nRight-hand-side matrix:\n")
#       print(rhs)
#       cat("\nEstimated linear function (hypothesis.matrix %*% coef - rhs):\n")
#       print(drop(L %*% B - rhs))
#       cat("\n")
#   }
#   SSPH <- t(L %*% B - rhs) %*% solve(L %*% V %*% t(L)) %*% (L %*% B - rhs)
#   rval <- list(SSPH=SSPH, SSPE=SSPE, df=q, r=r, df.residual=df.residual, P=P,
#       title=title, test=test)
#   class(rval) <- "linearHypothesis.mlm"
#   rval
#}

print.linearHypothesis.mlm <- function(x, SSP=TRUE, SSPE=SSP,
		digits=getOption("digits"), ...){
	test <- x$test
	if (!is.null(x$P) && SSP){
		P <- x$P
		cat("\n Response transformation matrix:\n")
		attr(P, "assign") <- NULL
		attr(P, "contrasts") <- NULL
		print(P, digits=digits)
	}
	if (SSP){
		cat("\nSum of squares and products for the hypothesis:\n")
		print(x$SSPH, digits=digits)
	}
	if (SSPE){
		cat("\nSum of squares and products for error:\n")
		print(x$SSPE, digits=digits)
	}
	SSPE.qr <- qr(x$SSPE)
	# the following code is adapted from summary.manova
	eigs <- Re(eigen(qr.coef(SSPE.qr, x$SSPH), symmetric = FALSE)$values)
	tests <- matrix(NA, 4, 4)
	rownames(tests) <- c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")
	if ("Pillai" %in% test)
		tests[1, 1:4] <- stats:::Pillai(eigs, x$df, x$df.residual)
	if ("Wilks" %in% test)
		tests[2, 1:4] <- stats:::Wilks(eigs, x$df, x$df.residual)
	if ("Hotelling-Lawley" %in% test)
		tests[3, 1:4] <- stats:::HL(eigs, x$df, x$df.residual)
	if ("Roy" %in% test)
		tests[4, 1:4] <- stats:::Roy(eigs, x$df, x$df.residual)
	tests <- na.omit(tests)
	ok <- tests[, 2] >= 0 & tests[, 3] > 0 & tests[, 4] > 0
	ok <- !is.na(ok) & ok
	tests <- cbind(x$df, tests, pf(tests[ok, 2], tests[ok, 3], tests[ok, 4],
					lower.tail = FALSE))
	colnames(tests) <- c("Df", "test stat", "approx F", "num Df", "den Df", "Pr(>F)")
	tests <- structure(as.data.frame(tests),
			heading = paste("\nMultivariate Test",
					if (nrow(tests) > 1) "s", ": ", x$title, sep=""),
			class = c("anova", "data.frame"))
	print(tests, digits=digits)
	invisible(x)
}

linearHypothesis.survreg <- function(model, hypothesis.matrix, rhs=NULL,
		test=c("Chisq", "F"), vcov., verbose=FALSE, ...){
	if (missing(vcov.)) {
		vcov. <- vcov(model)
		p <- nrow(vcov.)
		vcov. <- vcov.[-p, -p]
	}
	linearHypothesis.default(model, hypothesis.matrix, rhs, test, vcov., verbose=verbose, ...)
}

linearHypothesis.polr <- function (model, hypothesis.matrix, rhs=NULL, vcov., verbose=FALSE, ...){
	k <- length(coef(model))
	V <- vcov(model)[1:k, 1:k]
	linearHypothesis.default(model, hypothesis.matrix, rhs, vcov.=V, verbose=verbose, ...)
}

coef.multinom <- function(object, ...){
	b <- nnet:::coef.multinom(object, ...)
	cn <- colnames(b)
	rn <- rownames(b)
	b <- as.vector(t(b))
	names(b) <- as.vector(outer(cn, rn, function(c, r) paste(r, c, sep=":")))
	b
}

## functions for mixed models

linearHypothesis.mer <- function(model, hypothesis.matrix, rhs=NULL,
		vcov.=NULL, singular.ok=FALSE, verbose=FALSE, ...){
	V <- as.matrix(if (is.null(vcov.))vcov(model)
					else if (is.function(vcov.)) vcov.(model) else vcov.)
	b <- fixef(model)
	if (any(aliased <- is.na(b)) && !singular.ok)
		stop("there are aliased coefficients in the model")
	b <- b[!aliased]
	if (is.character(hypothesis.matrix)) {
		L <- makeHypothesis(names(b), hypothesis.matrix, rhs)
		if (is.null(dim(L))) L <- t(L)
		rhs <- L[, NCOL(L)]
		L <- L[, -NCOL(L), drop = FALSE]
		rownames(L) <- hypothesis.matrix
	}
	else {
		L <- if (is.null(dim(hypothesis.matrix))) t(hypothesis.matrix)
				else hypothesis.matrix
		if (is.null(rhs)) rhs <- rep(0, nrow(L))
	}
	q <- NROW(L)
	if (verbose){
		cat("\nHypothesis matrix:\n")
		print(L)
		cat("\nRight-hand-side vector:\n")
		print(rhs)
		cat("\nEstimated linear function (hypothesis.matrix %*% coef - rhs)\n")
		print(drop(L %*% b - rhs))
		cat("\n")
	}
	df <- Inf
	SSH <- as.vector(t(L %*% b - rhs) %*% solve(L %*% V %*% t(L)) %*% (L %*% b - rhs))
	name <- try(formula(model), silent = TRUE)
	if (inherits(name, "try-error")) name <- substitute(model)
	title <- "Linear hypothesis test\n\nHypothesis:"
	topnote <- paste("Model 1: restricted model","\n", "Model 2: ", 
			paste(deparse(name), collapse = "\n"), sep = "")
	note <- if (is.null(vcov.)) ""
			else "\nNote: Coefficient covariance matrix supplied.\n"
	rval <- matrix(rep(NA, 8), ncol = 4)
	colnames(rval) <- c("Res.Df", "Df", "Chisq",  paste("Pr(> Chisq)", sep = ""))
	rownames(rval) <- 1:2
	rval[,1] <- c(df+q, df)
	p <- pchisq(SSH, q, lower.tail = FALSE)
	rval[2, 2:4] <- c(q, SSH, p)
	rval <- rval[,-1]
	structure(as.data.frame(rval),
			heading = c(title, printHypothesis(L, rhs, names(b)), "", topnote, note),
			class = c("anova", "data.frame"))
}

linearHypothesis.lme <- function(model, hypothesis.matrix, rhs=NULL,
		vcov.=NULL, singular.ok=FALSE, verbose=FALSE, ...){
	V <- as.matrix(if (is.null(vcov.))vcov(model)
					else if (is.function(vcov.)) vcov.(model) else vcov.)
	b <- fixef(model)
	if (any(aliased <- is.na(b)) && !singular.ok)
		stop("there are aliased coefficients in the model")
	b <- b[!aliased]
	if (is.character(hypothesis.matrix)) {
		L <- makeHypothesis(names(b), hypothesis.matrix, rhs)
		if (is.null(dim(L))) L <- t(L)
		rhs <- L[, NCOL(L)]
		L <- L[, -NCOL(L), drop = FALSE]
		rownames(L) <- hypothesis.matrix
	}
	else {
		L <- if (is.null(dim(hypothesis.matrix))) t(hypothesis.matrix)
				else hypothesis.matrix
		if (is.null(rhs)) rhs <- rep(0, nrow(L))
	}
	q <- NROW(L)
	if (verbose){
		cat("\nHypothesis matrix:\n")
		print(L)
		cat("\nRight-hand-side vector:\n")
		print(rhs)
		cat("\nEstimated linear function (hypothesis.matrix %*% coef - rhs)\n")
		print(drop(L %*% b - rhs))
		cat("\n")
	}
	df <- Inf
	SSH <- as.vector(t(L %*% b - rhs) %*% solve(L %*% V %*% t(L)) %*% (L %*% b - rhs))
	name <- try(formula(model), silent = TRUE)
	if (inherits(name, "try-error")) name <- substitute(model)
	title <- "Linear hypothesis test\n\nHypothesis:"
	topnote <- paste("Model 1: restricted model","\n", "Model 2: ", 
			paste(deparse(name), collapse = "\n"), sep = "")
	note <- if (is.null(vcov.)) ""
			else "\nNote: Coefficient covariance matrix supplied.\n"
	rval <- matrix(rep(NA, 8), ncol = 4)
	colnames(rval) <- c("Res.Df", "Df", "Chisq",  paste("Pr(> Chisq)", sep = ""))
	rownames(rval) <- 1:2
	rval[,1] <- c(df+q, df)
	p <- pchisq(SSH, q, lower.tail = FALSE)
	rval[2, 2:4] <- c(q, SSH, p)
	rval <- rval[,-1]
	structure(as.data.frame(rval),
			heading = c(title, printHypothesis(L, rhs, names(b)), "", topnote, note),
			class = c("anova", "data.frame"))
}



## matchCoefs

matchCoefs <- function(model, pattern, ...) UseMethod("matchCoefs")

matchCoefs.default <- function(model, pattern, coef.=coef, ...){
	names <- names(coef.(model))
	grep(pattern, names, value=TRUE)
}

matchCoefs.mer <- function(model, pattern, ...) NextMethod(coef.=fixef)

matchCoefs.lme <- function(model, pattern, ...) NextMethod(coef.=fixef)


has.intercept <- function (model, ...) {
	UseMethod("has.intercept")
}

has.intercept.default <- function(model, ...) any(names(coefficients(model))=="(Intercept)")

term.names <- function (model, ...) {
	UseMethod("term.names")
}

term.names.default <- function (model, ...) {
	term.names <- labels(terms(model))
	if (has.intercept(model)) c("(Intercept)", term.names)
	else term.names
}

predictor.names <- function(model, ...) {
	UseMethod("predictor.names")
}

predictor.names.default <- function(model, ...){
	predictors <- attr(terms(model), "variables")
	as.character(predictors[3:length(predictors)])
}

responseName <- function (model, ...) {
	UseMethod("responseName")
}

responseName.default <- function (model, ...) deparse(attr(terms(model), "variables")[[2]])

response <- function(model, ...) {
	UseMethod("response")
}

response.default <- function (model, ...) model.response(model.frame(model))


ConjComp <- function(X, Z = diag( nrow(X)), ip = diag(nrow(X))) {
	# This function by Georges Monette
	# finds the conjugate complement of the proj of X in span(Z) wrt
	#    inner product ip
	# - assumes Z is of full column rank
	# - projects X conjugately wrt ip into span Z
	xq <- qr(t(Z) %*% ip %*% X)
	if (xq$rank == 0) return(Z)
	Z %*% qr.Q(xq, complete = TRUE) [ ,-(1:xq$rank)] 
}

relatives <- function(term, names, factors){
	is.relative <- function(term1, term2) {
		all(!(factors[,term1]&(!factors[,term2])))
	}
	if(length(names) == 1) return(NULL)
	which.term <- which(term==names)
	(1:length(names))[-which.term][sapply(names[-which.term], 
					function(term2) is.relative(term, term2))]
}


Anova <- function(mod, ...){
	UseMethod("Anova", mod)
}

# linear models

Anova.lm <- function(mod, error, type=c("II","III", 2, 3), 
		white.adjust=c(FALSE, TRUE, "hc3", "hc0", "hc1", "hc2", "hc4"), 
		singular.ok, ...){
	type <- as.character(type)
	white.adjust <- as.character(white.adjust)
	type <- match.arg(type)
	white.adjust <- match.arg(white.adjust)
	if (missing(singular.ok)){
		singular.ok <- type == "2" || type == "II"
	}
	if (has.intercept(mod) && length(coef(mod)) == 1 
			&& (type == "2" || type == "II")) {
		type <- "III"
		warning("the model contains only an intercept: Type III test substituted")
	}
	if (white.adjust != "FALSE"){
		if (white.adjust == "TRUE") white.adjust <- "hc3" 
		return(Anova.default(mod, type=type, vcov.=hccm(mod, type=white.adjust), test="F", 
						singular.ok=singular.ok))
	}
	switch(type,
			II=Anova.II.lm(mod, error, singular.ok=singular.ok, ...),
			III=Anova.III.lm(mod, error, singular.ok=singular.ok, ...),
			"2"=Anova.II.lm(mod, error, singular.ok=singular.ok, ...),
			"3"=Anova.III.lm(mod, error, singular.ok=singular.ok,...))
}

Anova.aov <- function(mod, ...){
	class(mod) <- "lm"
	Anova.lm(mod, ...)
}

Anova.II.lm <- function(mod, error, singular.ok=TRUE, ...){
	if (!missing(error)){
		sumry <- summary(error, corr=FALSE)
		s2 <- sumry$sigma^2
		error.df <- error$df.residual
		error.SS <- s2*error.df
	}
	SS.term <- function(term){
		which.term <- which(term == names)
		subs.term <- which(assign == which.term)
		relatives <- relatives(term, names, fac)
		subs.relatives <- NULL
		for (relative in relatives) 
			subs.relatives <- c(subs.relatives, which(assign == relative))
		hyp.matrix.1 <- I.p[subs.relatives,,drop=FALSE]
		hyp.matrix.1 <- hyp.matrix.1[, not.aliased, drop=FALSE]
		hyp.matrix.2 <- I.p[c(subs.relatives,subs.term),,drop=FALSE]
		hyp.matrix.2 <- hyp.matrix.2[, not.aliased, drop=FALSE]
		hyp.matrix.term <- if (nrow(hyp.matrix.1) == 0) hyp.matrix.2
				else t(ConjComp(t(hyp.matrix.1), t(hyp.matrix.2), vcov(mod)))
		hyp.matrix.term <- hyp.matrix.term[!apply(hyp.matrix.term, 1, 
						function(x) all(x == 0)), , drop=FALSE]
		if (nrow(hyp.matrix.term) == 0)
			return(c(SS=NA, df=0))
		lh <- linearHypothesis(mod, hyp.matrix.term, 
				singular.ok=singular.ok, ...)
		abs(c(SS=lh$"Sum of Sq"[2], df=lh$Df[2]))
	}
	not.aliased <- !is.na(coef(mod))
	if (!singular.ok && !all(not.aliased))
		stop("there are aliased coefficients in the model")
	fac <- attr(mod$terms, "factors")
	intercept <- has.intercept(mod)
	I.p <- diag(length(coefficients(mod)))
	assign <- mod$assign
	assign[!not.aliased] <- NA
	names <- term.names(mod)
	if (intercept) names <-names[-1]
	n.terms <- length(names)
	p <- df <- f <- SS <- rep(0, n.terms + 1)
	sumry <- summary(mod, corr = FALSE)
	SS[n.terms + 1] <- if (missing(error)) sumry$sigma^2*mod$df.residual 
			else error.SS   
	df[n.terms + 1] <- if (missing(error)) mod$df.residual else error.df
	p[n.terms + 1] <- f[n.terms + 1] <- NA
	for (i in 1:n.terms){
		ss <- SS.term(names[i])
		SS[i] <- ss["SS"]
		df[i] <- ss["df"]
		f[i] <- df[n.terms+1]*SS[i]/(df[i]*SS[n.terms + 1])
		p[i] <- pf(f[i], df[i], df[n.terms + 1], lower.tail = FALSE)
	}    
	result <- data.frame(SS, df, f, p)
	row.names(result) <- c(names,"Residuals")
	names(result) <- c("Sum Sq", "Df", "F value", "Pr(>F)")
	class(result) <- c("anova", "data.frame")
	attr(result, "heading") <- c("Anova Table (Type II tests)\n", 
			paste("Response:", responseName(mod)))
	result
}

# type III

Anova.III.lm <- function(mod, error, singular.ok=FALSE, ...){
	if (!missing(error)){
		error.df <- df.residual(error)
		error.SS <- deviance(error)
	}
	else {
		error.df <- df.residual(mod)
		error.SS <- deviance(mod)
	}
	intercept <- has.intercept(mod)
	I.p <- diag(length(coefficients(mod)))
	Source <- term.names(mod)
	n.terms <- length(Source)
	p <- df <- f <- SS <- rep(0, n.terms + 1)
	assign <- mod$assign
	not.aliased <- !is.na(coef(mod))
	if (!singular.ok && !all(not.aliased))
		stop("there are aliased coefficients in the model")
	for (term in 1:n.terms){
		subs <- which(assign == term - intercept)
		hyp.matrix <- I.p[subs,,drop=FALSE]
		hyp.matrix <- hyp.matrix[, not.aliased, drop=FALSE]
		hyp.matrix <- hyp.matrix[!apply(hyp.matrix, 1, function(x) all(x == 0)), , drop=FALSE]
		if (nrow(hyp.matrix) == 0){
			SS[term] <- NA
			df[term] <- 0
			f[term] <- NA
			p[term] <- NA
		}
		else {
			test <- if (missing(error)) linearHypothesis(mod, hyp.matrix, 
								singular.ok=singular.ok, ...)
					else linearHypothesis(mod, hyp.matrix, error.SS=error.SS, error.df=error.df, 
								singular.ok=singular.ok, ...)
			SS[term] <- test$"Sum of Sq"[2]
			df[term] <- test$"Df"[2]
			f[term] <- test$"F"[2]
			p[term] <- test$"Pr(>F)"[2]
		}
	}
	Source[n.terms + 1] <- "Residuals"
	SS[n.terms + 1] <- error.SS
	df[n.terms + 1] <- error.df
	p[n.terms + 1] <- f[n.terms + 1] <- NA
	result <- data.frame(SS, df, f, p)
	row.names(result) <- Source
	names(result) <- c("Sum Sq", "Df", "F value", "Pr(>F)")
	class(result) <- c("anova", "data.frame")
	attr(result, "heading") <- c("Anova Table (Type III tests)\n", paste("Response:", responseName(mod)))
	result
}




etasq <- function(x, ...){
	UseMethod("etasq", x)
}



etasq.lm <- function(x, anova=FALSE, partial=TRUE, ...) {
	aov <-Anova(x, ...)
	neff <- nrow(aov)
	SSH <- aov[-neff,1]
	SSE <- aov[neff,1]
	SST <- sum(SSH)
	eta2 <- if (partial) c(SSH / (SSH + SSE), NA) else c(SSH / SST, NA)
	etalab <- if (partial) "Partial eta^2" else "eta^2"
	if (anova) {
		result <- cbind(eta2, aov)
		rownames(result) <- rownames(aov)
		colnames(result) <- c(etalab, colnames(aov))
		result <- structure(as.data.frame(result), 
				heading = attr(aov, "heading"), 
				class = c("anova", "data.frame"))
	}
	else {
		result <- data.frame(eta2)
		rownames(result) <- rownames(aov)
		colnames(result) <- etalab
	}
	result      
}

#### HOOFDSTUK 5: CORRELATIES



pn <- function(X){crossprod(!is.na(X))}

cor.prob <- function(X, dfr = nrow(na.omit(X)) - 2,use="complete.obs") {
# Correlations Below Main Diagonal
# Significance Tests with Pairwise Deletion
# Above Main Diagonal
# Believe part of this came from Bill Venables
pair.SampSize <- pn(X)
above1 <- row(pair.SampSize) < col(pair.SampSize)
pair.df <- pair.SampSize[above1] - 2
R <- cor(X, use="pair")
above2 <- row(R) < col(R)
r2 <- R[above2]^2
Fstat <- (r2 * pair.df)/(1 - r2)
R[above2] <- 1 - pf(Fstat, 1, pair.df)
R
}

#### HOOFDSTUK 7: Regressie assumpties

cooks_plot=function (X) 
{
    p=length(X$coefficients)
    n=length(X$residuals)
    COOK<-cooks.distance(X)
    cutoff<-4/(n-p-1)
    plot(names(COOK),COOK,ylab="Cook's distance",xlab="Case nr")
    abline(h=cutoff,lty=2,col="red")
    identify(names(COOK),COOK,names(COOK))
}

residuals_plot2=function (X) 
{
    p=length(X$coefficients)
    n=length(X$residuals)
    RSTUDENT<-na.omit(rstudent(X))
    FITTED<-na.omit(X$fitted.values)
    plot(FITTED,RSTUDENT,ylab="Studentized residuals",xlab="Voorspelde waarden")
    abline(h=2,lty=2,col="red")
	abline(h=-2,lty=2,col="red")
    identify(FITTED,RSTUDENT)
}

residuals_plot=function (X) 
{
    p=length(X$coefficients)
    n=length(X$residuals)
    RSTUDENT<-rstudent(X)
    FITTED<-X$fitted.values
    plot(FITTED,RSTUDENT,ylab="Studentized residuals",xlab="Voorspelde waarden")
    abline(h=2,lty=2,col="red")
	abline(h=-2,lty=2,col="red")
    identify(FITTED,RSTUDENT,names(FITTED))
}

Bonferroni<-function(x,y) {pairwise.t.test(x,y,p.adjust.method="bonferroni", pool.sd=F)}

#### LOGISTISCHE REGRESSIE ####

alog<-function(X) 
{   exp(X)/(1+exp(X))
}

effectplot<-function(x,y,ylab="y" ,xlab="x",main=" " )
{   sequence<-order(x)
	plot(x[sequence],y[sequence],type="l",col="red",lwd=1.5,xlab=xlab,ylab=ylab,main=main)
}
