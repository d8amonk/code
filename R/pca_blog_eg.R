rm(list = ls())

knitr::opts_chunk$set(fig.width=7, fig.height=7,
                      echo=TRUE, warning=FALSE, message=FALSE)

library('ggplot2')
library('tidyr')
library('WVPlots') # devtools::install_github('WinVector/WVPlots',build_vignettes=TRUE)
library('ggthemes')

# https://github.com/WinVector/Examples/blob/master/PCR/XonlyPCA.Rmd
barbell_plot = function(frame, xvar, ymin, ymax, colorvar=NULL) {
  if(is.null(colorvar)) {
    gplot = ggplot(frame, aes_string(x=xvar))
  } else {
    gplot = ggplot(frame, aes_string(x=xvar, color=colorvar))
  }
  
  gplot + geom_point(aes_string(y=ymin)) + 
    geom_point(aes_string(y=ymax)) +
    geom_linerange(aes_string(ymin=ymin, ymax=ymax)) +
    ylab("value")
}

dotplot_identity = function(frame, xvar, yvar, colorvar=NULL) {
  if(is.null(colorvar)) {
    gplot = ggplot(frame, aes_string(x=xvar, y=yvar, ymax=yvar))
  } else {
    gplot = ggplot(frame, 
                   aes_string(x=xvar, y=yvar, ymax=yvar, 
                              color=colorvar))
  }
  gplot + geom_point() + geom_linerange(aes(ymin=0))
}

extractProjection <- function(ndim,princ) {
  # pull off the rotation.  
  proj <- princ$rotation[,1:ndim] 
  # sign was arbitrary, so flip in convenient form
  for(i in seq_len(ndim)) {
    si <- sign(mean(proj[,i]))
    if(si!=0) {
      proj[,i] <- proj[,i]*si
    }
  }
  proj
}

rsq <- function(x,y) {
  1 - sum((y-x)^2)/sum((y-mean(y))^2)
}


# build example where even and odd variables are bringing in noisy images
# of two different signals.
mkData <- function(n =50) {
  for(group in 1:10) {
    # y is the sum of two effects yA and yB
    yA <- rnorm(n)
    yB <- rnorm(n)
    if(group == 1){
      d <- data.frame(y = yA + yB + rnorm(n))
      code <- 'x'
    } else {
      code <- paste0('noise', group - 1)
    }
    yS <- list(yA, yB)
    # these variables are correlated with y in group 1,
    # but only to each other (and not y) in other groups
    for(i in 1:5) {
      vi <- yS[[1 + (i%%2)]] + rnorm(nrow(d))
      d[[paste(code, formatC(i, width=2, flag=0), sep='.')]] <- ncol(d)*vi
    }
  }
  d
}

# make data
set.seed(23525)
dTrain <- mkData(1000)
dTest <- mkData(1000)

# inspect a subset
summary(dTrain[, c("y", "x.01", "x.02",
                   "noise1.01", "noise1.02")])

# ideal situation - domain knowledge eliminates noise
goodVars <-  colnames(dTrain)[grep('^x.', colnames(dTrain))]
dTrainIdeal <- dTrain[, c('y', goodVars)]
dTestIdeal <-  dTrain[, c('y', goodVars)]

# do the PCA
dmTrainIdeal <- as.matrix(dTrainIdeal[, goodVars])
princIdeal <- prcomp(dmTrainIdeal, center = TRUE, scale. = TRUE)

# extract the principal components
rot5Ideal <- extractProjection(5, princIdeal)

# prepare the data to plot the variable loadings
rotfIdeal = as.data.frame(rot5Ideal)
rotfIdeal$varName = rownames(rotfIdeal)
rotflongIdeal = gather(rotfIdeal, "PC", "loading",
                       starts_with("PC"))
rotflongIdeal$vartype = ifelse(grepl("noise", 
                                     rotflongIdeal$varName),
                               "noise", "signal")

# plot the singular values
dotplot_identity(frame = data.frame(pc=1:length(princIdeal$sdev), 
                                    magnitude=princIdeal$sdev), 
                 xvar="pc",yvar="magnitude") +
  ggtitle("Ideal case: Magnitudes of singular values")

# The plot of the variable loadings is a graphical representation 
# of the coordinates of the principal components. Each coordinate
# corresponds to the contribution of one of the original variables 
# to that principal component.
dotplot_identity(rotflongIdeal, "varName", "loading", "vartype") + 
  facet_wrap                                                                                                                                                                                                                                                                                                              (~PC,nrow=1) + coord_flip() + 
  ggtitle("x scaled variable loadings, first 5 principal components") + 
  scale_color_manual(values = c("noise" = "#d95f02", "signal" = "#1b9e77"))

# Since most of the signal is in the first two principal components, we can 
# look at the projection of the data into that plane, using color to code y.

# signs are arbitrary on PCA, so instead of calling predict we pull out
# (and alter) the projection by hand
projectedTrainIdeal <-
  as.data.frame(dmTrainIdeal %*% extractProjection(2,princIdeal),
                stringsAsFactors = FALSE)
projectedTrainIdeal$y <- dTrain$y
ScatterHistN(projectedTrainIdeal,'PC1','PC2','y',
             "Ideal Data projected to first two principal components")

# In a messy data situation like the one we are emulating, the best 
# practice is to re-scale the x variables; however, we'll first naively 
# apply PCA to the data as it is. This is to demonstrate the sensitivity 
# of PCA to the units of the data.

vars <- setdiff(colnames(dTrain),'y')

duTrain <- as.matrix(dTrain[,vars])
prinU <- prcomp(duTrain,center = TRUE,scale. = FALSE) 

dotplot_identity(frame = data.frame(pc=1:length(prinU$sdev), 
  magnitude=prinU$sdev), 
  xvar="pc",yvar="magnitude") +
  ggtitle("Unscaled case: Magnitudes of singular values")

# In addition, when we look at the variable loading of the first 
# five principal components, we will see another problem:
  
rot5U <- extractProjection(5,prinU)
rot5U = as.data.frame(rot5U)
rot5U$varName = rownames(rot5U)
rot5U = gather(rot5U, "PC", "loading",
               starts_with("PC"))
rot5U$vartype = ifelse(grepl("noise", 
                             rot5U$varName),
                       "noise", "signal")

dotplot_identity(rot5U, "varName", "loading", "vartype") + 
  facet_wrap(~PC,nrow=1) + coord_flip() + 
  ggtitle("unscaled variable loadings, first 5 principal components") + 
  scale_color_manual(values = c("noise" = "#d95f02", "signal" = "#1b9e77"))
