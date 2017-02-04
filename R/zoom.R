zoom <- function (x, y, xlim, ylim, xd, yd) 
{
  rxlim <- x + c(-1, 1) * (diff(range(xd))/20)
  rylim <- y + c(-1, 1) * (diff(range(yd))/20)
  par(mfrow = c(1, 2))
  plot(xd, yd, xlab = "mean", ylab = "sd")
  xext <- yext <- rxext <- ryext <- 0
  if (par("xaxs") == "r") {
    xext <- diff(xlim) * 0.04
    rxext <- diff(rxlim) * 0.04
  }
  if (par("yaxs") == "r") {
    yext <- diff(ylim) * 0.04
    ryext <- diff(rylim) * 0.04
  }
  rect(rxlim[1] - rxext, rylim[1] - ryext, rxlim[2] + rxext, 
       rylim[2] + ryext)
  xylim <- par("usr")
  xypin <- par("pin")
  rxi0 <- xypin[1] * (xylim[2] - (rxlim[1] - rxext))/diff(xylim[1:2])
  rxi1 <- xypin[1] * (xylim[2] - (rxlim[2] + rxext))/diff(xylim[1:2])
  y01i <- xypin[2] * (xylim[4] - (rylim[2] + ryext))/diff(xylim[3:4])
  y02i <- xypin[2] * ((rylim[1] - ryext) - xylim[3])/diff(xylim[3:4])
  mu <- x
  curve(dnorm(x, mean = mu, sd = y), from = -4 * y + mu, to = 4 * y + mu, 
        xlab = paste("mean:", round(mu, 2), ", sd: ", round(y, 2)), ylab = "")
  xypin <- par("pin")
  par(xpd = NA)
  xylim <- par("usr")
  xymai <- par("mai")
  x0 <- xylim[1] - diff(xylim[1:2]) * (xymai[2] + xymai[4] + 
                                         rxi0)/xypin[1]
  x1 <- xylim[1] - diff(xylim[1:2]) * (xymai[2] + xymai[4] + 
                                         rxi1)/xypin[1]
  y01 <- xylim[4] - diff(xylim[3:4]) * y01i/xypin[2]
  y02 <- xylim[3] + diff(xylim[3:4]) * y02i/xypin[2]
  par(xpd = TRUE)
  xend <- xylim[1] - diff(xylim[1:2]) * xymai[2]/(2 * xypin[1])
  xprop0 <- (xylim[1] - xend)/(xylim[1] - x0)
  xprop1 <- (xylim[2] - xend)/(xylim[2] - x1)
  par(xpd = NA)
  segments(c(x0, x0, x1, x1), 
           c(y01, y02, y01, y02), 
           c(xend, xend, xend, xend), 
           c(xylim[4] - (xylim[4] - y01) * xprop0, 
             xylim[3] + (y02 - xylim[3]) * xprop0, 
             xylim[4] - (xylim[4] - y01) * xprop1, 
             xylim[3] + (y02 - xylim[3]) * xprop1))
  par(mfg = c(1, 1))
  plot(xd, yd, xlab = "mean", ylab = "sd")
}

ident <- function(x, y, ...)
{
  ans <- identify(x, y, n = 1, plot = FALSE, ...)
  if(length(ans)) {
    zoom(x[ans], y[ans], range(x), range(y), x, y)
    points(x[ans], y[ans], pch = 19)
    ident(x, y)
  }
}

x <- rnorm(100, mean = 50)
y <- rnorm(100, mean = 15)
par(mfrow = c(1, 2))
plot(x, y, xlab = "mean", ylab = "sd")
ident(x, y)