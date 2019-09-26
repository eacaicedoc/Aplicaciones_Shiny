shadow.dist <- function (dist = "dnorm", param = list(mean = 0, sd = 1), a = NULL, 
          b = NULL, type = "lower", col.shadow = "skyblue", col.line = "black", 
          lwd = 3, nbreaks = 10000, ylab = NULL, x, ...) 
{
  type <- match.arg(arg = type, choices = c("lower", "middle", 
                                            "upper", "two"))
  if (is.null(a) & is.null(b)) 
    stop("At least define the parameter a")
  if (type %in% c("middle", "two") & length(c(a, b)) <= 1) 
    stop("When type is 'middle' or 'two' you must define a & b")
  if (length(c(a, b)) == 2) {
    values <- c(a, b)
    a <- min(values)
    b <- max(values)
  }
  if (is.null(a) & !is.null(b)) 
    a <- b
  if (type == "lower") {
    b <- a
    a <- -999
  }
  if (type == "upper") {
    b <- 999
  }
  if (is.null(ylab)) 
    ylab <- "Density"
  step <- (b - a)/nbreaks
  cord.x <- c(a, seq(from = a, to = b, by = step), b)
  y <- seq(from = a, to = b, by = step)
  cord.y <- c(0, do.call(dist, c(list(x = y), param)), 0)
  edge <- 999
  cord.x2 <- c(-edge, seq(from = -edge, to = edge, length.out = 1e+06), 
               edge)
  y <- seq(from = -edge, to = edge, length.out = 1e+06)
  cord.y2 <- c(0, do.call(dist, c(list(x = y), param)), 0)
  curve(do.call(dist, c(list(x), param)), ylab = ylab, ...)
  if (type == "two") {
    polygon(cord.x2, cord.y2, col = col.shadow)
    col.shadow <- "white"
  }
  polygon(cord.x, cord.y, col = col.shadow)
  curve(do.call(dist, c(list(x), param)), add = TRUE, lwd = lwd, 
        col = col.line)
}

