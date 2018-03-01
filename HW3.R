
#---------------------------------------------------------------
# 2.(b)
rs1b <- function(N,theta) {
  
  X <- rep(0,N);
  pi1 <- 2*gamma(theta)/(2*gamma(theta)+gamma(theta+0.5))
  pi2 <- 1-pi1
  
  for (i in 1:N) {
    if (runif(1) > pi1) { X[i] <- rgamma(1,shape=theta,scale=1)}
    else { X[i] <- rgamma(1,shape=theta+0.5,scale=1)  }
  }
  return(X);
}
rs1b(100,3)

  
#---------------------------------------------------------------
# 2.(c)
rs1c <- function(N,theta) {
  
  X <- rep(0,N);
  
  for (i in 1:N) {
    while (1) {
      Y = rs1b(1, theta); 
      if (runif(1) < sqrt(4+Y)/(sqrt(Y)+2)) {
        X[i] = Y; break;
      }
    }
  }
  return(X);
}

theta <- 2

sample1c <-rs1c(10000,theta)

## Estimate the density and compare it to q(x) to see if they are
## proportional to each other
## Specify on which interval the density function is to be estimated
intvl=c(.2, floor(max(sample1c)+1)+.2);

## Specify the number of points in the interval on which density is
## to be estimated.  The points are equally spaced in the interval,
## with the first one and last one being the end points of the interval
n.p=1+(intvl[2]-intvl[1])/0.02;

## Estimate the density.  The returned is a 
df.obj = density(sample1c, from=intvl[1], to=intvl[2], n=n.p)
x = df.obj$x;
df = df.obj$y;

## Calculate the value of q(x) 
q.val = (4+x)^0.5 * x^(theta-1)*exp(-x);

## Make the two on the same scale
q.val = q.val*mean(df)/mean(q.val);

plot(ts(cbind(df, q.val), start=intvl[1], deltat=diff(intvl)/(n.p-1)),
     plot.type="single", col=c("black", "red"),
     ylab="Density", xlab="x", main = paste("2.c) Estimated density with 10,000 obs | theta:", theta));

legend('topright', legend=c("Simulated","Actual"),lty=c(1,1), lwd=c(2.5,2.5),col=c('black','red'))


#---------------------------------------------------------------
# 3.(a)
rs2a <- function(N,theta,beta) {
  
  X <- rep(0,N);
  lambda <- 3^0.5*theta/(beta+3^0.5*theta)
  
  for (i in 1:N) {
    while (1) {
      Z <- runif(1)
    if (runif(1) > lambda) { Y <- Z^(1/theta)}
    else { Y <- 1-Z^(1/beta)  }
      
      if (runif(1) < (Y^(theta-1)/(1+Y^2)+sqrt(Y^2+2)*(1-Y)^(beta-1))/(Y^(theta-1)+sqrt(3)*(1-Y)^(beta-1)))
          { X[i] = Y; break;      }
      }
  }
  return(X);
}

theta <- 2
beta <- 3

sample2a <-rs2a(10000,theta,beta)

## Estimate the density and compare it to q(x) to see if they are
## proportional to each other

## Specify on which interval the density function is to be estimated
intvl=c(.05, 0.95);

## Specify the number of points in the interval on which density is
## to be estimated.  The points are equally spaced in the interval,
## with the first one and last one being the end points of the interval
n.p=451;

## Estimate the density.  The returned is a 
df.obj = density(sample2a, from=intvl[1], to=intvl[2], n=n.p)
x = df.obj$x;
df = df.obj$y;

## Calculate the value of q(x) 
q.val = (x^(theta-1))/(1+x^2) + sqrt(2+x^2)*(1-x)^(beta-1);

## Make the two on the same scale
q.val = q.val*mean(df)/mean(q.val);

plot(ts(cbind(df, q.val), start=intvl[1], deltat=diff(intvl)/(n.p-1)),
     plot.type="single", col=c("dark red", "purple"),
     ylab="Density", xlab="x", main = paste("3.a) Estimated density with 10,000 obs | theta:", theta,", beta:",beta));

legend('topright', legend=c("Simulated","Actual"),lty=c(1,1), lwd=c(2.5,2.5),col=c('dark red','purple'))


#---------------------------------------------------------------
# 3.(b)
rs2b <- function(N,theta,beta) {

  X <- rep(0,N);
  lambda <- 3^0.5*theta/(beta+3^0.5*theta)
    
  for (i in 1:N) {
    if (runif(1) > lambda) {
      while (1) {
        Y = rbeta(1,theta,1); 
        if (runif(1) < 1/(1+Y^2)) {
          X[i] = Y; break;
        }
      }
    }
    else {
      while (1) {
        Y = rbeta(1,1,beta); 
        if (runif(1) < sqrt(2/3+Y^2/3)) {
          X[i] = Y; break;
        }
      }
    }
  }
  return(X);
}

theta <- 2
beta <- 3

sample2b <-rs2b(10000,theta,beta)

## Estimate the density and compare it to q(x) to see if they are
## proportional to each other

## Specify on which interval the density function is to be estimated
intvl=c(.05, 0.95);

## Specify the number of points in the interval on which density is
## to be estimated.  The points are equally spaced in the interval,
## with the first one and last one being the end points of the interval
n.p=451;

## Estimate the density.  The returned is a 
df.obj = density(sample2b, from=intvl[1], to=intvl[2], n=n.p)
x = df.obj$x;
df = df.obj$y;

## Calculate the value of q(x) 
q.val = (x^(theta-1))/(1+x^2) + sqrt(2+x^2)*(1-x)^(beta-1);

## Make the two on the same scale
q.val = q.val*mean(df)/mean(q.val);

plot(ts(cbind(df, q.val), start=intvl[1], deltat=diff(intvl)/(n.p-1)),
     plot.type="single", col=c("green", "blue"),
     ylab="Density", xlab="x", main = paste("3.b) Estimated density with 10,000 obs | theta:", theta,", beta:",beta));

legend('topright', legend=c("Simulated","Actual"),lty=c(1,1), lwd=c(2.5,2.5),col=c('green','blue'))
