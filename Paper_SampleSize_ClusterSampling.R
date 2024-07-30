# Load library
library(pwr)
## m is the number of clusters and ki is the size of the cluster 
## k_bar = sum(ki)/m
## DE = (1+rho*(k_bar-1))
#par(mfrow=c(3,2))
## Set values
rho=0.01
k_bar=220
alph=0.01
# Create range of correlations
r <- seq(0.2, 0.8, 0.01)
nr <- length(r)

# Create range of power values
p <- seq(0.3, 0.9, 0.1)
np <- length(p)

# Obtain sample sizes
samsize <- array(numeric(nr*np), dim = c(nr, np))
for (i in 1:np){
  for (j in 1:nr){
    result <- pwr.r.test(
      n = NULL, r = r[j],
      sig.level = alph, power = p[i],
      alternative = 'two.sided'
    )
    #k_bar = sum(ki)/m

    DE = (1+rho*(k_bar-1))
    samsize[j,i] <- result$n * DE   # ceiling(result$n)
  }
}

# Set ranges and colors for graph
xrange <- range(r)
#yrange <- round(range(samsize))
yrange <- c(0, round(max(samsize)))
#colors <- rainbow(length(p))
colors <- hcl.colors(length(p), 'Roma')

# Setup graph
plot(
  xrange, yrange, type = 'n',
  xlab = 'Expected Correlation Coefficient (r)',
  ylab = 'Sample Size (n)'
)

# Add power curves
for (i in 1:np){
  lines(r, samsize[,i], type = 'l', lwd = 2, col = colors[i])
}
points(.25, 738,col = "red", lwd =3, pch = 18)

legend('topright',cex=.4, title = 'Power', as.character(p), fill = colors)

############################################
library(pwr)
## m is the number of clusters and ki is the size of the cluster 
## k_bar = sum(ki)/m
## DE = (1+rho*(k_bar-1))
#par(mfrow=c(3,2))
## Set values
rho=0.01
k_bar=220
alph=0.01
# Create range of correlations
r <- seq(0.2, 0.8, 0.01)
nr <- length(r)

# Create range of power values
p <- seq(0.90, 0.99, 0.01)
np <- length(p)

# Obtain sample sizes
samsize <- array(numeric(nr*np), dim = c(nr, np))
for (i in 1:np){
  for (j in 1:nr){
    result <- pwr.r.test(
      n = NULL, r = r[j],
      sig.level = alph, power = p[i],
      alternative = 'two.sided'
    )
    #k_bar = sum(ki)/m
    
    DE = (1+rho*(k_bar-1))
    samsize[j,i] <- result$n * DE   # ceiling(result$n)
  }
}

# Set ranges and colors for graph
xrange <- range(r)
#yrange <- round(range(samsize))
yrange <- c(0, round(max(samsize)))
#colors <- rainbow(length(p))
colors <- hcl.colors(length(p), 'Roma')

# Setup graph
plot(
  xrange, yrange, type = 'n',
  xlab = 'Expected Correlation Coefficient (r)',
  ylab = 'Sample Size (n)'
)

# Add power curves
for (i in 1:np){
  lines(r, samsize[,i], type = 'l', lwd = 2, col = colors[i])
}
points(.25, 1185,col = "red", lwd =3, pch = 18)

legend('topright',cex=.4, title = 'Power', as.character(p), fill = colors)
############################################################
## Set values for Fish
rho=0.01
k_bar=14
alph=0.01
# Create range of correlations
r <- seq(0.2, 0.8, 0.01)
nr <- length(r)

# Create range of power values
p <- seq(0.8, 0.99, 0.01)
np <- length(p)

# Obtain sample sizes
samsize <- array(numeric(nr*np), dim = c(nr, np))
for (i in 1:np){
  for (j in 1:nr){
    result <- pwr.r.test(
      n = NULL, r = r[j],
      sig.level = alph, power = p[i],
      alternative = 'two.sided'
    )
    #k_bar = sum(ki)/m
    
    DE = (1+rho*(k_bar-1))
    samsize[j,i] <- result$n * DE   # ceiling(result$n)
  }
}

# Set ranges and colors for graph
xrange <- range(r)
#yrange <- round(range(samsize))
yrange <- c(0, round(max(samsize)))
#colors <- rainbow(length(p))
colors <- hcl.colors(length(p), 'Roma')

# Setup graph
plot(
  xrange, yrange, type = 'n',
  xlab = 'Expected Correlation Coefficient (r)',
  ylab = 'Sample Size (n)'
)

# Add power curves
for (i in 1:np){
  lines(r, samsize[,i], type = 'l', lwd = 2, col = colors[i])
}
points(.3, 286.8,col = "red", lwd =3, pch = 18)
title(
  main = c(
    'Fish: Average cluster size is 14 '
  ),
  cex.main = .9
)
legend('topright',cex=.4, title = 'Power', as.character(p), fill = colors)



###################################################
## Set values for chicken
rho=0.01
k_bar=20
alph=0.05
# Create range of correlations
r <- seq(0.2, 0.8, 0.01)
nr <- length(r)

# Create range of power values
p <- seq(0.8, 0.99, 0.01)
np <- length(p)

# Obtain sample sizes
samsize <- array(numeric(nr*np), dim = c(nr, np))
for (i in 1:np){
  for (j in 1:nr){
    result <- pwr.r.test(
      n = NULL, r = r[j],
      sig.level = alph, power = p[i],
      alternative = 'two.sided'
    )
    #k_bar = sum(ki)/m
    
    DE = (1+rho*(k_bar-1))
    samsize[j,i] <- result$n * DE   # ceiling(result$n)
  }
}

# Set ranges and colors for graph
xrange <- range(r)
#yrange <- round(range(samsize))
yrange <- c(0, round(max(samsize)))
#colors <- rainbow(length(p))
colors <- hcl.colors(length(p), 'Roma')

# Setup graph
plot(
  xrange, yrange, type = 'n',
  xlab = 'Expected Correlation Coefficient (r)',
  ylab = 'Sample Size (n)'
)

# Add power curves
for (i in 1:np){
  lines(r, samsize[,i], type = 'l', lwd = 2, col = colors[i])
}
points(.3, 231.7,col = "red", lwd =3, pch = 18)
title(
  main = c(
    'Chicken: Average cluster size is 20 '
  ),
  cex.main = .9
)
legend('topright',cex=.4, title = 'Power', as.character(p), fill = colors)




###################################################
## Set values for Goat
rho=0.01
k_bar=20
alph=0.05
# Create range of correlations
r <- seq(0.2, 0.8, 0.01)
nr <- length(r)

# Create range of power values
p <- seq(0.8, 0.99, 0.01)
np <- length(p)

# Obtain sample sizes
samsize <- array(numeric(nr*np), dim = c(nr, np))
for (i in 1:np){
  for (j in 1:nr){
    result <- pwr.r.test(
      n = NULL, r = r[j],
      sig.level = alph, power = p[i],
      alternative = 'two.sided'
    )
    #k_bar = sum(ki)/m
    
    DE = (1+rho*(k_bar-1))
    samsize[j,i] <- result$n * DE   # ceiling(result$n)
  }
}

# Set ranges and colors for graph
xrange <- range(r)
#yrange <- round(range(samsize))
yrange <- c(0, round(max(samsize)))
#colors <- rainbow(length(p))
colors <- hcl.colors(length(p), 'Roma')

# Setup graph
plot(
  xrange, yrange, type = 'n',
  xlab = 'Expected Correlation Coefficient (r)',
  ylab = 'Sample Size (n)'
)

# Add power curves
for (i in 1:np){
  lines(r, samsize[,i], type = 'l', lwd = 2, col = colors[i])
}
points(.3, 231.7,col = "red", lwd =3, pch = 18)
title(
  main = c(
    'Goat: Average cluster size is 20 '
  ),
  cex.main = .9
)
legend('topright',cex=.4, title = 'Power', as.character(p), fill = colors)

###################################################
## Set values for Cattle
rho=0.01
k_bar=20
alph=0.05
# Create range of correlations
r <- seq(0.2, 0.8, 0.01)
nr <- length(r)

# Create range of power values
p <- seq(0.8, 0.99, 0.01)
np <- length(p)

# Obtain sample sizes
samsize <- array(numeric(nr*np), dim = c(nr, np))
for (i in 1:np){
  for (j in 1:nr){
    result <- pwr.r.test(
      n = NULL, r = r[j],
      sig.level = alph, power = p[i],
      alternative = 'two.sided'
    )
    #k_bar = sum(ki)/m
    
    DE = (1+rho*(k_bar-1))
    samsize[j,i] <- result$n * DE   # ceiling(result$n)
  }
}

# Set ranges and colors for graph
xrange <- range(r)
#yrange <- round(range(samsize))
yrange <- c(0, round(max(samsize)))
#colors <- rainbow(length(p))
colors <- hcl.colors(length(p), 'Roma')

# Setup graph
plot(
  xrange, yrange, type = 'n',
  xlab = 'Expected Correlation Coefficient (r)',
  ylab = 'Sample Size (n)'
)

# Add power curves
for (i in 1:np){
  lines(r, samsize[,i], type = 'l', lwd = 2, col = colors[i])
}
points(.3, 231.7,col = "red", lwd =3, pch = 18)
title(
  main = c(
    'Cattle: Average cluster size is 20 '
  ),
  cex.main = .9
)
legend('topright',cex=.4, title = 'Power', as.character(p), fill = colors)

###################################################
## Set values for Soil
rho=0.01
k_bar=9
alph=0.05
# Create range of correlations
r <- seq(0.2, 0.8, 0.01)
nr <- length(r)

# Create range of power values
p <- seq(0.8, 0.99, 0.01)
np <- length(p)

# Obtain sample sizes
samsize <- array(numeric(nr*np), dim = c(nr, np))
for (i in 1:np){
  for (j in 1:nr){
    result <- pwr.r.test(
      n = NULL, r = r[j],
      sig.level = alph, power = p[i],
      alternative = 'two.sided'
    )
    #k_bar = sum(ki)/m
    
    DE = (1+rho*(k_bar-1))
    samsize[j,i] <- result$n * DE   # ceiling(result$n)
  }
}

# Set ranges and colors for graph
xrange <- range(r)
#yrange <- round(range(samsize))
yrange <- c(0, round(max(samsize)))
#colors <- rainbow(length(p))
colors <- hcl.colors(length(p), 'Roma')

# Setup graph
plot(
  xrange, yrange, type = 'n',
  xlab = 'Expected Correlation Coefficient (r)',
  ylab = 'Sample Size (n)'
)

# Add power curves
for (i in 1:np){
  lines(r, samsize[,i], type = 'l', lwd = 2, col = colors[i])
}
points(.2, 486,col = "red", lwd =3, pch = 18)
title(
  main = c(
    'Soil: Average cluster size is 20 '
  ),
  cex.main = .9
)
legend('topright',cex=.4, title = 'Power', as.character(p), fill = colors)


###################################################
## Set values for Water
rho=0.01
k_bar=14.5
alph=0.05
# Create range of correlations
r <- seq(0.15, 0.8, 0.01)
nr <- length(r)

# Create range of power values
p <- seq(0.8, 0.99, 0.01)
np <- length(p)

# Obtain sample sizes
samsize <- array(numeric(nr*np), dim = c(nr, np))
for (i in 1:np){
  for (j in 1:nr){
    result <- pwr.r.test(
      n = NULL, r = r[j],
      sig.level = alph, power = p[i],
      alternative = 'two.sided'
    )
    #k_bar = sum(ki)/m
    
    DE = (1+rho*(k_bar-1))
    samsize[j,i] <- result$n * DE   # ceiling(result$n)
  }
}

# Set ranges and colors for graph
xrange <- range(r)
#yrange <- round(range(samsize))
yrange <- c(0, round(max(samsize)))
#colors <- rainbow(length(p))
colors <- hcl.colors(length(p), 'Roma')

# Setup graph
plot(
  xrange, yrange, type = 'n',
  xlab = 'Expected Correlation Coefficient (r)',
  ylab = 'Sample Size (n)'
)

# Add power curves
for (i in 1:np){
  lines(r, samsize[,i], type = 'l', lwd = 2, col = colors[i])
}
points(.18, 633.1,col = "red", lwd =3, pch = 18)
title(
  main = c(
    'Water: Average cluster size is 14.5 '
  ),
  cex.main = .9
)
legend('topright',cex=.4, title = 'Power', as.character(p), fill = colors)


###################################################
## Set values for Lechate
rho=0.01
k_bar=10
alph=0.05
# Create range of correlations
r <- seq(0.2, 0.8, 0.01)
nr <- length(r)

# Create range of power values
p <- seq(0.8, 0.99, 0.01)
np <- length(p)

# Obtain sample sizes
samsize <- array(numeric(nr*np), dim = c(nr, np))
for (i in 1:np){
  for (j in 1:nr){
    result <- pwr.r.test(
      n = NULL, r = r[j],
      sig.level = alph, power = p[i],
      alternative = 'two.sided'
    )
    #k_bar = sum(ki)/m
    
    DE = (1+rho*(k_bar-1))
    samsize[j,i] <- result$n * DE   # ceiling(result$n)
  }
}

# Set ranges and colors for graph
xrange <- range(r)
#yrange <- round(range(samsize))
yrange <- c(0, round(max(samsize)))
#colors <- rainbow(length(p))
colors <- hcl.colors(length(p), 'Roma')

# Setup graph
plot(
  xrange, yrange, type = 'n',
  xlab = 'Expected Correlation Coefficient (r)',
  ylab = 'Sample Size (n)'
)

# Add power curves
for (i in 1:np){
  lines(r, samsize[,i], type = 'l', lwd = 2, col = colors[i])
}
points(.4, 114.8,col = "red", lwd =3, pch = 18)
title(
  main = c(
    'Lechate: Average cluster size is 10 '
  ),
  cex.main = .9
)
legend('topright',cex=.4, title = 'Power', as.character(p), fill = colors)

###################################################
## Set values for Litter
rho=0.01
k_bar=10
alph=0.05
# Create range of correlations
r <- seq(0.2, 0.8, 0.01)
nr <- length(r)

# Create range of power values
p <- seq(0.8, 0.99, 0.01)
np <- length(p)

# Obtain sample sizes
samsize <- array(numeric(nr*np), dim = c(nr, np))
for (i in 1:np){
  for (j in 1:nr){
    result <- pwr.r.test(
      n = NULL, r = r[j],
      sig.level = alph, power = p[i],
      alternative = 'two.sided'
    )
    #k_bar = sum(ki)/m
    
    DE = (1+rho*(k_bar-1))
    samsize[j,i] <- result$n * DE   # ceiling(result$n)
  }
}

# Set ranges and colors for graph
xrange <- range(r)
#yrange <- round(range(samsize))
yrange <- c(0, round(max(samsize)))
#colors <- rainbow(length(p))
colors <- hcl.colors(length(p), 'Roma')

# Setup graph
plot(
  xrange, yrange, type = 'n',
  xlab = 'Expected Correlation Coefficient (r)',
  ylab = 'Sample Size (n)'
)

# Add power curves
for (i in 1:np){
  lines(r, samsize[,i], type = 'l', lwd = 2, col = colors[i])
}
points(.4, 114.8,col = "red", lwd =3, pch = 18)
title(
  main = c(
    'Litter: Average cluster size is 10 '
  ),
  cex.main = .9
)
legend('topright',cex=.4, title = 'Power', as.character(p), fill = colors)


