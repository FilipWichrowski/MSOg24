### Zad 1 ###

# a)
X = matrix(0, nrow=10, ncol=2)
X[,1] = rnorm(10,0,1)
X[,2] = rnorm(10,0,1)


svd1 = svd(X)

U1 = svd1$u
D1 = diag(svd1$d)
V1 = svd1$v

# b)
all.equal(U1 %*% D1 %*% t(V1), X)

#c) 
X1 = cbind(X, 2*X[,2])
svd2 = svd(X1)
svd2

#d)
Sigma = t(X) %*% X
eigen1 = eigen(Sigma)
V = eigen1$vectors
D2 = diag(eigen1$values)

all.equal(V %*% D2 %*% t(V), Sigma)


### Zad 2 ###
M <- as.matrix(read.csv("zebra.csv"))
image(M,  asp = TRUE, col = c("white", "black"), xaxt = "n", yaxt = "n")
# Set xaxt = "n" and yaxt = "n" to remove the tick labels of the plot



# a)
svdM <- svd(M)

Um <- svdM$u
Vm <- svdM$v
Dm <- diag(svdM$d)

p <- nrow(M)


# b)
M_50 <- Um %*% diag(c(svdM$d[1:(p/2)], rep(0, p - p/2))) %*% t(Vm)
M_10 <- Um %*% diag(c(svdM$d[1:(p/10)], rep(0, p - p/10))) %*% t(Vm)
M_4 <- Um %*% diag(c(svdM$d[1:(p/25)], rep(0, p - p/25))) %*% t(Vm)
M_2 <- Um %*% diag(c(svdM$d[1:(p/50)], rep(0, p - p/50))) %*% t(Vm)

# c)
par(mfrow = c(2, 2))
image(M_50, asp = 1, col = c("white", "black"))
image(M_10, asp = 1, col = c("white", "black"))
image(M_4, asp = 1, col = c("white", "black"))
image(M_2, asp = 1, col = c("white", "black"))

# d)
plot(svdM$d)
abline(v = c(p/2, p/5, p/25, p/50))

### Zad 3 ###
M2 = as.matrix(read.csv2("lena.csv"))
image(M2, col = gray.colors(100))

# a)

svdM2 <- svd(M2)

Um2 <- svdM2$u
Vm2 <- svdM2$v
Dm2 <- diag(svdM2$d)

p <- nrow(M2)

# b)
M2_1 <- Um2 %*% diag(c(svdM2$d[1:10], rep(0, p -10))) %*% t(Vm2)
M2_2 <- Um2 %*% diag(c(svdM2$d[1:100], rep(0, p -100))) %*% t(Vm2)

# c)
par(mfrow = c(1, 3))
image(M2, col = gray.colors(100))
image(M2_2, col = gray.colors(100))
image(M2_1, col = gray.colors(100))


