#install the Baysian Network Learning library
install.packages('bnlearn')


#load library
library(bnlearn)

# a BN using expert knowledge
BN <- function(){
  set.seed(50)
  net <- model2network("[A][B][C][D|B:C][E|B:C][F|A:D][G|A:D:E][H|E]")
  plot(net)
  yn <- c("T", "F")
  cptA <- matrix(c(0.05, 0.95), ncol=2, dimnames=list(NULL, yn))
  cptB <- matrix(c(0.10, 0.90), ncol=2, dimnames=list(NULL, yn))
  cptC <- matrix(c(0.50, 0.50), ncol=2, dimnames=list(NULL, yn))
  
  
  # cptE and cptD are 3-d matrices, which don't exist in R, so
  # need to build these manually as below.
  cptD <- c(0.5, 0.5, 
            0.5, 0.5,
            0.7, 0.3,
            0.7, 0.3)
  dim(cptD) <- c(2, 2, 2)
  dimnames(cptD) <- list("D"=yn, "B"=yn, "C"=yn)
  
  cptE <- c(0.9, 0.1, 
            0.7, 0.3,
            0.5, 0.5,
            0.2, 0.8)
  dim(cptE) <- c(2, 2, 2)
  dimnames(cptE) <- list("E"=yn, "B"=yn, "C"=yn)
  
  cptF <- c(0.2, 0.8, 
            0.7, 0.3, 
            0.6, 0.4, 
            0.1, 0.9)
  dim(cptF) <- c(2, 2, 2)
  dimnames(cptF) <- list("F"=yn, "A"=yn, "D"=yn)
  
  cptG <- c(0.9, 0.1, 
            0.8, 0.2, 
            0.8, 0.2, 
            0.7, 0.3,
            0.7, 0.3, 
            0.5, 0.5,
            0.2, 0.8,
            0.1, 0.9)
  dim(cptG) <- c(2, 2, 2, 2)
  dimnames(cptG) <- list("G"=yn, "A"=yn, "D"=yn,"E"=yn)
  
  cptH <- matrix(c(0.70, 0.30, 0.40, 0.60), 
                 ncol=2, dimnames=list("H"=yn, "E"=yn))
  
  net.disc <- custom.fit(net, dist=list(A=cptA, B=cptB, C=cptC, D=cptD, 
                                        E=cptE, F=cptF, G=cptG,H=cptH))
  print(net.disc)
  
  return(net.disc)
}

q1B <- cpquery(BN(), (D=="T"), (G=="F" & F=="T"))
print(q1B)

