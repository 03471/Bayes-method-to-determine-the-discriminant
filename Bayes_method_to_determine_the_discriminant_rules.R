encoding = "utf-8"

#disable exponential notation
options(scipen=999)

"Suppose that x in {1, 2, 3, 4, 5, 6, 7, 8, 9, 10} and
Pi1 : X ~ Bi(10, 0.2) with the prior probability Pi1 = 0.5,
Pi2 : X ~ Bi(10, 0.3) with the prior probability Pi2 = 0.3,
Pi3 : X ~ Bi(10, 0.5) with the prior probability Pi3 = 0.2,"

"1) Are the data discrete or continuous?"
#discrete

"2) What is the sample space?"
#natural numbers
"
Rj = {x E {0,1,2,3,4,5,6,7,8,9,10}}
"

"3) Use the Bayes method to determine the discriminant rules R1; R2, and R3."
F1 <- c()
F2 <- c()
F3 <- c()

pi1 <- 0.5
pi2 <- 0.3
pi3 <- 0.2

pi1f1 <- c()
pi2f2 <- c()
pi3f3 <- c()

pijfj <- c()
x <- c()
j <- c()

for(i in 0:10) {
  x <- c(x,i)
  
  F1 <- c(F1,dbinom(i,10,.2))
  F2 <- c(F2,dbinom(i,10,.3))
  F3 <- c(F3,dbinom(i,10,.5))
  
  
  pi1f1 <- c(pi1f1, pi1 * dbinom(i,10,.2))
  pi2f2 <- c(pi2f2, pi2 * dbinom(i,10,.3))
  pi3f3 <- c(pi3f3, pi3 * dbinom(i,10,.5))
  
  m <- max(pi1 * dbinom(i,10,.2),pi2 * dbinom(i,10,.3),pi3 * dbinom(i,10,.5))
  pijfj <- c(pijfj, m)
  
  if(m == pi1 * dbinom(i,10,.2)){
    j <- c(j,1)  
  }
  else if(m == pi2 * dbinom(i,10,.3)){
    j <- c(j,2)
  }
  else if(m == pi3 * dbinom(i,10,.5)){
    j <- c(j,3)
  }
}

output <-  cbind(x,F1,F2,F3,pi1f1,pi2f2,pi3f3,pijfj,j)

output <- as.data.frame(output)
names(output) <- c('x','f1(x)','f2(x)','f3(x)','p1*f1(x)','p2*f2(x)','p3*f3(x)','pijfj','j')

R1 <- output$x[output$j ==1]
R2 <- output$x[output$j ==2]
R3 <- output$x[output$j ==3]

R1
R2
R3

"
R1= {0,1,2,3}
R2= {4}
R3= {5,6,7,8,9,10}
"

P3.F1<-pi1f1
P3.F2<-pi2f2
P3.F3<-pi3f3

"4) Construct the confusion matrix for this set of discriminant rules. It will be a
3 x 3 matrix with entries pij = P (X 2 RijX 2 Πj). The pij’s should add up to 1.
What is the probability of correct classification? What is the probability of incorrect
classification?"

par(mfrow=c(1,1))

plot(P3.F1,type='l',lwd=1,col='blue',xlab='R-space',ylab='Densities',cex.lab=1,cex.axis=1,xlim=range(0:10))
lines(P3.F2,type='l',lwd=1,col='red')
lines(P3.F3,type='l',lwd=1,col='orange')
axis(side=1, at=0:11, cex.axis=1)
title(main='problem-3')
grid()

"
R1= {0,1,2,3}
R2= {4}
R3= {5,6,7,8,9,10}
"

#a What is the probability of correct classification?
P12 <- sum(dbinom(0:3,10,0.3))
P13 <- sum(dbinom(0:3,10,0.5))
P21 <- dbinom(4,10,0.2)
P23 <- dbinom(4,10,0.5)
P31 <- sum(dbinom(5:10,10,0.2))
P32 <- sum(dbinom(5:10,10,0.3))

P12 + P13 
P21 + P23 
P31 + P32

#b What is the probability of incorrect classification?

P11 <- sum(dbinom(0:3,10,0.2))
P22 <- dbinom(4,10,0.3)
P33 <- sum(dbinom(5:10,10,0.5))

P11 + P22 + P33

"5) Find R1; R2, and R3 if the prior probabilities are all the same: π1 = π2 = π3 = 1/3."
F1 <- c()
F2 <- c()
F3 <- c()

pi1 <- 1/3
pi2 <- 1/3
pi3 <- 1/3

pi1f1 <- c()
pi2f2 <- c()
pi3f3 <- c()

pijfj <- c()
x <- c()
j <- c()

for(i in 0:10) {
  x <- c(x,i)
  
  F1 <- c(F1,dbinom(i,10,.2))
  F2 <- c(F2,dbinom(i,10,.3))
  F3 <- c(F3,dbinom(i,10,.5))
  
  
  pi1f1 <- c(pi1f1, pi1 * dbinom(i,10,.2))
  pi2f2 <- c(pi2f2, pi2 * dbinom(i,10,.3))
  pi3f3 <- c(pi3f3, pi3 * dbinom(i,10,.5))
  
  m <- max(pi1 * dbinom(i,10,.2),pi2 * dbinom(i,10,.3),pi3 * dbinom(i,10,.5))
  pijfj <- c(pijfj, m)
  
  if(m == pi1 * dbinom(i,10,.2)){
    j <- c(j,1)  
  }
  else if(m == pi2 * dbinom(i,10,.3)){
    j <- c(j,2)
  }
  else if(m == pi3 * dbinom(i,10,.5)){
    j <- c(j,3)
  }
}

output <-  cbind(x,F1,F2,F3,pi1f1,pi2f2,pi3f3,pijfj,j)

output <- as.data.frame(output)
names(output) <- c('x','f1(x)','f2(x)','f3(x)','p1*f1(x)','p2*f2(x)','p3*f3(x)','pijfj','j')

R1 <- output$x[output$j ==1]
R2 <- output$x[output$j ==2]
R3 <- output$x[output$j ==3]


P5.F1<-pi1f1
P5.F2<-pi2f2
P5.F3<-pi3f3

"6) (3 pts) Find a set of prior probabilities {π1, π2, π3} that yields R2 = {2, 3, 4}."

R2.expected <- c(2,3,4)

priorProbabilities <- c()

F1 <- c()
F2 <- c()
F3 <- c()

pi1 <- 0
pi2 <- 0
pi3 <- 0

pi1f1 <- c()
pi2f2 <- c()
pi3f3 <- c()

pijfj <- c()
x <- c()
j <- c()

for(pi1 in seq(from=0.0,to=1,by=.1)){
  for(pi2 in seq(from=0.0,to=1,by=.1)){
    for(pi3 in seq(from=0.0,to=1,by=.1)){
      if(sum(pi1,pi2,pi3)==1){
      for(i in 0:10) 
        {
        x <- c(x,i)
        
        F1 <- c(F1,dbinom(i,10,.2))
        F2 <- c(F2,dbinom(i,10,.3))
        F3 <- c(F3,dbinom(i,10,.5))
        
        pi1f1 <- c(pi1f1, pi1 * dbinom(i,10,.2))
        pi2f2 <- c(pi2f2, pi2 * dbinom(i,10,.3))
        pi3f3 <- c(pi3f3, pi3 * dbinom(i,10,.5))
        
        P6.F1<-pi1f1
        P6.F2<-pi2f2
        P6.F3<-pi3f3
        
        m <- max(pi1 * dbinom(i,10,.2),pi2 * dbinom(i,10,.3),pi3 * dbinom(i,10,.5))
        pijfj <- c(pijfj, m)
        
        if(m == pi1 * dbinom(i,10,.2)){
          j <- c(j,1)  
        }
        else if(m == pi2 * dbinom(i,10,.3)){
          j <- c(j,2)
        }
        else if(m == pi3 * dbinom(i,10,.5)){
          j <- c(j,3)
        }
      }
      output <-  cbind(x,F1,F2,F3,pi1f1,pi2f2,pi3f3,pijfj,j)
      output <- as.data.frame(output)
      names(output) <- c('x','f1(x)','f2(x)','f3(x)','p1*f1(x)','p2*f2(x)','p3*f3(x)','pijfj','j')
      
      R1 <- output$x[output$j ==1]
      R2 <- output$x[output$j ==2]
      R3 <- output$x[output$j ==3]
      
      if(length(R2) == length(R2.expected) && identical(R2.expected, as.vector(R2)))
         {
         priorProbabilities <- rbind(priorProbabilities, c(pi1,pi2,pi3))
      }
      output <- c()
      pi1f1 <- c()
      pi2f2 <- c()
      pi3f3 <- c()
      F1 <- c()
      F2 <- c()
      F3 <- c()
      j<- c()
      x <- c()
      pijfj <- c()
    }
    } 
  } 
}

#output
priorProbabilities <- as.data.frame(priorProbabilities)
names(priorProbabilities) <- c('π1','π2','π3')

#proof

F1 <- c()
F2 <- c()
F3 <- c()

pi1 <- 0.3
pi2 <- 0.4
pi3 <- 0.3

pi1f1 <- c()
pi2f2 <- c()
pi3f3 <- c()

pijfj <- c()
x <- c()
j <- c()

for(i in 0:10) {
  x <- c(x,i)
  
  F1 <- c(F1,dbinom(i,10,.2))
  F2 <- c(F2,dbinom(i,10,.3))
  F3 <- c(F3,dbinom(i,10,.5))
  
  
  pi1f1 <- c(pi1f1, pi1 * dbinom(i,10,.2))
  pi2f2 <- c(pi2f2, pi2 * dbinom(i,10,.3))
  pi3f3 <- c(pi3f3, pi3 * dbinom(i,10,.5))
  
  m <- max(pi1 * dbinom(i,10,.2),pi2 * dbinom(i,10,.3),pi3 * dbinom(i,10,.5))
  pijfj <- c(pijfj, m)
  
  if(m == pi1 * dbinom(i,10,.2)){
    j <- c(j,1)  
  }
  else if(m == pi2 * dbinom(i,10,.3)){
    j <- c(j,2)
  }
  else if(m == pi3 * dbinom(i,10,.5)){
    j <- c(j,3)
  }
}

output <-  cbind(x,F1,F2,F3,pi1f1,pi2f2,pi3f3,pijfj,j)

output <- as.data.frame(output)
names(output) <- c('x','f1(x)','f2(x)','f3(x)','p1*f1(x)','p2*f2(x)','p3*f3(x)','pijfj','j')

R1 <- output$x[output$j ==1]
R2 <- output$x[output$j ==2]
R3 <- output$x[output$j ==3]

R1
R2
R3

P6.F1<-pi1f1
P6.F2<-pi2f2
P6.F3<-pi3f3

"7) Suppose that n1 = 10; n2 = 15; n3 = 20 (instead of n1 = n2 = n3 = 10) and
the pi’s and πi’s are as given at the beginning."

#a
#natural numbers
"
Rj = {x E {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20}}
"

#b
F1 <- c()
F2 <- c()
F3 <- c()

pi1 <- 0.5
pi2 <- 0.3
pi3 <- 0.2

pi1f1 <- c()
pi2f2 <- c()
pi3f3 <- c()

pijfj <- c()
x <- c()
j <- c()

for(i in 0:20) {
  x <- c(x,i)
  
  F1 <- c(F1,dbinom(i,10,.2))
  F2 <- c(F2,dbinom(i,15,.3))
  F3 <- c(F3,dbinom(i,20,.5))
  
  
  pi1f1 <- c(pi1f1, pi1 * dbinom(i,10,.2))
  pi2f2 <- c(pi2f2, pi2 * dbinom(i,15,.3))
  pi3f3 <- c(pi3f3, pi3 * dbinom(i,20,.5))
  
  m <- max(pi1 * dbinom(i,10,.2),pi2 * dbinom(i,15,.3),pi3 * dbinom(i,20,.5))
  pijfj <- c(pijfj, m)
  
  if(m == pi1 * dbinom(i,10,.2)){
    j <- c(j,1)  
  }
  else if(m == pi2 * dbinom(i,15,.3)){
    j <- c(j,2)
  }
  else if(m == pi3 * dbinom(i,20,.5)){
    j <- c(j,3)
  }
}

output <-  cbind(x,F1,F2,F3,pi1f1,pi2f2,pi3f3,pijfj,j)

output <- as.data.frame(output)
names(output) <- c('x','f1(x)','f2(x)','f3(x)','p1*f1(x)','p2*f2(x)','p3*f3(x)','pijfj','j')

R1 <- output$x[output$j ==1]
R2 <- output$x[output$j ==2]
R3 <- output$x[output$j ==3]

R1
R2
R3

"
R1= {0,1,2,3}
R2= {4,5,6,7}
R3= {8,9,10,11,12,13,14,15,16,17,18,19,20}
"

P7.F1<-pi1f1
P7.F2<-pi2f2
P7.F3<-pi3f3

"
8) Figure 14.1 shows shows how two normal densities partition the x-axis into R1
and R2. Use the barplot() function with beside = TRUE to make a similar plot for
part 3 above. Choose three colors you really like. It would be even better to use
par(mfrow=c(2,2)) and make plots for parts 3, 5, 6, and 7b.
"
#sample problem-3 output

par(mfrow=c(2,2))

plot(P3.F1,type='l',lwd=1,col='blue',xlab='R-space',ylab='Densities',cex.lab=1,cex.axis=1,xlim=range(0:10))
lines(P3.F2,type='l',lwd=1,col='red')
lines(P3.F3,type='l',lwd=1,col='orange')
axis(side=1, at=0:11, cex.axis=1)
title(main='problem-3')
grid()

plot(P5.F1,type='l',lwd=1,col='blue',xlab='R-space',ylab='Densities',cex.lab=1,cex.axis=1,xlim=range(0:10)) 
lines(P5.F2,type='l',lwd=1,col='red')
lines(P5.F3,type='l',lwd=1,col='orange')
axis(side=1, at=0:11, cex.axis=1)
title(main='problem-5')
grid()

plot(P6.F1,type='l',lwd=1,col='blue',xlab='R-space',ylab='Densities',cex.lab=1,cex.axis=1,ylim=c(0,.12),xlim=range(0:10))
lines(P6.F2,type='l',lwd=1,col='red')
lines(P6.F3,type='l',lwd=1,col='orange')
axis(side=1, at=0:10, cex.axis=1)
title(main='problem-6')
grid()

plot(P7.F1,type='l',lwd=1,col='blue',xlab='R-space',ylab='Densities',cex.lab=1,cex.axis=1,xlim=range(0:20))
lines(P7.F2,type='l',lwd=1,col='red')
lines(P7.F3,type='l',lwd=1,col='orange')
axis(side=1, at=0:20, cex.axis=1)
title(main='problem-7')
grid()

par(mfrow=c(2,2))
barplot(rbind(P3.F1,P3.F2,P3.F3), main='problem-3',
xlab='R-space',ylab = 'R-density',col=c('darkblue','red','orange'),
legend = rownames(counts), beside=TRUE)

barplot(rbind(P5.F1,P5.F2,P5.F3), main='problem-5',
xlab='R-space',ylab = 'R-density',col=c('darkblue','red','orange'),
legend = rownames(counts), beside=TRUE)

barplot(rbind(P6.F1,P6.F2,P6.F3), main='problem-6',
xlab='R-space',ylab = 'R-density',col=c('darkblue','red','orange'),
legend = rownames(counts), beside=TRUE)

barplot(rbind(P7.F1,P7.F2,P7.F3), main='problem-7',
xlab='R-space',ylab = 'R-density',col=c('darkblue','red','orange'),
legend = rownames(counts), beside=TRUE)
