#########################################
## Image of the target function
############################################
a1=c( 3, 3)
a2=c(3, -3)

x1p=((-30:30)/10);
x2p=((-30:31)/10);

x1=x1p%x%array(1,dim=62)
x2=array(1,dim=61)%x%x2p
imageInput =data.frame(x1,x2)
z1=a1[1]*x1+a1[2]*x2
z2=a2[1]*x1+a2[2]*x2
imageF1true = 1/(1+exp(-z1)) +  1/(1+exp(-z2))
imageF2true = 1/(1+exp(-z1)) + (z2)^2

#Ex1
image(x2p,x1p,array(imageF1true,dim=c(62,61)),zlim = c(-0.3, 2.3), col = rainbow(128))

#Ex2
image(x2p,x1p,array(imageF2true,dim=c(62,61)),zlim = c(-0.3,325), col = rainbow(128))

##########################################
# Generate test data
##########################################
nx=1000
x1 = rnorm(nx,0,1)
x2 = rnorm(nx,0,1)
Z = rnorm(nx,0,1)

par(mfrow=c(1,1))
plot(x2,x1,xlim=c(-3 ,3),ylim=c(-3,3),main="Test data, Input values ")


z1=a1[1]*x1+a1[2]*x2
z2=a2[1]*x1+a2[2]*x2
y1test = 1/(1+exp(-z1)) +  1/(1+exp(-z2))+0.3*Z
y2test = 1/(1+exp(-z1)) + z2^2+0.3*Z

testDataInput=data.frame(x1,x2)

############################################
#   seed to use training  NN & PP
#############################################


nTrain=100
nRep=100

seed1=array(rnorm(nTrain*nRep,0,1),dim=c(nTrain,nRep))
seed2=array(rnorm(nTrain*nRep,0,1),dim=c(nTrain,nRep))
seed3=array(rnorm(nTrain*nRep,0,1),dim=c(nTrain,nRep))

##########################################
# PP Example 1
##########################################
 msePP1=array(dim=c(nRep,10))

for(j in 1:nRep){
  x1 = seed1[,j]
  x2 = seed2[,j]
  Z  = seed3[,j]
  
  z1=a1[1]*x1+a1[2]*x2
  z2=a2[1]*x1+a2[2]*x2
  y1 = 1/(1+exp(-z1)) +  1/(1+exp(-z2))+0.3*Z
  #y2 = 1/(1+exp(-z1)) + z2^2+0.3*Z
  trainData1=data.frame(y1,x1,x2)
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ## Train and test data example 1
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  for( i in 1:10)
  {  
    pprModel1 = ppr(y1~x1+x2,data=trainData1,nterms=i)
    f1=predict(pprModel1,testDataInput)
    msePP1[j,i]=mean((y1test-f1)^2)
  }
}

boxplot((msePP1))


############################################
## PP Image of the estimated function example 1 
############################################################
  x1 = seed1[,1]
  x2 = seed2[,1]
  Z  = seed3[,1]
  
  z1=a1[1]*x1+a1[2]*x2
  z2=a2[1]*x1+a2[2]*x2
  y1 = 1/(1+exp(-z1)) +  1/(1+exp(-z2))+0.3*Z
  trainData1=data.frame(y1,x1,x2)



pprModel1 = ppr(y1~x1+x2,data=trainData1,nterms=1)
f1Image = predict(pprModel1,imageInput)
image(x2p,x1p,array(f1Image,dim=c(62,61)),zlim = c(-0.3, 2.3), col = rainbow(128))

pprModel1 = ppr(y1~x1+x2,data=trainData1,nterms=2)
f1Image = predict(pprModel1,imageInput)
image(x2p,x1p,array(f1Image,dim=c(62,61)),zlim = c(-0.3, 2.3), col = rainbow(128))


pprModel1 = ppr(y1~x1+x2,data=trainData1,nterms=3)
f1Image = predict(pprModel1,imageInput)
image(x2p,x1p,array(f1Image,dim=c(62,61)),zlim = c(-0.3, 2.3), col = rainbow(128))


pprModel1 = ppr(y1~x1+x2,data=trainData1,nterms=4)
f1Image = predict(pprModel1,imageInput)
image(x2p,x1p,array(f1Image,dim=c(62,61)),zlim = c(-0.3, 2.3), col = rainbow(128))

summary(pprModel1 )
par(mfrow=c(2,2))
plot(pprModel1)

## difficult case for the greedy algorithm of ppr. two equally strong terms compete 
## about the first component it ends up in the middle
## see "Projection direction vectors"  




##########################################
# PP Example 2
##########################################
 msePP2=array(dim=c(nRep,10))

for(j in 1:nRep){
  x1 = seed1[,j]
  x2 = seed2[,j]
  Z  = seed3[,j]
  
  z1=a1[1]*x1+a1[2]*x2
  z2=a2[1]*x1+a2[2]*x2
  y2 = 1/(1+exp(-z1)) + z2^2+0.3*Z
  trainData2=data.frame(y2,x1,x2)
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ## Train and test data example 1
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  for( i in 1:10)
  {  
    pprModel2 = ppr(y2~x1+x2,data=trainData2,nterms=i)
    f2=predict(pprModel2,testDataInput)
    msePP2[j,i]=mean((y2test-f2)^2)
  }
}
par(mfrow=c(1,1))

boxplot((msePP2))

boxplot((msePP2),ylim=c(0, 100))



############################################
## Image of the estimated function example 2
######################################################

 x1 = seed1[,1]
  x2 = seed2[,1]
  Z  = seed3[,1]
  
  z1=a1[1]*x1+a1[2]*x2
  z2=a2[1]*x1+a2[2]*x2
  y2 = 1/(1+exp(-z1)) + z2^2+0.3*Z
  trainData2=data.frame(y2,x1,x2)


pprModel2 = ppr(y2~x1+x2,data=trainData2,nterms=1)
f2Image = predict(pprModel2,imageInput)
image(x2p,x1p,array(f2Image,dim=c(62,61)),zlim =  c(-0.3,325), col = rainbow(128))


pprModel2 = ppr(y2~x1+x2,data=trainData2,nterms=2)
f2Image = predict(pprModel2,imageInput)
image(x2p,x1p,array(f2Image,dim=c(62,61)),zlim = c(-0.3,325), col = rainbow(128))


pprModel2 = ppr(y2~x1+x2,data=trainData2,nterms=3)
f2Image = predict(pprModel2,imageInput)
image(x2p,x1p,array(f2Image,dim=c(62,61)),zlim = c(-0.3,325), col = rainbow(128))

names(pprModel2 )

##  directions are much well defined
summary(pprModel2 )
par(mfrow=c(3,1))
plot(pprModel2)


##  Reasonable fit inside training data range (big deviations outside) 
f2test = predict(pprModel2,testDataInput)
plot(f2test,y2test)

points(pprModel2$fitted.values,y2,col='#FF0000')
lines(f2,f2)




##########################################
# NN Example 1
##########################################
 mseNN1=array(dim=c(nRep,10))
th=0.1 # threshold
sm=1e5 # stepmax

for(j in 1:nRep){
  x1 = seed1[,j]
  x2 = seed2[,j]
  Z  = seed3[,j]
  
  z1=a1[1]*x1+a1[2]*x2
  z2=a2[1]*x1+a2[2]*x2
  y1 = 1/(1+exp(-z1)) +  1/(1+exp(-z2))+0.3*Z
  #y2 = 1/(1+exp(-z1)) + z2^2+0.3*Z
  trainData1=data.frame(y1,x1,x2)
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ## Train and test data example 1
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  for( i in 1:10)
  {  
    nnModel1 = neuralnet(y1~x1+x2,data=trainData1,hidden=c(i),rep=10,stepmax=sm,learningrate=lr,threshold=th)
    f1=compute(nnModel1,testDataInput,rep=1)
    mseNN1[j,i]=mean((y1test-f1$net.result)^2)
  }
}

boxplot((mseNN1))

boxplot((mseNN1),ylim=c(0, 0.3))


############################################
## Image of the estimated function example 1 
############################################################
  x1 = seed1[,1]
  x2 = seed2[,1]
  Z  = seed3[,1]
  
  z1=a1[1]*x1+a1[2]*x2
  z2=a2[1]*x1+a2[2]*x2
  y1 = 1/(1+exp(-z1)) +  1/(1+exp(-z2))+0.3*Z
  trainData1=data.frame(y1,x1,x2)

par(mfrow = c(2, 2))
  
image(x2p,x1p,array(imageF1true,dim=c(62,61)),zlim = c(-0.3, 2.3), col = rainbow(128))


nnModel1 = neuralnet(y1~x1+x2,data=trainData,hidden=c(1),rep=10,stepmax=sm,learningrate=lr,threshold=th)
    f1Image=compute(nnModel1,imageInput,rep=1)
image(x2p,x1p,array(f1Image$net.result,dim=c(62,61)),zlim = c(-0.3, 2.3), col = rainbow(128))


nnModel1 = neuralnet(y1~x1+x2,data=trainData1,hidden=c(2),rep=1,stepmax=sm,learningrate=lr,threshold=th)
    f1Image=compute(nnModel1,imageInput,rep=1)
image(x2p,x1p,array(f1Image$net.result,dim=c(62,61)),zlim = c(-0.3, 2.3), col = rainbow(128))

nnModel1 = neuralnet(y1~x1+x2,data=trainData1,hidden=c(3),rep=10,stepmax=sm,learningrate=lr,threshold=th)
    f1Image=compute(nnModel1,imageInput,rep=1)
image(x2p,x1p,array(f1Image$net.result,dim=c(62,61)),zlim = c(-0.3, 2.3), col = rainbow(128))





############################################
## NN Image of the estimated function example 2 
############################################################
  x1 = seed1[,1]
  x2 = seed2[,1]
  Z  = seed3[,1]
  
  z1=a1[1]*x1+a1[2]*x2
  z2=a2[1]*x1+a2[2]*x2
  y2 = 1/(1+exp(-z1)) + z2^2+0.3*Z
  trainData2=data.frame(y2,x1,x2)

lr=0.1
th=0.3
sm=1e5


nnModel2 = neuralnet(y2~x1+x2,data=trainData2,hidden=c(2),rep=10,stepmax=sm,learningrate=lr,threshold=th)
    f2Image=compute(nnModel2,imageInput,rep=1)
image(x2p,x1p,array(f2Image$net.result,dim=c(62,61)),zlim = c(-0.3, 325), col = rainbow(128))

 nnModel2$weights

nnModel2 = neuralnet(y2~x1+x2,data=trainData2,hidden=c(4),rep=10,stepmax=sm,learningrate=lr,threshold=th)
    f2Image=compute(nnModel2,imageInput,rep=1)
image(x2p,x1p,array(f2Image$net.result,dim=c(62,61)),zlim = c(-0.3, 325), col = rainbow(128))

nnModel2 = neuralnet(y2~x1+x2,data=trainData2,hidden=c(6),rep=10,stepmax=sm,learningrate=lr,threshold=th)
    f2Image=compute(nnModel2,imageInput,rep=1)
image(x2p,x1p,array(f2Image$net.result,dim=c(62,61)),zlim = c(-0.3, 325), col = rainbow(128))

nnModel2$weights 


##########################################
# NN Example 2
##########################################
 

lr=0.1
th=0.3 ## increased  to define convergence within a reasonable stepmax
sm=1e5


mseNN2=array(dim=c(nRep,10))
for(j in 1:nRep){
  x1 = seed1[,j]
  x2 = seed2[,j]
  Z  = seed3[,j]
  
  z1=a1[1]*x1+a1[2]*x2
  z2=a2[1]*x1+a2[2]*x2
  y2 = 1/(1+exp(-z1)) + z2^2+0.3*Z
  trainData2=data.frame(y2,x1,x2)
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ## Train and test data example 1
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  for( i in 1:5)
  {  
    nnModel2 = neuralnet(y2~x1+x2,data=trainData2,hidden=c(i),rep=10,stepmax=sm,learningrate=lr,threshold=th)
    f2=compute(nnModel2,testDataInput,rep=1)
    mseNN2[j,i]=mean((y2test-f2$net.result)^2)
  }
}
par(mfrow=c(1,1))

boxplot((mseNN2))

boxplot((mseNN2),ylim=c(0, 100))








