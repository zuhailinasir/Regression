estJS4=function(u1,u2,u3,u4,v){
  
  
  n <- length(u1)
  
  cv4 <- cos(v)
  sv4 <- sin(v)
  
  cos.u1.cos.u2.cos.u3.cos.u4 <- cos(u1)*cos(u2)*cos(u3)*cos(u4)
  cos.u1.cos.u2.cos.u3.sin.u4 <- cos(u1)*cos(u2)*cos(u3)*sin(u4)
  cos.u1.cos.u2.sin.u3.cos.u4 <- cos(u1)*cos(u2)*sin(u3)*cos(u4)
  cos.u1.sin.u2.cos.u3.cos.u4 <- cos(u1)*sin(u2)*cos(u3)*cos(u4)
  cos.u1.sin.u2.sin.u3.cos.u4 <- cos(u1)*sin(u2)*sin(u3)*cos(u4)
  cos.u1.sin.u2.sin.u3.sin.u4 <- cos(u1)*sin(u2)*sin(u3)*sin(u4)
  cos.u1.sin.u2.cos.u3.sin.u4 <- cos(u1)*sin(u2)*cos(u3)*sin(u4)
  cos.u1.cos.u2.sin.u3.sin.u4 <- cos(u1)*cos(u2)*sin(u3)*sin(u4)
  sin.u1.sin.u2.sin.u3.sin.u4 <- sin(u1)*sin(u2)*sin(u3)*sin(u4)
  sin.u1.sin.u2.sin.u3.cos.u4 <- sin(u1)*sin(u2)*sin(u3)*cos(u4)
  sin.u1.sin.u2.cos.u3.sin.u4 <- sin(u1)*sin(u2)*cos(u3)*sin(u4)
  sin.u1.cos.u2.sin.u3.sin.u4 <- sin(u1)*cos(u2)*sin(u3)*sin(u4)
  sin.u1.cos.u2.cos.u3.sin.u4 <- sin(u1)*cos(u2)*cos(u3)*sin(u4)
  sin.u1.cos.u2.cos.u3.cos.u4 <- sin(u1)*cos(u2)*cos(u3)*cos(u4)
  sin.u1.cos.u2.sin.u3.cos.u4 <- sin(u1)*cos(u2)*sin(u3)*cos(u4)
  sin.u1.sin.u2.cos.u3.cos.u4 <- sin(u1)*sin(u2)*cos(u3)*cos(u4)
  
  
  ones <- matrix(1., n, 1.)
  
  U4 <- cbind(ones, cos.u1.cos.u2.cos.u3.cos.u4, cos.u1.cos.u2.cos.u3.sin.u4, cos.u1.cos.u2.sin.u3.cos.u4, cos.u1.sin.u2.cos.u3.cos.u4, cos.u1.sin.u2.sin.u3.cos.u4,
              cos.u1.sin.u2.sin.u3.sin.u4, cos.u1.sin.u2.cos.u3.sin.u4, cos.u1.cos.u2.sin.u3.sin.u4, sin.u1.sin.u2.sin.u3.sin.u4, sin.u1.sin.u2.sin.u3.cos.u4, 
              sin.u1.sin.u2.cos.u3.sin.u4, sin.u1.cos.u2.sin.u3.sin.u4, sin.u1.cos.u2.cos.u3.sin.u4, sin.u1.cos.u2.cos.u3.cos.u4, sin.u1.cos.u2.sin.u3.cos.u4, 
              sin.u1.sin.u2.cos.u3.cos.u4)
  
  
  cos4.lm <- lm(cv4 ~ cos.u1.cos.u2.cos.u3.cos.u4 + cos.u1.cos.u2.cos.u3.sin.u4 + cos.u1.cos.u2.sin.u3.cos.u4 + cos.u1.sin.u2.cos.u3.cos.u4 + cos.u1.sin.u2.sin.u3.cos.u4 +
                cos.u1.sin.u2.sin.u3.sin.u4 + cos.u1.sin.u2.cos.u3.sin.u4 + cos.u1.cos.u2.sin.u3.sin.u4 + sin.u1.sin.u2.sin.u3.sin.u4 + sin.u1.sin.u2.sin.u3.cos.u4 + 
                sin.u1.sin.u2.cos.u3.sin.u4 + sin.u1.cos.u2.sin.u3.sin.u4 + sin.u1.cos.u2.cos.u3.sin.u4 + sin.u1.cos.u2.cos.u3.cos.u4 + sin.u1.cos.u2.sin.u3.cos.u4 + 
                sin.u1.sin.u2.cos.u3.cos.u4 )
  sin4.lm <- lm(sv4 ~ cos.u1.cos.u2.cos.u3.cos.u4 + cos.u1.cos.u2.cos.u3.sin.u4 + cos.u1.cos.u2.sin.u3.cos.u4 + cos.u1.sin.u2.cos.u3.cos.u4 + cos.u1.sin.u2.sin.u3.cos.u4 +
                  cos.u1.sin.u2.sin.u3.sin.u4 + cos.u1.sin.u2.cos.u3.sin.u4 + cos.u1.cos.u2.sin.u3.sin.u4 + sin.u1.sin.u2.sin.u3.sin.u4 + sin.u1.sin.u2.sin.u3.cos.u4 + 
                  sin.u1.sin.u2.cos.u3.sin.u4 + sin.u1.cos.u2.sin.u3.sin.u4 + sin.u1.cos.u2.cos.u3.sin.u4 + sin.u1.cos.u2.cos.u3.cos.u4 + sin.u1.cos.u2.sin.u3.cos.u4 + 
                  sin.u1.sin.u2.cos.u3.cos.u4 )
  
  #print("cos4.lm")
  #print(cos4.lm)
  
  #print("sin4.lm")
  #print(sin4.lm)
  
  
  cos4.fit <- cos4.lm$fitted		# estimate parameters
  sin4.fit <- sin4.lm$fitted
  
  cos4.coef <- cos4.lm$coef
  sin4.coef <- sin4.lm$coef
  
  L4=cbind(cos4.coef,sin4.coef)
  
  cos4sum=summary(cos4.lm)	
  cos4sum.v=as.vector(cos4sum)
  cos4.R2=cos4sum.v$r.squared
  
  sin4sum=summary(sin4.lm)	
  sin4sum.v=as.vector(sin4sum)
  sin4.R2=sin4sum.v$r.squared
  
  g1.sq4 <- t(cos4.fit) %*% cos4.fit
  g2.sq4 <- t(sin4.fit) %*% sin4.fit
  
  rho4 <- sqrt((g1.sq4 + g2.sq4)/n)		# estimate the concentration parameter
  
  
  #list(L3=L3,cos3.R2=cos3.R2,sin3.R2=sin3.R2,U3=U3,cv3=cv3,sv3=sv3,rho3=rho3)
  
  list(L4=L4,cos4.R2=cos4.R2,sin4.R2=sin4.R2,U4=U4,cv4=cv4,sv4=sv4,rho4=rho4)
  
}	

##############################################################


estJS3=function(u1,u2,u3,v){
  
  
  n <- length(u1)
  
  cv3 <- cos(v)
  sv3 <- sin(v)
  
  cos.u1.cos.u2.cos.u3 <- cos(u1)*cos(u2)*cos(u3)
  cos.u1.cos.u2.sin.u3 <- cos(u1)*cos(u2)*sin(u3)
  cos.u1.sin.u2.cos.u3 <- cos(u1)*sin(u2)*cos(u3)
  cos.u1.sin.u2.sin.u3 <- cos(u1)*sin(u2)*sin(u3)
  sin.u1.cos.u2.sin.u3 <- sin(u1)*cos(u2)*sin(u3)
  sin.u1.cos.u2.cos.u3 <- sin(u1)*cos(u2)*cos(u3)
  sin.u1.sin.u2.cos.u3 <- sin(u1)*sin(u2)*cos(u3)
  sin.u1.sin.u2.sin.u3 <- sin(u1)*sin(u2)*sin(u3)
  
  ones <- matrix(1., n, 1.)
  
  U3 <- cbind(ones, cos.u1.cos.u2.cos.u3, cos.u1.cos.u2.sin.u3, cos.u1.sin.u2.cos.u3, cos.u1.sin.u2.sin.u3,
              sin.u1.cos.u2.sin.u3, sin.u1.cos.u2.cos.u3, sin.u1.sin.u2.cos.u3, sin.u1.sin.u2.sin.u3 )
  
  cos3.lm <- lm(cv3 ~ cos.u1.cos.u2.cos.u3 + cos.u1.cos.u2.sin.u3 + cos.u1.sin.u2.cos.u3 + cos.u1.sin.u2.sin.u3 
                + sin.u1.cos.u2.sin.u3 + sin.u1.cos.u2.cos.u3 + sin.u1.sin.u2.cos.u3 + sin.u1.sin.u2.sin.u3 )
  sin3.lm <- lm(sv3 ~ cos.u1.cos.u2.cos.u3 + cos.u1.cos.u2.sin.u3 + cos.u1.sin.u2.cos.u3 + cos.u1.sin.u2.sin.u3 
                + sin.u1.cos.u2.sin.u3 + sin.u1.cos.u2.cos.u3 + sin.u1.sin.u2.cos.u3 + sin.u1.sin.u2.sin.u3 )
  
  #print("cos3.lm")
  #print(cos3.lm)
  
  #print("sin3.lm")
  #print(sin3.lm)
  
  
  cos3.fit <- cos3.lm$fitted		# estimate parameters
  sin3.fit <- sin3.lm$fitted
  
  cos3.coef <- cos3.lm$coef
  sin3.coef <- sin3.lm$coef
  
  L3=cbind(cos3.coef,sin3.coef)
  
  cos3sum=summary(cos3.lm)	
  cos3sum.v=as.vector(cos3sum)
  cos3.R2=cos3sum.v$r.squared
  
  sin3sum=summary(sin3.lm)	
  sin3sum.v=as.vector(sin3sum)
  sin3.R2=sin3sum.v$r.squared
  
  g1.sq3 <- t(cos3.fit) %*% cos3.fit
  g2.sq3 <- t(sin3.fit) %*% sin3.fit
  
  rho3 <- sqrt((g1.sq3 + g2.sq3)/n)		# estimate the concentration parameter
  
  
  #list(L3=L3,cos3.R2=cos3.R2,sin3.R2=sin3.R2,U3=U3,cv3=cv3,sv3=sv3,rho3=rho3)
  
  list(L3=L3,cos3.R2=cos3.R2,sin3.R2=sin3.R2,U3=U3,cv3=cv3,sv3=sv3,rho3=rho3)
  
}	

##############################################################
#estJS2(u1,u2,v)

estJS2=function(u1,u2,v){
  
  n <- length(u1)
  
  cv2 <- cos(v)
  sv2 <- sin(v)
  
  #  print(cv2)
  
  cos.u1 <- cos(u1)
  sin.u1 <- sin(u1)
  cos.u2 <- cos(u2)
  sin.u2 <- sin(u2)
  
  # print(cos.u1)
  
  cos.u1.cos.u2 <- cos(u1)*cos(u2)
  cos.u1.sin.u2 <- cos(u1)*sin(u2)
  sin.u1.cos.u2 <- sin(u1)*cos(u2)
  sin.u1.sin.u2 <- sin(u1)*sin(u2)
  
  aaa=cbind(cos.u1.cos.u2,)
  print(aaa)
  
  cos2.lm <- lm(cv2 ~ cos.u1.cos.u2 + cos.u1.sin.u2 + sin.u1.cos.u2 + sin.u1.sin.u2)
  sin2.lm <- lm(sv2 ~ cos.u1.cos.u2 + cos.u1.sin.u2 + sin.u1.cos.u2 + sin.u1.sin.u2)
  
  cos2.fit <- cos2.lm$fitted
  sin2.fit <- sin2.lm$fitted
  
  cos2.coef <- cos2.lm$coef
  sin2.coef <- sin2.lm$coef
  
  L2=cbind(cos2.coef,sin2.coef)
  
  cos2sum=summary(cos2.lm)	
  cos2sum.v=as.vector(cos2sum)
  cos2.R2=cos2sum.v$r.squared
  
  sin2sum=summary(sin2.lm)	
  sin2sum.v=as.vector(sin2sum)
  sin2.R2=sin2sum.v$r.squared
  
  
  list(L2=L2,cos2.R2=cos2.R2,sin2.R2=sin2.R2)
  
}

############################################################


estJS1=function(u1,v){
  
  L1=circ.reg(u1,v)$coef
  
  list(L1=L1)
  
}