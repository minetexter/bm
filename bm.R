BM = function(N, var) {
  #Simple Branching Process Simualtion in Random Walk Zhijun Yang 4
  cumsum(rnorm(N, 0 ,var));}

total = 200 # WE TAKE THE VARIACNE OF WEINER PROCESS TO BE LARGE IN STEM
xdis = BM(total,5);
ydis = BM(total,5);
zdis = BM(total,5);
N = 20 #WE TAKE THE STEM TO HAVE 20 BRANCHES AND VARIANCE TO BE LESS
fxdis1 = rep(0,N*total);
fydis1 = rep(0,N*total);
fzdis1 = rep(0,N*total);
for(i in 1:(N-1)){
  xreplace = BM(total,2);
  yreplace = BM(total,2);
  zreplace = BM(total,2);
  fxdis1[((i-1)*total+1):(i*total)] = (xreplace + xdis[(i-1)*(total/N)+1]);
  fydis1[((i-1)*total+1):(i*total)] = (yreplace + ydis[(i-1)*(total/N)+1]);
  fzdis1[((i-1)*total+1):(i*total)] = (zreplace + zdis[(i-1)*(total/N)+1]);
}
fxdis1 = c(xdis,fxdis1);
fydis1 = c(ydis,fydis1);
fzdis1 = c(xdis,fzdis1);
library(scatterplot3d)

scatterplot3d(fxdis1,fydis1,fzdis1,type="l")
              ,main="3-Dim Simulation Pseudo-Fractal",xlab="x-displacement",ylab="ydisplacement",zlab="z-displacement")