	gap=4
	n=gap*5
	
	x=mat.or.vec(n+1,1)
	y=mat.or.vec(n+1,1)

	z=rnorm(n,0,1)
	x[1]=0.05
	y[1]=0.05
	for(i in 1:n)
	{
		s=0.25*(0.1-x[i])*(1/gap)+0.1*sqrt(x[i])*sqrt(1/gap)*z[i]
		s2=0.25*(0.1-x[i])*(1/gap)+0.1*sqrt(x[i])*sqrt(1/gap)*z[i]+1/2*(1/2*0.1*(x[i]^(-0.5)))*0.1*sqrt(x[i])*(1/gap)*(z[i]^2-1)
		x[i+1]=x[i]+s
		y[i+1]=y[i]+s2
	}
	plot(x,type="l",col="red",ylab="Interest Rate",lwd=2)
	 lines(y,col="blue",lty=2)
	abline(h=0.07,lty=3)
	
	legend(5,0.15,legend=c("Without Refinement","First Refinement"),lty=c(1,2),col=c("red","blue"))
	title("Effect of Discretization")
	
	for(i in 1:n)
	{
		if(x[i]>0.07)
		lines(c(i,i),c(0.07,x[i]),lty=2,col="blue")
	}
	title("Simulated Path of CIR model")

# write.table(c(x,y),"simu.csv")
	
# 20 plots	
for(i in 1:n)
{	
	z=rnorm(n,0,1)
	s=0.25*(0.1-x[i])*(1/gap)+0.1*sqrt(x[i])*sqrt(1/gap)*z[i]+1/2*(1/2*0.1*(x[i]^(-0.5)))*0.1*sqrt(x[i])*(1/gap)*(z[i]^2-1)
	x[i+1]=x[i]+s
}
plot(x,type="l",ylim=c(0,0.18),col="red")
abline(h=0.07,lty=3)
		
for(k in 1:20)
{
	z=rnorm(n,0,1)
	x=mat.or.vec(n+1,1)
	x[1]=0.05
	for(i in 1:n)
	{
		s=0.25*(0.1-x[i])*(1/gap)+0.1*sqrt(x[i])*sqrt(1/gap)*z[i]+1/2*(1/2*0.1*(x[i]^(-0.5)))*0.1*sqrt(x[i])*(1/gap)*(z[i]^2-1)
		x[i+1]=x[i]+s
	}
	c=runif(3)
	col=rgb(c[1],c[2],c[3])
	lines(x,type="l",col=col)
}
	title("Sample of 20 Paths")

# price without VR
mean=mat.or.vec(200,1)
for(kk in 1:200)
{
	v=mat.or.vec(1000,1)
	for(k in 1:1000)
	{
		z=rnorm(n,0,1)
		x=mat.or.vec(n+1,1)
		pay=mat.or.vec(n+1,1)
		dis=mat.or.vec(n+1,1)
		dis[1]=1
		x[1]=0.05
		for(i in 1:n)
		{
			s=0.25*(0.1-x[i])*(1/gap)+0.1*sqrt(x[i])*sqrt(1/gap)*z[i]+1/2*(1/2*0.1*(x[i]^(-0.5)))*0.1*sqrt(x[i])*(1/gap)*(z[i]^2-1)
			x[i+1]=x[i]+s
			pay[i+1]=max(0,x[i+1]-0.07)/gap*1000000
			dis[i+1]=dis[i]*(1+x[i+1]/gap)
		}
		for(i in 1:n)
		{
			v[k]=v[k]+pay[i+1]/dis[i+1]
		}
	}
	hist(v, col="gray", breaks=c((0:16)*25000))
	mean[kk]=mean(v)
}
hist(mean)
hist(mean,breaks=c((45:57)*1000),col="red",prob=TRUE)
curve(dnorm(x, mean=mean(mean), sd=sd(mean)), add=TRUE)


# price with VR
mean=mat.or.vec(200,1)
for(kk in 1:200)
{
	v=mat.or.vec(1000,1)
	rate=mat.or.vec(1000,1)
	
	for(k in 1:1000)
	{
		z=rnorm(n,0,1)
		x=mat.or.vec(n+1,1)
		pay=mat.or.vec(n+1,1)
		dis=mat.or.vec(n+1,1)
		dis[1]=1
		x[1]=0.05
		for(i in 1:n)
		{
			s=0.25*(0.1-x[i])*(1/gap)+0.1*sqrt(x[i])*sqrt(1/gap)*z[i]+1/2*(1/2*0.1*(x[i]^(-0.5)))*0.1*sqrt(x[i])*(1/gap)*(z[i]^2-1)
			x[i+1]=x[i]+s
			pay[i+1]=max(0,x[i+1]-0.07)/gap*1000000
			dis[i+1]=dis[i]*(1+x[i+1]/gap)
		}
		for(i in 1:n)
		{
			v[k]=v[k]+pay[i+1]/dis[i+1]
		}
		rate[k]=x[n+1]
	}
	# hist(v)
	m1=lm(v~rate)
	b=m1$coef[2]
	mean[kk]=mean(v)-b*(mean(rate)-0.085675)
}

t=(45:57)*1000
hist(mean,xlim=t,col="red",prob=TRUE,ylim=c(0,0.00025))
curve(dnorm(x, mean=mean(mean), sd=sd(mean)), add=TRUE)


# price with VR2
mean=mat.or.vec(200,1)
for(kk in 1:200)
{
	v=mat.or.vec(1000,1)
	rate=mat.or.vec(1000,1)
	
	for(k in 1:500)
	{
		z=rnorm(n,0,1)
		x=mat.or.vec(n+1,1)
		pay=mat.or.vec(n+1,1)
		dis=mat.or.vec(n+1,1)
		dis[1]=1
		x[1]=0.05
		
		for(i in 1:n)
		{
			s=0.25*(0.1-x[i])*(1/gap)+0.1*sqrt(x[i])*sqrt(1/gap)*z[i]+1/2*(1/2*0.1*(x[i]^(-0.5)))*0.1*sqrt(x[i])*(1/gap)*(z[i]^2-1)
			x[i+1]=x[i]+s
			pay[i+1]=max(0,x[i+1]-0.07)/gap*1000000
			dis[i+1]=dis[i]*(1+x[i+1]/gap)
		}
		for(i in 1:n)
		{
			v[k]=v[k]+pay[i+1]/dis[i+1]
		}
		
		for(i in 1:n)
		{
			s=0.25*(0.1-x[i])*(1/gap)+0.1*sqrt(x[i])*sqrt(1/gap)*(-z[i])+1/2*(1/2*0.1*(x[i]^(-0.5)))*0.1*sqrt(x[i])*(1/gap)*(z[i]^2-1)
			x[i+1]=x[i]+s
			pay[i+1]=max(0,x[i+1]-0.07)/gap*1000000
			dis[i+1]=dis[i]*(1+x[i+1]/gap)
		}
		for(i in 1:n)
		{
			v[1001-k]=v[1001-k]+pay[i+1]/dis[i+1]
		}
	}
	mean[kk]=mean(v)
}

hist(mean,breaks=c((45:57)*1000),col="red",prob=TRUE,ylim=c(0,0.0004))
curve(dnorm(x, mean=mean(mean), sd=sd(mean)), add=TRUE)

#####month
x=mat.or.vec(3*n+1,1)
x[1]=0.05
y=mat.or.vec(3*n+1,1)
y[1]=0.05
	
z=rnorm(n,0,1)

###week
x=mat.or.vec(4*3*n+1,1)
x[1]=0.05
y=mat.or.vec(4*3*n+1,1)
y[1]=0.05
w=mat.or.vec(4*3*n+1,1)
w[1]=0.05
	
for(i in 1:n)
{	
	s=0.25*(0.1-x[12*(i-1)+1])*(1/gap)+0.1*sqrt(x[12*(i-1)+1])*sqrt(1/gap)*z[i]+1/2*(1/2*0.1*(x[12*(i-1)+1]^(-0.5)))*0.1*sqrt(x[12*(i-1)+1])*(1/gap)*(z[i]^2-1)
	x[12*i+1]=x[12*(i-1)+1]+s
	x[12*i-3]=(2/3)*x[12*i+1]+(1/3)*x[12*(i-1)+1]
	x[12*i-7]=(1/3)*x[12*i+1]+(2/3)*x[12*(i-1)+1]
	y[12*i+1]=x[12*i+1]
	y[12*i-3]=(2/3)*x[12*i+1]+(1/3)*x[12*(i-1)+1]+rnorm(1,0,sqrt(1/18))*0.1*sqrt(sqrt(x[12*(i-1)+1])*sqrt(x[12*i+1]))
	y[12*i-7]=(1/3)*x[12*i+1]+(2/3)*x[12*(i-1)+1]+rnorm(1,0,sqrt(1/18))*0.1*sqrt(sqrt(x[12*(i-1)+1])*sqrt(x[12*i+1]))
	w[12*i+1]=y[12*i+1]
	w[12*i]=(3/4)*y[12*i+1]+(1/4)*y[12*i-3]+rnorm(1,0,sqrt(1/64))*0.1*sqrt(sqrt(y[12*i+1])*sqrt(y[12*i-3]))
	w[12*i-1]=(2/4)*y[12*i+1]+(2/4)*y[12*i-3]+rnorm(1,0,sqrt(1/48))*0.1*sqrt(sqrt(y[12*i+1])*sqrt(y[12*i-3]))
	w[12*i-2]=(1/4)*y[12*i+1]+(3/4)*y[12*i-3]+rnorm(1,0,sqrt(1/64))*0.1*sqrt(sqrt(y[12*i+1])*sqrt(y[12*i-3]))
	w[12*i-3]=y[12*i-3]
	w[12*i-4]=(3/4)*y[12*i-3]+(1/4)*y[12*i-7]+rnorm(1,0,sqrt(1/64))*0.1*sqrt(sqrt(y[12*i-3])*sqrt(y[12*i-7]))
	w[12*i-5]=(2/4)*y[12*i-3]+(2/4)*y[12*i-7]+rnorm(1,0,sqrt(1/48))*0.1*sqrt(sqrt(y[12*i-3])*sqrt(y[12*i-7]))
	w[12*i-6]=(1/4)*y[12*i-3]+(3/4)*y[12*i-7]+rnorm(1,0,sqrt(1/64))*0.1*sqrt(sqrt(y[12*i-3])*sqrt(y[12*i-7]))
	w[12*i-7]=y[12*i-7]
	w[12*i-8]=(3/4)*y[12*i-7]+(1/4)*y[12*i-11]+rnorm(1,0,sqrt(1/64))*0.1*sqrt(sqrt(y[12*i-7])*sqrt(y[12*i-11]))
	w[12*i-9]=(2/4)*y[12*i-7]+(2/4)*y[12*i-11]+rnorm(1,0,sqrt(1/48))*0.1*sqrt(sqrt(y[12*i-7])*sqrt(y[12*i-11]))
	w[12*i-10]=(1/4)*y[12*i-7]+(3/4)*y[12*i-11]+rnorm(1,0,sqrt(1/64))*0.1*sqrt(sqrt(y[12*i-7])*sqrt(y[12*i-11]))
}
plot.ts(mat.or.vec(240,1),ylim=c(0.025,0.1))
for(i in 0:19)
{
lines(c(12*i+1,12*(i+1)+1),c(x[12*i+1],x[12*(i+1)+1]),lwd=2)
lines(12*i+1,x[12*i+1],type="p")
}

for(i in 0:20)
{
lines(12*i+1,x[12*i+1],type="p")
}

for(i in 0:59)
lines(c(4*i+1,4*(i+1)+1),c(y[4*i+1],y[4*(i+1)+1]),col="red",lwd=1.5)

for(i in 0:239)
lines(c(i,i+1),c(w[i+1],w[i+2]),col="blue")

title("Brownian Bridge(Monthly)")
title("Brownian Bridge(Weekly)")

###############jump diffusion
	gap=4
	n=gap*5
	
	y=mat.or.vec(n+1,1)
	x=mat.or.vec(n+1,1)

	z=rnorm(n,0,1)
	y[1]=0.05
	x[1]=0.05
	
	for(i in 1:n)
	{
		s2=0.25*(0.1-y[i])*(1/gap)+0.1*sqrt(y[i])*sqrt(1/gap)*z[i]+1/2*(1/2*0.1*(y[i]^(-0.5)))*0.1*sqrt(y[i])*(1/gap)*(z[i]^2-1)
		y[i+1]=y[i]+s2
		
		x[i+1]=y[i+1]
		
		nj=rpois(1,4/4)

		if(nj>0)
		{
			for(j in 1:nj)
			{
				zz=runif(1)
				if(zz<0.05)
				{
					jump=0.0075
				}
				else if(zz<0.15)
				{
					jump=0.005
				}
				else if(zz<0.5)
				{
					jump=0.0025
				}
				else if(zz<0.85)
				{
					jump=-0.0025
				}
				else if(zz<0.95)
				{
					jump=-0.005
				}
				else if(zz<1)
				{
					jump=-0.0075
				}
				y[i+1]=y[i+1]+jump	
			}
		}
	}

	plot.ts(rep(-1,20),ylim=c(min(y)-0.01,max(y)+0.01),ylab="Interest rate")
	for(i in 1:n)
	{
		lines(c(i-1,i),c(y[i],x[i+1]),col="red")
		if(x[i+1]!=y[i+1])
		{
				lines(c(i,i),c(x[i+1],y[i+1]),col="blue",lty=2)
		}
	}

	title("Interest Rate Path with Jump Diffusion")

