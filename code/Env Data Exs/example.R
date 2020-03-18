#let's say we have u and v as arrays of dimensions length(lon), length(lat).


#reformat currents data to work with arrows function
xx=rep(lon,length(lat))
yy=rep(lat,each=length(lon))
new_u=array(NA,dim=c(length(lon),length(lat))) 
new_v=array(NA,dim=c(length(lon),length(lat)))
for (i in seq(1,length(lat),3)) {    # we subsample u and v by picking one points every 3 points.
        new_u[,i]=u[,i]
        new_v[,i]=v[,i]
}

n=length(lon)*length(lat)
Zu=array(new_u,dim=c(length(lon)*length(lat),1))
Zv=array(new_v,dim=c(length(lon)*length(lat),1))
#set currents < 2 cm/s to zero to avoid overcrowding the plot with mini arrows. Adjust the value as needed.
Zu2=Zu
I=which(abs(Zu2)<2)
Zu2[I]=0
Zv2=Zv
I=which(abs(Zv2)<2)
Zv2[I]=0

#if it's still too crowded, subsample again.
i=seq(1,n,20)  #adjust the value (here: 20) as needed
Zu3=array(NA,dim=c(length(lon)*length(lat),1))
Zv3=array(NA,dim=c(length(lon)*length(lat),1))
Zu3[i]=Zu2[i]
Zv3[i]=Zv2[i]



#put everything in a data frame 
DF=data.frame(xx,yy,Zu3,Zv3)

# if the arrows are too big, you can also rescale by dividing DF by a coefficient.
DF=DF/10


#Then, once the color plot is done, you can add the arrows:
arrows(DF[,1], DF[,2], DF[,1] + DF[,3], DF[,2] + DF[,4],length=0.05)


