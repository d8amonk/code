require(ggtern)
require(ggplot2)
require(dplyr)

# basic idea
ggtern(data=data.frame(x=1,y=1,z=1),aes(x,y,z)) + geom_point()

df = data.frame(x = runif(50),
                y = runif(50),
                z = runif(50),
                Value = runif(50,1,10),
                Group = as.factor(round(runif(50,1,2))))
# paths and colors
ggtern(data=df,aes(x,y,z,color=Group)) + 
  theme_rgbw() + 
  geom_point() + geom_path() + 
  labs(x="X",y="Y",z="Z",title="Title")

# confint
data(Feldspar)
glimpse(Feldspar)
ggtern(data=Feldspar,aes(An,Ab,Or)) + 
  geom_point() + 
  geom_confidence_tern()

#Plot Density Estimate, on isometric log ratio transformation of original data
ggtern(Feldspar,aes(Ab,An,Or)) + 
  geom_density_tern(aes(color=..level..),bins=5) +
  geom_point()


#Plot Density Estimate w/ Polygon Geometry
data('Feldspar')
ggtern(data=Feldspar,aes(Ab,An,Or)) + 
  stat_density_tern(
    geom='polygon',
    aes(fill=..level..),
    bins=5,
    color='grey') +
  geom_point()

# interpolation
ggtern(Feldspar,aes(Ab,An,Or,value=T.C)) + 
  stat_interpolate_tern(geom="polygon",
                        formula=value~x+y,
                        method=lm,n=100,
                        breaks=seq(0,1000,by=100),
                        aes(fill=..level..),expand=1) +
  geom_point()

ggtern(Feldspar,aes(Ab,An,Or)) + 
  stat_confidence_tern(geom='polygon',aes(fill=..level..),color='white') + 
  geom_mask() + 
  geom_point_swap(aes(colour=T.C,shape=Feldspar),fill='black',size=5) +
  scale_shape_manual(values=c(21,24)) +
  scale_color_gradient(low='green',high='red') +
  labs(title="Feldspar",color="Temperature",fill='Confidence')

