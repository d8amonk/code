func1=function() {
  ddd<<-paste(rep(c("A","B"),each=10),1:10,sep=',')
  matrix(unlist(strsplit(ddd, ',')),ncol=2,byrow=T)
}

func2=function() {
  #ddd=paste(rep(c("A","B"),each=10),1:10,sep=',')
  transform(ddd, 
            CountyName = sapply(strsplit(County, ","), function(x) x[1]), 
            State      = sapply(strsplit(County, ","), function(x) x[2])
  )
}

func2=function() {matrix(unlist(strsplit(ddd, ',')),ncol=2,byrow=T)}

system.time(replicate(10000, func1() ) )
system.time(replicate(10000, func2() ) )

# Random Sampler
x  <- data.frame(row_sqrd = (1:26)^2, let = letters, LET = LETTERS)
#set.seed(10)
split(x, sample(rep(1:2, 13)))
sample(x)
