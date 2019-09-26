library(devtools)

if (!require('devtools')) install.packages('devtools')
devtools::install_github('fhernanb/usefultools', force=TRUE)


library(usefultools)

shadow.dist(dist='df', param=list(df1=2, df2=18),
            a=0, type='upper', col.shadow='blue', xlim=c(0,10))

shade.F(x=0,nu1=2,nu2=18,tail="upper")

# Ajuste de gráfico 

shadow.dist(dist='df', param=list(df1=ph$df1, df2=ph$df2),
            a=(as.numeric(ph$estadistico)/ph$a), type='upper', 
            col.shadow='blue', xlim=c(0,10))


shadow.dist(dist='dchisq', param=list(df=ph$p),
            a=as.numeric(ph$estadistico), type='upper', 
            col.shadow='blue', xlim=c(0,10))