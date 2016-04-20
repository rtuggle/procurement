library(ggplot2)
d.405 <- data.frame(abs(rnorm(30)),abs(rnorm(30)),abs(rnorm(30)),abs(rnorm(30)),type="405")
d.409 <- data.frame(abs(rnorm(30)),abs(rnorm(30)),abs(rnorm(30)),abs(rnorm(30)),type="409")
all <- rbind(d.405,d.409)
colnames(all) <- c("401","402","403","404","type")

library("reshape2");
allM <- melt(all, id.vars = "type")


combis <- expand.grid(levels(allM$variable),levels(allM$variable))

plotdat <- lapply(seq_len(nrow(combis)),function(i) cbind(allM[allM$variable==combis[i,1] & allM$type=="405",],
                                                          allM[allM$variable==combis[i,2] & allM$type=="409",c("type","variable","value")]))
plotdat <- do.call(rbind,plotdat)
names(plotdat) <- c("type.x","var.x","x","type.y","var.y","y")
plotdat$var.x <- paste("x:",plotdat$var.x)
plotdat$var.y <- paste("y:",plotdat$var.y)

library(plyr)
cors <- ddply(plotdat,.(var.x,var.y),summarize,cor=format(signif(cor(x,y),2),scientific=-2))
cors$x <- 2.2
cors$y <- 2.5

ggplot(plotdat,aes(x=x,y=y)) + 
    geom_point() + 
    geom_smooth(method="lm") +
    geom_text(data=cors,aes(label=paste("r =",cor))) +
    facet_wrap(~var.y*var.x,ncol=4) +
    xlab("405") + ylab("409")

data("iris")


print(p)