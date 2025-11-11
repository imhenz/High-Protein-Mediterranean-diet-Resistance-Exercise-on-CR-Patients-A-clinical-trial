# install.packages(file.path(getwd(), "QoLR_1.0.3.tar.gz"), repos = NULL, type="source")

require(QoLR)

?dataqol2

data(dataqol2)

head(dataqol2)

dataqol2$id <- as.factor(dataqol2$id)
dataqol2$time <- as.factor(dataqol2$time)
dataqol2$arm <- as.factor(dataqol2$arm)

# long to wide
qol2.wide <- reshape(dataqol2, v.names="QoL", idvar = "id", timevar = "time", direction = "wide", drop=c("date","pain"))

qol2.wide$bestQoL <- apply(qol2.wide[,5:9], 1 ,function(x) ifelse(sum(!is.na(x)) == 0, NA, max(x,na.rm=TRUE)))

qol2.wide$bestQoL.PerChb <- ((qol2.wide$bestQoL-qol2.wide$QoL.0)/qol2.wide$QoL.0)*100

o <- order(qol2.wide$bestQoL.PerChb,decreasing=TRUE,na.last=NA)
qol2.wide <- qol2.wide[o,]

barplot(qol2.wide$bestQoL.PerChb, col="blue", border="blue", space=0.5, ylim=c(-100,100),
        main = "Waterfall plot for changes in QoL scores", ylab="Change from baseline (%) in QoL score",
        cex.axis=1.2, cex.lab=1.4)

col <- ifelse(qol2.wide$arm == 0, "#BC5A42", "#009296")
barplot(qol2.wide$bestQoL.PerChb, col=col, border=col, space=0.5, ylim=c(-100,100),
        main = "Waterfall plot for changes in QoL scores", ylab="Change from baseline (%) in QoL score",
        cex.axis=1.2, cex.lab=1.4, legend.text=c(0,1),
        args.legend=list(title="Treatment arm", fill=c("#BC5A42","#009296"), border=NA, cex=0.9))

require(ggplot2)
x <- 1:nrow(qol2.wide)


b <- ggplot(qol2.wide, aes(x=x, y=bestQoL.PerChb, fill=arm, color=arm)) +
  scale_fill_discrete(name="Treatmentnarm") + 
  scale_color_discrete(guide="none") +
  labs(list(title = "Waterfall plot for changes in QoL scores", 
            x = NULL, 
            y = "Change from baseline (%) in QoL score")) +
  theme_classic() %+replace%
  theme(axis.line.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(face="bold",angle=90)) +
  coord_cartesian(ylim = c(-100,100))

b <- b + geom_bar(stat="identity", width=0.7, position = position_dodge(width=0.4))

b
