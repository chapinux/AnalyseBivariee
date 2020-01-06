library(ggplot2)
library(cowplot)


n <-  4000
xval <-  rbeta(n, shape1 = 2, shape2 = 5) * 1000
mydf <-  data.frame(x=xval)
xmean <-  mean(xval)
xsd <-  sd(xval)

xlabels <- data.frame(
  x = c(quantile(mydf$x)),
  y = c(rep(330,5) ),
  text = paste0(c("","Q1: ", "Q2: ", "Q3: ", ""),names(quantile(mydf$x)))
)
distriPlot <-  ggplot(mydf, aes(x=x))+
  geom_histogram(bins = 50, fill="#0FAF96", color="grey")+
  geom_vline(data = xlabels, xintercept = xlabels$x, linetype=c(2,2,1,2,2))+
  geom_label(data= xlabels, aes(x=x,y=y,label=text) )+
  xlab("X")+
  ylab("Effectif")
distriPlot



# Marginal density plot of x (top panel) and y (right panel)
yplot <- ggplot(mydf, aes(y = xval))+
  geom_boxplot(outlier.alpha = 0.2)+
  coord_flip()+
  xlab("")+
  ylab("")+
  theme_void()
yplot

#assemblage
plot_grid(yplot, distriPlot,  ncol = 1, align = "hv",scale = c(1,1), 
          rel_widths = c(1, 1), rel_heights = c(1, 3))



