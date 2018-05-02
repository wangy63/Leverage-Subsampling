setwd("/Users/wangyuexi/Desktop/research/leverage sampling/my work")
load("GA1000_P10.Rdata")
load("GA1000_P50.Rdata")
load("GA1000_P100.Rdata")
load("GA5000_P50.Rdata")
load("GA5000_P100.Rdata")
load("GA5000_P500.Rdata")
load("GA10000_P50.Rdata")
load("GA10000_P100.Rdata")
load("GA10000_P500.Rdata")
load("T31000_P10.Rdata")
load("T31000_P50.Rdata")
load("T31000_P100.Rdata")
load("T35000_P50.Rdata")
load("T35000_P100.Rdata")
load("T35000_P500.Rdata")
load("T310000_P50.Rdata")
load("T310000_P100.Rdata")
load("T310000_P500.Rdata")
load("T11000_P10.Rdata")
load("T11000_P50.Rdata")
load("T11000_P100.Rdata")
load("T15000_P50.Rdata")
load("T15000_P100.Rdata")
load("T15000_P500.Rdata")
load("T110000_P50.Rdata")
load("T110000_P100.Rdata")
load("T110000_P500.Rdata")

#### plot graph
library(plotly)
plot_GA1000_P10<-ggplotly(ggplot(data=GA1000_P10, aes(x=`subsample size`, y=MSE, color=method))+
                            geom_line()+
                            ggtitle("GA1000_P10"))
save(plot_GA1000_P10, file="plot_GA1000_P10")
plot_GA1000_P50<-ggplotly(ggplot(data=GA1000_P50, aes(x=`subsample size`, y=MSE, color=method))+
                            geom_line()+
                            ggtitle("GA1000_P50"))
save(plot_GA1000_P50, file="plot_GA1000_P50")

plot_GA1000_P100<-ggplotly(ggplot(data=GA1000_P100, aes(x=`subsample size`, y=MSE, color=method))+
                            geom_line()+
                            ggtitle("GA1000_P100"))
save(plot_GA1000_P100, file="plot_GA1000_P100")

plot_GA5000_P50<-ggplotly(ggplot(data=GA5000_P50, aes(x=`subsample size`, y=MSE, color=method))+
                            geom_line()+
                            ggtitle("GA5000_P50"))
save(plot_GA5000_P50, file="plot_GA5000_P50")

plot_GA5000_P100<-ggplotly(ggplot(data=GA5000_P100, aes(x=`subsample size`, y=MSE, color=method))+
                            geom_line()+
                            ggtitle("GA5000_P100"))
save(plot_GA5000_P100, file="plot_GA5000_P100")

plot_GA5000_P500<-ggplotly(ggplot(data=GA5000_P500, aes(x=`subsample size`, y=MSE, color=method))+
                            geom_line()+
                            ggtitle("GA5000_P500"))
save(plot_GA5000_P500, file="plot_GA5000_P500")

plot_GA10000_P50<-ggplotly(ggplot(data=GA10000_P50, aes(x=`subsample size`, y=MSE, color=method))+
                             geom_line()+
                             ggtitle("GA10000_P50"))
save(plot_GA10000_P50, file="plot_GA10000_P50")

plot_GA10000_P100<-ggplotly(ggplot(data=GA10000_P100, aes(x=`subsample size`, y=MSE, color=method))+
                             geom_line()+
                             ggtitle("GA10000_P100"))
save(plot_GA10000_P100, file="plot_GA10000_P100")

plot_GA10000_P500<-ggplotly(ggplot(data=GA10000_P500, aes(x=`subsample size`, y=MSE, color=method))+
                             geom_line()+
                             ggtitle("GA10000_P500"))
save(plot_GA10000_P500, file="plot_GA10000_P500")

plot_T31000_P10<-ggplotly(ggplot(data=T31000_P10, aes(x=`subsample size`, y=MSE, color=method))+
                            geom_line()+
                            ggtitle("T31000_P10"))
save(plot_T31000_P10, file="plot_T31000_P10")

plot_T31000_P50<-ggplotly(ggplot(data=T31000_P50, aes(x=`subsample size`, y=MSE, color=method))+
                            geom_line()+
                            ggtitle("T31000_P50"))
save(plot_T31000_P50, file="plot_T31000_P50")

plot_T31000_P100<-ggplotly(ggplot(data=T31000_P100, aes(x=`subsample size`, y=MSE, color=method))+
                             geom_line()+
                             ggtitle("T31000_P100"))
save(plot_T31000_P100, file="plot_T31000_P100")

plot_T35000_P50<-ggplotly(ggplot(data=T35000_P50, aes(x=`subsample size`, y=MSE, color=method))+
                            geom_line()+
                            ggtitle("T35000_P50"))
save(plot_T35000_P50, file="plot_T35000_P50")

plot_T35000_P100<-ggplotly(ggplot(data=T35000_P100, aes(x=`subsample size`, y=MSE, color=method))+
                             geom_line()+
                             ggtitle("T35000_P100"))
save(plot_T35000_P100, file="plot_T35000_P100")

plot_T35000_P500<-ggplotly(ggplot(data=T35000_P500, aes(x=`subsample size`, y=MSE, color=method))+
                             geom_line()+
                             ggtitle("T35000_P500"))
save(plot_T35000_P500, file="plot_T35000_P500")

plot_T310000_P50<-ggplotly(ggplot(data=T310000_P50, aes(x=`subsample size`, y=MSE, color=method))+
                             geom_line()+
                             ggtitle("T310000_P50"))
save(plot_T310000_P50, file="plot_T310000_P50")

plot_T310000_P100<-ggplotly(ggplot(data=T310000_P100, aes(x=`subsample size`, y=MSE, color=method))+
                              geom_line()+
                              ggtitle("T310000_P100"))
save(plot_T310000_P100, file="plot_T310000_P100")

plot_T310000_P500<-ggplotly(ggplot(data=T310000_P500, aes(x=`subsample size`, y=MSE, color=method))+
                              geom_line()+
                              ggtitle("T310000_P500"))
save(plot_T310000_P500, file="plot_T310000_P500")

plot_T11000_P10<-ggplotly(ggplot(data=T11000_P10, aes(x=`subsample size`, y=MSE, color=method))+
                            geom_line()+
                            ggtitle("T11000_P10"))
save(plot_T11000_P10, file="plot_T11000_P10")

plot_T11000_P50<-ggplotly(ggplot(data=T11000_P50, aes(x=`subsample size`, y=MSE, color=method))+
                            geom_line()+
                            ggtitle("T11000_P50"))
save(plot_T11000_P50, file="plot_T11000_P50")

plot_T11000_P100<-ggplotly(ggplot(data=T11000_P100, aes(x=`subsample size`, y=MSE, color=method))+
                             geom_line()+
                             ggtitle("T11000_P100"))
save(plot_T11000_P100, file="plot_T11000_P100")

plot_T15000_P50<-ggplotly(ggplot(data=T15000_P50, aes(x=`subsample size`, y=MSE, color=method))+
                            geom_line()+
                            ggtitle("T15000_P50"))
save(plot_T15000_P50, file="plot_T15000_P50")

plot_T15000_P100<-ggplotly(ggplot(data=T15000_P100, aes(x=`subsample size`, y=MSE, color=method))+
                             geom_line()+
                             ggtitle("T15000_P100"))
save(plot_T15000_P100, file="plot_T15000_P100")

plot_T15000_P500<-ggplotly(ggplot(data=T15000_P500, aes(x=`subsample size`, y=MSE, color=method))+
                             geom_line()+
                             ggtitle("T15000_P500"))
save(plot_T15000_P500, file="plot_T15000_P500")

plot_T110000_P50<-ggplotly(ggplot(data=T110000_P50, aes(x=`subsample size`, y=MSE, color=method))+
                             geom_line()+
                             ggtitle("T110000_P50"))
save(plot_T110000_P50, file="plot_T110000_P50")

plot_T110000_P100<-ggplotly(ggplot(data=T110000_P100, aes(x=`subsample size`, y=MSE, color=method))+
                              geom_line()+
                              ggtitle("T110000_P100"))
save(plot_T110000_P100, file="plot_T110000_P100")

plot_T110000_P500<-ggplotly(ggplot(data=T110000_P500, aes(x=`subsample size`, y=MSE, color=method))+
                              geom_line()+
                              ggtitle("T110000_P500"))
save(plot_T110000_P500, file="plot_T110000_P500")


