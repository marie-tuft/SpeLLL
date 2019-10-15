plot.C <- function(C,s, title = "Heatmap of C Matrix"){

temp <-  rep(1:ncol(C),nrow(C))
temp <- temp[order(temp)]
df <- as.data.frame(cbind(c(C),rep(1:nrow(C),ncol(C)),temp))
colnames(df) <- c("Coefficient","col","row")

ggplot2::ggplot(data = df, ggplot2::aes(x = col, y = -row)) +
  ggplot2::geom_tile(ggplot2::aes(fill = Coefficient)) +
  ggplot2::scale_colour_gradient2(low = "black",
                                  high = "yellow", midpoint = 0, space = "Lab",
                                  na.value = "grey50", guide = "colourbar", aesthetics = "fill")+
  ggplot2::theme_classic()+
  #ggplot2::theme(axis.line=ggplot2::element_blank(),
        #axis.text.x=ggplot2::element_blank(),
        #axis.text.y=ggplot2::element_blank(),
        #axis.ticks=ggplot2::element_blank(),
        #axis.title.x=ggplot2::element_blank(),
        #axis.title.y=ggplot2::element_blank(),
        #panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=2)+
  ggplot2::scale_y_continuous(breaks = -(1:nrow(C)),labels=s)+
  ggplot2::scale_x_continuous(breaks = seq(1,ncol(C),length.out=15), labels =round( seq(1,ncol(C),length.out=15)/(2*ncol(C)),2))+
  ggplot2::labs(x="Frequency", y="", title = title)
  

  
}
