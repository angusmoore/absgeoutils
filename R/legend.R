plotlegend <- function(colorlist,ls,labels) {
  par(new=TRUE,mar=c(0,0,0,0))
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]
  xrange <- xmax-xmin
  yrange <- ymax-ymin

  if (ls == 1) {
    plotrix::color.legend(xmin-5.5*xrange/100,ymin+yrange/20,xmin-2*xrange/100,ymax-yrange/20,labels,colorlist,gradient="y")
  } else if (ls == 2) {
    plotrix::color.legend(xmax+7*xrange/100,ymin+yrange/20,xmax+10.5*xrange/100,ymax-yrange/20,labels,colorlist,grandient="y")
  } else {
    stop(paste('Invalid option for ls. Only 1 (left), 2 (right) or 0 (none) are allowed.'))
  }

}
