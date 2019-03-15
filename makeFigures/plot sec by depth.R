hr.swc.lai.df.1314$Ring <- as.factor(hr.swc.lai.df.1314$Ring)
palette(rainbow(6))
plot(`-25`~DateTime,data = hr.swc.lai.df.1314,col=palette()[(Ring)],pch=16)
legend('topright',legend = levels(hr.swc.lai.df.1314$Ring),pch=16,col=palette(),horiz = T)
for(depth.neutron in paste0(seq(-450,-25,25))){
  
  col.plot <- which(names(hr.swc.lai.df.1314) == depth.neutron)
  
  palette(rainbow(6))
  plot(c(hr.swc.lai.df.1314[,depth.neutron])~DateTime,data = hr.swc.lai.df.1314,col=palette()[(Ring)],pch=16)
  legend('topright',legend = levels(hr.swc.lai.df.1314$Ring),pch=16,col=palette(),horiz = T)
}

plot(`-25`~DateTime,data = hr.swc.lai.df.1314[Ring == 'R1'],type='l',ylim=c(0,40))
points(`-50`~DateTime,data = hr.swc.lai.df.1314[Ring == 'R1'],type='l')
points(`-75`~DateTime,data = hr.swc.lai.df.1314[Ring == 'R1'],type='l')
points(`-100`~DateTime,data = hr.swc.lai.df.1314[Ring == 'R1'],type='l')
points(`-125`~DateTime,data = hr.swc.lai.df.1314[Ring == 'R1'],type='l')
points(`-150`~DateTime,data = hr.swc.lai.df.1314[Ring == 'R1'],type='l')

# plot swc####
par(mfrow=c(3,2),mar=c(2,5,1,1))
for(rings in paste0('R',c(1,2,4,3,5,6))){
  plot.df <- hr.swc.lai.df.1314[Ring == rings,]
  plot(`-25`~DateTime,data = plot.df,type='l',ylim=c(0,35),
       ylab="SWC (%)",xlab='')
  
  for(depth.neutron in paste0(seq(-75,-25,25))){
    
    col.plot <- which(names(hr.swc.lai.df.1314) == depth.neutron)
    if(length(col.plot) >0){  
      temp.df <- data.frame(y = plot.df[,col.plot,with=FALSE],
                            x = plot.df$DateTime)
      names(temp.df) <- c('y','x')
      points(y~x,data = temp.df,
             type='l')
    }
    points(swc.tdr.5~DateTime,data = plot.df,
           type='l',col='red')
    points(swc.tdr.30~DateTime,data = plot.df,
           type='l',col='red')
    # points(swc.tdr.75~DateTime,data = plot.df,
    #        type='l',col='red')
    
  }
  legend("topleft",legend = paste0(rings))
}
legend("topright",legend = c('TDR',"neutron"),lty='solid',bty='n',col=c('red','black'))

