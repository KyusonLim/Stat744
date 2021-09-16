## Double pie chart

# double bar chart
a<-c(7,10,44,88,120,211,250,288,365,179) # hospitalized
b<-c(1,1,8,7,26,77,80,63,28,2) # intubated
cc<-c(0,3,5,6,16,26,34,30,18,4) # ICU case 
d<-c(rep('Hospitalized case',10), rep('ICU case',10), rep('Intubated case',10))
e<-c(rep(c('0-9','10-19','20-29','30-39','40-49','50-59','60-69',
'70-79','80-89','90+'),3))


# reform
a<-c(51,88,120,211,250,288,365,179) # hospitalized
b<-c(10,7,26,77,80,63,28,2) # intubated
cc<-c(8,6,16,26,34,30,18,4) # ICU case 

ne<-matrix(nrow=24, ncol=1)
i<-1;j<-1
while (i<25 && j<25){
	ne[j,1]<-a[i]
	ne[j+1,1]<-b[i]
	ne[j+2,1]<-cc[i]
	j<-j+3;i<-i+1
}
ne<-as.data.frame(ne)
d<-c(rep(c('Hospitalized case','ICU case','Intubated case'),8))
e<-c(rep('0-29',3), rep('30-39',3),rep('40-49',3),rep('50-59',3),
rep('60-69',3), rep('70-79',3),rep('80-89',3),rep('90+',3))


#new 
aa<-round(ne[1]/sum(ne),4)*100

f<-matrix(nrow=24, ncol=2)
i<-1;j<-0
while (i<25){
	j<-j+aa[i,1]
	f[i,1]<-j
	i<-i+1
}
for (i in 2:24){
	f[i,2]<-f[i-1,1]
}
#f<-round(f,1)
f[1,2]<-0

browsers<-data.frame(e,d, aa, f)
colnames(browsers)<-c('browser','version','share','ymax','ymin')
browsers



# (code) to use, double donut chart
donuts <- function(x, group = 1, labels = NA, col = NULL, radius = c(.7, 1)) {
  group <- rep_len(group, length(x))
  ug  <- unique(group)
  tbl <- table(group)[order(ug)]
  
  col <- if (is.null(col))
    seq_along(ug) else rep_len(col, length(ug))
  col.main <- Map(rep, col[seq_along(tbl)], tbl)
  col.sub  <- lapply(col.main, function(x) {
    al <- head(seq(0, 1, length.out = length(x) + 2L)[-1L], -1L)
    Vectorize(adjustcolor)(x, alpha.f = al)
  })
  plot.new()
  par(new = TRUE)
  pie(x, border = NA, radius = radius[2L], cex=0.75,
      col = unlist(col.sub), labels = labels)

  par(new = TRUE)
  pie(x, border = NA, radius = radius[1L], cex=0.75,
      col = unlist(col.main), labels = NA, main='created by Kyuson')
}



# only code need to understand
with(browsers,
     donuts(share, browser, sprintf('%s: %s%%', version, share),
            col = c('cyan2','red','orange','green','dodgerblue2','grey',
            'purple','yellow','brown'))
)
age<-c('0-29','30-39','40-49','50-59','60-69','70-79','80-89','90+')
text(x = c(0.5,0.4,0.3,0.1,-0.5,-0.4,0.1,0.5), y = c(0.03,
0.15,0.3,0.5,0.08,-0.4,-0.6, -0.2), 
       labels =unique(age), col = 'black', cex = 0.75)
