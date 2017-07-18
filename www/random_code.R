

library(archiDART)

path <- "~/OneDrive - UCL/03_research/archidart/rsmls/"

## Compute RSA parameters for DART files only
res2a <- architect(inputrac=path, inputtps=path, res=75, unitlength="cm")

## Compute RSA parameters for RSML files only
res2b <- architect(inputrsml=path, rsml.connect=F, rsml.date="age")

res2c <- architect(inputrac=path, inputtps=path, inputrsml=path, res=75, unitlength="cm",
                   rsml.connect=TRUE, rsml.date="age")



ggplot(res2b, aes(Time, GR2L, colour=FileName)) + 
  geom_line() + 
  theme_bw()

archidraw(inputlie=path, res=75, 
          unitlength="cm", 
          numdate=c(15,31), 
          finalscale=TRUE,
          coldate=rainbow(31), 
          lwddate=2,las=1, bty="l", asp=1, 
          xaxp=c(0,30,3), yaxp=c(0,90,9))

test <- list(archidraw(inputrsml=path, unitlength="cm", 
          rsml.date="age", coldate=rainbow(16), 
          lwddate=2, twod = c("x","y")))

archigrow(inputrsml = path, 
                   res=75, unittime="day",
                   unitlength="cm", rsml.connect=TRUE, rsml.date="age", plot=TRUE, export.colors=TRUE,
                   coldyn=c("blue", "orange", "red"), las=1, bty="l", asp=1, xaxp=c(0,30,3), lwd=2, twod = c("x","y"))

list.files(path)


LIE<-list()
res1<-c()
unitlength1<-c()
filenameslie<-c()


filenames.rsml<-list.files(path=path, pattern="\\.rsml$")
dat <- NULL
for(f in filenames.rsml){
  dat <- rbind(dat, rsmlToTable(paste0(path, f)))
  message(paste0(f," done"))
  
}

genotypes <- unlist(lapply(strsplit(as.character(dat$plant), "-"), `[[`, 1))[]
rep <- unlist(lapply(strsplit(as.character(dat$plant), "-"), `[[`, 3))[]
dat$genotype <- genotypes
dat$rep <- rep
dat$age <- as.numeric(dat$age)


dat1 <- ddply(dat, .(genotype, rep, plant, age), summarise, value = sum(length))


ggplot(dat1) +  
  xlab("Time [days]") + 
  theme_classic() +
  geom_line(aes(age, value, colour=genotype, group=plant))


ggplot(dat1) +  
  xlab("Time [days]") + 
  theme_classic() +
  stat_smooth(aes(age, value, colour=genotype))


ggplot(dat[]) + 
  geom_segment(aes(x = x1, y = -y1, xend = x2, yend = -y2, colour=age)) + 
  coord_fixed() + 
  theme_bw() + 
  facet_grid(genotype~rep)




RSML <- lapply(paste(path, "/", filenames.rsml, sep=""), rsmlToDART, final.date=NULL, connect=T)
for (i in 1:length(RSML)){
  res1<-append(res1, rep(as.numeric(RSML[[i]]$resolution), length(RSML[[i]]$lie)))
  unitlength1<-append(unitlength1, rep(as.character(RSML[[i]]$length), length(RSML[[i]]$lie)))
  LIE<-append(LIE, RSML[[i]]$lie)
  length1<-length(RSML[[i]]$lie)
}




archi$y_mean <- -round(archi$y1/10)
temp <- ddply(archi, .(plant, genotype, y_mean), summarise, rld=sum(length))
ggplot(temp) +
  geom_line(aes(y_mean, rld, group=plant), colour="grey") +
  stat_smooth(aes(y_mean, rld, colour=genotype)) +
  facet_grid(~genotype) +
  theme_bw() +
  coord_flip()


