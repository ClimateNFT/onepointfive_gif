library(tidyverse)
library(magick)
library(ggpubr)
library(png)

## Data, OGL License
## HadCRUT.5.0.1.0 data were obtained from http://www.metoffice.gov.uk/hadobs/hadcrut5 on 03/11/2021 and are © British Crown Copyright, Met Office [year of first publication], provided under an Open Government License, http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/
## Base Image, Vecteezy
## https://www.vecteezy.com/vector-art/3339936-the-team-is-working-together-to-achieve-a-corporate-goal 
## https://www.nagraj.net/notes/gifs-in-r/ was a useful resource

# directory
working_dir <- "~/Downloads/onepointfive"
ifelse(!dir.exists(working_dir), dir.create(working_dir), FALSE)
setwd(working_dir)

# read in
dat <- read.csv(url("https://www.metoffice.gov.uk/hadobs/hadcrut5/data/current/analysis/diagnostics/HadCRUT.5.0.1.0.analysis.summary_series.global.annual.csv"),col.names = c("Time","Anom","Lower","Upper"))

# we want 1.5 degrees relative to 'pre-industrial, i.e. 1850-1900', so we need to offset
offset <- abs(as.numeric (dat %>% filter(Time >= 1850 & Time <= 1900) %>% summarise(offset=mean(Anom))))
dat$Anom_1.5 <- dat$Anom + offset
dat$Lower_1.5 <- dat$Lower + offset
dat$Upper_1.5 <- dat$Upper + offset
dat <- subset(dat,dat$Time < 2021) # haven't finished 2021 yet so chucking it out
rm(offset)

# base image
img1 <- png::readPNG("~/Downloads/Untitled-1.png")
for_colour <- runmed(dat2$Anom_1.5,31) # colour indicator
fillcol <- ifelse(for_colour >= 1,"DarkRed",ifelse(for_colour >= 0.5,"Orange",ifelse(for_colour >= 0,"Yellow","Green")))


# loop through time
for (i in seq(1850,2020)) {
  dat2 <- data.frame( sapply( dat[,5:7],function(x) { ifelse( dat$Time > i,NA_real_,x)}), Time=dat$Time,fillcol)
  fillcol2 <- dat2 %>% filter(Time==i)
  p1 <- ggplot(dat2,aes(x=Time,ymin=Lower_1.5,ymax=Upper_1.5)) + geom_ribbon(alpha=.6,fill=as.character(fillcol2$fillcol)) + geom_point(aes(x=2020,y=1.5),col="DarkRed",size=0.5) + geom_point(aes(x=2020,y=1),col="Orange",size=0.5) + geom_point(aes(x=2020,y=0.5),col="Yellow",size=0.5) + geom_point(aes(x=2020,y=0),col="Green",size=0.5) + theme_void() + ylim(c(-0.4,1.5)) 
  ggsave(plot = p1+background_image(img1),filename = sprintf("%s/plot_year_%s.jpg",working_dir,i),width = 800,height = 450,units = "px",dpi = 300)
}

# get all the files
imgs <- list.files(working_dir,pattern=".jpg", full.names = TRUE)
img_list <- lapply(imgs, image_read)

# and merge together, crudely adding a delay for final img
imgs <- image_join(img_list,rep(img_list[length(img_list)],50))

# and animate
img_an <- image_animate(imgs, fps = 25)

# save 
image_write(img_an,path = sprintf("%s/onepointfive.gif",working_dir))

# or view
img_an



