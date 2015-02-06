setwd("K:/THESIS/Ceden_data/R/data") 
setwd("N:/THESIS/Ceden_data/R/data") 
dir()
getwd()

###download data from ceden, clipped CA data to just sd county in arcmap, copy LocationCo, paste and do text to columns so transect and location sep cols
#now ready to read in the data, need to revise so i can clip in this script. 

data = read.csv("ceden_sd_phab_text2col.csv",header=TRUE,sep=",")
names(data)
data$LocationCo[1:5]


#FORMAT DATE
	data$SampleDate
	date.posix = strptime(data$SampleDate,format="%m/%d/%Y %H:%M")
	date.month = format(date.posix,"%m")
	date.year = format(date.posix,"%Y")
	date.day = format(date.posix,"%d")
	as.numeric(date.month)
	as.numeric(date.year)
	month.day.year = format(date.posix, "%m/%d/%Y") #actual date column
	date.numeric = as.numeric(as.Date(month.day.year, format="%m/%d/%Y", origin="1970-01-01"))
	#date.numeric is the date shown as a number for the unique station.date.trans vector

#ADD UNIQUE COLUMN (COMBINED UNIQUE COL)
	station.date.trans =  paste(as.character(data$StationCod),date.numeric, as.character(data$Transect))
	names(station.date.trans)<-("station.date.trans") 
	#names() is to add column name to the vector I created
	#add to data frame using cbind(data, station.date.trans)
	data.2 = cbind(data, station.date.trans)
	names(data.2)
	

	#make analyte.location so stationwaterdepth.center  and use gsub to take away spaces n replace with .
	analyte.loc = paste(as.character(data.2$Analyte),as.character(data.2$Location))
	nodoublespace = gsub("  "," ",analyte.loc)
	analyte.loc1 = gsub(" ",".",nodoublespace)
	names(analyte.loc1)<-("analyte.loc1") 

	#create another unique column with 
	station.date.location = paste(as.character(data$StationCod),date.numeric, as.character(data$LocationCo))
	names(station.date.location)<-("station.date.location")
	data.3 = cbind(data.2, station.date.location, analyte.loc1)
	names(data.3)
write.csv(data.2,file="data.2.csv",row.names=FALSE)
write.csv(data.3,file="data.3.csv",row.names=FALSE)

#Make matrix of only station.date.trans, analyte, and result for pivot table
	
	#pivot.matrix = matrix(cbind(as.character(data.3$station.date.trans),as.character(data.3$analyte.loc1),as.numeric(data.3$Result)),length(data.3$Analyte),3)
    #pivot.matrix below is with the added variable column for substrate size class (for manning's equation)
pivot.matrix = matrix(cbind(as.character(data.3$station.date.trans),as.character(data.3$analyte.loc1),as.numeric(data.3$Result),as.character(data.3$VariableRe)),length(data.3$Analyte),4)
  pivot.matrix2 = as.data.frame(pivot.matrix)
	names(pivot.matrix2) <- c("station.date.trans",
			"analyte.loc1",
			"Result",
          "Variable")
	names(pivot.matrix2)
	pivot.matrix2[1:11,]
		unique(pivot.matrix2$analyte.loc1)
		class(pivot.matrix2$Result)
		levels(pivot.matrix2$Result)
		write.csv(pivot.matrix2,file="pivot.matrix2.csv",row.names=FALSE)	
	

#WRITE LOOP TO TRANSPOSE THE ANALYTE AND RESULT COLUMN, and skip station if doesn't have bankfull, if does, find area
#in loop the if bankfull width is in analyte column OR (|| which means if left condition is true, skip the right condition) if bankfull height = 0, then xsa is NA
	stlist = unique(pivot.matrix2$station.date.trans)

	outmat = matrix(0,length(stlist),6)
	outmat2 = as.data.frame(outmat)
	names(outmat2) <- c("station.date.trans",
				"xsa.bf.tri",
				"xsa.bf.rect",
        "hyd.radius",
        "sub.size.class",
              "bfw")
install.packages("Hmisc")
library(Hmisc)		
	for (i in 1: length(stlist)) {
		sub = pivot.matrix2[pivot.matrix2$station.date.trans==stlist[i],]
 		outmat2[i,]$station.date.trans = as.character(stlist[i])
			if(("Bankfull.Width." %nin% sub$analyte.loc1)||(as.character(sub[sub$analyte.loc1=="Bankfull.Height.",]$Result)=="0")) {
				outmat2[i,]$xsa.bf.tri = NA  
				outmat2[i,]$xsa.bf.rect = NA
			}else {

				bfw = as.numeric(as.character(sub[sub$analyte.loc1=="Bankfull.Width.",]$Result))
				bfd = as.numeric(as.character(sub[sub$analyte.loc1=="Bankfull.Height.",]$Result))
				ww = as.numeric(as.character(sub[sub$analyte.loc1=="Wetted.Width.",]$Result))			
				wd.lctr = as.numeric(as.character(sub[sub$analyte.loc1=="StationWaterDepth.LCtr",]$Result))/100	
				wd.ctr = as.numeric(as.character(sub[sub$analyte.loc1=="StationWaterDepth.Ctr",]$Result))/100
				wd.rctr = as.numeric(as.character(sub[sub$analyte.loc1=="StationWaterDepth.RCtr",]$Result))/100
					#all of the station water depths are in CM, need to convert to meters so final xsa output is m^2	

				xsa.bf.dry = 0.5*(bfw + ww)* bfd
				xsa.wet1.tri = (ww/4) * wd.lctr *0.5 
				xsa.wet1.rect = (ww/4) * wd.lctr					
					#first polygon treated as triangle or rectangle
				xsa.wet2 = (ww/4) * (wd.lctr + wd.ctr)*0.5
				xsa.wet3 = (ww/4) * (wd.ctr + wd.rctr)*0.5
				xsa.wet4.tri = (ww/4) * wd.rctr*0.5
				xsa.wet4.rect = (ww/4) * wd.rctr
					#last polygon treated as triangle or rectangle
				xsa.wet.sum.tri = xsa.wet1.tri + xsa.wet2 + xsa.wet3 + xsa.wet4.tri
				xsa.wet.sum.rect = xsa.wet1.rect + xsa.wet2 + xsa.wet3 + xsa.wet4.rect
				xsa.bf.tri = xsa.bf.dry + xsa.wet.sum.tri
				xsa.bf.rect = xsa.bf.dry + xsa.wet.sum.rect
				hyd.radius = xsa.bf.rect/((2*(bfd+wd.ctr))+bfw)
        sub.size.class = as.character(sub[sub$analyte.loc1=="Substrate.Size.Class.Ctr",]$Variable)
				  #hyd.radius is for manning's equation which is xsa.bf.rect/((bfd+wd.ctr)*2+ww)
				#outmat2[i,]$xs.bf = xsa.bf
				outmat2[i,]= c(as.character(stlist[i]), xsa.bf.tri, xsa.bf.rect, hyd.radius, sub.size.class, bfw)

			}
	}



outmat2[1:100,]
 write.csv(outmat2,file="xsa.CEDEN.csv")
class(stlist)

#now will cheat and use excel: text to columns to sep station.date.trans into sep columns (not sure how to do in r yet)
	xsa.CEDEN.cheat = read.csv("xsa.CEDEN.cheat.csv",header=TRUE,sep=",")
	names(xsa.CEDEN.cheat)
	station.trans =  paste(as.character(xsa.CEDEN.cheat$station), as.character(xsa.CEDEN.cheat$tran))
	station.trans2 = gsub(" ",".",station.trans)
		names(station.trans2)<-("station.trans2") 
	station.date = paste(as.character(xsa.CEDEN.cheat$station), as.character(xsa.CEDEN.cheat$date))
	station.date.unique = gsub(" ",".",station.date)
		names(station.date.unique)<-("station.date.unique") 
		#names() is to add column name to the vector I created
	date.2 = as.Date(xsa.CEDEN.cheat$date,origin="1970-01-01")
		#add to data frame using cbind(data, station.date.trans)
	xsa.CEDEN.cheat2 = cbind(xsa.CEDEN.cheat, station.trans2, date.2, station.date.unique)
	write.csv(xsa.CEDEN.cheat2, file="xsa.CEDEN.cheat2.csv")
		#file xsa.CEDEN.cheat2.csv is the matrix that I will now aggregate xsa based on 
names(xsa.CEDEN.cheat2)
#take out NA rows in excel, below function didn't work properly?
  #cut.xsa.data = na.omit(xsa.CEDEN.cheat2)
cut.xsa.data = read.csv("xsa.CEDEN.cheat2.noNAs.csv",header=TRUE,sep=",")
names(cut.xsa.data)
transect.average.rect = aggregate(cut.xsa.data$xsa.bf.rect,by=list(as.character(cut.xsa.data$station.date.unique)), FUN="mean")
transect.average.tri = aggregate(cut.xsa.data$xsa.bf.tri,by=list(cut.xsa.data$station.date.unique), FUN="mean")

write.csv(transect.average.rect, file="transect.average.rect.csv")
write.csv(transect.average.tri, file="transect.average.tri.csv")


transect.med.rect = aggregate(cut.xsa.data$xsa.bf.rect,by=list(cut.xsa.data$station.date.unique), FUN="median")
transect.med.tri = aggregate(cut.xsa.data$xsa.bf.tri,by=list(cut.xsa.data$station.date.unique), FUN="median")

write.csv(transect.med.rect, file="transect.med.rect.csv")
write.csv(transect.med.tri, file="transect.med.tri.csv")














#outmat2.cheat is the old script results, that didn't give NA to BFW and BFD that = 0
#now will cheat and use excel: text to columns to sep station.date.trans into sep columns (not sure how to do in r yet)
	outmat2.cheat = read.csv("outmat2.cheat.csv",header=TRUE,sep=",")
	names(outmat2.cheat)
	station.trans =  paste(as.character(outmat2.cheat$station), as.character(outmat2.cheat$tran))
	station.trans2 = gsub(" ",".",station.trans)
	names(station.trans2)<-("station.trans2") 
	#names() is to add column name to the vector I created
	date.2 = as.Date(outmat2.cheat$date,origin="1970-01-01")
	#add to data frame using cbind(data, station.date.trans)
	outmat2.cheat2 = cbind(outmat2.cheat, station.trans2, date.2)
	write.csv(outmat2.cheat2, file="outmat2.cheat2.csv")
#read in watershed area, impervious %, and ceden small watersheds <10mi2 to match stations
	ceden.small = read.csv("CEDEN_wtshds_10mi2_and_below.csv",header=TRUE,sep=",")
	names(ceden.small)
#cheated again, used vlookup in excel...use match like vlookup in r when i fix this
	#match(outmat2.cheat2$station.trans2, ceden.small$Station_Lo,nomatch = NA_integer_, incomparables = NULL)
	#not sure how to use match
#plot!
	outmat2.cheat2.vlookup = read.csv("outmat2.cheat2.vlookup.csv",header=TRUE,sep=",")
	names(outmat2.cheat2.vlookup)
	class(outmat2.cheat2.vlookup$date.2)

	y.2009.highimperv = outmat2.cheat2.vlookup$xsa.bf.rect[which(as.numeric(format(as.Date(outmat2.cheat2.vlookup$date.2, format="%m/%d/%Y"), "%Y")) == 2009 & outmat2.cheat2.vlookup$imperv.09>10)]
	x.2009.highimperv = outmat2.cheat2.vlookup$wtshd.area.mi2[which(as.numeric(format(as.Date(outmat2.cheat2.vlookup$date.2, format="%m/%d/%Y"), "%Y")) == 2009 & outmat2.cheat2.vlookup$imperv.09>10)]	
	
	y.2009.lowimperv = outmat2.cheat2.vlookup$xsa.bf.rect[which(as.numeric(format(as.Date(outmat2.cheat2.vlookup$date.2, format="%m/%d/%Y"), "%Y")) == 2009 & outmat2.cheat2.vlookup$imperv.09<10)]	
	x.2009.lowimperv= outmat2.cheat2.vlookup$wtshd.area.mi2[which(as.numeric(format(as.Date(outmat2.cheat2.vlookup$date.2, format="%m/%d/%Y"), "%Y")) == 2009 & outmat2.cheat2.vlookup$imperv.09<10)]

	plot(x.2009.highimperv,y.2009.highimperv, col="red", xlim=range(0:220),ylim=range(0:25))
	points(log(x.2009.lowimperv),log(y.2009.lowimperv), col="blue") #to add in another set of pts into the same graph from above, just make diff colors

plot(log(x.2009.highimperv),log(y.2009.highimperv), col="red", xlim=range(0:10),ylim=range(-2:5))
	points(x.2009.lowimperv,y.2009.lowimperv, col="blue") #to add in another set of pts into the same graph from above, just make diff colors


length(outmat2.cheat2.vlookup$date.2)


####AGGREGATE BY AVERAGE transects A-K for each site


















####Test loop that didn't work
	stlist = unique(pivot.matrix2$station.date.trans)

	outmat = matrix(1,length(stlist),2)
	outmat2 = as.data.frame(outmat)
	names(outmat2) <- c("station.date.trans",
				"xsa.bf")
	library(Hmisc)
	for (i in 1: 1000) {
		sub = pivot.matrix2[pivot.matrix2$station.date.trans==stlist[i],]
		d = as.data.frame(t(sub[,2:3]))
		names(d) <- sub$analyte.loc1 
		#later, change all numbers to numeric before we go into if, but for now do it for the wet1-4
		#d.2 = as.data.frame(as.numeric(as.character(d[2,])))		
		#names(d.2) <- sub$analyte.loc1 
class(sub$Result)
		#d.2 <- sapply(d[2,1:11],as.character)
		#d.3 = as.numeric(d.2)
		#rbind(names(d),d.3)
		
		outmat2[i,]$station.date.trans = as.character(stlist[i])
			if("Bankfull.Width." %nin% names(d)) {
				outmat2[i,]$xsa.bf = NA
			} else {
				xsa.bf.dry = 0.5*(as.numeric(as.character(d$Bankfull.Width.[2]))+ as.numeric(as.character(d$Wetted.Width.[2]))) * as.numeric(as.character(d$Bankfull.Height.[2]))
				xsa.wet1 = (as.numeric(as.character((d$Wetted.Width.[2]))/4) * as.numeric(as.character(d$StationWaterDepth.LCtr[2]))*0.5
				xsa.wet2 = (as.numeric(as.character(d$Wetted.Width.[2]))/4) * (as.numeric(as.character(d$StationWaterDepth.LCtr[2])) + as.numeric(as.character(d$StationWaterDepth.Ctr[2])))*0.5
				xsa.wet3 = (as.numeric(as.character(d$Wetted.Width.[2])/4) * (as.numeric(as.character(d$StationWaterDepth.Ctr[2]) + as.numeric(as.character(d$StationWaterDepth.RCtr[2]))*0.5
				xsa.wet4 = (as.numeric(as.character(d$Wetted.Width.[2]))/4) * as.numeric(as.character(d$StationWaterDepth.RCtr[2]))*0.5
				xsa.wet.sum = xsa.wet1 + xsa.wet2 + xsa.wet3 + xsa.wet4
				xsa.bf = xsa.bf.dry + xsa.wet.sum
				outmat2[i,]$xsa.bf = xsa.bf
			}
	}

outmat2[1:1000,]

























which(pivot.matrix2$station.date.trans==stlist[1])



#NOW DO A PIVOT TABLE to get the analyte column into individual columns with result as the value
	#install.packages('reshape')
	#library(reshape)
	#pivot.melt = melt(pivot.matrix2,id=c("station.date.location","Analyte"))
	#pivot.3 = cast(pivot.melt, station.date.location~Analyte)
#fun.aggregrate


	#pivot.3 = cast(pivot.matrix2, station.date.location ~ Analyte)
	#pivot.3[1:1000,]
	#now have ready matrix of unique id, analytes in each column, and results underneath
write.csv(pivot.3,file="pivot.3.cvs",row.names=FALSE)	
pivot.3$Wetted.Width







