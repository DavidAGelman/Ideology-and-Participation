#------------------------------------------------
#------------------------------------------------
# 'Walkthrough of implmenting W-NOMINATE for Delegates to the Constitutional Convention of 1787'
# Part of 'Ideology and Participation'
# written under version Version 0.99.486 of RStudio
# Data description and individual delegate analysis (both length and count)
# Origiinally written by DAG on 08/22/2016
# Cleaned and edited for purposes of positing for reproduction on 10/29/2017
#------------------------------------------------
#------------------------------------------------

# load in the needed libraries
library(wnominate) # for using the nominate procedure
library(pscl) # for use with nominate
library(foreign) # for using .dta files

#------------------------------------------------
# Notes
# The example at this link (ftp://voteview.com/wf1/wnominate_un_stata_file.r) provides an annotated example of how to implement
#W-NOMINATE from Poole and Rosenthal using a .dta [Stata data] file.

#For more examples of implementing W-NOMINATE, see http://www.voteview.com/wnominate_in_R.html
#For the .R file located at ftp://voteview.com/wf1/wnominate_un_stata_file.r, use "un31-33.dta".

#------------------------------------------------
# optional, set working directory
setwd("/Users/DavidAGelman/Documents/PROJECTS/Dissertation/Written/Chapter 1 (Convention)/PRQ Submission/Final_Submission")

#------------------------------------------------
#load in the roll call data for the Constitutional Convention published in ICPSR study 33865
#http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/33865
load("33865-0001-Data.rda")

#assign the data to an object
convention_matrix<-da33865.0001

#trim out the states and issues from the data so that all that's left are the delegates
delegates=cbind(convention_matrix[,1],convention_matrix[,17:71])

#rename the first column
colnames(delegates)[1]<-"ROLL_CALL"

#throw a "v" in front of the roll call number
delegates$ROLL_CALL=paste("v",delegates$ROLL_CALL,sep="")


#turn strings into numbers for the vote codes
delegates_matrix<-ifelse(delegates[,2:56]== "(0) delegate attended but his vote could not be inferred",0,
                         ifelse(delegates[,2:56]=="(1) delegate voted yea",1,
                                ifelse(delegates[,2:56]=="(2) delegate did not vote (because his delegation did not vote or he did not attend) but the delegate made a statement",2,
                                       ifelse(delegates[,2:56]== "(6) delegate voted nay",6,
                                              ifelse(delegates[,2:56]=="(7) delegate did not vote (because his delegation did not vote or he did not attend) but the delegate made a statement",7,
                                                     ifelse(delegates[,2:56]== "(8) delegate attended and made a statement related to this specific roll call but his state delegation did not vote and",8,
                                                            ifelse(delegates[,2:56]=="(9) delegate did not attend",9, NA)))))))

#make it a dataframe with the roll calls
delegates_matrix1<- data.frame(delegates_matrix)

#call the working frame something new
delegates_matrix<- delegates_matrix1

#make sure there are no NAs being induced (if done incorrectly, Mason gets NAs)
unique(delegates_matrix$GEORGE_MASON)

#------------------------------------------------
# For use with pscl package, you need to make the votes the columns and create a column of delegate names
# so transpose the matrix
data<-t(delegates_matrix)
#and now reformat the matrix as a data frame
data<-as.data.frame(data)

#now create a column of delegate names and then reorder the frame so that the names are the first column
data$Delegate<-rownames(data)
data=data[c(621,1:620)]

#------------------------------------------------
#now we start the NOMINATE procedure

#first we create the rollcall object
rc_convention<-rollcall(as.matrix(data[,-1]), #use the object called data for the data except the frist column (the names)
                        yea=c(1,2), #these are the yes vote codes based on the codebook
                        nay=c(6,7), #these are the nay vote codes based on the codebook
                        missing=c(0,8), #these are the missing vote codes based on the codebook
                        notInLegis=9, #this is the code for absent in the codebook
                        legis.names=data[,1], #use the first column for the legislators
                        desc="Convention",
                        vote.names=colnames(data)[-1]) #use all the columns except the first as the votes

#run a 1 dimension nominate with James Wilson as the anchor
result_1dim_Wilson <- wnominate(rc_convention, dims=1, polarity=c("JAMES_WILSON"))	
summary(result_1dim_Wilson)

#run a 1 dimension nominate with Gerry as the anchor
result_1dim_Gerry <- wnominate(rc_convention, dims=1, polarity=c("ELBRIDGE_GERRY"))	
summary(result_1dim_Gerry)

#run a 2 dimension nominate with Wilson as the anchor
result_2dim <- wnominate(rc_convention, dims=2, polarity=c("JAMES_WILSON","JAMES_WILSON"))	
summary(result_2dim)

#run a 2 dimension nominate with Gerry as the anchor
result_2dim_alternative <- wnominate(rc_convention, dims=2, polarity=c("ELBRIDGE_GERRY","ELBRIDGE_GERRY"))	
summary(result_2dim_alternative)

#plot the analytics
par(mar=c(2,2,2,2))
plot(result_2dim)

#make a data object with the legislator names, and the nominate coordinates
nominate<-cbind(result_2dim$legislators,result_2dim$legislators$coord1D,result_2dim$legislators$coord2D )
nominate<-cbind(rownames(nominate[1]),nominate$coord1D,nominate$coord2D )
nominate_2dim<-as.data.frame(nominate)

#need to make the nominate values numeric because they are factors
nominate_2dim[,2]<-as.numeric(as.character(nominate_2dim[,2]))
nominate_2dim[,3]<-as.numeric(as.character(nominate_2dim[,3]))

#since we want to look at things in order to eyeball them, we need to order the data
nominate_2dim_plotdata<-nominate_2dim[order(nominate_2dim$V2),]

#drop out legislators who didn't get a nominate score
nominate_2dim_plotdata<-na.omit(nominate_2dim_plotdata)

#now we're going to create a more visually appealing form of the name by taking the
#legisaltor last name and initial
nominate_2dim_plotdata$last_name=gsub("(.*)_","",nominate_2dim_plotdata$V1)
nominate_2dim_plotdata$first_name=gsub("_.*","",nominate_2dim_plotdata$V1)
nominate_2dim_plotdata$first_initial=strtrim(nominate_2dim_plotdata$first_name,1)
nominate_2dim_plotdata$plot_name=paste(nominate_2dim_plotdata$last_name,", ",nominate_2dim_plotdata$first_initial,sep="")
nominate_2dim_plotdata$plot_name[22]=paste(nominate_2dim_plotdata$plot_name[22],"C",sep="")

#now make a plot of the legislator name and 1st dim estimates, ordered by the estimate value from smallest to largest
del_2dim_plot<-ggplot(nominate_2dim_plotdata,aes(x=reorder(plot_name, V2))) + geom_point(aes(y=V2))
del_2dim_plot<- del_2dim_plot+ theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Delegate") + ylab("W-NOMINATE Score")
del_2dim_plot<- del_2dim_plot+ geom_hline(aes(y=0), linewidth=2, linetype="dashed", colour="gray40")
del_2dim_plot
ggsave("Wilson2DimPlot.pdf")


#run a 3dimension nominate with Wilson as the anchor
result_3dim <- wnominate(rc_convention, dims=3, polarity=c("JAMES_WILSON","JAMES_WILSON","JAMES_WILSON"))	
summary(result_3dim)

#save the legislator names and the coordinates
nominate2<-cbind(result_3dim$legislators,result_3dim$legislators$coord1D,result_3dim$legislators$coord2D,result_3dim$legislators$coord3D)
nominate2<-cbind(rownames(nominate2[1]),nominate2$coord1D,nominate2$coord2D,nominate2$coord3D )
nominate_3dim<-as.data.frame(nominate2)

#turn factors into numbers
nominate_3dim[,2]<-as.numeric(as.character(nominate_3dim[,2]))
nominate_3dim[,3]<-as.numeric(as.character(nominate_3dim[,3]))
nominate_3dim[,4]<-as.numeric(as.character(nominate_3dim[,4]))

#order the results and drop non-estimated legs.
nominate_3dim_plotdata<-nominate_3dim[order(nominate_3dim$V2),]
nominate_3dim_plotdata<-na.omit(nominate_3dim_plotdata)

#create data to compare the 1d estimates between the 2dim and 3dim model
nominate_compare<-merge(nominate_2dim,nominate_3dim,by="V1")
nominate_compare$difference=nominate_compare$V2.x-nominate_compare$V2.y

#------------------------------------------------
#------------------------------------------------
#------------------------------------------------
#------------------------------------------------
# Here I make 1 dimension plots using both Wilson and Gerry as anchors.
# Reproduces Figure 10 from the supplementary materials
# Wilson
nominate_Wilson<-cbind(result_1dim_Wilson$legislators,result_1dim_Wilson$legislators$coord1D)
nominate_Wilson<-cbind(rownames(nominate_Wilson[1]),nominate_Wilson$coord1D)
nominate_Wilson<-as.data.frame(nominate_Wilson)

nominate_Wilson[,2]<-as.numeric(as.character(nominate_Wilson[,2]))

nominate_Wilson_plotdata<-na.omit(nominate_Wilson)

nominate_Wilson_plotdata$last_name=gsub("(.*)_","",nominate_Wilson_plotdata$V1)
nominate_Wilson_plotdata$first_name=gsub("_.*","",nominate_Wilson_plotdata$V1)
nominate_Wilson_plotdata$first_initial=strtrim(nominate_Wilson_plotdata$first_name,1)
nominate_Wilson_plotdata$plot_name=paste(nominate_Wilson_plotdata$last_name,", ",nominate_Wilson_plotdata$first_initial,sep="")
nominate_Wilson_plotdata$plot_name[26]=paste(nominate_Wilson_plotdata$plot_name[26],"C",sep="")

Wilson_plot<-ggplot(nominate_Wilson_plotdata,aes(x=reorder(plot_name, V2))) + geom_point(aes(y=V2))
Wilson_plot<- Wilson_plot+ theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Delegate") + ylab("W-NOMINATE Score")
Wilson_plot<-Wilson_plot+ geom_hline(yintercept=0, size=1.25, linetype="dashed", colour="gray40")
print(Wilson_plot)

#------------------------------------------------
# Gerry
nominate_Gerry<-cbind(result_1dim_Gerry$legislators,result_1dim_Gerry$legislators$coord1D)
nominate_Gerry<-cbind(rownames(nominate_Gerry[1]),nominate_Gerry$coord1D)
nominate_Gerry<-as.data.frame(nominate_Gerry)

nominate_Gerry[,2]<-as.numeric(as.character(nominate_Gerry[,2]))

nominate_Gerry_plotdata<-na.omit(nominate_Gerry)

nominate_Gerry_plotdata$last_name=gsub("(.*)_","",nominate_Gerry_plotdata$V1)
nominate_Gerry_plotdata$first_name=gsub("_.*","",nominate_Gerry_plotdata$V1)
nominate_Gerry_plotdata$first_initial=strtrim(nominate_Gerry_plotdata$first_name,1)
nominate_Gerry_plotdata$plot_name=paste(nominate_Gerry_plotdata$last_name,", ",nominate_Gerry_plotdata$first_initial,sep="")
nominate_Gerry_plotdata$plot_name[26]=paste(nominate_Gerry_plotdata$plot_name[26],"C",sep="")

Gerry_plot<-ggplot(nominate_Gerry_plotdata,aes(x=reorder(plot_name, -V2))) + geom_point(aes(y=V2))
Gerry_plot<- Gerry_plot+ theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Delegate") + ylab("W-NOMINATE Score")
Gerry_plot<-Gerry_plot+ geom_hline(yintercept=0, size=1.25, linetype="dashed", colour="gray40")
print(Gerry_plot)
