#------------------------------------------------
#------------------------------------------------
# 'Ideology and Participation'
# written under version Version 0.99.486 of RStudio
# Madison's Notes Bias Assessment
# Origiinally written by DAG on 09/2/2014
# Cleaned and edited for purposes of positing for reproduction on 10/29/2017
#------------------------------------------------
#------------------------------------------------

#------------------------------------------------
# optional, set working directory
setwd("~")

#------------------------------------------------
# load in the data
DelegateDiaryBias=read.csv("DiaryDelegateBiasData.csv", header=TRUE)

# These t-tests are used to create Table 4

#subset by individual speaker
baldwin<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Baldwin")
nrow(baldwin)
#do a t.test betwen diarists
# not enough obervations
# t.test(baldwin$Madison_Proportion_Days_Text,baldwin$Yates_Proportion_Days_Text)


###
bedford<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Bedford")
nrow(bedford)
t.test(bedford$Madison_Proportion_Days_Text,bedford$Yates_Proportion_Days_Text)


###
Brearley<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Brearley")
nrow(Brearley)
# not enough obervations
# t.test(Brearley$Madison_Proportion_Days_Text,Brearley$Yates_Proportion_Days_Text)

###
Broom<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Broom")
nrow(Broom)
# not enough obervations
#t.test(Broom$Madison_Proportion_Days_Text,Broom$Yates_Proportion_Days_Text)

###
butler<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Butler")
nrow(butler)
t.test(butler$Madison_Proportion_Days_Text,butler$Yates_Proportion_Days_Text)

###
Davie<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Davie")
nrow(Davie)
t.test(Davie$Madison_Proportion_Days_Text,Davie$Yates_Proportion_Days_Text)

###
Dayton<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Dayton")
nrow(Dayton)
t.test(Dayton$Madison_Proportion_Days_Text,Dayton$Yates_Proportion_Days_Text)

###
Dickinson<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Dickinson")
nrow(Dickinson)
t.test(Dickinson$Madison_Proportion_Days_Text,Dickinson$Yates_Proportion_Days_Text)

###
Ellsworth<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Ellsworth")
nrow(Ellsworth)
t.test(Ellsworth$Madison_Proportion_Days_Text,Ellsworth$Yates_Proportion_Days_Text)

###
Franklin<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Franklin")
nrow(Franklin)
t.test(Franklin$Madison_Proportion_Days_Text,Franklin$Yates_Proportion_Days_Text)

###
Gerry<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Gerry")
nrow(Gerry)
t.test(Gerry$Madison_Proportion_Days_Text,Gerry$Yates_Proportion_Days_Text)

###
Gorham<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Gorham")
nrow(Gorham)
t.test(Gorham$Madison_Proportion_Days_Text,Gorham$Yates_Proportion_Days_Text)

###
Hamilton<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Hamilton")
nrow(Hamilton)
t.test(Hamilton$Madison_Proportion_Days_Text,Hamilton$Yates_Proportion_Days_Text)

###
Jenifer<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Jenifer")
nrow(Jenifer)
t.test(Jenifer$Madison_Proportion_Days_Text,Jenifer$Yates_Proportion_Days_Text)

###
Johnson<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Johnson")
nrow(Johnson)
t.test(Johnson$Madison_Proportion_Days_Text,Johnson$Yates_Proportion_Days_Text)

###
King<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="King")
nrow(King)
t.test(King$Madison_Proportion_Days_Text,King$Yates_Proportion_Days_Text)

###
Lansing<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Lansing")
nrow(Lansing)
t.test(Lansing$Madison_Proportion_Days_Text,Lansing$Yates_Proportion_Days_Text)

###
Madison<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Madison")
nrow(Madison)
t.test(Madison$Madison_Proportion_Days_Text,Madison$Yates_Proportion_Days_Text)

###
Martin_A<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Martin_A")
nrow(Martin_A)
t.test(Martin_A$Madison_Proportion_Days_Text,Martin_A$Yates_Proportion_Days_Text)

###
Martin_L<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Martin_L")
nrow(Martin_L)
t.test(Martin_L$Madison_Proportion_Days_Text,Martin_L$Yates_Proportion_Days_Text)

###
Mason<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Mason")
nrow(Mason)
t.test(Mason$Madison_Proportion_Days_Text,Mason$Yates_Proportion_Days_Text)

###
Morris_G<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Morris_G")
nrow(Morris_G)
t.test(Morris_G$Madison_Proportion_Days_Text,Morris_G$Yates_Proportion_Days_Text)

###
Morris_R<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Morris_R")
nrow(Morris_R)
t.test(Morris_R$Madison_Proportion_Days_Text,Morris_R$Yates_Proportion_Days_Text)

###
Paterson<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Paterson")
nrow(Paterson)
t.test(Paterson$Madison_Proportion_Days_Text,Paterson$Yates_Proportion_Days_Text)


###
Pierce<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Pierce")
nrow(Pierce)
t.test(Pierce$Madison_Proportion_Days_Text,Pierce$Yates_Proportion_Days_Text)


###
Pinckney_C<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Pinckney_C")
nrow(Pinckney_C)
t.test(Pinckney_C$Madison_Proportion_Days_Text,Pinckney_C$Yates_Proportion_Days_Text)


###
Pinckney_CC<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Pinckney_CC")
nrow(Pinckney_CC)
t.test(Pinckney_CC$Madison_Proportion_Days_Text,Pinckney_CC$Yates_Proportion_Days_Text)


###
Randolph<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Randolph")
nrow(Randolph)
t.test(Randolph$Madison_Proportion_Days_Text,Randolph$Yates_Proportion_Days_Text)


###
Read<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Read")
nrow(Read)
t.test(Read$Madison_Proportion_Days_Text,Read$Yates_Proportion_Days_Text)

###
Rutledge<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Rutledge")
nrow(Rutledge)
t.test(Rutledge$Madison_Proportion_Days_Text,Rutledge$Yates_Proportion_Days_Text)

###
Sherman<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Sherman")
nrow(Sherman)
t.test(Sherman$Madison_Proportion_Days_Text,Sherman$Yates_Proportion_Days_Text)

###
Spaight<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Spaight")
nrow(Spaight)
t.test(Spaight$Madison_Proportion_Days_Text,Spaight$Yates_Proportion_Days_Text)

###
Strong<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Strong")
nrow(Strong)
t.test(Strong$Madison_Proportion_Days_Text,Strong$Yates_Proportion_Days_Text)


###
Washington<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Washington")
nrow(Washington)
# not enough obervations
# t.test(Washington$Madison_Proportion_Days_Text,Washington$Yates_Proportion_Days_Text)

###
Williamson<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Williamson")
nrow(Williamson)
t.test(Williamson$Madison_Proportion_Days_Text,Williamson$Yates_Proportion_Days_Text)

###
Wilson<-subset(DelegateDiaryBias,DelegateDiaryBias$Delegate=="Wilson")
nrow(Wilson)
t.test(Wilson$Madison_Proportion_Days_Text,Wilson$Yates_Proportion_Days_Text)
