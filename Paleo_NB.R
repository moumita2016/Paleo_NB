# ######### Part I: pigment concentration computations ##############
# This part is based on Maltampec data only; it was meant to serve
# as a "practice".
#####################################################################
epath = file.path("C:/Users/Moumita")
epath
M=read.delim2(paste(epath,"/Post Doc at Shipgaan/First set data/Maltampec_pigs.txt",sep=""),header=TRUE)
names(M)
M$Fuco2=(M$Fuco/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/FucoMM)
summary(M$Fuco2)
M$Fuco2
edit(M)
plot(M$Median_depth_cm~ M$Fuco2, type="b", ylim=rev(c(0,50)), xlab="Pigment concentration (nmol/g OM)")
PeriMM=630.82
M$Peri2=(M$Peri/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/PeriMM)
summary(M$Peri2)
M$Peri2
AphaMM=
M$Apha2=(M$Apha/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/AphaMM)
summary(M$Apha2)
M$Apha2
MyxoMM=731.01
M$Myxo2=(M$Myxo/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/MyxoMM)
summary(M$Myxo2)
M$Myxo2
AlloxMM=564.84
M$Allox2=(M$Allox/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/AlloxMM)
summary(M$Allox2)
M$Allox2
DiatoxMM=566.86
M$Diatox2=(M$Diatox/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/DiatoxMM)
summary(M$Diatox2)
M$Diatox2
LutZeaMM=568.87
M$LutZea2=(M$LutZea/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/LutZeaMM)
summary(M$LutZea2)
M$LutZea2
CanthMM=564.82
M$Canth2=(M$Canth/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/CanthMM)
summary(M$Canth2)
M$Canth2
ChlbMM=907.49
M$Chlb2=(M$Chlb/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/ChlbMM)
summary(M$Chlb2)
M$Chlb2
EchiMM=550.86
M$Echi2=(M$Echi/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/EchiMM)
summary(M$Echi2)
M$Echi2
ChlaMM=893.51
M$Chla2=(M$Chla/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/ChlaMM)
summary(M$Chla2)
M$Chla2
alphacarotMM=536.87
M$alphacarot2=(M$alphacarot/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/alphacarotMM)
summary(M$alphacarot2)
M$alphacarot2
Beta_caroMM=536.89
M$Beta_caro2=(M$Beta_caro/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/Beta_caroMM)
summary(M$Beta_caro2)
M$Beta_caro2
PheoMM=871.19
M$Pheo2=(M$Pheo/M$Fraction_injected) / (M$Sediment_dry_mass_g * M$Organic_content_pct) * (1000/PheoMM)
summary(M$Pheo2)
M$Pheo2

png("MaltampecPigments.png")
plot(M$Median_depth_cm~ M$Fuco2, type="b", ylim=rev(c(0,50)), xlim=c(0,400),
     xlab="Pigment concentration (nmol/g OM)", pch="f", col=1)
points(M$Median_depth_cm~ M$Allox2, type="b", ylim=rev(c(0,50)), pch="a", col=2)
points(M$Median_depth_cm~ M$Diatox2, type="b", ylim=rev(c(0,50)), pch="d", col=3)
points(M$Median_depth_cm~ M$LutZea2, type="b", ylim=rev(c(0,50)), pch="l", col=4)
points(M$Median_depth_cm~ M$Canth2, type="b", ylim=rev(c(0,50)), pch="c", col=5)
points(M$Median_depth_cm~ M$Chlb2, type="b", ylim=rev(c(0,50)), pch="b", col=6)
points(M$Median_depth_cm~ M$Echi2, type="b", ylim=rev(c(0,50)), pch="e", col=7)
points(M$Median_depth_cm~ M$Chla2, type="b", ylim=rev(c(0,50)), pch="*", col=8)
points(M$Median_depth_cm~ M$Beta_caro2, type="b", ylim=rev(c(0,50)), pch="B", col=10)
title(main="Maltampec pigments as of 2015-11-17")
dev.off()

#############################################################################
# Convert pigment concentration into nmol per g OM
# based on the "Pigments_analyses_to_Moumita.txt" file that Alain
# sent to Moumita on 2016-
#############################################################################

epath = file.path("C:/Users/Moumita")
epath
allpigs=read.delim("C:/Users/Moumita/Post Doc at Shippagan/All pigments/Pigments_analyses_to_Moumita.txt",header=T)
summary(allpigs)
dim(allpigs)
# 212 rows x 27 columns
names(allpigs)

# Compute Fuco2 concentration in nmol/g for all records:
FucoMM=658.91
allpigs$Fuco2=(allpigs$Fuco/allpigs$Fraction_injected) / (allpigs$Sediment_dry_mass_g * allpigs$Organic_content_pct) * (1000/FucoMM)

summary(allpigs$Fuco2)
# Identify negative values by sorting Fuco2
allpigs[order(allpigs$Fuco2),c("Station","Fuco","Sediment_dry_mass_g", "Fuco2")]

# Let's make all negative values (probably due to calibration detection limit) equal to zero
allpigs$Fuco2[allpigs$Fuco2<0]=0

# Check result of precedent line of code
allpigs[order(allpigs$Fuco2),c("Station","DateCoreSampled","Fuco", "Fuco2")]

# Check distribution with the "stem" function
stem(allpigs$Fuco2)

# List values of Sediment dry mass used for extraction
allpigs$Sediment_dry_mass_g

# List values Sediment dry mass values that are Not Available
subset(allpigs, is.na(Sediment_dry_mass_g),select=c(Station, MedianDepth_cm, Sediment_dry_mass_g))

# Get the mean value of Sediment dry mass used for extraction
mean(allpigs$Sediment_dry_mass_g, na.rm=T)
# Replace missing values by the mean
allpigs$Sediment_dry_mass_g[is.na(allpigs$Sediment_dry_mass_g)]=mean(allpigs$Sediment_dry_mass_g,na.rm=T)

# Check that replacement is OK
allpigs$Sediment_dry_mass_g

# Check why we still have NAs for Fuco2
subset(allpigs,is.na(Fuco2),select=c("Station", "MedianDepth_cm", "Sediment_dry_mass_g", "Organic_content_pct","Fraction_injected", "Fuco","Fuco2"))

# There are NAs for "Organic_content_pct".
# Get a representative estimate of Organic content for those records with missing values (Tabusintac aval).
subset(allpigs, Station=="Tabusintac_AVAL", select=c(Station, MedianDepth_cm, Organic_content_pct))

# Get the mean Organic content of depths 83, 85, 87 cm (corresponding to rows 208, 209, 210):
allpigs[208:210,"Organic_content_pct"]
mean(allpigs[208:210,"Organic_content_pct"])

# Attribute mean to NAs
allpigs[,"Organic_content_pct"]
allpigs$Organic_content_pct[is.na(allpigs$Organic_content_pct)]=mean(allpigs[208:210,"Organic_content_pct"])
allpigs[,"Organic_content_pct"]

# Once all NAs are "fixed", recompute Fuco2:
allpigs$Fuco2=(allpigs$Fuco/allpigs$Fraction_injected) / (allpigs$Sediment_dry_mass_g * allpigs$Organic_content_pct) * (1000/FucoMM)

# Save data as csv, if you want
write.csv(allpigs,"allpigscsv.csv")

# If you want to save file elsewhere:
# write.csv(allpigs,"C:/path/to/directory/allpigscsv.csv")
epath = file.path("C:/Users/Moumita")
write.csv(allpigs("C:/epath,"/Post Doc at Shipgaan/allpigscsv.csv"))

##############################################################
# Save the "allpigs" data as RData, if you want...
#############################################
save(allpigs,file="allpigs.RData")
# Notice how RData files are smaller than equivalent csv files...

# Load a RData file
load("allpigs.RData")
# ... or if needed:
# load("C:/path/to/directory/allpigs.RData")

# Get the levels of factor "Station"----------------------
levels(allpigs$Station)

# Create a subset for "Caraquet AVAL
CarUppigs=subset(allpigs,Station=="Caraquet AVAL")
dim(CarUppigs) # 14 x 28
summary(CarUppigs$Fuco2)
summary(CarUppigs$MedianDepth_cm)

# Generate a plot
plot(CarUppigs$MedianDepth_cm~CarUppigs$Fuco2,ylim=rev(c(range(CarUppigs$MedianDepth_cm))),xlim=range(CarUppigs$Fuco2), type="b")

names(allpigs)
allpigs$Fuco2=(allpigs$Fuco/allpigs$Fraction_injected) / (allpigs$Sediment_dry_mass_g * allpigs$Organic_content_pct) * (1000/FucoMM)

PeriMM=630.82
allpigs$Peri2=(allpigs$Peri/allpigs$Fraction_injected) / (allpigs$Sediment_dry_mass_g * allpigs$Organic_content_pct) * (1000/PeriMM)
summary(allpigs$Peri2)
allpigs$Peri2

AphaMM=
allpigs$Apha2=(allpigs$Apha/allpigs$Fraction_injected) / (allpigs$Sediment_dry_mass_g * allpigs$Organic_content_pct) * (1000/AphaMM)
summary(allpigs$Apha2)
allpigs$Apha2

MyxoMM=731.01
allpigs$Myxo2=(allpigs$Myxo/allpigs$Fraction_injected) / (allpigs$Sediment_dry_mass_g * allpigs$Organic_content_pct) * (1000/MyxoMM)
summary(allpigs$Myxo2)
allpigs$Myxo2

AlloxMM=564.84
allpigs$Allox2=(allpigs$Allox/allpigs$Fraction_injected) / (allpigs$Sediment_dry_mass_g * allpigs$Organic_content_pct) * (1000/AlloxMM)
summary(allpigs$Allox2)
allpigs$Allox2

DiatoxMM=566.86
allpigs$Diatox2=(allpigs$Diatox/allpigs$Fraction_injected) / (allpigs$Sediment_dry_mass_g * allpigs$Organic_content_pct) * (1000/DiatoxMM)
summary(allpigs$Diatox2)
allpigs$Diatox2

LutZeaMM=568.87
allpigs$LutZea2=(allpigs$LutZea/allpigs$Fraction_injected) / (allpigs$Sediment_dry_mass_g * allpigs$Organic_content_pct) * (1000/LutZeaMM)
summary(allpigs$LutZea2)
allpigs$LutZea2

CanthMM=564.82
allpigs$Canth2=(allpigs$Canth/allpigs$Fraction_injected) / (allpigs$Sediment_dry_mass_g * allpigs$Organic_content_pct) * (1000/CanthMM)
summary(allpigs$Canth2)
allpigs$Canth2

allpigs[order(allpigs$Canth2),c("Station","Canth","Sediment_dry_mass_g", "Canth2")]
allpigs$Canth2[allpigs$Canth2<0]=0
allpigs[order(allpigs$Canth2),c("Station","DateCoreSampled","Canth", "Canth2")]
stem(allpigs$Canth2)
allpigs$Canth2=(allpigs$Canth/allpigs$Fraction_injected) / (allpigs$Sediment_dry_mass_g * allpigs$Organic_content_pct) * (1000/CanthMM)

ChlbMM=907.49
allpigs$Chlb2=(allpigs$Chlb/allpigs$Fraction_injected) / (allpigs$Sediment_dry_mass_g * allpigs$Organic_content_pct) * (1000/ChlbMM)
summary(allpigs$Chlb2)
allpigs$Chlb2

EchiMM=550.86
allpigs$Echi2=(allpigs$Echi/allpigs$Fraction_injected) / (allpigs$Sediment_dry_mass_g * allpigs$Organic_content_pct) * (1000/EchiMM)
summary(allpigs$Echi2)
allpigs$Echi2

ChlaMM=893.51
allpigs$Chla2=(allpigs$Chla/allpigs$Fraction_injected) / (allpigs$Sediment_dry_mass_g * allpigs$Organic_content_pct) * (1000/ChlaMM)
summary(allpigs$Chla2)
allpigs$Chla2

Alpha_carotMM=536.87
allpigs$Alpha_carot2=(allpigs$Alpha_carot/allpigs$Fraction_injected) / (allpigs$Sediment_dry_mass_g * allpigs$Organic_content_pct) * (1000/Alpha_carotMM)
summary(allpigs$Alpha_carot2)
allpigs$Alpha_carot2

Beta_caroMM=536.89
allpigs$Beta_caro2=(allpigs$Beta_caro/allpigs$Fraction_injected) / (allpigs$Sediment_dry_mass_g * allpigs$Organic_content_pct) * (1000/Beta_caroMM)
summary(allpigs$Beta_caro2)
allpigs$Beta_caro2

PheoMM=871.19
allpigs$Pheo2=(allpigs$Pheo/allpigs$Fraction_injected) / (allpigs$Sediment_dry_mass_g * allpigs$Organic_content_pct) * (1000/PheoMM)
summary(allpigs$Pheo2)
allpigs$Pheo2


##############################################
# Compute ratios MK and AP 2016-03-31
###############################################
allpigs$chla_Pheo=allpigs$Chla2/allpigs$Pheo2
write.csv(allpigs$chla_Pheo,"allpigs$chla_Pheo.csv")
summary(allpigs$chla_Pheo)
allpigs$Chla2
allpigs$Pheo2
edit(allpigs$chla_Pheo)

##=============================================================================================
# Creating subset for each station with Chla_Pheo
MK: 2014-04-15---------------------------------------
I was not sure how to calculate the raio between Chl a to Pheo, when Alain showed that to
me, I ran the script again to calculate the Chla:Pheo and then I saved for each 
station, to have them in separate file, for incouporating in the master analisis file. 
#================================================================================================
TabusiAVChla_Pheo=subset(allpigs, Station=="Tabusintac_AVAL", select=c(Station,chla_Pheo))
write.csv(allpigs$chla_Pheo,"allpigs$chla_Pheo.csv")
write.csv(TabusiAVChla_Pheo,"TabusiAVChla_Pheocsv.csv")

CarAVChla_Pheo=subset(allpigs, Station=="Caraquet AVAL", select=c(Station,chla_Pheo))
write.csv(CarAVChla_Pheo,"CarAVChla_Pheocsv.csv")

PokAMMaltChla_Pheo=subset(allpigs, Station=="Pokemouche_amont_Maltampec", select=c(Station,chla_Pheo))
write.csv(PokAMMaltChla_Pheo,"PokAMMaltChla_Pheocsv.csv")

PokAmWauChla_Pheo=subset(allpigs, Station=="Pokemouche_amont_Waugh", select=c(Station,chla_Pheo))
write.csv(PokAmWauChla_Pheo,"PokAmWaucsvChla_Pheo.csv")

PetAmChla_Pheo=subset(allpigs, Station=="Petite_Tracadie_amont", select=c(Station,chla_Pheo))
write.csv(PetAmChla_Pheo,"PetAmcsvChla_Pheo.csv")
##########################################################################################

##=============================================================================================
# Get the levels of factor "Station"
levels(allpigs$Station)

# Create a subset for "Caraquet AVAL
CarUppigs=subset(allpigs,Station=="Caraquet AVAL")
dim(CarUppigs) # 14 x 28
summary(CarUppigs$Fuco2)
summary(CarUppigs$MedianDepth_cm)

# Generate a plot
plot(CarUppigs$MedianDepth_cm~CarUppigs$Fuco2,ylim=rev(c(range(CarUppigs$MedianDepth_cm))),xlim=range(CarUppigs$Fuco2), type="b")
names(allpigs)

plot(CarUppigs$MedianDepth_cm~CarUppigs$Myxo2,ylim=rev(c(range(CarUppigs$MedianDepth_cm))),xlim=range(CarUppigs$Myxo2), type="b")
plot(CarUppigs$MedianDepth_cm~CarUppigs$Allox2,ylim=rev(c(range(CarUppigs$MedianDepth_cm))),xlim=range(CarUppigs$Allox2), type="b")
plot(CarUppigs$MedianDepth_cm~CarUppigs$Diatox2,ylim=rev(c(range(CarUppigs$MedianDepth_cm))),xlim=range(CarUppigs$Diatox2), type="b")
plot(CarUppigs$MedianDepth_cm~CarUppigs$Alpha_carot2,ylim=rev(c(range(CarUppigs$MedianDepth_cm))),xlim=range(CarUppigs$Alpha_carot2), type="b")

write.csv(CarUppigs,"CarUppigscsv.csv")
save(CarUppigs,file="CarUppigs.RData")

# Create a subset for Caraquet AVAL
CarUppigs=subset(allpigs,Station=="Caraquet AVAL")
dim(CarUppigs) # 14 x 28
summary(CarUppigs$Fuco2)

# Create a subset for Pokemouche_amont_Maltampec
PokAmMalt=subset(allpigs,Station=="Pokemouche_amont_Maltampec")
dim(PokAmMalt)
summary(PokAmMalt$Fuco2)
summary(PokAmMalt$MedianDepth_cm)

plot(PokAmMalt$MedianDepth_cm~PokAmMalt$Fuco2,ylim=rev(c(range(PokAmMalt$MedianDepth_cm))),xlim=range(PokAmMalt$Fuco2), type="b")

write.csv(PokAmMalt,"PokAmMaltcsv.csv")
save(PokAmMalt,file="PokAmMalt.RData")

# Create a subset for Pokemouche_amont_Waugh
PokAmWau=subset(allpigs,Station=="Pokemouche_amont_Waugh")
dim(PokAmWau)
summary(PokAmWau$Fuco2)
summary(PokAmWau$MedianDepth_cm)
plot(PokAmWau$MedianDepth_cm~PokAmWau$Fuco2,ylim=rev(c(range(PokAmWau$MedianDepth_cm))),xlim=range(PokAmWau$Fuco2), type="b")
write.csv(PokAmWau,"PokAmWaucsv.csv")
save(PokAmMalt,file="PokAmWau.RData")

#Create a subset for Pokemouche_aval_Lac_Inkerman

PokAvLacInk=subset(allpigs,Station=="Pokemouche_aval_Lac_Inkerman")
dim(PokAvLacInk)
summary(PokAvLacInk$Fuco2)
summary(PokAvLacInk$MedianDepth_cm)
plot(PokAvLacInk$MedianDepth_cm~PokAvLacInk$Fuco2,ylim=rev(c(range(PokAvLacInk$MedianDepth_cm))),xlim=range(PokAvLacInk$Fuco2), type="b")
write.csv(PokAvLacInk,"PokAvLacInkcsv.csv")
save(PokAvLacInk,file="PokAvLacInk.RData")

#Create a subset for Petite_Tracadie_amont
PetTraAm=subset(allpigs,Station=="Petite_Tracadie_amont")
dim(PetTraAm)
summary(PetTraAm$Fuco2)
summary(PetTraAm$MedianDepth_cm)
plot(PetTraAm$MedianDepth_cm~PetTraAm$Fuco2,ylim=rev(c(range(PetTraAm$MedianDepth_cm))),xlim=range(PetTraAm$Fuco2), type="b")
write.csv(PetTraAm,"PetTraAmcsv.csv")
save(PetTraAm,file="PetTraAm.RData")

#Create a subset for Petite_Tracadie_AVAL

PetTraAv=subset(allpigs,Station=="Petite_Tracadie_AVAL")
dim(PetTraAv)
summary(PetTraAv$Fuco2)
summary(PetTraAv$MedianDepth_cm)
plot(PetTraAv$MedianDepth_cm~PetTraAv$Fuco2,ylim=rev(c(range(PetTraAv$MedianDepth_cm))),xlim=range(PetTraAv$Fuco2), type="b")
write.csv(PetTraAv,"PetTraAvcsv.csv")
save(PetTraAv,file="PetTraAv.RData")

#Create a subset for Tabusintac_amont

TabusiAm=subset(allpigs,Station=="Tabusintac_amont")
dim(TabusiAm)
summary(TabusiAm$Fuco2)
summary(TabusiAm$MedianDepth_cm)
plot(TabusiAm$MedianDepth_cm~TabusiAm$Fuco2,ylim=rev(c(range(TabusiAm$MedianDepth_cm))),xlim=range(TabusiAm$Fuco2), type="b")
write.csv(TabusiAm,"TabusiAmcsv.csv")
save(TabusiAm,file="TabusiAm.RData")

#Create a subset for Tabusintac_AVAL

TabusiAV=subset(allpigs,Station=="Tabusintac_AVAL")
dim(TabusiAV)
summary(TabusiAV$Fuco2)
summary(TabusiAV$MedianDepth_cm)
plot(TabusiAV$MedianDepth_cm~TabusiAV$Fuco2,ylim=rev(c(range(TabusiAV$MedianDepth_cm))),xlim=range(TabusiAV$Fuco2), type="b")
write.csv(TabusiAV,"TabusiAVcsv.csv")
save(TabusiAV,file="TabusiAV.RData")

##############################################################
# First try at a correlation matrix
#############################################################
allpigssub1=subset(allpigs,select=c(Fuco2, Allox2, Diatox2, LutZea2, Canth2, Chla2, Beta_caro2, Pheo2))
dim(allpigssub1)
allpigssub2=subset(allpigs, MedianDepth_cm > 10)
dim(allpigssub2)
allpigssub3=subset(allpigs, Sample="Malt. 2010 11cm" | Sample="Malt. 2010 13cm", select=(Fuco2:Pheo2))
dim(allpigssub3)
allpigssub4=subset(allpigs,!is.na(Fuco2),select=(Fuco2:Pheo2))
cor(alpigssub4, use="pairwise.complete.obs",method="pearson")

########## Dec 1st 2015-12-01
# 1) Correlation for "Pokemouche_aval_Lac_Inkerman"

> allpigssub3=subset(allpigs, Station=="Pokemouche_aval_Lac_Inkerman")
> dim(allpigssub3)
[1] 29 38
> allpigssub3=subset(allpigs, Station=="Pokemouche_aval_Lac_Inkerman",select=(Fuco2:Pheo2))
> dim(allpigssub3)
[1] 29 11
>cor(allpigssub3, use="pairwise.complete.obs",method="pearson")

# 2) Correlation for "Pokemouche_amont_Maltampec"
>allpigssub3=subset(allpigs, Station=="Pokemouche_amont_Maltampec")
dim(allpigssub3)
>allpigssub3=subset(allpigs, Station=="Pokemouche_amont_Maltampec",select=(Fuco2:Pheo2))
dim(allpigssub3)
>cor(allpigssub3, use="pairwise.complete.obs",method="pearson")

# 3) Correlation for "Pokemouche_amont_Waugh"
>allpigssub3=subset(allpigs, Station=="Pokemouche_amont_Waugh")
dim(allpigssub3)
>allpigssub3=subset(allpigs, Station=="Pokemouche_amont_Waugh",select=(Fuco2:Pheo2))
dim(allpigssub3)
>cor(allpigssub3, use="pairwise.complete.obs",method="pearson")

# 4) Correlation for "Petite_Tracadie_AVAL"
>allpigssub3=subset(allpigs, Station=="Petite_Tracadie_AVAL")
dim(allpigssub3)
>allpigssub3=subset(allpigs, Station=="Petite_Tracadie_AVAL",select=(Fuco2:Pheo2))
dim(allpigssub3)
>cor(allpigssub3, use="pairwise.complete.obs",method="pearson")

# 5) Correlation for "Petite_Tracadie_amont"
>allpigssub3=subset(allpigs, Station=="Petite_Tracadie_amont")
dim(allpigssub3)
>allpigssub3=subset(allpigs, Station=="Petite_Tracadie_amont",select=(Fuco2:Pheo2))
dim(allpigssub3)
>cor(allpigssub3, use="pairwise.complete.obs",method="pearson")

# 6) Correlation for "Caraquet AVAL"
>allpigssub3=subset(allpigs, Station=="Caraquet AVAL")
dim(allpigssub3)
>allpigssub3=subset(allpigs, Station=="Caraquet AVAL",select=(Fuco2:Pheo2))
dim(allpigssub3)
>cor(allpigssub3, use="pairwise.complete.obs",method="pearson")

# 7) Correlation for "Tabusintac_amont"
>allpigssub3=subset(allpigs, Station=="Tabusintac_amont")
dim(allpigssub3)
>allpigssub3=subset(allpigs, Station=="Tabusintac_amont",select=(Fuco2:Pheo2))
dim(allpigssub3)
>cor(allpigssub3, use="pairwise.complete.obs",method="pearson")

# 8) Correlation for "Tabusintac_AVAL"
>allpigssub3=subset(allpigs, Station=="Tabusintac_AVAL")
dim(allpigssub3)
>allpigssub3=subset(allpigs, Station=="Tabusintac_AVAL",select=(Fuco2:Pheo2))
dim(allpigssub3)
>cor(allpigssub3, use="pairwise.complete.obs",method="pearson")

##########################################################################
##### First try at PCA; use the function prcomp 2015-12-01
##########################################################################

> prcomppigs=prcomp(pigments, scale.=TRUE)
> summary(prcomppigs)

> principal10pgs=principal(pigments, nfactors=2, rotate="varimax")
> principal10pgs
> par(mfrow=c(1,2)) Use "par(mfrow..." to combine severall plots (here on 1 row x 2 columns)
> biplot(prcomppigs)
> text(-2, -2, "prcomp")
> biplot(principal10pgs)
> text(-2,-2, "principal")

# Using this prcomp function in subset
allpigssub1=subset(allpigs,select=c(Fuco2, Allox2, Diatox2, LutZea2, Canth2, Chla2, Beta_caro2, Pheo2))
dim(allpigssub1)
allpigssub2=subset(allpigs, MedianDepth_cm > 10)
allpigssub3=subset(allpigs, Station=="Pokemouche_aval_Lac_Inkerman")
dim(allpigssub3)
allpigssub3=subset(allpigs, Station=="Pokemouche_aval_Lac_Inkerman",select=(Fuco2:Pheo2))
dim(allpigssub3)
prcomppigs=prcomp(allpigssub3, scale.=TRUE)
summary(prcomppigs)

> allpigssub1=subset(allpigs,select=c(Fuco2, Allox2, Diatox2, LutZea2, Canth2, Chla2, Beta_caro2, Pheo2))
> dim(allpigssub1)
[1] 212   8
> allpigssub2=subset(allpigs, MedianDepth_cm > 10)
> allpigssub3=subset(allpigs, Station=="Pokemouche_aval_Lac_Inkerman")
> dim(allpigssub3)
[1] 29 38
> allpigssub3=subset(allpigs, Station=="Pokemouche_aval_Lac_Inkerman",select=(Fuco2:Pheo2))
> dim(allpigssub3)
[1] 29 11
> prcomppigs=prcomp(allpigssub3, scale.=TRUE)
Error in prcomp.default(allpigssub3, scale. = TRUE) :
  cannot rescale a constant/zero column to unit variance
> summary(prcomppigs)

############# PCA Dec 2nd 2015-12-02
# Pokemouche downstream: Inkerman
allpigssub3=subset(allpigs, Station=="Pokemouche_aval_Lac_Inkerman" & !is.na(Fuco2),select=c(Fuco2, Allox2:Chla2,Beta_caro2,Pheo2))
dim(allpigssub3) # 28 x 10
prcomppigs=prcomp(allpigssub3, scale.=TRUE)
prcomppigs
png("prcompigs.png")
biplot(prcomppigs)
dev.off()

# Pokemouche upstream (main branch): Maltempec
allpigssub3=subset(allpigs, Station=="Pokemouche_amont_Maltampec" & !is.na(Fuco2),select=c(Fuco2, Allox2:Chla2,Beta_caro2,Pheo2))
dim(allpigssub3)
prcomppigs=prcomp(allpigssub3, scale.=TRUE)
prcomppigs
png("prcompigs.png")
biplot(prcomppigs)
dev.off()

# Pokemouche upstream tributary: Waugh
allpigssub3=subset(allpigs, Station=="Pokemouche_amont_Waugh" & !is.na(Fuco2),select=c(Fuco2, Allox2:Chla2,Beta_caro2,Pheo2))
dim(allpigssub3)
prcomppigs=prcomp(allpigssub3, scale.=TRUE)
prcomppigs
png("prcompigs.png")
biplot(prcomppigs)
dev.off()

allpigssub3=subset(allpigs, Station=="Petite_Tracadie_AVAL" & !is.na(Fuco2),select=c(Fuco2, Allox2:Chla2,Beta_caro2,Pheo2))
dim(allpigssub3)

# Petite Tracadie upstream
allpigssub3=subset(allpigs, Station=="Petite_Tracadie_amont" & !is.na(Fuco2),select=c(Fuco2, Allox2:Chla2,Beta_caro2,Pheo2))
dim(allpigssub3)

# Petite Tracadie downstream
allpigssub3=subset(allpigs, Station=="Caraquet AVAL" & !is.na(Fuco2),select=c(Fuco2, Allox2:Chlb2,Chla2,Beta_caro2,Pheo2))
prcomppigs=prcomp(allpigssub3, scale.=TRUE)

allpigssub3=subset(allpigs, Station=="Tabusintac_AVAL" & !is.na(Fuco2),select=c(Fuco2, Allox2:Chlb2,Chla2,Beta_caro2,Pheo2))
dim(allpigssub3)

allpigssub3=subset(allpigs, Station=="Tabusintac_AVAL" & !is.na(Fuco2),select=c(Fuco2, Allox2:Chla2,Beta_caro2,Pheo2))
dim(allpigssub3)

# Ratio between Chla to pheo
# AP 2016-04-13: see if the ratio code higher up can be deleted
allpigs$chlatopheo=allpigs$chla2/allpigs$pheo2

allpigssub3=subset(allpigs, Station=="Pokemouche_aval_Lac_Inkerman" & !is.na(Fuco2),select=c(Fuco2, Allox2:Chla2,Beta_caro2,Pheo2))

allpigssub3$chla to pheo=subset(allpigs, Station=="Pokemouche_aval_Lac_Inkerman" & !is.na(Fuco2),select=c(Chla2:Pheo2))

write.csv(allpigssub3$chlatopheo,"allpigscsv.csv")
save(allpigssub3,file="allpigssub3.RData")

save.image() http://www.statmethods.net/interface/workspace.html

#PCA summary for Waugh (AP 2016-04-13, again???)
> allpigssub3=subset(allpigs, Station=="Pokemouche_amont_Waugh" & !is.na(Fuco2),select=c(Fuco2, Allox2:Chla2,Beta_caro2,Pheo2))
> dim(allpigssub3)
[1] 23 10
> prcomppigs=prcomp(allpigssub3, scale.=TRUE)
> prcomppigs
> png("prcompigs.png")
> biplot(prcomppigs)
> plot(prcomppigs)
> plot(prcomppigs)
> dev.off()
null device
          1
> plot(prcomppigs)

# Dec 15th 2015
load("allpigs.RData")
load("CarUppigs.RData")
names(allpigs)
class(allpigs$SampleName)
levels(allpigs$SampleName)
summary(allpigs)
levels(allpigs$Station)

###############################################################################
# Create a subset of "allpigs" containing selected pigments
# AP 2016-04-13 TODO: need to explain why these pigments were selected here
################################################################################
> allpigssub1=subset(allpigs,select=c(Fuco2, Allox2, Diatox2, LutZea2, Canth2, Chla2, Beta_caro2, Pheo2))
> dim(allpigssub1)
[1] 212   8
> allpigssub2=subset(allpigs, MedianDepth_cm > 10)
> allpigssub3=subset(allpigs, Station=="Pokemouche_aval_Lac_Inkerman" & !is.na(Fuco2),select=c(Fuco2, Allox2:Chla2,Beta_caro2,Pheo2))
> dim(allpigssub3)
[1] 29 10
> prcomppigs=prcomp(allpigssub3, scale.=TRUE)
> prcomppigs
> summary(prcomppigs)

################################################################
# Part II: define periods for the meteo data
# Dec_2015_Moumita 2015-12
# Dec 16th_2015_Moumita 2015-12-16
##################################################################
library(car)
# to use function "scatterplot"
library(gdata)
# to use function "drop.cases"
oldpar <- par()
# to keep original graphic parameters

meteopath = file.path("C:/Users/Moumita")
meteopath
meteopath = file.path("/Post Doc at Shipgaan/151210_env_scripts_from_Alain_to_Moumita/Meteo_data")
setwd(meteopath)
getwd()

# AP 2016-04-13 TODO: File MBSTM etc was sent from AP to MK 2015...
# and originates from Frantz's work

load("MBSTMmLKkC.RData 2012-11-20.RData_last.RData")
meteot=subset(MBSTMmLKkC,select=c(-2:  -6, -8,-10,-12,-14,-16, -18, -20, -22, -24, -26, -28,-29: -33,-35,-37,-39,-41,-43, -45, -47, -49, -51, -53, -55,-56: -60,-62,-64,-66,-68,-70, -72, -74, -76, -78, -80, -82,-83: -87,-89,-91,-93,-95,-97, -99,-101,-103,-105,-107,-109,-110: -114,-116,-118,-120,-122,-124,-126,-128,-130,-132,-134,-136,-137: -141,-143,-145,-147,-149,-151,-153,-155,-157,-159,-161,-163,-164: -168,-170,-172,-174,-176,-178,-180,-182,-184,-186,-188,-190,-191: -195,-197,-199,-201,-203,-205,-207,-209,-211,-213,-215,-217,-218: -222,-224,-226,-228,-230,-232,-234,-236,-238,-240,-242,-244,-245: -249,-251,-253,-255,-257,-259,-261,-263,-265,-267,-269,-271))
dim(meteot)
#[1] 50222   114
subset(meteot,select=c(Date_fusion,Tmoy_C_Moncton, Tmoy_C_Bathurst))[c(1:10,100,500,1000,10000,20000,30000, 40000, 50200:50222),]
summary(subset(meteot,complete.cases(Tmoy_C_Moncton),select=c(Date_fusion,Tmoy_C_Moncton)))
summary(subset(meteot,complete.cases(Tmoy_C_Bathurst),select=c(Date_fusion,Tmoy_C_Bathurst)))
summary(subset(meteot,complete.cases(Precip_mm_Moncton),select=c(Date_fusion,Precip_mm_Moncton)))
summary(subset(meteot,complete.cases(Precip_mm_Bathurst),select=c(Date_fusion,Precip_mm_Bathurst)))
load("ty4.RData"); load("ty5.RData");load("ty6.RData"); load("ty7.RData"); load("ty8.RData")
summary(meteot$year_all)
save("meteot",file="meteot.RData")
summary(meteot$month_all)

# From Alain's Win8 computer on 2016-01-05:
load("C:/Users/alain/Documents/RECHERCHE_Labos_GIZC/_Analyses_redaction/Papier_Frantz_131126_MANUSCRIT/Analyses/Meteo/ty4.RData")

ty4$year_fact # NAs...
# Redefine the "year_fact" variable as so:
ty4$year_fact=as.factor(ty4$year_all)
ty4$year_fact
# The "Period" variable here is for Lake Inkerman, as defined in Frantz's work
subset(ty4,select=c(year_all, year_fact,Period, Precip_mm_Moncton3, Tmoy_C_Moncton3))


# ===== Periods for Maltampec, by Moumita 2015-12-17 ======
# =====(Dec 17th_2015_Maltempec 2010) ===========

# AP to MK 2016-01-05: We need to have a different name
# for each "Period" variable (one for Maltampec, one for Waugh...)
# Periods are defined in file "All dates together...xlsx"
# and are here based on the 3rd CRS age estimates.
# These age estimates were defined in individual Excel files
# (one per core station) by Moumita
#=================================================================
######################################################################
#MK: 2016-04-15
Periods are corrected based on the 3rd series of CRS age (Binford), 
corrected by AP, on 2016-04-15
##########################################################################

# 1) Maltampec April----------------------------------------
ty4$PeriodMalt[ty4$year_all>=1876 & ty4$year_all<=1908]=("P1")
ty4$PeriodMalt[ty4$year_all>=1909 & ty4$year_all<=1936]=("P2")
ty4$PeriodMalt[ty4$year_all>=1937 & ty4$year_all<=1960]=("P3")
ty4$PeriodMalt[ty4$year_all>=1961 & ty4$year_all<=1983]=("P4")
ty4$PeriodMalt[ty4$year_all>=1984 & ty4$year_all<=2010]=("P5")
ty4$PeriodMalt=as.factor(ty4$PeriodMalt)
summary(ty4$PeriodMalt)
dim(ty4)
#[1] 137 121

# --- Maltampec May ------------------
ty5$PeriodMalt[ty5$year_all>=1876 & ty5$year_all<=1908]=("P1")
ty5$PeriodMalt[ty5$year_all>=1909 & ty5$year_all<=1936]=("P2")
ty5$PeriodMalt[ty5$year_all>=1937 & ty5$year_all<=1960]=("P3")
ty5$PeriodMalt[ty5$year_all>=1961 & ty5$year_all<=1983]=("P4")
ty5$PeriodMalt[ty5$year_all>=1984 & ty5$year_all<=2010]=("P5")
ty5$PeriodMalt=as.factor(ty5$PeriodMalt)
summary(ty5$PeriodMalt)
dim(ty5)
#[1] 137 121

# --- Maltampec June -----------------
ty6$PeriodMalt[ty6$year_all>=1876 & ty6$year_all<=1908]=("P1")
ty6$PeriodMalt[ty6$year_all>=1909 & ty6$year_all<=1936]=("P2")
ty6$PeriodMalt[ty6$year_all>=1937 & ty6$year_all<=1960]=("P3")
ty6$PeriodMalt[ty6$year_all>=1961 & ty6$year_all<=1983]=("P4")
ty6$PeriodMalt[ty6$year_all>=1984 & ty6$year_all<=2010]=("P5")
ty6$PeriodMalt=as.factor(ty6$PeriodMalt)
summary(ty6$PeriodMalt)
dim(ty6)
#[1] 137 121

# --- Maltampec July ------------------------
ty7$PeriodMalt[ty7$year_all>=1876 & ty7$year_all<=1908]=("P1")
ty7$PeriodMalt[ty7$year_all>=1909 & ty7$year_all<=1936]=("P2")
ty7$PeriodMalt[ty7$year_all>=1937 & ty7$year_all<=1960]=("P3")
ty7$PeriodMalt[ty7$year_all>=1961 & ty7$year_all<=1983]=("P4")
ty7$PeriodMalt[ty7$year_all>=1984 & ty7$year_all<=2010]=("P5")
ty7$PeriodMalt=as.factor(ty7$PeriodMalt)
summary(ty7$PeriodMalt)
dim(ty7)
#[1] 138 121

# --- Maltampec August -------------------
ty8$PeriodMalt[ty8$year_all>=1876 & ty8$year_all<=1908]=("P1")
ty8$PeriodMalt[ty8$year_all>=1909 & ty8$year_all<=1936]=("P2")
ty8$PeriodMalt[ty8$year_all>=1937 & ty8$year_all<=1960]=("P3")
ty8$PeriodMalt[ty8$year_all>=1961 & ty8$year_all<=1983]=("P4")
ty8$PeriodMalt[ty8$year_all>=1984 & ty8$year_all<=2010]=("P5")
ty8$PeriodMalt=as.factor(ty8$PeriodMalt)
summary(ty8$PeriodMalt)
dim(ty8)
#[1] 138 121

# =======================================================
# Aggregate data by period, Dec17th_2015-12-17
# AP to MK 2016-01-05:
# Now, we need to create Meteo files specific to each station,
# because each station have different period definitions
# reflecting the specific sedimentation history of each station.
# Hence, the names of files will harbour the station name,
# like so:

# Maltampec April -----------------------------
ty4PMalt=aggregate(ty4, by=list(ty4$PeriodMalt), FUN=mean, na.rm=TRUE)
dim(ty4PMalt)
# 5 x 122 
ty4PMalt[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty4PMalt", file="ty4PMalt.RData")

# Maltampec May -----------------------------
ty5PMalt=aggregate(ty5, by=list(ty5$PeriodMalt), FUN=mean, na.rm=TRUE)
dim(ty5PMalt)
# 5 x 122
ty5PMalt[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty5PMalt", file="ty5PMalt.RData")

# Maltampec June -----------------------------
ty6PMalt=aggregate(ty6, by=list(ty6$PeriodMalt), FUN=mean, na.rm=TRUE)
dim(ty6PMalt)
# 5 x 122
ty6PMalt[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty6PMalt", file="ty6PMalt.RData")

# Maltampec July -----------------------------
ty7PMalt=aggregate(ty7, by=list(ty7$PeriodMalt), FUN=mean, na.rm=TRUE)
dim(ty7PMalt)
# 5 x 122
ty7PMalt[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty7PMalt", file="ty7PMalt.RData")

# Maltampec August -----------------------------
ty8PMalt=aggregate(ty8, by=list(ty8$PeriodMalt), FUN=mean, na.rm=TRUE)
dim(ty8PMalt)
#[1]   5 122
ty8PMalt[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty8PMalt", file="ty8PMalt.RData")
#========================================================================
# AP to MK 2016-01-05: create a new series of tables containing only the
# variables of interest

ty4PMaltbis=ty4PMalt[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty4PMaltbis)
# 5 x 3 by AP on 2016-01-05
ty5PMaltbis=ty5PMalt[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty5PMaltbis)
# 5 x 3
ty6PMaltbis=ty6PMalt[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty6PMaltbis)
#[1] 5 3
ty7PMaltbis=ty7PMalt[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty7PMaltbis)
#[1] 5 3
ty8PMaltbis=ty8PMalt[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty8PMaltbis)
#[1] 5 3

# AP to MK 2016-01-05:
# Make one single file by binding columns with the "cbind" function
meteoMonctonMalt=c(ty4PMaltbis,ty5PMaltbis,ty6PMaltbis,ty7PMaltbis,ty8PMaltbis)
save("meteoMonctonMalt",file="meteoMonctonMalt.RData")
write.csv(meteoMonctonMalt,"meteoMonctonMalt.csv")

# ================ Periods for Waugh =================
# ========== corrected by MK 2016-04-15 =============

# ---- Waugh April ---------------------------
class(ty4$year_all) # numeric, so we should be able to do maths on it
stem(ty4$year_all)
ty4$PeriodWaugh=as.numeric(ty4$PeriodWaugh)
# Define as numeric first if you get error message
# "invalid factor level, NA generated"
ty4$PeriodWaugh[ty4$year_all>=1943 & ty4$year_all<=1963]="P1"
ty4$PeriodWaugh[ty4$year_all>=1964 & ty4$year_all<=1982]="P2"
ty4$PeriodWaugh[ty4$year_all>=1983 & ty4$year_all<=1996]="P3"
ty4$PeriodWaugh[ty4$year_all>=1997 & ty4$year_all<=2010]="P4"
ty4$PeriodWaugh=as.factor(ty4$PeriodWaugh)
summary(ty4$PeriodWaugh)
dim(ty4)
#[1] 137 121

# --- Waugh May ------------------------
ty5$PeriodWaugh[ty5$year_all>=1943 & ty5$year_all<=1963]=("P1")
ty5$PeriodWaugh[ty5$year_all>=1964 & ty5$year_all<=1982]=("P2")
ty5$PeriodWaugh[ty5$year_all>=1983 & ty5$year_all<=1996]=("P3")
ty5$PeriodWaugh[ty5$year_all>=1997 & ty5$year_all<=2010]=("P4")
ty5$PeriodWaugh=as.factor(ty5$PeriodWaugh)
summary(ty5$PeriodWaugh)
dim(ty5)
[1] 137 121

class(ty6$year_all) # numeric, so we should be able to do maths on it
stem(ty6$year_all)
ty6$PeriodWaugh=as.numeric(ty6$PeriodWaugh)
# Define as numeric first if you get error message
# "invalid factor level, NA generated"

# --- Waugh June -------------------------
ty6$PeriodWaugh[ty6$year_all>=1943 & ty6$year_all<=1963]=("P1")
ty6$PeriodWaugh[ty6$year_all>=1964 & ty6$year_all<=1982]=("P2")
ty6$PeriodWaugh[ty6$year_all>=1983 & ty6$year_all<=1996]=("P3")
ty6$PeriodWaugh[ty6$year_all>=1997 & ty6$year_all<=2010]=("P4")
ty6$PeriodWaugh=as.factor(ty6$PeriodWaugh)
summary(ty6$PeriodWaugh)
dim(ty6)
[1] 137 121

class(ty7$year_all) # numeric, so we should be able to do maths on it
stem(ty7$year_all)
ty7$PeriodWaugh=as.numeric(ty7$PeriodWaugh)
# Define as numeric first if you get error message
# "invalid factor level, NA generated"

# --- Waugh July -------------------------
ty7$PeriodWaugh[ty7$year_all>=1943 & ty7$year_all<=1963]=("P1")
ty7$PeriodWaugh[ty7$year_all>=1964 & ty7$year_all<=1982]=("P2")
ty7$PeriodWaugh[ty7$year_all>=1983 & ty7$year_all<=1996]=("P3")
ty7$PeriodWaugh[ty7$year_all>=1997 & ty7$year_all<=2010]=("P4")
ty7$PeriodWaugh=as.factor(ty7$PeriodWaugh)
summary(ty7$PeriodWaugh)
dim(ty7)
#[1] 138 121

class(ty8$year_all) # numeric, so we should be able to do maths on it
stem(ty8$year_all)
ty8$PeriodWaugh=as.numeric(ty8$PeriodWaugh)

# --- Waugh August -------------------------------
ty8$PeriodWaugh[ty8$year_all>=1943 & ty8$year_all<=1963]=("P1")
ty8$PeriodWaugh[ty8$year_all>=1964 & ty8$year_all<=1982]=("P2")
ty8$PeriodWaugh[ty8$year_all>=1983 & ty8$year_all<=1996]=("P3")
ty8$PeriodWaugh[ty8$year_all>=1997 & ty8$year_all<=2010]=("P4")
ty8$PeriodWaugh=as.factor(ty8$PeriodWaugh)
summary(ty8$PeriodWaugh)
dim(ty8)
#[1] 138 121

#=======================================================
# Waugh April -----------------------------
ty4PWau=aggregate(ty4, by=list(ty4$PeriodWaugh), FUN=mean, na.rm=TRUE)
dim(ty4PWau)
# 4 x 122
ty4PWau[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty4PWau", file="ty4PWau.RData")

# Waugh May -----------------------------
ty5PWau=aggregate(ty5, by=list(ty5$PeriodWaugh), FUN=mean, na.rm=TRUE)
dim(ty5PWau)
# 4 x 122
ty5PWau[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty5PWau", file="ty5PWau.RData")

# Waugh June -----------------------------
ty6PWau=aggregate(ty6, by=list(ty6$PeriodWaugh), FUN=mean, na.rm=TRUE)
dim(ty6PWau)
# 4 x 122
ty6PWau[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty6PWau", file="ty6PWau.RData")

# Waugh July -----------------------------
ty7PWau=aggregate(ty7, by=list(ty7$PeriodWaugh), FUN=mean, na.rm=TRUE)
dim(ty7PWau)
# 4 x 122
ty7PWau[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty7PWau", file="ty7PWau.RData")

# Waugh August -----------------------------
ty8PWau=aggregate(ty8, by=list(ty8$PeriodWaugh), FUN=mean, na.rm=TRUE)
dim(ty8PWau)
# 6 x 122
ty8PWau[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty8PWau", file="ty8PWau.RData")
#====================================================================
MK 2016-01-09: create a new series of tables containing only the
# variables of interest

ty4PWaubis=ty4PWau[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty4PWaubis)
# 4 x 3
ty5PWaubis=ty5PWau[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty5PWaubis)
# 4 x 3
ty6PWaubis=ty6PWau[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty6PWaubis)
# 4 x 3
ty7PWaubis=ty7PWau[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty7PWaubis)
# 4 x 3
ty8PWaubis=ty8PWau[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty8PWaubis)
# 4 x 3

#Jan 9th 2016==============================================
meteoMonctonWau=c(ty4PWaubis,ty5PWaubis,ty6PWaubis,ty7PWaubis,ty8PWaubis)
save("meteoMonctonWau",file="meteoMonctonWau.RData")
write.csv(meteoMonctonWau,"meteoMonctonWau.csv")

# ================ Periods for Petite Tracadie =================
# --- Petite Tracadie April ---------------
# All the seperation of periods based on CRS 3rd model (Binford, 1990)
#_can be found in file "all_dates_together" excel sheet
# P1 does not exsist as the first environmental data starts at 1873.
# Corrected periods by MK done on 2016-04-15-----------------------------------------------------

ty4$PeriodPetitAM[ty4$year_all>=1839 & ty4$year_all<=1879]=("P1")
ty4$PeriodPetitAM[ty4$year_all>=1880 & ty4$year_all<=1908]=("P2")
ty4$PeriodPetitAM[ty4$year_all>=1909 & ty4$year_all<=1934]=("P3")
ty4$PeriodPetitAM[ty4$year_all>=1935 & ty4$year_all<=1958]=("P4")
ty4$PeriodPetitAM[ty4$year_all>=1959 & ty4$year_all<=1972]=("P5")
ty4$PeriodPetitAM[ty4$year_all>=1973 & ty4$year_all<=1981]=("P6")
ty4$PeriodPetitAM[ty4$year_all>=1982 & ty4$year_all<=1989]=("P7")
ty4$PeriodPetitAM[ty4$year_all>=1990 & ty4$year_all<=1994]=("P8")
ty4$PeriodPetitAM[ty4$year_all>=1995 & ty4$year_all<=2000]=("P9")
ty4$PeriodPetitAM[ty4$year_all>=2001 & ty4$year_all<=2004]=("P10")
ty4$PeriodPetitAM[ty4$year_all>=2005 & ty4$year_all<=2010]=("P11")
ty4$PeriodPetitAM=as.factor(ty4$PeriodPetitAM)
summary(ty4$PeriodPetitAM)
dim(ty4)
[1] 137 121
# --- Petite Tracadie May ---------------
ty5$PeriodPetitAM[ty5$year_all>=1839 & ty5$year_all<=1879]=("P1")
ty5$PeriodPetitAM[ty5$year_all>=1880 & ty5$year_all<=1908]=("P2")
ty5$PeriodPetitAM[ty5$year_all>=1909 & ty5$year_all<=1934]=("P3")
ty5$PeriodPetitAM[ty5$year_all>=1935 & ty5$year_all<=1958]=("P4")
ty5$PeriodPetitAM[ty5$year_all>=1959 & ty5$year_all<=1972]=("P5")
ty5$PeriodPetitAM[ty5$year_all>=1973 & ty5$year_all<=1981]=("P6")
ty5$PeriodPetitAM[ty5$year_all>=1982 & ty5$year_all<=1989]=("P7")
ty5$PeriodPetitAM[ty5$year_all>=1990 & ty5$year_all<=1994]=("P8")
ty5$PeriodPetitAM[ty5$year_all>=1995 & ty5$year_all<=2000]=("P9")
ty5$PeriodPetitAM[ty5$year_all>=2001 & ty5$year_all<=2004]=("P10")
ty5$PeriodPetitAM[ty5$year_all>=2005 & ty5$year_all<=2010]=("P11")
ty5$PeriodPetitAM=as.factor(ty5$PeriodPetitAM)
summary(ty5$PeriodPetitAM)
dim(ty5)
[1] 137 121
# --- Petite Tracadie June ---------------

ty6$PeriodPetitAM[ty6$year_all>=1839 & ty6$year_all<=1879]=("P1")
ty6$PeriodPetitAM[ty6$year_all>=1880 & ty6$year_all<=1908]=("P2")
ty6$PeriodPetitAM[ty6$year_all>=1909 & ty6$year_all<=1934]=("P3")
ty6$PeriodPetitAM[ty6$year_all>=1935 & ty6$year_all<=1958]=("P4")
ty6$PeriodPetitAM[ty6$year_all>=1959 & ty6$year_all<=1972]=("P5")
ty6$PeriodPetitAM[ty6$year_all>=1973 & ty6$year_all<=1981]=("P6")
ty6$PeriodPetitAM[ty6$year_all>=1982 & ty6$year_all<=1989]=("P7")
ty6$PeriodPetitAM[ty6$year_all>=1990 & ty6$year_all<=1994]=("P8")
ty6$PeriodPetitAM[ty6$year_all>=1995 & ty6$year_all<=2000]=("P9")
ty6$PeriodPetitAM[ty6$year_all>=2001 & ty6$year_all<=2004]=("P10")
ty6$PeriodPetitAM[ty6$year_all>=2005 & ty6$year_all<=2010]=("P11")
ty6$PeriodPetitAM=as.factor(ty6$PeriodPetitAM)
summary(ty6$PeriodPetitAM)
dim(ty6)
[1] 137 121
# --- Petite Tracadie July ---------------
ty7$PeriodPetitAM[ty7$year_all>=1839 & ty7$year_all<=1879]=("P1")
ty7$PeriodPetitAM[ty7$year_all>=1880 & ty7$year_all<=1908]=("P2")
ty7$PeriodPetitAM[ty7$year_all>=1909 & ty7$year_all<=1934]=("P3")
ty7$PeriodPetitAM[ty7$year_all>=1935 & ty7$year_all<=1958]=("P4")
ty7$PeriodPetitAM[ty7$year_all>=1959 & ty7$year_all<=1972]=("P5")
ty7$PeriodPetitAM[ty7$year_all>=1973 & ty7$year_all<=1981]=("P6")
ty7$PeriodPetitAM[ty7$year_all>=1982 & ty7$year_all<=1989]=("P7")
ty7$PeriodPetitAM[ty7$year_all>=1990 & ty7$year_all<=1994]=("P8")
ty7$PeriodPetitAM[ty7$year_all>=1995 & ty7$year_all<=2000]=("P9")
ty7$PeriodPetitAM[ty7$year_all>=2001 & ty7$year_all<=2004]=("P10")
ty7$PeriodPetitAM[ty7$year_all>=2005 & ty7$year_all<=2010]=("P11")
ty7$PeriodPetitAM=as.factor(ty7$PeriodPetitAM)
summary(ty7$PeriodPetitAM)
dim(ty7)
[1] 138 121
# --- Petite Tracadie August ---------------

ty8$PeriodPetitAM[ty8$year_all>=1839 & ty8$year_all<=1879]=("P1")
ty8$PeriodPetitAM[ty8$year_all>=1880 & ty8$year_all<=1908]=("P2")
ty8$PeriodPetitAM[ty8$year_all>=1909 & ty8$year_all<=1934]=("P3")
ty8$PeriodPetitAM[ty8$year_all>=1935 & ty8$year_all<=1958]=("P4")
ty8$PeriodPetitAM[ty8$year_all>=1959 & ty8$year_all<=1972]=("P5")
ty8$PeriodPetitAM[ty8$year_all>=1973 & ty8$year_all<=1981]=("P6")
ty8$PeriodPetitAM[ty8$year_all>=1982 & ty8$year_all<=1989]=("P7")
ty8$PeriodPetitAM[ty8$year_all>=1990 & ty8$year_all<=1994]=("P8")
ty8$PeriodPetitAM[ty8$year_all>=1995 & ty8$year_all<=2000]=("P9")
ty8$PeriodPetitAM[ty8$year_all>=2001 & ty8$year_all<=2004]=("P10")
ty8$PeriodPetitAM[ty8$year_all>=2005 & ty8$year_all<=2010]=("P11")
ty8$PeriodPetitAM=as.factor(ty8$PeriodPetitAM)
summary(ty8$PeriodPetitAM)
dim(ty8)
[1] 138 121
#======================================================================
#Petite Travadie Amont April---------------------------
ty4PPetit=aggregate(ty4, by=list(ty4$PeriodPetitAM), FUN=mean, na.rm=TRUE)
dim(ty4PPetit)
# 11 x 122
ty4PPetit[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty4PPetit", file="ty4PPetit.RData")

# Petite Tracadie May -----------------------------
ty5PPetit=aggregate(ty5, by=list(ty5$PeriodPetitAM), FUN=mean, na.rm=TRUE)
dim(ty5PPetit)
# 11 x 122
ty5PPetit[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty5PPetit", file="ty5PPetit.RData")

# Petitte Tracadie June -----------------------------
ty6PPetit=aggregate(ty6, by=list(ty6$PeriodPetitAM), FUN=mean, na.rm=TRUE)
dim(ty6PPetit)
# 11 x 122
ty6PPetit[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty6PPetit", file="ty6PPetit.RData")

# Petite Tracadie July -----------------------------
ty7PPetit=aggregate(ty7, by=list(ty7$PeriodPetitAM), FUN=mean, na.rm=TRUE)
dim(ty7PPetit)
# 11 x 122
ty7PPetit[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty7PPetit", file="ty7PPetit.RData")

# Petite Tracadie August -----------------------------
ty8PPetit=aggregate(ty8, by=list(ty8$PeriodPetitAM), FUN=mean, na.rm=TRUE)
dim(ty8PPetit)
# 11 x 122
ty8PPetit[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty8PPetit", file="ty8PPetit.RData")

# ======================================================================
# MK 2016-01-10: create a new series of tables containing only the
# variables of interest

ty4PPetitbis=ty4PPetit[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty4PPetitbis)
# [1] 11  3
ty5PPetitbis=ty5PPetit[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty5PPetitbis)
#[1] 11  3
ty6PPetitbis=ty6PPetit[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty6PPetitbis)
#[1] 11  3
ty7PPetitbis=ty7PPetit[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty7PPetitbis)
#[1] 11  3
ty8PPetitbis=ty8PPetit[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty8PPetitbis)
#[1] 11  3
meteoMonctonPetit=c(ty4PPetitbis,ty5PPetitbis,ty6PPetitbis,ty7PPetitbis,ty8PPetitbis)
save("meteoMonctonPetit",file="meteoMonctonPetit.RData")
write.csv(meteoMonctonPetit,"meteoMonctonPetit.csv")

#==================Shippagan west=============================
# MK corrected on 2016-04-15
#---- Shippagan west April ---------------
class(ty4$year_all) # numeric, so we should be able to do maths on it
stem(ty4$year_all)
ty4$PeriodShiwes=as.numeric(ty4$PeriodShiwes)

ty4$PeriodShiwes[ty4$year_all>=1916 & ty4$year_all<=1954]=("P1")
ty4$PeriodShiwes[ty4$year_all>=1955 & ty4$year_all<=1984]=("P2")
ty4$PeriodShiwes[ty4$year_all>=1985 & ty4$year_all<=2000]=("P3")
ty4$PeriodShiwes[ty4$year_all>=2001 & ty4$year_all<=2010]=("P4")
ty4$PeriodShiwes=as.factor(ty4$PeriodShiwes)
summary(ty4$PeriodShiwes)
dim(ty4)
#[1] 137 121
# --- Shippagan west May ------------------
ty5$PeriodShiwes[ty5$year_all>=1916 & ty5$year_all<=1954]=("P1")
ty5$PeriodShiwes[ty5$year_all>=1955 & ty5$year_all<=1984]=("P2")
ty5$PeriodShiwes[ty5$year_all>=1985 & ty5$year_all<=2000]=("P3")
ty5$PeriodShiwes[ty5$year_all>=2001 & ty5$year_all<=2010]=("P4")
ty5$PeriodShiwes=as.factor(ty5$PeriodShiwes)
summary(ty5$PeriodShiwes)
dim(ty5)
#[1] 137 121
# --- Shippagan west June -----------------
ty6$PeriodShiwes[ty6$year_all>=1916 & ty6$year_all<=1954]=("P1")
ty6$PeriodShiwes[ty6$year_all>=1955 & ty6$year_all<=1984]=("P2")
ty6$PeriodShiwes[ty6$year_all>=1985 & ty6$year_all<=2000]=("P3")
ty6$PeriodShiwes[ty6$year_all>=2001 & ty6$year_all<=2010]=("P4")
ty6$PeriodShiwes=as.factor(ty6$PeriodShiwes)
summary(ty6$PeriodShiwes)
dim(ty6)
#[1] 137 121
# --- Shippagan west July ------------------------
ty7$PeriodShiwes[ty7$year_all>=1916 & ty7$year_all<=1954]=("P1")
ty7$PeriodShiwes[ty7$year_all>=1955 & ty7$year_all<=1984]=("P2")
ty7$PeriodShiwes[ty7$year_all>=1985 & ty7$year_all<=2000]=("P3")
ty7$PeriodShiwes[ty7$year_all>=2001 & ty7$year_all<=2010]=("P4")
ty7$PeriodShiwes=as.factor(ty7$PeriodShiwes)
summary(ty7$PeriodShiwes)
dim(ty7)
#[1] 138 121
# --- Shippagan west August -------------------
ty8$PeriodShiwes[ty8$year_all>=1916 & ty8$year_all<=1954]=("P1")
ty8$PeriodShiwes[ty8$year_all>=1955 & ty8$year_all<=1984]=("P2")
ty8$PeriodShiwes[ty8$year_all>=1985 & ty8$year_all<=2000]=("P3")
ty8$PeriodShiwes[ty8$year_all>=2001 & ty8$year_all<=2010]=("P4")
ty8$PeriodShiwes=as.factor(ty8$PeriodShiwes)
summary(ty8$PeriodShiwes)
dim(ty8)
#[1] 138 121

# MK 2016-03-30================================================

# Shippagan west April -----------------------------
ty4PShiwes=aggregate(ty4, by=list(ty4$PeriodShiwes), FUN=mean, na.rm=TRUE)
dim(ty4PShiwes)
# 4 x 123
ty4PShiwes[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty4PShiwes", file="ty4PShiwes.RData")

# Shippagan west May -----------------------------
ty5PShiwes=aggregate(ty5, by=list(ty5$PeriodShiwes), FUN=mean, na.rm=TRUE)
dim(ty5PShiwes)
# 4 x 123
ty5PShiwes[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty5PShiwes", file="ty5PShiwes.RData")

# Shippagan west June -----------------------------
ty6PShiwes=aggregate(ty6, by=list(ty6$PeriodShiwes), FUN=mean, na.rm=TRUE)
dim(ty6PShiwes)
# 4 x 123
ty6PShiwes[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty6PShiwes", file="ty6PShiwes.RData")

# Shippagan west July -----------------------------
ty7PShiwes=aggregate(ty7, by=list(ty7$PeriodShiwes), FUN=mean, na.rm=TRUE)
dim(ty7PShiwes)
# 4 x 123
ty7PShiwes[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty7PShiwes", file="ty7PShiwes.RData")

# Shippagan west Aug -----------------------------
ty8PShiwes=aggregate(ty8, by=list(ty8$PeriodShiwes), FUN=mean, na.rm=TRUE)
dim(ty8PShiwes)
# 4 x 123
ty8PShiwes[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty8PShiwes", file="ty8PShiwes.RData")

#==========================================================================
# MK 2016-03-30

ty4PShiwesbis=ty4PShiwes[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty4PShiwesbis)
# 4 x 3
ty5PShiwesbis=ty5PShiwes[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty5PShiwesbis)
# 4 x 3
ty6PShiwesbis=ty6PShiwes[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty6PShiwesbis)
# 4 x 3
ty7PShiwesbis=ty7PShiwes[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty7PShiwesbis)
# 4 x 3
ty8PShiwesbis=ty8PShiwes[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty8PShiwesbis)
# 4 x 3
meteoMonctonShiwes=c(ty4PShiwesbis,ty5PShiwesbis,ty6PShiwesbis,ty7PShiwesbis,ty8PShiwesbis)
save("meteoMonctonShiwes",file="meteoMonctonShiwes.RData")
write.csv(meteoMonctonShiwes,"meteoMonctonShiwes.csv")

#================Shippagan east===============================
# Corrected on 2016-04-18--------------------------------------

#-------Shippagan east April------------------
ty4$PeriodShieas[ty4$year_all>=1932 & ty4$year_all<=1939]=("P1")
ty4$PeriodShieas[ty4$year_all>=1940 & ty4$year_all<=1952]=("P2")
ty4$PeriodShieas[ty4$year_all>=1953 & ty4$year_all<=1969]=("P3")
ty4$PeriodShieas[ty4$year_all>=1970 & ty4$year_all<=1982]=("P4")
ty4$PeriodShieas[ty4$year_all>=1983 & ty4$year_all<=1992]=("P5")
ty4$PeriodShieas[ty4$year_all>=1993 & ty4$year_all<=2001]=("P6")
ty4$PeriodShieas[ty4$year_all>=2002 & ty4$year_all<=2010]=("P7")
ty4$PeriodShieas=as.factor(ty4$PeriodShieas)
summary(ty4$PeriodShieas)
dim(ty4)
# 137 x 121
#-------Shippagan east May------------------
ty5$PeriodShieas[ty5$year_all>=1932 & ty5$year_all<=1939]=("P1")
ty5$PeriodShieas[ty5$year_all>=1940 & ty5$year_all<=1952]=("P2")
ty5$PeriodShieas[ty5$year_all>=1953 & ty5$year_all<=1969]=("P3")
ty5$PeriodShieas[ty5$year_all>=1970 & ty5$year_all<=1982]=("P4")
ty5$PeriodShieas[ty5$year_all>=1983 & ty5$year_all<=1992]=("P5")
ty5$PeriodShieas[ty5$year_all>=1993 & ty5$year_all<=2001]=("P6")
ty5$PeriodShieas[ty5$year_all>=2002 & ty5$year_all<=2010]=("P7")
ty5$PeriodShieas=as.factor(ty5$PeriodShieas)
summary(ty5$PeriodShieas)
dim(ty5)
# 137 x 121

#-------Shippagan east June------------------
ty6$PeriodShieas[ty6$year_all>=1932 & ty6$year_all<=1939]=("P1")
ty6$PeriodShieas[ty6$year_all>=1940 & ty6$year_all<=1952]=("P2")
ty6$PeriodShieas[ty6$year_all>=1953 & ty6$year_all<=1969]=("P3")
ty6$PeriodShieas[ty6$year_all>=1970 & ty6$year_all<=1982]=("P4")
ty6$PeriodShieas[ty6$year_all>=1983 & ty6$year_all<=1992]=("P5")
ty6$PeriodShieas[ty6$year_all>=1993 & ty6$year_all<=2001]=("P6")
ty6$PeriodShieas[ty6$year_all>=2002 & ty6$year_all<=2010]=("P7")
ty6$PeriodShieas=as.factor(ty6$PeriodShieas)
summary(ty6$PeriodShieas)
dim(ty6)
# 137 x 121

#-------Shippagan east July------------------
ty7$PeriodShieas[ty7$year_all>=1932 & ty7$year_all<=1939]=("P1")
ty7$PeriodShieas[ty7$year_all>=1940 & ty7$year_all<=1952]=("P2")
ty7$PeriodShieas[ty7$year_all>=1953 & ty7$year_all<=1969]=("P3")
ty7$PeriodShieas[ty7$year_all>=1970 & ty7$year_all<=1982]=("P4")
ty7$PeriodShieas[ty7$year_all>=1983 & ty7$year_all<=1992]=("P5")
ty7$PeriodShieas[ty7$year_all>=1993 & ty7$year_all<=2001]=("P6")
ty7$PeriodShieas[ty7$year_all>=2002 & ty7$year_all<=2010]=("P7")
ty7$PeriodShieas=as.factor(ty7$PeriodShieas)
summary(ty7$PeriodShieas)
dim(ty7)
# 138 x 121
#-------Shippagan east August------------------
ty8$PeriodShieas[ty8$year_all>=1932 & ty8$year_all<=1939]=("P1")
ty8$PeriodShieas[ty8$year_all>=1940 & ty8$year_all<=1952]=("P2")
ty8$PeriodShieas[ty8$year_all>=1953 & ty8$year_all<=1969]=("P3")
ty8$PeriodShieas[ty8$year_all>=1970 & ty8$year_all<=1982]=("P4")
ty8$PeriodShieas[ty8$year_all>=1983 & ty8$year_all<=1992]=("P5")
ty8$PeriodShieas[ty8$year_all>=1993 & ty8$year_all<=2001]=("P6")
ty8$PeriodShieas[ty8$year_all>=2002 & ty8$year_all<=2010]=("P7")
ty8$PeriodShieas=as.factor(ty8$PeriodShieas)
summary(ty8$PeriodShieas)
dim(ty8)
# 138 x 121

#============================================================================
# Shippagan east April------------------
ty4PShieas=aggregate(ty4, by=list(ty4$PeriodShieas), FUN=mean, na.rm=TRUE)
dim(ty4PShieas)
# 7 x 122
ty4PShieas[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty4PShieas", file="ty4PShieas.RData")

########################################################################

# Shippagan east May------------------
ty5PShieas=aggregate(ty5, by=list(ty5$PeriodShieas), FUN=mean, na.rm=TRUE)
dim(ty5PShieas)
# 7 x 122
ty5PShieas[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty5PShieas", file="ty5PShieas.RData")

# Shippagan east June------------------
ty6PShieas=aggregate(ty6, by=list(ty6$PeriodShieas), FUN=mean, na.rm=TRUE)
dim(ty6PShieas)
#7 x 122
ty6PShieas[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty6PShieas", file="ty6PShieas.RData")

# Shippagan east July------------------
ty7PShieas=aggregate(ty7, by=list(ty7$PeriodShieas), FUN=mean, na.rm=TRUE)
dim(ty7PShieas)
#7 x 122
ty7PShieas[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty7PShieas", file="ty7PShieas.RData")

# Shippagan east Aug------------------
ty8PShieas=aggregate(ty8, by=list(ty8$PeriodShieas), FUN=mean, na.rm=TRUE)
dim(ty8PShieas)
#7 x 122
ty8PShieas[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty8PShieas", file="ty8PShieas.RData")

#=================================================================================

ty4PShieasbis=ty4PShieas[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty4PShieasbis)
# 7 x 3
ty5PShieasbis=ty5PShieas[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty5PShieasbis)
# 7 x 3
ty6PShieasbis=ty6PShieas[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty6PShieasbis)
# 7 x 3
ty7PShieasbis=ty7PShieas[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty7PShieasbis)
# 7 x 3
ty8PShieasbis=ty8PShieas[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty8PShieasbis)
# 7 x 3
meteoMonctonShieas=c(ty4PShieasbis,ty5PShieasbis,ty6PShieasbis,ty7PShieasbis,ty8PShieasbis)
save("meteoMonctonShieas",file="meteoMonctonShieas.RData")
write.csv(meteoMonctonShieas,"meteoMonctonShieas.csv")


# Periods for Caraquet========================================
# MK done on 2014-04-14------------------------
#-----------Caraquet April-----------------
ty4$PeriodCar[ty4$year_all>=1719 & ty4$year_all<=1832]=("P1")
ty4$PeriodCar[ty4$year_all>=1833 & ty4$year_all<=1900]=("P2")
ty4$PeriodCar[ty4$year_all>=1901 & ty4$year_all<=1921]=("P3")
ty4$PeriodCar[ty4$year_all>=1922 & ty4$year_all<=1937]=("P4")
ty4$PeriodCar[ty4$year_all>=1938 & ty4$year_all<=1951]=("P5")
ty4$PeriodCar[ty4$year_all>=1952 & ty4$year_all<=1961]=("P6")
ty4$PeriodCar[ty4$year_all>=1962 & ty4$year_all<=1970]=("P7")
ty4$PeriodCar[ty4$year_all>=1971 & ty4$year_all<=1978]=("P8")
ty4$PeriodCar[ty4$year_all>=1979 & ty4$year_all<=1985]=("P9")
ty4$PeriodCar[ty4$year_all>=1986 & ty4$year_all<=1990]=("P10")
ty4$PeriodCar[ty4$year_all>=1991 & ty4$year_all<=1995]=("P11")
ty4$PeriodCar[ty4$year_all>=1996 & ty4$year_all<=2000]=("P12")
ty4$PeriodCar[ty4$year_all>=2001 & ty4$year_all<=2003]=("P13")
ty4$PeriodCar[ty4$year_all>=2004 & ty4$year_all<=2006]=("P14")
ty4$PeriodCar[ty4$year_all>=2007 & ty4$year_all<=2012]=("P15")
ty4$PeriodCar=as.factor(ty4$PeriodCar)
summary(ty4$PeriodCar)
dim(ty4)
# 137 X 121

# --- Caraquet May ------------------
ty5$PeriodCar[ty5$year_all>=1719 & ty5$year_all<=1832]=("P1")
ty5$PeriodCar[ty5$year_all>=1833 & ty5$year_all<=1900]=("P2")
ty5$PeriodCar[ty5$year_all>=1901 & ty5$year_all<=1921]=("P3")
ty5$PeriodCar[ty5$year_all>=1922 & ty5$year_all<=1937]=("P4")
ty5$PeriodCar[ty5$year_all>=1938 & ty5$year_all<=1951]=("P5")
ty5$PeriodCar[ty5$year_all>=1952 & ty5$year_all<=1961]=("P6")
ty5$PeriodCar[ty5$year_all>=1962 & ty5$year_all<=1970]=("P7")
ty5$PeriodCar[ty5$year_all>=1971 & ty5$year_all<=1978]=("P8")
ty5$PeriodCar[ty5$year_all>=1979 & ty5$year_all<=1985]=("P9")
ty5$PeriodCar[ty5$year_all>=1986 & ty5$year_all<=1990]=("P10")
ty5$PeriodCar[ty5$year_all>=1991 & ty5$year_all<=1995]=("P11")
ty5$PeriodCar[ty5$year_all>=1996 & ty5$year_all<=2000]=("P12")
ty5$PeriodCar[ty5$year_all>=2001 & ty5$year_all<=2003]=("P13")
ty5$PeriodCar[ty5$year_all>=2004 & ty5$year_all<=2006]=("P14")
ty5$PeriodCar[ty5$year_all>=2007 & ty5$year_all<=2012]=("P15")
ty5$PeriodCar=as.factor(ty5$PeriodCar)
summary(ty5$PeriodCar)
dim(ty5)
#[1] 137 121

# --- Caraquet June -----------------
ty6$PeriodCar[ty6$year_all>=1719 & ty6$year_all<=1832]=("P1")
ty6$PeriodCar[ty6$year_all>=1833 & ty6$year_all<=1900]=("P2")
ty6$PeriodCar[ty6$year_all>=1901 & ty6$year_all<=1921]=("P3")
ty6$PeriodCar[ty6$year_all>=1922 & ty6$year_all<=1937]=("P4")
ty6$PeriodCar[ty6$year_all>=1938 & ty6$year_all<=1951]=("P5")
ty6$PeriodCar[ty6$year_all>=1952 & ty6$year_all<=1961]=("P6")
ty6$PeriodCar[ty6$year_all>=1962 & ty6$year_all<=1970]=("P7")
ty6$PeriodCar[ty6$year_all>=1971 & ty6$year_all<=1978]=("P8")
ty6$PeriodCar[ty6$year_all>=1979 & ty6$year_all<=1985]=("P9")
ty6$PeriodCar[ty6$year_all>=1986 & ty6$year_all<=1990]=("P10")
ty6$PeriodCar[ty6$year_all>=1991 & ty6$year_all<=1995]=("P11")
ty6$PeriodCar[ty6$year_all>=1996 & ty6$year_all<=2000]=("P12")
ty6$PeriodCar[ty6$year_all>=2001 & ty6$year_all<=2003]=("P13")
ty6$PeriodCar[ty6$year_all>=2004 & ty6$year_all<=2006]=("P14")
ty6$PeriodCar[ty6$year_all>=2007 & ty6$year_all<=2012]=("P15")
ty6$PeriodCar=as.factor(ty6$PeriodCar)
summary(ty6$PeriodCar)
dim(ty6)
#[1] 137 121

# --- Caraquet July ------------------------
ty7$PeriodCar[ty7$year_all>=1719 & ty7$year_all<=1832]=("P1")
ty7$PeriodCar[ty7$year_all>=1833 & ty7$year_all<=1900]=("P2")
ty7$PeriodCar[ty7$year_all>=1901 & ty7$year_all<=1921]=("P3")
ty7$PeriodCar[ty7$year_all>=1922 & ty7$year_all<=1937]=("P4")
ty7$PeriodCar[ty7$year_all>=1938 & ty7$year_all<=1951]=("P5")
ty7$PeriodCar[ty7$year_all>=1952 & ty7$year_all<=1961]=("P6")
ty7$PeriodCar[ty7$year_all>=1962 & ty7$year_all<=1970]=("P7")
ty7$PeriodCar[ty7$year_all>=1971 & ty7$year_all<=1978]=("P8")
ty7$PeriodCar[ty7$year_all>=1979 & ty7$year_all<=1985]=("P9")
ty7$PeriodCar[ty7$year_all>=1986 & ty7$year_all<=1990]=("P10")
ty7$PeriodCar[ty7$year_all>=1991 & ty7$year_all<=1995]=("P11")
ty7$PeriodCar[ty7$year_all>=1996 & ty7$year_all<=2000]=("P12")
ty7$PeriodCar[ty7$year_all>=2001 & ty7$year_all<=2003]=("P13")
ty7$PeriodCar[ty7$year_all>=2004 & ty7$year_all<=2006]=("P14")
ty7$PeriodCar[ty7$year_all>=2007 & ty7$year_all<=2012]=("P15")
ty7$PeriodCar=as.factor(ty7$PeriodCar)
summary(ty7$PeriodCar)
dim(ty7)
#[1] 138 121

# --- Caraquet August -------------------
ty8$PeriodCar[ty8$year_all>=1719 & ty8$year_all<=1832]=("P1")
ty8$PeriodCar[ty8$year_all>=1833 & ty8$year_all<=1900]=("P2")
ty8$PeriodCar[ty8$year_all>=1901 & ty8$year_all<=1921]=("P3")
ty8$PeriodCar[ty8$year_all>=1922 & ty8$year_all<=1937]=("P4")
ty8$PeriodCar[ty8$year_all>=1938 & ty8$year_all<=1951]=("P5")
ty8$PeriodCar[ty8$year_all>=1952 & ty8$year_all<=1961]=("P6")
ty8$PeriodCar[ty8$year_all>=1962 & ty8$year_all<=1970]=("P7")
ty8$PeriodCar[ty8$year_all>=1971 & ty8$year_all<=1978]=("P8")
ty8$PeriodCar[ty8$year_all>=1979 & ty8$year_all<=1985]=("P9")
ty8$PeriodCar[ty8$year_all>=1986 & ty8$year_all<=1990]=("P10")
ty8$PeriodCar[ty8$year_all>=1991 & ty8$year_all<=1995]=("P11")
ty8$PeriodCar[ty8$year_all>=1996 & ty8$year_all<=2000]=("P12")
ty8$PeriodCar[ty8$year_all>=2001 & ty8$year_all<=2003]=("P13")
ty8$PeriodCar[ty8$year_all>=2004 & ty8$year_all<=2006]=("P14")
ty8$PeriodCar[ty8$year_all>=2007 & ty8$year_all<=2012]=("P15")
ty8$PeriodCar=as.factor(ty8$PeriodCar)
summary(ty8$PeriodCar)
dim(ty8)
#[1] 138 121

# =======================================================
# Caraquet April -----------------------------
ty4PCar=aggregate(ty4, by=list(ty4$PeriodCar), FUN=mean, na.rm=TRUE)
dim(ty4PCar)
#14 X 122
ty4PCar[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty4PCar", file="ty4PCar.RData")

# Caraquet May -----------------------------
ty5PCar=aggregate(ty5, by=list(ty5$PeriodCar), FUN=mean, na.rm=TRUE)
dim(ty5PCar)
#14 X 122
ty5PCar[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty5PCar", file="ty5PCar.RData")

# Caraquet June -----------------------------
ty6PCar=aggregate(ty6, by=list(ty6$PeriodCar), FUN=mean, na.rm=TRUE)
dim(ty6PCar)
#14 X 122
ty6PCar[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty6PCar", file="ty6PCar.RData")

# Caraquet July -----------------------------
ty7PCar=aggregate(ty7, by=list(ty7$PeriodCar), FUN=mean, na.rm=TRUE)
dim(ty7PCar)
#14 X 122
ty7PCar[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty7PCar", file="ty7PCar.RData")

# Caraquet August -----------------------------
ty8PCar=aggregate(ty8, by=list(ty8$PeriodCar), FUN=mean, na.rm=TRUE)
dim(ty8PCar)
#14 X 122
ty8PCar[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty8PCar", file="ty8PCar.RData")
#========================================================================
# MK 2016-04-14: create a new series of tables containing only the
# variables of interest

ty4PCarbis=ty4PCar[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty4PCarbis)
# 14 x 3
ty5PCarbis=ty5PCar[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty5PCarbis)
# 14 x 3
ty6PCarbis=ty6PCar[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty6PCarbis)
# 14 x 3
ty7PCarbis=ty7PCar[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty7PCarbis)
# 14 x 3
ty8PCarbis=ty8PCar[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty8PCarbis)
# 14 x 3

# MK 2016-04-14:
# Make one single file by binding columns with the "cbind" function
meteoMonctonCar=c(ty4PCarbis,ty5PCarbis,ty6PCarbis,ty7PCarbis,ty8PCarbis)
save("meteoMonctonCar",file="meteoMonctonCar.RData")
write.csv(meteoMonctonCar,"meteoMonctonCar.csv")

###################################################################
# PART III: define periods for the landuse data ####
# Dec_2015_Moumita
###################################################################
# Dec17th _2015-12-17_Moumita

pathland = file.path("C:/Users/Moumita")
pathland
pathland = file.path("/Post Doc at Shipgaan/151210_env_scripts_from_Alain_to_Moumita/land_data")
getwd()
# AP 2016-04-13 The file "Land_use_data4.txt" was sent from AP
# to MK on date... It originates from the Frantz ms
# Here, the "Period" variable referts to Lake Inkerman,
# and originates from Frantz's thesis
landt=read.delim2("Land_use_data4.txt", header=TRUE)
# From Alain's UMCS Win8 computer:
landt=read.delim2("C:/Users/alain/Documents/RECHERCHE_Labos_GIZC/_Base_donnees/Fichiers_txt/Land_use_data4.txt", header=TRUE)
landt
names(landt)
summary(landt$Year)

# Agr_Pok_ha and Agr_Pok_pct have two values based on Frantz's
# work of photointerpretation of the 1950s aerial photographs

# Replace blanks with NA
# landt[landt==""]=NA# Use "read.delim" and not "read.delim2" because this file
# uses decimal points rather than decimal commas.

plot(Year~Foin_hay_pct,data=landt,type="b",xaxt="n", pch="h", xlim=c(0,0.035),xlab="Percent land area (or ha/km)", col="green", ylim=c(1850,2020))
axis(1,at=pretty(landt$Foin_hay_pct),lab=paste(pretty(landt$Foin_hay_pct)*100, " %"))
points(Year~Ble_weat_pct, data=landt, type="b",pch="w", col="orange")
points(Year~Avoine_oats_pct,data=landt, type="b",pch="o", col="red")
points(Year~Orge_barley_pct, data=landt,type="b", pch="b", col="blue")
points(Year~Cumul_Peat_extract_Pok_pct, data=landt,type="b", pch="p", col="black")
points(Year~Cumul_Peat_extract_Pok_pct, data=landt,type="b", pch="p", col="black")
title(main="Hist_land_data_150122.Rnw")
grid(ny=NULL, col="black", lty="dotted", lwd=1)

range(landt$Year)
plot(Year~Foin_hay_pct,data=landt, ylim=rev(range(landt$Year)))

summary(landt)
subset(landt,select=c(Year,Period,Cumul_Peat_extract_Pok_pct))
landt$Year
class(landt$Year)
summary(landt$Year)

# ============================================================
# _1) DEFINE LAND PERIODS FOR MALTAMPEC ==========
# MK April, 2016-04-15
# Create period division based on CRS age estimates specific
# to Maltampec, as defined in "All dats corrected...xlsx",
# and as previously done for the meteo data (above).
# =========================================================
landt$PeriodMalt[landt$Year>=1876 & landt$Year<=1908]=("P1")
landt$PeriodMalt[landt$Year>=1809 & landt$Year<=1936]=("P2")
landt$PeriodMalt[landt$Year>=1937 & landt$Year<=1960]=("P3")
landt$PeriodMalt[landt$Year>=1961 & landt$Year<=1983]=("P4")
landt$PeriodMalt[landt$Year>=1984 & landt$Year<=2010]=("P5")
landt$PeriodMalt=as.factor(landt$PeriodMalt)
summary(landt$PeriodMalt)
dim(landt)
# 40 x 21
landt$PeriodMalt=as.factor(landt$PeriodMalt)
summary(landt$PeriodMalt)


# Gloucester represesents the agricultural data while Pokemouche pertains to the
# peat extraction data, specific to the Pokemouche catchment

# AP to Moumita: we have peat extraction data specific to other catchments
# (Caraquet, Petite Tracadie, Tabusintac). I need to get them out of the
# GIS files and send them to you. Because the Gloucester county
# encompasses all catchments, you can use the same agricultural data for
# all catchments.
# AP extracted peat data from other catchments 2016-04-07

# Gloucester subset----------------------------

Gloucester=subset(landt,Territorial_delim=="Gloucester",select=c(Year, Period, PeriodMalt, Territorial_delim,Foin_hay_pct, Ble_weat_pct,Orge_barley_pct, Avoine_oats_pct,Hay_weat_barley_oats_pct))
Gloucester[,1:4]
# Get number of cases by variable
sapply(Gloucester, function(x)(sum(complete.cases(x))))

# Get number of Foin cases by Period
PeriodCases=tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodMalt,sum,na.rm=T)/tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodMalt,mean,na.rm=T)
PeriodCases
# P2 P3 P4 P5 
 7  3  4  5 
sum(PeriodCases)
# 19

# --------------------------------------------------------------
# Aggregate the agriculural data (i.e. the Gloucester delimitation)
# based on Maltampec age estimates
# By PeriodMalt done 2016-03-31
# -----------------------------------------------------------
GPeriodMeanMalt=aggregate(Gloucester, by=list(Gloucester$PeriodMalt), FUN="mean", na.rm=TRUE)
GPeriodMeanMalt

# --------------------------------------------------------
# Aggregate the peat data based on Maltampec age estimates
# --------------------------------------------------------
Pok=subset(landt, Territorial_delim=="Bassin_Pokemouche",select=c(Year,PeriodMalt,Territorial_delim,Cumul_Peat_extract_Pok_pct))
Pok
PokPeriodMeanMalt=aggregate(Pok, by=list(Pok$PeriodMalt), mean, na.rm=T)
PokPeriodMeanMalt

# Merge Agr and peat data 

landtMalt=merge(GPeriodMeanMalt, PokPeriodMeanMalt, by="Group.1", all=TRUE)
landtMalt
summary(landtMalt)
save(landtMalt,file="landtMalt.RData")
write.csv(landtMalt,"landtMalt.csv")

# Plot Malt land ---- 
plot(landtMalt$Year.x~landtMalt$Hay_weat_barley_oats_pct, type="b", xlim=c(0,0.06))
points(landtMalt$Year.x~landtMalt$Foin_hay_pct, type="b", col="blue", pch="h")
points(landtMalt$Year.x~landtMalt$Ble_weat_pct, type="b", col="green", pch="w")
points(landtMalt$Year.x~landtMalt$Orge_barley_pct, type="b", col="red", pch="b")
points(landtMalt$Year.x~landtMalt$Avoine_oats_pct, type="b", col="purple", pch="o")
points(landtMalt$Year.x~landtMalt$Cumul_Peat_extract_Pok_pct, type="b",pch="p", col="black")

# =====================================================
# _2) DEFINE LAND PERIODS FOR WAUGH ====
# =====================================================
landt$PeriodWau[landt$Year>=1943 & landt$Year<=1963]=("P1")
landt$PeriodWau[landt$Year>=1964 & landt$Year<=1982]=("P2")
landt$PeriodWau[landt$Year>=1983 & landt$Year<=1996]=("P3")
landt$PeriodWau[landt$Year>=1997 & landt$Year<=2010]=("P4")
landt$PeriodWau=as.factor(landt$PeriodWau)
summary(landt$PeriodWau)
dim(landt)
# 40 X 22

# Gloucester subset by PeriodWau done 2016-03-31 ----------------------------
Gloucester=subset(landt,Territorial_delim=="Gloucester",select=c(Year, Period, PeriodWau, Territorial_delim,Foin_hay_pct, Ble_weat_pct,Orge_barley_pct, Avoine_oats_pct,Hay_weat_barley_oats_pct))
# Get number of Foin cases by Period

Gloucester$Foin_hay_pct
names(Gloucester)
tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodWau,sum,na.rm=T)
PeriodCases=tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodWau,sum,na.rm=T)/tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodWau,mean,na.rm=T)
PeriodCases=tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodWau,sum,na.rm=T)/tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodWau,mean,na.rm=T)
PeriodCases
#P1 P2 P3 P4 
 3  3  3  2 
sum(PeriodCases)
# 11
GPeriodMeanWau=aggregate(Gloucester, by=list(Gloucester$PeriodWau), FUN="mean", na.rm=TRUE)
GPeriodMeanWau

# Pokemouche subset------------------------------------------------

Pok=subset(landt, Territorial_delim=="Bassin_Pokemouche",select=c(Year,PeriodWau,Territorial_delim,Cumul_Peat_extract_Pok_pct))
Pok
PokPeriodMeanWau=aggregate(Pok, by=list(Pok$PeriodWau), mean, na.rm=T)
PokPeriodMeanWau

#  Merge Agr and peat data for Waugh ----

landtWau=merge(GPeriodMeanWau, PokPeriodMeanWau, by="Group.1", all=TRUE)
summary(landtWau)
save(landtWau,file="landtWau.RData")
write.csv(landtWau,"landtWau.csv")

# For plotting---------------------------
plot(landtWau$Year.x~landtWau$Hay_weat_barley_oats_pct, type="b", xlim=c(0,0.06))
points(landtWau$Year.x~landtWau$Foin_hay_pct, type="b", col="blue", pch="h")
points(landtWau$Year.x~landtWau$Ble_weat_pct, type="b", col="green", pch="w")
points(landtWau$Year.x~landtWau$Orge_barley_pct, type="b", col="red", pch="b")
points(landtWau$Year.x~landtWau$Avoine_oats_pct, type="b", col="purple", pch="o")
points(landtWau$Year.x~landtWau$Cumul_Peat_extract_Pok_pct, type="b",pch="p", col="black")

# ========================================================
# _3) DEFINE LAND PERIODS FOR Petite Tracadie Amont  ====
# =======================================================
# MK corrected the Period, 2016-04-16----------------------
pathland = file.path("C:/Users/Moumita")
pathland
pathland = file.path("/Post Doc at Shipgaan/151210_env_scripts_from_Alain_to_Moumita/land_data")
getwd()
landtMK=read.delim("C:/Users/alain/Documents/RECHERCHE_Labos_GIZC/_Base_donnees/Fichiers_txt/Land_use_data4_MK.txt", header=TRUE)
# AP 2016-04-13 The "Land_use_data4_MK.txt" was created by Moumita
# building on the original "Land_use_data4.txt" that was used
# for the Frantz paper. "Land_use4_MK.txt" simply has extra rows
# containing the peat data that Alain extracted 2016-04-07:
# see file sent to Moumita 2016-04-07 "Peat_exploitation.xlsx
# 135 ko

#===================================================================
# MK 2016-04-14
landtMK=read.delim("C:/Users/Moumita/Post Doc at Shippagan/151210_env_scripts_from_Alain_to_Moumita/Land_data/Land_use_data4_MK.txt", header=TRUE)
summary(landtMK)

# See how "Territoral_delim" now contains Caraquet, PT, St-Simon
# (Shippgan) and Tabusintac: these rows have peat data added by
# Alain 2016-04-07

# For St-Simon 2009, check that peat (ha) / census area (km) gives 
# the right percentage values (computed in Excel by Moumita)
subset(landtMK,Year=="2009" & Territorial_delim=="St-Simon",select=c(Year, Territorial_delim,Surface_census_km2,Cumul_Peat_extract_Pok_ha, Cumul_Peat_extract_Pok_pct))
622/ (138.26*100) # 0.044, or 4.4% = "Cumul_Peat_extract_Pok_pct

landtMK$PeriodPtAm[landtMK$Year>=1839 & landtMK$Year<=1879]=("P1")
landtMK$PeriodPtAm[landtMK$Year>=1880 & landtMK$Year<=1908]=("P2")
landtMK$PeriodPtAm[landtMK$Year>=1909 & landtMK$Year<=1934]=("P3")
landtMK$PeriodPtAm[landtMK$Year>=1935 & landtMK$Year<=1958]=("P4")
landtMK$PeriodPtAm[landtMK$Year>=1959 & landtMK$Year<=1972]=("P5")
landtMK$PeriodPtAm[landtMK$Year>=1973 & landtMK$Year<=1981]=("P6")
landtMK$PeriodPtAm[landtMK$Year>=1982 & landtMK$Year<=1989]=("P7")
landtMK$PeriodPtAm[landtMK$Year>=1990 & landtMK$Year<=1994]=("P8")
landtMK$PeriodPtAm[landtMK$Year>=1995 & landtMK$Year<=2000]=("P9")
landtMK$PeriodPtAm[landtMK$Year>=2001 & landtMK$Year<=2004]=("P10")
landtMK$PeriodPtAm[landtMK$Year>=2005 & landtMK$Year<=2010]=("P11")
landtMK$PeriodPtAm=as.factor(landtMK$PeriodPtAm)
summary(landtMK$PeriodPtAm)
dim(landtMK)
# 45 X 21

# __ Get PT agricultural data ------
Gloucester=subset(landtMK,Territorial_delim=="Gloucester",select=c(Year, Period, PeriodPtAm, Territorial_delim,Foin_hay_pct, Ble_weat_pct,Orge_barley_pct, Avoine_oats_pct,Hay_weat_barley_oats_pct))
Gloucester[,1:4]
Gloucester
# Get number of cases by variable
sapply(Gloucester, function(x)(sum(complete.cases(x))))

# Get number of Foin cases by Period
PeriodCases=tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodPtAm,sum,na.rm=T)/tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodPtAm,mean,na.rm=T)
PeriodCases
#P1 P10 P11  P2  P3  P4  P5  P6  P7  P8  P9 
  1   1   1   3   3   3   3   1   1   1   1  
sum(PeriodCases,na.rm=T)
# x 19

GPeriodMeanPtAm=aggregate(Gloucester, by=list(Gloucester$PeriodPtAm), FUN="mean", na.rm=TRUE)
GPeriodMeanPtAm

# __ Get PT amont peat data, MK:Done on 2016-04-11 ---------

Pet=subset(landtMK, Territorial_delim=="Petite_Tracadie",select=c(Year,PeriodPtAm,Territorial_delim,Cumul_Peat_extract_Pok_pct))
Pet
PetPeriodMeanPtAm=aggregate(Pet, by=list(Pet$PeriodPtAm), mean, na.rm=T)
PetPeriodMeanPtAm

# PTamont Merge Agr and peat data ----------

landtMKPtAm=merge(GPeriodMeanPtAm, PetPeriodMeanPtAm, by="Group.1", all=TRUE)
summary(landtMKPtAm)
save(landtMKPtAm,file="landtMKPtAm.RData")
write.csv(landtMKPtAm,"landtMKPtAm.csv")

# PT amont plotting----------------------------------
plot(landtMKPtAm$Year.x~landtMKPtAm$Hay_weat_barley_oats_pct, type="b", xlim=c(0,0.06))
points(landtMKPtAm$Year.x~landtMKPtAm$Foin_hay_pct, type="b", col="blue", pch="h")
points(landtMKPtAm$Year.x~landtMKPtAm$Ble_weat_pct, type="b", col="green", pch="w")
points(landtMKPtAm$Year.x~landtMKPtAm$Orge_barley_pct, type="b", col="red", pch="b")
points(landtMKPtAm$Year.x~landtMKPtAm$Avoine_oats_pct, type="b", col="purple", pch="o")
points(landtMKPtAm$Year.x~landtMKPtAm$Cumul_Peat_extract_Pok_ha, type="b",pch="p", col="black")

# ==================================================================
# _4) DEFINE LAND PERIODS FOR Shippagan West defined periods =========
#----MK: corrected on 2016-04-15-------------
# ==================================================================
landtMK$PeriodShiwes[landtMK$Year>=1916 & landtMK$Year<=1954]=("P1")
landtMK$PeriodShiwes[landtMK$Year>=1955 & landtMK$Year<=1984]=("P2")
landtMK$PeriodShiwes[landtMK$Year>=1985 & landtMK$Year<=2000]=("P3")
landtMK$PeriodShiwes[landtMK$Year>=2001 & landtMK$Year<=2010]=("P4")
landtMK$PeriodShiwes=as.factor(landtMK$PeriodShiwes)
summary(landtMK$PeriodShiwes)
dim(landtMK) # 45 x 22
# Gloucester subset for Shippagan ----------------------------
# By Shiwes done 2016-03-31 
Gloucester=subset(landtMK,Territorial_delim=="Gloucester",select=c(Year, PeriodShiwes, Territorial_delim,Foin_hay_pct, Ble_weat_pct,Orge_barley_pct, Avoine_oats_pct,Hay_weat_barley_oats_pct))
Gloucester[,1:4]
# Get number of cases by variable
sapply(Gloucester, function(x)(sum(complete.cases(x))))

# Get number of Foin cases by Period
PeriodCases=tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodShiwes,sum,na.rm=T)/tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodShiwes,mean,na.rm=T)
PeriodCases
#P2 P3 P4 
 5  3  2  
sum(PeriodCases)
# 10
GPeriodMeanShiwes=aggregate(Gloucester, by=list(Gloucester$PeriodShiwes), FUN="mean", na.rm=TRUE)
GPeriodMeanShiwes

#MK done on 2016-04-14########################
# St-Simon subset ------------------------------------------------

StS=subset(landtMK, Territorial_delim=="St-Simon",select=c(Year,PeriodShiwes,Territorial_delim,Cumul_Peat_extract_Pok_pct))
StS
StSPeriodMeanShiwes=aggregate(StS, by=list(StS$PeriodShiwes), mean, na.rm=T)
StSPeriodMeanShiwes

# ++++++++ Merge Agr and peat data+++++++++++++++++++++++++

landtMKShiwes=merge(GPeriodMeanShiwes, StSPeriodMeanShiwes, by="Group.1", all=TRUE)
summary(landtMKShiwes)
save(landtMKShiwes,file="landtMKShiwes.RData")
write.csv(landtMKShiwes,"landtMKShiwes.csv")

# For plotting----------------------------------
plot(landtMKShiwes$Year.x~landtMKShiwes$Hay_weat_barley_oats_pct, type="b", xlim=c(0,0.06))
points(landtMKShiwes$Year.x~landtMKShiwes$Foin_hay_pct, type="b", col="blue", pch="h")
points(landtMKShiwes$Year.x~landtMKShiwes$Ble_weat_pct, type="b", col="green", pch="w")
points(landtMKShiwes$Year.x~landtMKShiwes$Orge_barley_pct, type="b", col="red", pch="b")
points(landtMKShiwes$Year.x~landtMKShiwes$Avoine_oats_pct, type="b", col="purple", pch="o")
points(landtMKShiwes$Year.x~landtMKShiwes$Cumul_Peat_extract_Pok_pct, type="b",pch="p", col="black")

# =========================================================
# _5) DEFINE LAND PERIODS FOR Shippagan east ========
# =======================================================
landtMK$PeriodShieas[landtMK$Year>=1932 & landtMK$Year<=1939]=("P1")
landtMK$PeriodShieas[landtMK$Year>=1940 & landtMK$Year<=1952]=("P2")
landtMK$PeriodShieas[landtMK$Year>=1953 & landtMK$Year<=1969]=("P3")
landtMK$PeriodShieas[landtMK$Year>=1970 & landtMK$Year<=1982]=("P4")
landtMK$PeriodShieas[landtMK$Year>=1983 & landtMK$Year<=1992]=("P5")
landtMK$PeriodShieas[landtMK$Year>=1993 & landtMK$Year<=2001]=("P6")
landtMK$PeriodShieas[landtMK$Year>=2002 & landtMK$Year<=2010]=("P7")
landtMK$PeriodShieas=as.factor(landtMK$PeriodShieas)
summary(landtMK$PeriodShieas)
dim(landtMK)
# 45 x 23

#Gloucester subset (agricultural data) By Shieas done 2016-03-31----------------
Gloucester=subset(landtMK,Territorial_delim=="Gloucester",select=c(Year, Period, PeriodShieas, Territorial_delim,Foin_hay_pct, Ble_weat_pct,Orge_barley_pct, Avoine_oats_pct,Hay_weat_barley_oats_pct))
Gloucester[,1:4]
# Get number of cases by variable
sapply(Gloucester, function(x)(sum(complete.cases(x))))

# Get number of Foin cases by Period
PeriodCases=tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodShieas,sum,na.rm=T)/tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodShieas,mean,na.rm=T)
PeriodCases
#P2 P3 P4 P5 P6 P7 
 2  3  2  2  2  1  
sum(PeriodCases)
# 12
GPeriodMeanShieas=aggregate(Gloucester, by=list(Gloucester$PeriodShieas), FUN="mean", na.rm=TRUE)
GPeriodMeanShieas

# Replace "Bassin_Pokemouche" by St-Simon ##########
# MK done on 2016-04-14 ##################

# St-Simon subset ------------------------------------------------

StS=subset(landtMK, Territorial_delim=="St-Simon",select=c(Year,PeriodShieas,Territorial_delim,Cumul_Peat_extract_Pok_pct))
StS
StSPeriodMeanShieas=aggregate(StS, by=list(StS$PeriodShieas), mean, na.rm=T)
StSPeriodMeanShieas

# ++++++++ Merge Agr and peat data+++++++++++++++++++++++++

landtMKShieas=merge(GPeriodMeanShieas, StSPeriodMeanShieas, by="Group.1", all=TRUE)
summary(landtMKShieas)
save(landtMKShieas,file="landtMKShieas.RData")
write.csv(landtMKShieas,"landtMKShieas.csv")

# For plotting----------------------------------
plot(landtMKShieas$Year.x~landtMKShieas$Hay_weat_barley_oats_pct, type="b", xlim=c(0,0.06))
points(landtMKShieas$Year.x~landtMKShieas$Foin_hay_pct, type="b", col="blue", pch="h")
points(landtMKShieas$Year.x~landtMKShieas$Ble_weat_pct, type="b", col="green", pch="w")
points(landtMKShieas$Year.x~landtMKShieas$Orge_barley_pct, type="b", col="red", pch="b")
points(landtMKShieas$Year.x~landtMKShieas$Avoine_oats_pct, type="b", col="purple", pch="o")
points(landtMKShieas$Year.x~landtMKShieas$Cumul_Peat_extract_Pok_pct, type="b",pch="p", col="black")

# =========================================================
# _6) DEFINE LAND PERIODS FOR Caraquet ========
# =======================================================

# MK 2016-04-14
landtMK=read.delim("C:/Users/Moumita/Post Doc at Shippagan/151210_env_scripts_from_Alain_to_Moumita/Land_data/Land_use_data4_MK.txt", header=TRUE)
summary(landtMK)
landtMK$PeriodCar[landtMK$Year>=1719 & landtMK$Year<=1832]=("P1")
landtMK$PeriodCar[landtMK$Year>=1833 & landtMK$Year<=1900]=("P2")
landtMK$PeriodCar[landtMK$Year>=1901 & landtMK$Year<=1921]=("P3")
landtMK$PeriodCar[landtMK$Year>=1922 & landtMK$Year<=1937]=("P4")
landtMK$PeriodCar[landtMK$Year>=1938 & landtMK$Year<=1951]=("P5")
landtMK$PeriodCar[landtMK$Year>=1952 & landtMK$Year<=1961]=("P6")
landtMK$PeriodCar[landtMK$Year>=1962 & landtMK$Year<=1970]=("P7")
landtMK$PeriodCar[landtMK$Year>=1971 & landtMK$Year<=1978]=("P8")
landtMK$PeriodCar[landtMK$Year>=1979 & landtMK$Year<=1985]=("P9")
landtMK$PeriodCar[landtMK$Year>=1986 & landtMK$Year<=1990]=("P10")
landtMK$PeriodCar[landtMK$Year>=1991 & landtMK$Year<=1995]=("P11")
landtMK$PeriodCar[landtMK$Year>=1996 & landtMK$Year<=2000]=("P12")
landtMK$PeriodCar[landtMK$Year>=2001 & landtMK$Year<=2003]=("P13")
landtMK$PeriodCar[landtMK$Year>=2004 & landtMK$Year<=2006]=("P14")
landtMK$PeriodCar[landtMK$Year>=2007 & landtMK$Year<=2012]=("P15")
landtMK$PeriodCar=as.factor(landtMK$PeriodCar)
summary(landtMK$PeriodCar)
dim(landtMK)
# 45 x 21

# Gloucester subset for Caraquet ----------------------------
Gloucester=subset(landtMK,Territorial_delim=="Gloucester",select=c(Year, PeriodCar, Territorial_delim,Foin_hay_pct, Ble_weat_pct,Orge_barley_pct, Avoine_oats_pct,Hay_weat_barley_oats_pct))
Gloucester[,1:4]
# Get number of cases by variable
sapply(Gloucester, function(x)(sum(complete.cases(x))))

# Get number of Foin cases by Period
PeriodCases=tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodCar,sum,na.rm=T)/tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodCar,mean,na.rm=T)
PeriodCases
P10 P11 P12 P13 P14 P15  P2  P3  P4  P5  P6  P7  P8  P9 
  1   1   1   1   1   1   3   3   1   2   2   1   1   1 
sum(PeriodCases)
# 20
GPeriodMeanCar=aggregate(Gloucester, by=list(Gloucester$PeriodCar), FUN="mean", na.rm=TRUE)
GPeriodMeanCar

#MK done on 2016-04-14########################
# Caraquet subset ------------------------------------------------

Cart=subset(landtMK, Territorial_delim=="Caraquet",select=c(Year,PeriodCar,Territorial_delim,Cumul_Peat_extract_Pok_pct))
Cart
CartPeriodMeanCar=aggregate(Cart, by=list(Cart$PeriodCar), mean, na.rm=T)
CartPeriodMeanCar

# ++++++++ Merge Agr and peat data+++++++++++++++++++++++++

landtMKCar=merge(GPeriodMeanCar, CartPeriodMeanCar, by="Group.1", all=TRUE)
summary(landtMKCar)

save(landtMKCar,file="landtMKCar.RData")
write.csv(landtMKCar,"landtMKCar.csv")

# For plotting----------------------------------
plot(landtMKCar$Year.x~landtMKCar$Hay_weat_barley_oats_pct, type="b", xlim=c(0,0.06))
points(landtMKCar$Year.x~landtMKCar$Foin_hay_pct, type="b", col="blue", pch="h")
points(landtMKCar$Year.x~landtMKCar$Ble_weat_pct, type="b", col="green", pch="w")
points(landtMKCar$Year.x~landtMKCar$Orge_barley_pct, type="b", col="red", pch="b")
points(landtMKCar$Year.x~landtMKCar$Avoine_oats_pct, type="b", col="purple", pch="o")
points(landtMKCar$Year.x~landtMKCar$Cumul_Peat_extract_Pok_pct, type="b",pch="p", col="black")

############################################################
# Working on Lake Inkerman pigment concentration MK: 2016-04-12 ####
########################################################

epath = file.path("C:/Users/Moumita")
epath
allpigs=read.delim("C:/Users/Moumita/Post Doc at Shippagan/All pigments/Pigments_analyses_to_Moumita.txt",header=T)
summary(allpigs)
dim(allpigs)
# 212 rows x 27 columns
names(allpigs)

# Get the levels of factor "Station"----------------------
levels(allpigs$Station)

# Create a subset for Pokemouche_aval_Lac_Inkerman

PokAvLacInk=subset(allpigs,Station=="Pokemouche_aval_Lac_Inkerman")
dim(PokAvLacInk)
# --------------------------------------------------------------------
allpigs$chla_Pheo=allpigs$Chla2/allpigs$Pheo2
write.csv(allpigs$chla_Pheo,"allpigs$chla_Pheo.csv")
summary(allpigs$chla_Pheo)
allpigs$Chla2
allpigs$Pheo2
edit(allpigs$chla_Pheo)
# Creating subset for Chla_Pheo--------------------
LakeInkAvChla_Pheo=subset(allpigs, Station=="Pokemouche_aval_Lac_Inkerman", select=c(Station,chla_Pheo))
write.csv(allpigs$chla_Pheo,"allpigs$chla_Pheo.csv")
write.csv(LakeInkAvChla_Pheo,"LakeInkAvChla_Pheocsv.csv")



