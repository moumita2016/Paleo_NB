# ######### Part I: pigment concentration computations ##############
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
#============================================================================
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

# Save data as RData, if you want...
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

#MK and AP: March 31st 2016==========================
allpigs$chla_Pheo=allpigs$Chla2/allpigs$Pheo2
write.csv(allpigs$chla_Pheo,"allpigs$chla_Pheo.csv")
summary(allpigs$chla_Pheo)
allpigs$Chla2
allpigs$Pheo2
edit(allpigs$chla_Pheo)

# Creating subset for each station with Chla_Pheo--------------------

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

#--------------------------------------------------------------
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

##########Subset function===============
allpigssub1=subset(allpigs,select=c(Fuco2, Allox2, Diatox2, LutZea2, Canth2, Chla2, Beta_caro2, Pheo2))
dim(allpigssub1)
allpigssub2=subset(allpigs, MedianDepth_cm > 10) 
dim(allpigssub2)
allpigssub3=subset(allpigs, Sample="Malt. 2010 11cm" | Sample="Malt. 2010 13cm", select=(Fuco2:Pheo2))
dim(allpigssub3)
allpigssub4=subset(allpigs,!is.na(Fuco2),select=(Fuco2:Pheo2)) 
cor(alpigssub4, use="pairwise.complete.obs",method="pearson")

########## Dec 1st=================
#Correlation for "Pokemouche_aval_Lac_Inkerman"

> allpigssub3=subset(allpigs, Station=="Pokemouche_aval_Lac_Inkerman")
> dim(allpigssub3)
[1] 29 38
> allpigssub3=subset(allpigs, Station=="Pokemouche_aval_Lac_Inkerman",select=(Fuco2:Pheo2))
> dim(allpigssub3)
[1] 29 11
>cor(allpigssub3, use="pairwise.complete.obs",method="pearson")

#Correlation for "Pokemouche_amont_Maltampec"
>allpigssub3=subset(allpigs, Station=="Pokemouche_amont_Maltampec")
dim(allpigssub3)
>allpigssub3=subset(allpigs, Station=="Pokemouche_amont_Maltampec",select=(Fuco2:Pheo2))
dim(allpigssub3)
>cor(allpigssub3, use="pairwise.complete.obs",method="pearson")

# Correlation for "Pokemouche_amont_Waugh"
>allpigssub3=subset(allpigs, Station=="Pokemouche_amont_Waugh")
dim(allpigssub3)
>allpigssub3=subset(allpigs, Station=="Pokemouche_amont_Waugh",select=(Fuco2:Pheo2))
dim(allpigssub3)
>cor(allpigssub3, use="pairwise.complete.obs",method="pearson")

# Correlation for "Petite_Tracadie_AVAL"
>allpigssub3=subset(allpigs, Station=="Petite_Tracadie_AVAL")
dim(allpigssub3)
>allpigssub3=subset(allpigs, Station=="Petite_Tracadie_AVAL",select=(Fuco2:Pheo2))
dim(allpigssub3)
>cor(allpigssub3, use="pairwise.complete.obs",method="pearson")

# Correlation for "Petite_Tracadie_amont" 
>allpigssub3=subset(allpigs, Station=="Petite_Tracadie_amont")
dim(allpigssub3)
>allpigssub3=subset(allpigs, Station=="Petite_Tracadie_amont",select=(Fuco2:Pheo2))
dim(allpigssub3)
>cor(allpigssub3, use="pairwise.complete.obs",method="pearson")

# Correlation for "Caraquet AVAL" 
>allpigssub3=subset(allpigs, Station=="Caraquet AVAL")
dim(allpigssub3)
>allpigssub3=subset(allpigs, Station=="Caraquet AVAL",select=(Fuco2:Pheo2))
dim(allpigssub3)
>cor(allpigssub3, use="pairwise.complete.obs",method="pearson")

# Correlation for "Tabusintac_amont" 
>allpigssub3=subset(allpigs, Station=="Tabusintac_amont")
dim(allpigssub3)
>allpigssub3=subset(allpigs, Station=="Tabusintac_amont",select=(Fuco2:Pheo2)) 
dim(allpigssub3)
>cor(allpigssub3, use="pairwise.complete.obs",method="pearson")

# Correlation for "Tabusintac_AVAL"  
>allpigssub3=subset(allpigs, Station=="Tabusintac_AVAL")
dim(allpigssub3)
>allpigssub3=subset(allpigs, Station=="Tabusintac_AVAL",select=(Fuco2:Pheo2)) 
dim(allpigssub3)
>cor(allpigssub3, use="pairwise.complete.obs",method="pearson")

##### For PCA use the function prcomp Dec 1st 2015)============================

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

############# PCA Dec 2nd================

allpigssub3=subset(allpigs, Station=="Pokemouche_aval_Lac_Inkerman" & !is.na(Fuco2),select=c(Fuco2, Allox2:Chla2,Beta_caro2,Pheo2))
dim(allpigssub3) # 28 x 10
prcomppigs=prcomp(allpigssub3, scale.=TRUE) 
prcomppigs
png("prcompigs.png")
biplot(prcomppigs)
dev.off()

allpigssub3=subset(allpigs, Station=="Pokemouche_amont_Maltampec" & !is.na(Fuco2),select=c(Fuco2, Allox2:Chla2,Beta_caro2,Pheo2))
dim(allpigssub3)
prcomppigs=prcomp(allpigssub3, scale.=TRUE) 
prcomppigs
png("prcompigs.png")
biplot(prcomppigs)
dev.off()

allpigssub3=subset(allpigs, Station=="Pokemouche_amont_Waugh" & !is.na(Fuco2),select=c(Fuco2, Allox2:Chla2,Beta_caro2,Pheo2))
dim(allpigssub3)
prcomppigs=prcomp(allpigssub3, scale.=TRUE) 
prcomppigs
png("prcompigs.png")
biplot(prcomppigs)
dev.off()

allpigssub3=subset(allpigs, Station=="Petite_Tracadie_AVAL" & !is.na(Fuco2),select=c(Fuco2, Allox2:Chla2,Beta_caro2,Pheo2))
dim(allpigssub3)

allpigssub3=subset(allpigs, Station=="Petite_Tracadie_amont" & !is.na(Fuco2),select=c(Fuco2, Allox2:Chla2,Beta_caro2,Pheo2))
dim(allpigssub3)

allpigssub3=subset(allpigs, Station=="Caraquet AVAL" & !is.na(Fuco2),select=c(Fuco2, Allox2:Chlb2,Chla2,Beta_caro2,Pheo2))
prcomppigs=prcomp(allpigssub3, scale.=TRUE) 

allpigssub3=subset(allpigs, Station=="Tabusintac_AVAL" & !is.na(Fuco2),select=c(Fuco2, Allox2:Chlb2,Chla2,Beta_caro2,Pheo2))
dim(allpigssub3)

allpigssub3=subset(allpigs, Station=="Tabusintac_AVAL" & !is.na(Fuco2),select=c(Fuco2, Allox2:Chla2,Beta_caro2,Pheo2))
dim(allpigssub3)

# Ratio between Chla to pheo
allpigs$chlatopheo=allpigs$chla2/allpigs$pheo2

allpigssub3=subset(allpigs, Station=="Pokemouche_aval_Lac_Inkerman" & !is.na(Fuco2),select=c(Fuco2, Allox2:Chla2,Beta_caro2,Pheo2))

allpigssub3$chla to pheo=subset(allpigs, Station=="Pokemouche_aval_Lac_Inkerman" & !is.na(Fuco2),select=c(Chla2:Pheo2))

write.csv(allpigssub3$chlatopheo,"allpigscsv.csv")
save(allpigssub3,file="allpigssub3.RData")

save.image() http://www.statmethods.net/interface/workspace.html

#PCA summary
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


# ########## Part II: define periods for the meteo data ########
# ################# Dec_2015_Moumita #####################

#Dec 16th_2015_Moumita================================

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
#=============================================================================
# From Alain's computer on 2016-01-05:
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

# --- Maltampec April ---------------
ty4$PeriodMalt[ty4$year_all>=1856 & ty4$year_all<=1895]=("P1")
ty4$PeriodMalt[ty4$year_all>=1896 & ty4$year_all<=1929]=("P2")
ty4$PeriodMalt[ty4$year_all>=1930 & ty4$year_all<=1956]=("P3")
ty4$PeriodMalt[ty4$year_all>=1957 & ty4$year_all<=1981]=("P4")
ty4$PeriodMalt[ty4$year_all>=1982 & ty4$year_all<=2010]=("P5")
ty4$PeriodMalt=as.factor(ty4$PeriodMalt)
summary(ty4$PeriodMalt)
dim(ty4)
#[1] 137 121

# --- Maltampec May ------------------
ty5$PeriodMalt[ty5$year_all>=1856 & ty5$year_all<=1895]=("P1")
ty5$PeriodMalt[ty5$year_all>=1896 & ty5$year_all<=1929]=("P2")
ty5$PeriodMalt[ty5$year_all>=1930 & ty5$year_all<=1956]=("P3")
ty5$PeriodMalt[ty5$year_all>=1957 & ty5$year_all<=1981]=("P4")
ty5$PeriodMalt[ty5$year_all>=1982 & ty5$year_all<=2010]=("P5")
ty5$PeriodMalt=as.factor(ty5$PeriodMalt)
summary(ty5$PeriodMalt)
dim(ty5)
#[1] 137 121

# --- Maltampec June -----------------
ty6$PeriodMalt[ty6$year_all>=1856 & ty6$year_all<=1895]=("P1")
ty6$PeriodMalt[ty6$year_all>=1896 & ty6$year_all<=1929]=("P2")
ty6$PeriodMalt[ty6$year_all>=1930 & ty6$year_all<=1956]=("P3")
ty6$PeriodMalt[ty6$year_all>=1957 & ty6$year_all<=1981]=("P4")
ty6$PeriodMalt[ty6$year_all>=1982 & ty6$year_all<=2010]=("P5")
ty6$PeriodMalt=as.factor(ty6$PeriodMalt)
summary(ty6$PeriodMalt)
dim(ty6)
#[1] 137 121

# --- Maltampec July ------------------------
ty7$PeriodMalt[ty7$year_all>=1856 & ty7$year_all<=1895]=("P1")
ty7$PeriodMalt[ty7$year_all>=1896 & ty7$year_all<=1929]=("P2")
ty7$PeriodMalt[ty7$year_all>=1930 & ty7$year_all<=1956]=("P3")
ty7$PeriodMalt[ty7$year_all>=1957 & ty7$year_all<=1981]=("P4")
ty7$PeriodMalt[ty7$year_all>=1982 & ty7$year_all<=2010]=("P5")
ty7$PeriodMalt=as.factor(ty7$PeriodMalt)
summary(ty7$PeriodMalt)
dim(ty7)
#[1] 138 121

# --- Maltampec August -------------------
ty8$PeriodMalt[ty8$year_all>=1856 & ty8$year_all<=1895]=("P1")
ty8$PeriodMalt[ty8$year_all>=1896 & ty8$year_all<=1929]=("P2")
ty8$PeriodMalt[ty8$year_all>=1930 & ty8$year_all<=1956]=("P3")
ty8$PeriodMalt[ty8$year_all>=1957 & ty8$year_all<=1981]=("P4")
ty8$PeriodMalt[ty8$year_all>=1982 & ty8$year_all<=2010]=("P5")
ty8$PeriodMalt=as.factor(ty8$PeriodMalt)
summary(ty8$PeriodMalt)
dim(ty8)
#[1] 138 121

# =======================================================
# Aggregate data by period, Dec17th_2015
# AP to MK 2016-01-05:
# Now, we need to create Meteo files specific to each station,
# because each station have different period definitions
# reflecting the specific sedimentation history of each station.
# Hence, the names of files will harbour the station name,
# like so:

# Maltampec April -----------------------------
ty4PMalt=aggregate(ty4, by=list(ty4$PeriodMalt), FUN=mean, na.rm=TRUE)
dim(ty4PMalt)
# 5 x 122 on 2016-01-05 by AP
ty4PMalt[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty4PMalt", file="ty4PMalt.RData")
# 2.36 Ko file generated

# Maltampec May -----------------------------
ty5PMalt=aggregate(ty5, by=list(ty5$PeriodMalt), FUN=mean, na.rm=TRUE)
dim(ty5PMalt)
#[1]   5 122 
ty5PMalt[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty5PMalt", file="ty5PMalt.RData")

# Maltampec June -----------------------------
ty6PMalt=aggregate(ty6, by=list(ty6$PeriodMalt), FUN=mean, na.rm=TRUE)
dim(ty6PMalt) 
#[1]   5 122
ty6PMalt[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty6PMalt", file="ty6PMalt.RData")

# Maltampec July -----------------------------
ty7PMalt=aggregate(ty7, by=list(ty7$PeriodMalt), FUN=mean, na.rm=TRUE)
dim(ty7PMalt) 
#[1]   5 122
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

#Jan 8th 2016==============================================
meteoMonctonMalt=c(ty4PMaltbis,ty5PMaltbis,ty6PMaltbis,ty7PMaltbis,ty8PMaltbis)
save("meteoMonctonMalt",file="meteoMonctonMalt.RData")
write.csv(meteoMonctonMalt,"meteoMonctonMalt.csv")

# ================ Periods for Waugh =================
# ========== corrected by AP 2016-01-05 =============

# ---- Waugh April ---------------------------
class(ty4$year_all) # numeric, so we should be able to do maths on it
stem(ty4$year_all)
ty4$PeriodWaugh=as.numeric(ty4$PeriodWaugh)
# Define as numeric first if you get error message
# "invalid factor level, NA generated"
ty4$PeriodWaugh[ty4$year_all>=1866 & ty4$year_all<=1897]="P1"
ty4$PeriodWaugh[ty4$year_all>=1898 & ty4$year_all<=1937]="P2"
ty4$PeriodWaugh[ty4$year_all>=1938 & ty4$year_all<=1968]="P3"
ty4$PeriodWaugh[ty4$year_all>=1969 & ty4$year_all<=1984]="P4"
ty4$PeriodWaugh[ty4$year_all>=1985 & ty4$year_all<=1996]="P5"
ty4$PeriodWaugh[ty4$year_all>=1997 & ty4$year_all<=2010]="P6"
ty4$PeriodWaugh=as.factor(ty4$PeriodWaugh)
summary(ty4$PeriodWaugh)
# AP 2016-01-05:
# P1 P2 P3 P4 P5 P6
# 25 40 31 16 12 13
dim(ty4)
#[1] 137 121
# AP to MK 2016-01-05: execute the following lines
# for Waugh May through August...

# --- Waugh May ------------------------
ty5$PeriodWaugh[ty5$year_all>=1866 & ty5$year_all<=1897]=("P1")
ty5$PeriodWaugh[ty5$year_all>=1898 & ty5$year_all<=1937]=("P2")
ty5$PeriodWaugh[ty5$year_all>=1938 & ty5$year_all<=1968]=("P3")
ty5$PeriodWaugh[ty5$year_all>=1969 & ty5$year_all<=1984]=("P4")
ty5$PeriodWaugh[ty5$year_all>=1985 & ty5$year_all<=1996]=("P5")
ty5$PeriodWaugh[ty5$year_all>=1997 & ty5$year_all<=2010]=("P6")
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
ty6$PeriodWaugh[ty6$year_all>=1866 & ty6$year_all<=1897]=("P1")
ty6$PeriodWaugh[ty6$year_all>=1898 & ty6$year_all<=1937]=("P2")
ty6$PeriodWaugh[ty6$year_all>=1938 & ty6$year_all<=1968]=("P3")
ty6$PeriodWaugh[ty6$year_all>=1969 & ty6$year_all<=1984]=("P4")
ty6$PeriodWaugh[ty6$year_all>=1985 & ty6$year_all<=1996]=("P5")
ty6$PeriodWaugh[ty6$year_all>=1997 & ty6$year_all<=2010]=("P6")
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
ty7$PeriodWaugh[ty7$year_all>=1866 & ty7$year_all<=1897]=("P1")
ty7$PeriodWaugh[ty7$year_all>=1898 & ty7$year_all<=1937]=("P2")
ty7$PeriodWaugh[ty7$year_all>=1938 & ty7$year_all<=1968]=("P3")
ty7$PeriodWaugh[ty7$year_all>=1969 & ty7$year_all<=1984]=("P4")
ty7$PeriodWaugh[ty7$year_all>=1985 & ty7$year_all<=1996]=("P5")
ty7$PeriodWaugh[ty7$year_all>=1997 & ty7$year_all<=2010]=("P6")
ty7$PeriodWaugh=as.factor(ty7$PeriodWaugh)
summary(ty7$PeriodWaugh)
dim(ty7)
#[1] 138 121

class(ty8$year_all) # numeric, so we should be able to do maths on it
stem(ty8$year_all)
ty8$PeriodWaugh=as.numeric(ty8$PeriodWaugh)

# --- Waugh August -------------------------------
ty8$PeriodWaugh[ty8$year_all>=1866 & ty8$year_all<=1897]=("P1")
ty8$PeriodWaugh[ty8$year_all>=1898 & ty8$year_all<=1937]=("P2")
ty8$PeriodWaugh[ty8$year_all>=1938 & ty8$year_all<=1968]=("P3")
ty8$PeriodWaugh[ty8$year_all>=1969 & ty8$year_all<=1984]=("P4")
ty8$PeriodWaugh[ty8$year_all>=1985 & ty8$year_all<=1996]=("P5")
ty8$PeriodWaugh[ty8$year_all>=1997 & ty8$year_all<=2010]=("P6")
ty8$PeriodWaugh=as.factor(ty8$PeriodWaugh)
summary(ty8$PeriodWaugh)
dim(ty8)
#[1] 138 121

#=======================================================
# Waugh April -----------------------------
ty4PWau=aggregate(ty4, by=list(ty4$PeriodWaugh), FUN=mean, na.rm=TRUE)
dim(ty4PWau)
#[1]   6 122
ty4PWau[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty4PWau", file="ty4PWau.RData")

# Waugh May -----------------------------
ty5PWau=aggregate(ty5, by=list(ty5$PeriodWaugh), FUN=mean, na.rm=TRUE)
dim(ty5PWau) 
#[1]   6 122
ty5PWau[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty5PWau", file="ty5PWau.RData")

# Waugh June -----------------------------
ty6PWau=aggregate(ty6, by=list(ty6$PeriodWaugh), FUN=mean, na.rm=TRUE)
dim(ty6PWau) 
#[1]   6 122
ty6PWau[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty6PWau", file="ty6PWau.RData")

# Waugh July -----------------------------
ty7PWau=aggregate(ty7, by=list(ty7$PeriodWaugh), FUN=mean, na.rm=TRUE)
dim(ty7PWau) 
#[1]   6 122
ty7PWau[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty7PWau", file="ty7PWau.RData")

# Waugh August -----------------------------
ty8PWau=aggregate(ty8, by=list(ty8$PeriodWaugh), FUN=mean, na.rm=TRUE)
dim(ty8PWau) 
#[1]   6 122
ty8PWau[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty8PWau", file="ty8PWau.RData")
#====================================================================
MK 2016-01-09: create a new series of tables containing only the
# variables of interest

ty4PWaubis=ty4PWau[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty4PWaubis) 
#[1] 6 3
ty5PWaubis=ty5PWau[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty5PWaubis) 
#[1] 6 3
ty6PWaubis=ty6PWau[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty6PWaubis)
#[1] 6 3
ty7PWaubis=ty7PWau[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty7PWaubis)
#[1] 6 3
ty8PWaubis=ty8PWau[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty8PWaubis)
#[1] 6 3

#Jan 9th 2016==============================================
meteoMonctonWau=c(ty4PWaubis,ty5PWaubis,ty6PWaubis,ty7PWaubis,ty8PWaubis)
save("meteoMonctonWau",file="meteoMonctonWau.RData")
write.csv(meteoMonctonWau,"meteoMonctonWau.csv")

# ================ Periods for Petite Tracadie =================
# --- Petite Tracadie April ---------------
# All the seperation of periods based on CRS 3rd model (Binford, 1990)_can find at all_dates_together excel sheet
# P1 does not exsist as the first environmental data starts at 1873.

ty4$PeriodPetitAM[ty4$year_all>=1817 & ty4$year_all<=1866]=("P1")
ty4$PeriodPetitAM[ty4$year_all>=1867 & ty4$year_all<=1901]=("P2")
ty4$PeriodPetitAM[ty4$year_all>=1902 & ty4$year_all<=1930]=("P3")
ty4$PeriodPetitAM[ty4$year_all>=1931 & ty4$year_all<=1957]=("P4")
ty4$PeriodPetitAM[ty4$year_all>=1958 & ty4$year_all<=1970]=("P5")
ty4$PeriodPetitAM[ty4$year_all>=1971 & ty4$year_all<=1981]=("P6")
ty4$PeriodPetitAM[ty4$year_all>=1982 & ty4$year_all<=1988]=("P7")
ty4$PeriodPetitAM[ty4$year_all>=1989 & ty4$year_all<=1994]=("P8")
ty4$PeriodPetitAM[ty4$year_all>=1995 & ty4$year_all<=1999]=("P9")
ty4$PeriodPetitAM[ty4$year_all>=2000 & ty4$year_all<=2004]=("P10")
ty4$PeriodPetitAM[ty4$year_all>=2005 & ty4$year_all<=2010]=("P11")
ty4$PeriodPetitAM=as.factor(ty4$PeriodPetitAM)
summary(ty4$PeriodPetitAM)
dim(ty4)
[1] 137 121
# --- Petite Tracadie May ---------------
ty5$PeriodPetitAM[ty5$year_all>=1817 & ty5$year_all<=1866]=("P1")
ty5$PeriodPetitAM[ty5$year_all>=1867 & ty5$year_all<=1901]=("P2")
ty5$PeriodPetitAM[ty5$year_all>=1902 & ty5$year_all<=1930]=("P3")
ty5$PeriodPetitAM[ty5$year_all>=1931 & ty5$year_all<=1957]=("P4")
ty5$PeriodPetitAM[ty5$year_all>=1958 & ty5$year_all<=1970]=("P5")
ty5$PeriodPetitAM[ty5$year_all>=1971 & ty5$year_all<=1981]=("P6")
ty5$PeriodPetitAM[ty5$year_all>=1982 & ty5$year_all<=1988]=("P7")
ty5$PeriodPetitAM[ty5$year_all>=1989 & ty5$year_all<=1994]=("P8")
ty5$PeriodPetitAM[ty5$year_all>=1995 & ty5$year_all<=1999]=("P9")
ty5$PeriodPetitAM[ty5$year_all>=2000 & ty5$year_all<=2004]=("P10")
ty5$PeriodPetitAM[ty5$year_all>=2005 & ty5$year_all<=2010]=("P11")
ty5$PeriodPetitAM=as.factor(ty5$PeriodPetitAM)
summary(ty5$PeriodPetitAM)
dim(ty5)
[1] 137 121
# --- Petite Tracadie June ---------------

ty6$PeriodPetitAM[ty6$year_all>=1817 & ty6$year_all<=1866]=("P1")
ty6$PeriodPetitAM[ty6$year_all>=1867 & ty6$year_all<=1901]=("P2")
ty6$PeriodPetitAM[ty6$year_all>=1902 & ty6$year_all<=1930]=("P3")
ty6$PeriodPetitAM[ty6$year_all>=1931 & ty6$year_all<=1957]=("P4")
ty6$PeriodPetitAM[ty6$year_all>=1958 & ty6$year_all<=1970]=("P5")
ty6$PeriodPetitAM[ty6$year_all>=1971 & ty6$year_all<=1981]=("P6")
ty6$PeriodPetitAM[ty6$year_all>=1982 & ty6$year_all<=1988]=("P7")
ty6$PeriodPetitAM[ty6$year_all>=1989 & ty6$year_all<=1994]=("P8")
ty6$PeriodPetitAM[ty6$year_all>=1995 & ty6$year_all<=1999]=("P9")
ty6$PeriodPetitAM[ty6$year_all>=2000 & ty6$year_all<=2004]=("P10")
ty6$PeriodPetitAM[ty6$year_all>=2005 & ty6$year_all<=2010]=("P11")
ty6$PeriodPetitAM=as.factor(ty6$PeriodPetitAM)
summary(ty6$PeriodPetitAM)
dim(ty6)
[1] 137 121
# --- Petite Tracadie July ---------------
ty7$PeriodPetitAM[ty7$year_all>=1817 & ty7$year_all<=1866]=("P1")
ty7$PeriodPetitAM[ty7$year_all>=1867 & ty7$year_all<=1901]=("P2")
ty7$PeriodPetitAM[ty7$year_all>=1902 & ty7$year_all<=1930]=("P3")
ty7$PeriodPetitAM[ty7$year_all>=1931 & ty7$year_all<=1957]=("P4")
ty7$PeriodPetitAM[ty7$year_all>=1958 & ty7$year_all<=1970]=("P5")
ty7$PeriodPetitAM[ty7$year_all>=1971 & ty7$year_all<=1981]=("P6")
ty7$PeriodPetitAM[ty7$year_all>=1982 & ty7$year_all<=1988]=("P7")
ty7$PeriodPetitAM[ty7$year_all>=1989 & ty7$year_all<=1994]=("P8")
ty7$PeriodPetitAM[ty7$year_all>=1995 & ty7$year_all<=1999]=("P9")
ty7$PeriodPetitAM[ty7$year_all>=2000 & ty7$year_all<=2004]=("P10")
ty7$PeriodPetitAM[ty7$year_all>=2005 & ty7$year_all<=2010]=("P11")
ty7$PeriodPetitAM=as.factor(ty7$PeriodPetitAM)
summary(ty7$PeriodPetitAM)
dim(ty7)
[1] 138 121
# --- Petite Tracadie August ---------------

ty8$PeriodPetitAM[ty8$year_all>=1817 & ty8$year_all<=1866]=("P1")
ty8$PeriodPetitAM[ty8$year_all>=1867 & ty8$year_all<=1901]=("P2")
ty8$PeriodPetitAM[ty8$year_all>=1902 & ty8$year_all<=1930]=("P3")
ty8$PeriodPetitAM[ty8$year_all>=1931 & ty8$year_all<=1957]=("P4")
ty8$PeriodPetitAM[ty8$year_all>=1958 & ty8$year_all<=1970]=("P5")
ty8$PeriodPetitAM[ty8$year_all>=1971 & ty8$year_all<=1981]=("P6")
ty8$PeriodPetitAM[ty8$year_all>=1982 & ty8$year_all<=1988]=("P7")
ty8$PeriodPetitAM[ty8$year_all>=1989 & ty8$year_all<=1994]=("P8")
ty8$PeriodPetitAM[ty8$year_all>=1995 & ty8$year_all<=1999]=("P9")
ty8$PeriodPetitAM[ty8$year_all>=2000 & ty8$year_all<=2004]=("P10")
ty8$PeriodPetitAM[ty8$year_all>=2005 & ty8$year_all<=2010]=("P11")
ty8$PeriodPetitAM=as.factor(ty8$PeriodPetitAM)
summary(ty8$PeriodPetitAM)
dim(ty8)
[1] 138 121
#======================================================================
#Petite Travadie Amont April---------------------------
ty4PPetit=aggregate(ty4, by=list(ty4$PeriodPetitAM), FUN=mean, na.rm=TRUE)
dim(ty4PPetit)
[1]  10 122
ty4PPetit[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty4PPetit", file="ty4PPetit.RData")

# Petite Tracadie May -----------------------------
ty5PPetit=aggregate(ty5, by=list(ty5$PeriodPetitAM), FUN=mean, na.rm=TRUE)
dim(ty5PPetit) 
10 122
ty5PPetit[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty5PPetit", file="ty5PPetit.RData")

# Petitte Tracadie June -----------------------------
ty6PPetit=aggregate(ty6, by=list(ty6$PeriodPetitAM), FUN=mean, na.rm=TRUE)
dim(ty6PPetit) 
[1]  10 122
ty6PPetit[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty6PPetit", file="ty6PPetit.RData")

# Petite Tracadie July -----------------------------
ty7PPetit=aggregate(ty7, by=list(ty7$PeriodPetitAM), FUN=mean, na.rm=TRUE)
dim(ty7PPetit) 
[1]  10 122
ty7PPetit[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty7PPetit", file="ty7PPetit.RData")

# Petite Tracadie August -----------------------------
ty8PPetit=aggregate(ty8, by=list(ty8$PeriodPetitAM), FUN=mean, na.rm=TRUE)
dim(ty8PPetit) 
[1]  10 122
ty8PPetit[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty8PPetit", file="ty8PPetit.RData")

# ======================================================================
MK 2016-01-10: create a new series of tables containing only the
# variables of interest

ty4PPetitbis=ty4PPetit[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty4PPetitbis)
# [1] 10  3
ty5PPetitbis=ty5PPetit[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty5PPetitbis) 
#[1] 10  3
ty6PPetitbis=ty6PPetit[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty6PPetitbis)
#[1] 10  3
ty7PPetitbis=ty7PPetit[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty7PPetitbis)
#[1] 10  3
ty8PPetitbis=ty8PPetit[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty8PPetitbis)
#[1] 10  3
meteoMonctonPetit=c(ty4PPetitbis,ty5PPetitbis,ty6PPetitbis,ty7PPetitbis,ty8PPetitbis)
save("meteoMonctonPetit",file="meteoMonctonPetit.RData")
write.csv(meteoMonctonPetit,"meteoMonctonPetit.csv")-----------------------

#==================Shippagan west=============================
MK 2016-03-30
# --- Shippagan west April ---------------
class(ty4$year_all) # numeric, so we should be able to do maths on it
stem(ty4$year_all)
ty4$PeriodShiwes=as.numeric(ty4$PeriodShiwes)

ty4$PeriodShiwes[ty4$year_all>=1905 & ty4$year_all<=1949]=("P1")
ty4$PeriodShiwes[ty4$year_all>=1950 & ty4$year_all<=1983]=("P2")
ty4$PeriodShiwes[ty4$year_all>=1984 & ty4$year_all<=2000]=("P3")
ty4$PeriodShiwes[ty4$year_all>=2001 & ty4$year_all<=2010]=("P4")
ty4$PeriodShiwes=as.factor(ty4$PeriodShiwes)
summary(ty4$PeriodShiwes)
dim(ty4)
#[1] 137 121
# --- Shippagan west May ------------------
ty5$PeriodShiwes[ty5$year_all>=1905 & ty5$year_all<=1949]=("P1")
ty5$PeriodShiwes[ty5$year_all>=1950 & ty5$year_all<=1983]=("P2")
ty5$PeriodShiwes[ty5$year_all>=1984 & ty5$year_all<=2000]=("P3")
ty5$PeriodShiwes[ty5$year_all>=2001 & ty5$year_all<=2010]=("P4")
ty5$PeriodShiwes=as.factor(ty5$PeriodShiwes)
summary(ty5$PeriodShiwes)
dim(ty5)
#[1] 137 121
# --- Shippagan west June -----------------
ty6$PeriodShiwes[ty6$year_all>=1905 & ty6$year_all<=1949]=("P1")
ty6$PeriodShiwes[ty6$year_all>=1950 & ty6$year_all<=1983]=("P2")
ty6$PeriodShiwes[ty6$year_all>=1984 & ty6$year_all<=2000]=("P3")
ty6$PeriodShiwes[ty6$year_all>=2001 & ty6$year_all<=2010]=("P4")
ty6$PeriodShiwes=as.factor(ty6$PeriodShiwes)
summary(ty6$PeriodShiwes)
dim(ty6)
#[1] 137 121
# --- Shippagan west July ------------------------
ty7$PeriodShiwes[ty7$year_all>=1905 & ty7$year_all<=1949]=("P1")
ty7$PeriodShiwes[ty7$year_all>=1950 & ty7$year_all<=1983]=("P2")
ty7$PeriodShiwes[ty7$year_all>=1984 & ty7$year_all<=2000]=("P3")
ty7$PeriodShiwes[ty7$year_all>=2001 & ty7$year_all<=2010]=("P4")
ty7$PeriodShiwes=as.factor(ty7$PeriodShiwes)
summary(ty7$PeriodShiwes)
dim(ty7)
#[1] 138 121
# --- Shippagan west August -------------------
ty8$PeriodShiwes[ty8$year_all>=1905 & ty8$year_all<=1949]=("P1")
ty8$PeriodShiwes[ty8$year_all>=1950 & ty8$year_all<=1983]=("P2")
ty8$PeriodShiwes[ty8$year_all>=1984 & ty8$year_all<=2000]=("P3")
ty8$PeriodShiwes[ty8$year_all>=2001 & ty8$year_all<=2010]=("P4")
ty8$PeriodShiwes=as.factor(ty8$PeriodShiwes)
summary(ty8$PeriodShiwes)
dim(ty8)
#[1] 138 121

#MK 2016-03-30================================================

# Shippagan west April -----------------------------
ty4PShiwes=aggregate(ty4, by=list(ty4$PeriodShiwes), FUN=mean, na.rm=TRUE)
dim(ty4PShiwes)
#[1]   4 122
ty4PShiwes[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty4PShiwes", file="ty4PShiwes.RData")

# Shippagan west May -----------------------------
ty5PShiwes=aggregate(ty5, by=list(ty5$PeriodShiwes), FUN=mean, na.rm=TRUE)
dim(ty5PShiwes)
#[1]   4 122
ty5PShiwes[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty5PShiwes", file="ty5PShiwes.RData")

# Shippagan west June -----------------------------
ty6PShiwes=aggregate(ty6, by=list(ty6$PeriodShiwes), FUN=mean, na.rm=TRUE)
dim(ty6PShiwes)
#[1]   4 122
ty6PShiwes[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty6PShiwes", file="ty6PShiwes.RData")

# Shippagan west July -----------------------------
ty7PShiwes=aggregate(ty7, by=list(ty7$PeriodShiwes), FUN=mean, na.rm=TRUE)
dim(ty7PShiwes)
ty7PShiwes[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty7PShiwes", file="ty7PShiwes.RData")

# Shippagan west Aug -----------------------------
ty8PShiwes=aggregate(ty8, by=list(ty8$PeriodShiwes), FUN=mean, na.rm=TRUE)
dim(ty8PShiwes)
#[1]   4 122
ty8PShiwes[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty8PShiwes", file="ty8PShiwes.RData")

#==========================================================================
MK 2016-03-30

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
#-------Shippagan east April------------------
ty4$PeriodShieas[ty4$year_all>=1921 & ty4$year_all<=1942]=("P1")
ty4$PeriodShieas[ty4$year_all>=1943 & ty4$year_all<=1963]=("P2")
ty4$PeriodShieas[ty4$year_all>=1964 & ty4$year_all<=1979]=("P3")
ty4$PeriodShieas[ty4$year_all>=1980 & ty4$year_all<=1991]=("P4")
ty4$PeriodShieas[ty4$year_all>=1992 & ty4$year_all<=2000]=("P5")
ty4$PeriodShieas[ty4$year_all>=2001 & ty4$year_all<=2010]=("P6")
ty4$PeriodShieas=as.factor(ty4$PeriodShieas)
summary(ty4$PeriodShieas)
dim(ty4)
# 137 x 121
#-------Shippagan east May------------------
ty5$PeriodShieas[ty5$year_all>=1921 & ty5$year_all<=1942]=("P1")
ty5$PeriodShieas[ty5$year_all>=1943 & ty5$year_all<=1963]=("P2")
ty5$PeriodShieas[ty5$year_all>=1964 & ty5$year_all<=1979]=("P3")
ty5$PeriodShieas[ty5$year_all>=1980 & ty5$year_all<=1991]=("P4")
ty5$PeriodShieas[ty5$year_all>=1992 & ty5$year_all<=2000]=("P5")
ty5$PeriodShieas[ty5$year_all>=2001 & ty5$year_all<=2010]=("P6")
ty5$PeriodShieas=as.factor(ty5$PeriodShieas)
summary(ty5$PeriodShieas)
dim(ty5)
# 137 x 121

#-------Shippagan east June------------------
ty6$PeriodShieas[ty6$year_all>=1921 & ty6$year_all<=1942]=("P1")
ty6$PeriodShieas[ty6$year_all>=1943 & ty6$year_all<=1963]=("P2")
ty6$PeriodShieas[ty6$year_all>=1964 & ty6$year_all<=1979]=("P3")
ty6$PeriodShieas[ty6$year_all>=1980 & ty6$year_all<=1991]=("P4")
ty6$PeriodShieas[ty6$year_all>=1992 & ty6$year_all<=2000]=("P5")
ty6$PeriodShieas[ty6$year_all>=2001 & ty6$year_all<=2010]=("P6")
ty6$PeriodShieas=as.factor(ty6$PeriodShieas)
summary(ty6$PeriodShieas)
dim(ty6)
# 137 x 121

#-------Shippagan east July------------------
ty7$PeriodShieas[ty7$year_all>=1921 & ty7$year_all<=1942]=("P1")
ty7$PeriodShieas[ty7$year_all>=1943 & ty7$year_all<=1963]=("P2")
ty7$PeriodShieas[ty7$year_all>=1964 & ty7$year_all<=1979]=("P3")
ty7$PeriodShieas[ty7$year_all>=1980 & ty7$year_all<=1991]=("P4")
ty7$PeriodShieas[ty7$year_all>=1992 & ty7$year_all<=2000]=("P5")
ty7$PeriodShieas[ty7$year_all>=2001 & ty7$year_all<=2010]=("P6")
ty7$PeriodShieas=as.factor(ty7$PeriodShieas)
summary(ty7$PeriodShieas)
dim(ty7)
# 138 x 121
#-------Shippagan east August------------------
ty8$PeriodShieas[ty8$year_all>=1921 & ty8$year_all<=1942]=("P1")
ty8$PeriodShieas[ty8$year_all>=1943 & ty8$year_all<=1963]=("P2")
ty8$PeriodShieas[ty8$year_all>=1964 & ty8$year_all<=1979]=("P3")
ty8$PeriodShieas[ty8$year_all>=1980 & ty8$year_all<=1991]=("P4")
ty8$PeriodShieas[ty8$year_all>=1992 & ty8$year_all<=2000]=("P5")
ty8$PeriodShieas[ty8$year_all>=2001 & ty8$year_all<=2010]=("P6")
ty8$PeriodShieas=as.factor(ty8$PeriodShieas)
summary(ty8$PeriodShieas)
dim(ty8)
# 138 x 121

#============================================================================
# Shippagan east April------------------
ty4PShieas=aggregate(ty4, by=list(ty4$PeriodShieas), FUN=mean, na.rm=TRUE)
dim(ty4PShieas)
# 6 x 122
ty4PShieas[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty4PShieas", file="ty4PShieas.RData")

# Shippagan east May------------------
ty5PShieas=aggregate(ty5, by=list(ty5$PeriodShieas), FUN=mean, na.rm=TRUE)
dim(ty5PShieas)
# 6 x 122
ty5PShieas[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty5PShieas", file="ty5PShieas.RData")

# Shippagan east June------------------
ty6PShieas=aggregate(ty6, by=list(ty6$PeriodShieas), FUN=mean, na.rm=TRUE)
dim(ty6PShieas)
#6 x 122
ty6PShieas[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty6PShieas", file="ty6PShieas.RData")

# Shippagan east July------------------
ty7PShieas=aggregate(ty7, by=list(ty7$PeriodShieas), FUN=mean, na.rm=TRUE)
dim(ty7PShieas)
#6 x 122
ty7PShieas[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty7PShieas", file="ty7PShieas.RData")

# Shippagan east Aug------------------
ty8PShieas=aggregate(ty8, by=list(ty8$PeriodShieas), FUN=mean, na.rm=TRUE)
dim(ty8PShieas)
#6 x 122
ty8PShieas[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
save("ty8PShieas", file="ty8PShieas.RData")

#=================================================================================

ty4PShieasbis=ty4PShieas[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty4PShieasbis)
# 6 x 3
ty5PShieasbis=ty5PShieas[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty5PShieasbis) 
# 6 x 3
ty6PShieasbis=ty6PShieas[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty6PShieasbis)
# 6 x 3
ty7PShieasbis=ty7PShieas[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty7PShieasbis)
# 6 x 3
ty8PShieasbis=ty8PShieas[,c("Group.1","Precip_mm_Moncton3", "Tmoy_C_Moncton3")]
dim(ty8PShieasbis)
# 6 x 3
meteoMonctonShieas=c(ty4PShieasbis,ty5PShieasbis,ty6PShieasbis,ty7PShieasbis,ty8PShieasbis)
save("meteoMonctonShieas",file="meteoMonctonShieas.RData")
write.csv(meteoMonctonShieas,"meteoMonctonShieas.csv")

# ########## Part III: define periods for the landuse data ########
# ################# Dec_2015_Moumita #####################

###########Dec17th _2015_Moumita=================

pathland = file.path("C:/Users/Moumita")
pathland
pathland = file.path("/Post Doc at Shipgaan/151210_env_scripts_from_Alain_to_Moumita/land_data")
getwd()
landt=read.delim2("Land_use_data4.txt", header=TRUE)
ls()
landt
names(landt)
summary(landt$Year)
edit(landt)

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

#Summary-----------------------------------
summary(landt)
names(landt)
subset(landt,select=c(Year,Period,Cumul_Peat_extract_Pok_pct))
landt$Year
class(landt$Year)
summary(landt$Year)

#MK March 31st 2016=================================
#######Based on Maltempec defined period=============

landt$PeriodMalt[landt$Year>=1856 & landt$Year<=1895]=("P1")
landt$PeriodMalt[landt$Year>=1896 & landt$Year<=1929]=("P2")
landt$PeriodMalt[landt$Year>=1930 & landt$Year<=1956]=("P3")
landt$PeriodMalt[landt$Year>=1957 & landt$Year<=1981]=("P4")
landt$PeriodMalt[landt$Year>=1982 & landt$Year<=2010]=("P5")
landt$PeriodMalt=as.factor(landt$PeriodMalt)
summary(landt$PeriodMalt)
dim(landt)
# 29 x 21
landt$PeriodMalt=as.factor(landt$PeriodMalt)
summary(landt$PeriodMalt)
save(file="landt$PeriodMalt.RData")

Gloucester represesents the agricultural data while Pokemouche pertains to the
peat extraction data.

Moumita: we have peat extraction data specific to other catchments
(Caraquet, Petite Tracadie, Tabusintac). I need to get them out of the
GIS files and send them to you. Because the Gloucester county
encompasses all catchments, you can use the same agricultural data for
all catchments.

#Gloucester subset----------------------------

Gloucester=subset(landt,Territorial_delim=="Gloucester",select=c(Year, Period, PeriodMalt, Territorial_delim,Foin_hay_pct, Ble_weat_pct,Orge_barley_pct, Avoine_oats_pct,Hay_weat_barley_oats_pct))
Gloucester[,1:4]
Gloucester
# Get number of cases by variable
sapply(Gloucester, function(x)(sum(complete.cases(x))))

# Get number of Foin cases by Period
PeriodCases=tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodMalt,sum,na.rm=T)/tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodMalt,mean,na.rm=T)
PeriodCases
P1 P2 P3 P4 P5 
 3  3  4  4  5 
sum(PeriodCases)
# 19

++++++++++By PeriodMalt done 2016-03-31+++++++++++++++++++++++

GPeriodMeanMalt=aggregate(Gloucester, by=list(Gloucester$PeriodMalt), FUN="mean", na.rm=TRUE)
GPeriodMeanMalt

#Pokemouche subset------------------------------------------------

Pok=subset(landt, Territorial_delim=="Bassin_Pokemouche",select=c(Year,PeriodMalt,Territorial_delim,Cumul_Peat_extract_Pok_pct))
Pok
PokPeriodMeanMalt=aggregate(Pok, by=list(Pok$PeriodMalt), mean, na.rm=T)
PokPeriodMeanMalt

#Merge Agr and peat data+++++++++++++++++++++++++

landtMalt=merge(GPeriodMeanMalt, PokPeriodMeanMalt, by="Group.1", all=TRUE)
summary(landtMalt)

save(landtMalt,file="landtMalt.RData")
write.csv(landtMalt,"landtMalt.csv")

# For plotting---------------------------
plot(landtMalt$Year.x~landtMalt$Hay_weat_barley_oats_pct, type="b", xlim=c(0,0.06))
points(landtMalt$Year.x~landtMalt$Foin_hay_pct, type="b", col="blue", pch="h")
points(landtMalt$Year.x~landtMalt$Ble_weat_pct, type="b", col="green", pch="w")
points(landtMalt$Year.x~landtMalt$Orge_barley_pct, type="b", col="red", pch="b")
points(landtMalt$Year.x~landtMalt$Avoine_oats_pct, type="b", col="purple", pch="o")
points(landtMalt$Year.x~landtMalt$Cumul_Peat_extract_Pok_pct, type="b",pch="p", col="black")

#######Based on Waugh defined period=============

landt$PeriodWau[landt$Year>=1866 & landt$Year<=1897]=("P1")
landt$PeriodWau[landt$Year>=1898 & landt$Year<=1937]=("P2")
landt$PeriodWau[landt$Year>=1938 & landt$Year<=1968]=("P3")
landt$PeriodWau[landt$Year>=1969 & landt$Year<=1984]=("P4")
landt$PeriodWau[landt$Year>=1985 & landt$Year<=1996]=("P5")
landt$PeriodWau[landt$Year>=1997 & landt$Year<=2010]=("P6")
landt$PeriodWau=as.factor(landt$PeriodWau)
summary(landt$PeriodWau)
dim(landt)
# 29 X 21

#Gloucester subset----------------------------
#By PeriodWau done 2016-03-31+++++++++++++++++++++++

Gloucester=subset(landt,Territorial_delim=="Gloucester",select=c(Year, Period, PeriodWau, Territorial_delim,Foin_hay_pct, Ble_weat_pct,Orge_barley_pct, Avoine_oats_pct,Hay_weat_barley_oats_pct))
Gloucester[,1:4]
Gloucester
# Get number of cases by variable
sapply(Gloucester, function(x)(sum(complete.cases(x))))

# Get number of Foin cases by Period
PeriodCases=tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodWau,sum,na.rm=T)/tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodWau,mean,na.rm=T)
PeriodCases
P1 P2 P3 P4 P5 P6 
3  4  5  2  3  2 
sum(PeriodCases)
# 19
GPeriodMeanWau=aggregate(Gloucester, by=list(Gloucester$PeriodWau), FUN="mean", na.rm=TRUE)
GPeriodMeanWau

#Pokemouche subset------------------------------------------------

Pok=subset(landt, Territorial_delim=="Bassin_Pokemouche",select=c(Year,PeriodWau,Territorial_delim,Cumul_Peat_extract_Pok_pct))
Pok
PokPeriodMeanWau=aggregate(Pok, by=list(Pok$PeriodWau), mean, na.rm=T)
PokPeriodMeanWau

++++++++ Merge Agr and peat data+++++++++++++++++++++++++

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


#######Based on Petite Tracadie Amont defined period=============

#MK April 11th 2016------------------------------
pathland = file.path("C:/Users/Moumita")
pathland
pathland = file.path("/Post Doc at Shipgaan/151210_env_scripts_from_Alain_to_Moumita/land_data")
getwd()
landt=read.delim("Land_use_data4_MK.txt", header=TRUE)
ls()
landt
subset(landt,select=c(Year, Territorial_delim,Cumul_Peat_extract_Pok_ha))
names(landt)
summary(landt$Year)
edit(landt)

Cumul_Peat_extract_Pok_pct=tapply(landt$Territorial_delim=="Petite_Tracadie",Cumul_Peat_extract_Pok_ha,na.rm=T)/tapply(landt$Territorial_delim=="Petite_Tracadie",Surface_census_km2,na.rm=T)

landt$PeriodPtAm[landt$Year>=1817 & landt$Year<=1866]=("P1")
landt$PeriodPtAm[landt$Year>=1867 & landt$Year<=1901]=("P2")
landt$PeriodPtAm[landt$Year>=1902 & landt$Year<=1930]=("P3")
landt$PeriodPtAm[landt$Year>=1931 & landt$Year<=1957]=("P4")
landt$PeriodPtAm[landt$Year>=1958 & landt$Year<=1970]=("P5")
landt$PeriodPtAm[landt$Year>=1971 & landt$Year<=1981]=("P6")
landt$PeriodPtAm[landt$Year>=1982 & landt$Year<=1988]=("P7")
landt$PeriodPtAm[landt$Year>=1989 & landt$Year<=1994]=("P8")
landt$PeriodPtAm[landt$Year>=1995 & landt$Year<=1999]=("P9")
landt$PeriodPtAm[landt$Year>=2000 & landt$Year<=2004]=("P10")
landt$PeriodPtAm[landt$Year>=2005 & landt$Year<=2010]=("P11")
landt$PeriodPtAm=as.factor(landt$PeriodPtAm)
summary(landt$PeriodPtAm)
dim(landt)
# 45 X 21

++++++++++By Petite Tracadie done 2016-04-11+++++++++++++++++++++++

Gloucester=subset(landt,Territorial_delim=="Gloucester",select=c(Year, Period, PeriodPtAm, Territorial_delim,Foin_hay_pct, Ble_weat_pct,Orge_barley_pct, Avoine_oats_pct,Hay_weat_barley_oats_pct))
Gloucester[,1:4]
Gloucester
# Get number of cases by variable
sapply(Gloucester, function(x)(sum(complete.cases(x))))

# Get number of Foin cases by Period
PeriodCases=tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodPtAm,sum,na.rm=T)/tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodPtAm,mean,na.rm=T)
PeriodCases
 P1 P10 P11  P2  P3  P4  P5  P6  P7  P8  P9 
NaN   1   1   4   2   4   2   2   1   1   1 
sum(PeriodCases,na.rm=T)
x 19

GPeriodMeanPtAm=aggregate(Gloucester, by=list(Gloucester$PeriodPtAm), FUN="mean", na.rm=TRUE)
GPeriodMeanPtAm

#MK:Done on 2016-04-11--------------------------

Pet=subset(landt, Territorial_delim=="Petite_Tracadie",select=c(Year,PeriodPtAm,Territorial_delim,Cumul_Peat_extract_Pok_pct))
Pet
PetPeriodMeanPtAm=aggregate(Pet, by=list(Pet$PeriodPtAm), mean, na.rm=T)
PetPeriodMeanPtAm

++++++++ Merge Agr and peat data+++++++++++++++++++++++++

landtPtAm=merge(GPeriodMeanPtAm, PetPeriodMeanPtAm, by="Group.1", all=TRUE)
summary(landtPtAm)

save(landtPtAm,file="landtPtAm.RData")
write.csv(landtPtAm,"landtPtAm.csv")

# For plotting----------------------------------
plot(landtPtAm$Year.x~landtPtAm$Hay_weat_barley_oats_pct, type="b", xlim=c(0,0.06))
points(landtPtAm$Year.x~landtPtAm$Foin_hay_pct, type="b", col="blue", pch="h")
points(landtPtAm$Year.x~landtPtAm$Ble_weat_pct, type="b", col="green", pch="w")
points(landtPtAm$Year.x~landtPtAm$Orge_barley_pct, type="b", col="red", pch="b")
points(landtPtAm$Year.x~landtPtAm$Avoine_oats_pct, type="b", col="purple", pch="o")
points(landtPtAm$Year.x~landtPtAm$Cumul_Peat_extract_Pok_ha, type="b",pch="p", col="black")


#########Based on Shippagan west defined period====================

landt$PeriodShiwes[landt$Year>=1905 & landt$Year<=1949]=("P1")
landt$PeriodShiwes[landt$Year>=1950 & landt$Year<=1983]=("P2")
landt$PeriodShiwes[landt$Year>=1984 & landt$Year<=2000]=("P3")
landt$PeriodShiwes[landt$Year>=2001 & landt$Year<=2010]=("P4")
landt$PeriodShiwes=as.factor(landt$PeriodShiwes)
summary(landt$PeriodShiwes)
dim(landt)
#  29 x 21
#Gloucester subset----------------------------
++++++++++By Shiwes done 2016-03-31+++++++++++++++++++++++

Gloucester=subset(landt,Territorial_delim=="Gloucester",select=c(Year, Period, PeriodShiwes, Territorial_delim,Foin_hay_pct, Ble_weat_pct,Orge_barley_pct, Avoine_oats_pct,Hay_weat_barley_oats_pct))
Gloucester[,1:4]
Gloucester
# Get number of cases by variable
sapply(Gloucester, function(x)(sum(complete.cases(x))))

# Get number of Foin cases by Period
PeriodCases=tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodShiwes,sum,na.rm=T)/tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodShiwes,mean,na.rm=T)
PeriodCases
P1 P2 P3 P4 
 4  6  3  2 
sum(PeriodCases)
# 15
GPeriodMeanShiwes=aggregate(Gloucester, by=list(Gloucester$PeriodShiwes), FUN="mean", na.rm=TRUE)
GPeriodMeanShiwes

#Pokemouche subset------------------------------------------------

Pok=subset(landt, Territorial_delim=="Bassin_Pokemouche",select=c(Year,PeriodShiwes,Territorial_delim,Cumul_Peat_extract_Pok_pct))
Pok
PokPeriodMeanShiwes=aggregate(Pok, by=list(Pok$PeriodShiwes), mean, na.rm=T)
PokPeriodMeanShiwes

++++++++ Merge Agr and peat data+++++++++++++++++++++++++

landtShiwes=merge(GPeriodMeanShiwes, PokPeriodMeanShiwes, by="Group.1", all=TRUE)
summary(landtShiwes)

save(landtShiwes,file="landtShiwes.RData")
write.csv(landtShiwes,"landtShiwes.csv")

# For plotting----------------------------------
plot(landtShiwes$Year.x~landtShiwes$Hay_weat_barley_oats_pct, type="b", xlim=c(0,0.06))
points(landtShiwes$Year.x~landtShiwes$Foin_hay_pct, type="b", col="blue", pch="h")
points(landtShiwes$Year.x~landtShiwes$Ble_weat_pct, type="b", col="green", pch="w")
points(landtShiwes$Year.x~landtShiwes$Orge_barley_pct, type="b", col="red", pch="b")
points(landtShiwes$Year.x~landtShiwes$Avoine_oats_pct, type="b", col="purple", pch="o")
points(landtShiwes$Year.x~landtShiwes$Cumul_Peat_extract_Pok_pct, type="b",pch="p", col="black")

########Based on Shippagan east defined period=========================

landt$PeriodShieas[landt$Year>=1921 & landt$Year<=1942]=("P1")
landt$PeriodShieas[landt$Year>=1943 & landt$Year<=1963]=("P2")
landt$PeriodShieas[landt$Year>=1964 & landt$Year<=1979]=("P3")
landt$PeriodShieas[landt$Year>=1980 & landt$Year<=1991]=("P4")
landt$PeriodShieas[landt$Year>=1992 & landt$Year<=2000]=("P5")
landt$PeriodShieas[landt$Year>=2001 & landt$Year<=2010]=("P6")
landt$PeriodShieas=as.factor(landt$PeriodShieas)
summary(landt$PeriodShieas)
dim(landt)
# 29 x 21
#Gloucester subset----------------------------
++++++++++By Shieas done 2016-03-31+++++++++++++++++++++++

Gloucester=subset(landt,Territorial_delim=="Gloucester",select=c(Year, Period, PeriodShieas, Territorial_delim,Foin_hay_pct, Ble_weat_pct,Orge_barley_pct, Avoine_oats_pct,Hay_weat_barley_oats_pct))
Gloucester[,1:4]
Gloucester
# Get number of cases by variable
sapply(Gloucester, function(x)(sum(complete.cases(x))))

# Get number of Foin cases by Period
PeriodCases=tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodShieas,sum,na.rm=T)/tapply(Gloucester$Foin_hay_pct,Gloucester$PeriodShieas,mean,na.rm=T)
PeriodCases
P1 P2 P3 P4 P5 P6 
 3  3  2  3  1  2
sum(PeriodCases)
# 14
GPeriodMeanShieas=aggregate(Gloucester, by=list(Gloucester$PeriodShieas), FUN="mean", na.rm=TRUE)
GPeriodMeanShieas

#Pokemouche subset------------------------------------------------

Pok=subset(landt, Territorial_delim=="Bassin_Pokemouche",select=c(Year,PeriodShieas,Territorial_delim,Cumul_Peat_extract_Pok_pct))
Pok
PokPeriodMeanShieas=aggregate(Pok, by=list(Pok$PeriodShieas), mean, na.rm=T)
PokPeriodMeanShieas

++++++++ Merge Agr and peat data+++++++++++++++++++++++++

landtShieas=merge(GPeriodMeanShieas, PokPeriodMeanShieas, by="Group.1", all=TRUE)
summary(landtShieas)

save(landtShieas,file="landtShieas.RData")
write.csv(landtShieas,"landtShieas.csv")

# For plotting----------------------------------
plot(landtShieas$Year.x~landtShieas$Hay_weat_barley_oats_pct, type="b", xlim=c(0,0.06))
points(landtShieas$Year.x~landtShieas$Foin_hay_pct, type="b", col="blue", pch="h")
points(landtShieas$Year.x~landtShieas$Ble_weat_pct, type="b", col="green", pch="w")
points(landtShieas$Year.x~landtShieas$Orge_barley_pct, type="b", col="red", pch="b")
points(landtShieas$Year.x~landtShieas$Avoine_oats_pct, type="b", col="purple", pch="o")
points(landtShieas$Year.x~landtShieas$Cumul_Peat_extract_Pok_pct, type="b",pch="p", col="black")


# Work on Petite tracadie landuse data-----------------
