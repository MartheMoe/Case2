suppressPackageStartupMessages(library(WDI))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(countrycode))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(hrbrthemes))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(vtable))
suppressPackageStartupMessages(library(sjPlot))
suppressPackageStartupMessages(library(sjmisc))
suppressPackageStartupMessages(library(sjlabelled))
suppressPackageStartupMessages(library(gapminder))



#gjore om kontinentene til en faktor
gapminder <- mutate(gapminder, continent = as.factor(continent))


#aar 2000

gapminder2002 <- subset(gapminder, year == 2002)



#Plot
plot <- ggplot(gapminder2002, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point() +
  labs(x = "BNP per capita", y = "Forventet levealder") +
  scale_color_brewer(palette = "Set1") +
  theme(panel.grid.major = element_line(color = "gray", linetype = "dashed")) +
  ggtitle("Forventet levealder vs BNP per capita i aar 2002") 

plot + geom_jitter(aes(col=continent)) + 
  geom_smooth(aes(col=continent), method="lm", se=F)


# 1. BNP PER INNBYGGER (alle ar) og initial niva pa BNP per innbyggere. WDI-variabel =  "NY.GDP.PCAP.PP.KD". 
# Velg startar = 2000 og slutter = 2019
df_gdp0<-WDI(
  country = "all",
  indicator = c('gdppc'="NY.GDP.PCAP.PP.KD"),  
  start = 2000,
  end = 2019,
  extra = TRUE, 
  cache = NULL,
  latest = NULL,
  language = "en"
)



# velg ut relevante variabler
df_gdp <- subset(df_gdp0, select = c(country, region, income, iso2c, iso3c, year, gdppc) ) %>%  
  arrange(iso3c, year) 


# Tar bort land som mangler iso3c kode. 
df_gdp <-  df_gdp %>% mutate_all(na_if,"") 


# Ta vekk observasjoner som mangler data pa gdppc og iso3c. 
df_gdp <- df_gdp[complete.cases( df_gdp$gdppc, df_gdp$iso3c),] 

# Se til at year er en numerisk variabel. 
df_gdp = df_gdp  %>%  
  mutate(year = as.numeric(year)) 


# Noen land har flere observasjoner for samme ar (f.eks afghanistan ar 2010). Vi onsker a ha en observasjon per land og ar.

df_gdp <- df_gdp[!duplicated(df_gdp[c("iso3c", "year", max("gdppc"))]), ]  %>% 
arrange(iso3c, year) 



# Datasett med Y0 (niva pa BNP per innbyggere i ar 2000)
df_gdp2000  <- df_gdp %>%  arrange(iso3c, year) %>% 
  group_by(iso3c) %>% 
  slice(1) %>%
  ungroup()

df_gdp2000 = subset(df_gdp2000, select = -c(year) ) 
df_gdp2000 <-   plyr:: rename(df_gdp2000,c("gdppc" = "gdppc0")) 

df_gdp <- left_join(df_gdp,df_gdp2000, by=c("country", "iso2c", "iso3c", "region", "income"))  



# 2. HUMANKAPITAL (gjennomsnittlig antall ar i skole blant befolkningen eldre enn 15 ar) 
df_educ0<-WDI(
  country = "all",
  indicator = c('educ'="BAR.SCHL.15UP"),  
  start = 2000,
  end = 2019,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

#Behold nodvendige variabler
df_educ <- subset(df_educ0, select = c(country, region, income, iso2c, iso3c, year, educ) ) %>%  
  arrange(iso3c, year)


# Slett observasjoner med manglende data
df_educ <- df_educ[complete.cases(df_educ$educ),] %>%  
  arrange(iso3c, year) 


df_educ = df_educ %>%  
  arrange(iso3c, year) %>%  
  mutate(educ = as.numeric(educ, na.rm = TRUE)) %>% 
  ddply("iso3c",transform,
        avg_educ=mean(educ, na.rm = TRUE))  # Beregne gjennomsnittlig ar i skole for tidsperioden 2000 - 2019 for hvert land, basert pa tilgjenglig data (vil vare 2000.2005,2010)

df_educ <- subset(df_educ, select = c(country, region, income, iso2c, iso3c, avg_educ)) # Her tar jeg vekk variabelen "year". Jeg gjor dette fordi vi bare har en observasjon pa utdanning per land. Vi onsker a bruke denne verdi for alle ar. 
df_educ <- df_educ[!duplicated(df_educ[c("iso3c")]), ]  %>%  
  arrange(iso3c) 



# 3. Sparing som andel av BNI
#Gjennomsnittlig sparing for perioden 2000-2015 (lagg fordi det kan ta litt tid for sparing a bli til investering)
df_nsy0<-WDI(
  country = "all",
  indicator = c( 'nsy'="NY.ADJ.NNAT.GN.ZS"),  
  start = 2000,
  end = 2015,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)

df_nsy <- subset(df_nsy0, select = c(country, region, income, iso2c, iso3c, year, nsy) ) %>%  
  arrange(iso3c, year) 

df_nsy <- df_nsy[complete.cases(df_nsy$nsy),] %>%  
  arrange(iso3c, year) 


df_nsy = df_nsy %>%  
  arrange(iso3c, year) %>%   
  mutate(nsy = as.numeric(nsy, na.rm = TRUE)) %>% 
  ddply("iso3c",transform,
        avg_nsy=mean(nsy, na.rm = TRUE))  # Beregne gjennomsnittlig ar i skole for tidsperioden 2000 - 2019 for hvert land, basert pa tilgjenglig data (vil vare 2000.2005,2010)

# Her tar jeg vekk variabelen "year". Jeg gjor dette fordi vi bare har en observasjon pa utdanning per land. Vi onsker a bruke denne verdi for alle ar.
df_nsy <- subset(df_nsy, select = c(country, region, income, iso2c, iso3c, avg_nsy)) 

# Ta vekk duplikater for hvert land.
df_nsy <- df_nsy[!duplicated(df_nsy[c("iso3c")]), ]  %>%  
  arrange(iso3c) 



# 4. Vekst i arbeidskraften (n)
df_lf0<-WDI(
  country = "all",
  indicator = c('lf'="JI.TLF.TOTL"),  # lf = labor force
  start = 2000,
  end = 2019,
  extra = TRUE, 
  cache = NULL,
  latest = NULL,
  language = "en"
)

df_lf <- subset(df_lf0, select = c(country, region, income, iso2c, year, lf) ) %>%  
  arrange(iso2c, year) 
df_lf <-   plyr:: rename(df_lf,c("iso2c" = "iso3c")) 
df_lf [df_lf == 0]<-NA
df_lf <- df_lf[complete.cases(df_lf$iso3c, df_lf$lf),]  
df_lf = df_lf  %>%  
  mutate(year = as.numeric(year)) 

df_lf <- df_lf[!duplicated(df_lf[c("iso3c", "year")]), ]  %>%  
  arrange(iso3c, year) 



# Ta fram vekstraten i arbeidskraften (n). Vi har ikke data for hvert ar i alle land. 
# For a beregne gjennomsnittlig arlig vekst ma vi lage en variabel som maler antallet tidsperioder mellom hver observasjon.
df_n = df_lf %>%  
  arrange(iso3c, year) %>%  
  ddply("iso3c",transform,
        t=c(NA,diff(year)),
        lf_growth=c(NA,diff(log(lf)))) 

df_n <- df_n[complete.cases(df_n$t, df_n$lf_growth),] 


#Na kan vi ta fram arlig vekstrate
df_n = df_n %>%  
  mutate(t = as.numeric(t)) %>%   
  mutate(lf_growth = as.numeric(lf_growth))
df_n <- transform(df_n, n =lf_growth/t)


# gjennomsnittlig vekstrate i arbeidskraften for hvert land
df_n <- df_n %>%  
  ddply("iso3c",transform,
        avg_n=mean(n, na.rm = TRUE)) 

df_n <- subset(df_n, select = c(iso3c, avg_n) )
df_n <- df_n[!duplicated(df_n["iso3c"]), ]  %>%  
  arrange(iso3c)



# 5. Lag et datasett som inneholder BNP data, utdanningsdata, sparing, og arbeidskraftsdata

df <- left_join(df_gdp, df_educ, by=c("country", "iso2c", "iso3c", "region", "income"))
df <- left_join(df, df_nsy, by=c("country", "iso2c", "iso3c", "region", "income"))
df <- left_join(df, df_n, by="iso3c")
df <- subset(df, select = c(country, region, income, iso2c, iso3c, year, gdppc, gdppc0, avg_educ, avg_nsy, avg_n)) # Behold nodvendige variabler

# Mange observasjoner representerer aggregerte regioner.

df <- df  %>%  filter(iso2c!='1A' & iso2c !='1W' & iso2c != '4E' & iso2c != '7E' & iso2c !='8S'
                      & iso2c !='B8' & iso2c !='EU' & iso2c !='F1' & iso2c !='OE' & iso2c !='S1' & iso2c !='S2' & iso2c !="S3" 
                      & iso2c !='S4' & iso2c !='T2' & iso2c !='T3' & iso2c !='T4' & iso2c !='T5' & iso2c !='T6' & iso2c !='T7' 
                      & iso2c !='V1' & iso2c !='V2' & iso2c !='V3' & iso2c !='V4' & iso2c !='XC' & iso2c !='XD' & iso2c !='XE' 
                      & iso2c !='XF' & iso2c !='XG' & iso2c !='XH' & iso2c !='XI' & iso2c !='XJ' & iso2c !='XL' & iso2c !='XM' 
                      & iso2c !='XN' & iso2c !='XO' & iso2c !='XP' & iso2c !='XQ' & iso2c !='XT' & iso2c !='XU' & iso2c !='Z4' 
                      & iso2c !='Z7' & iso2c !='ZF'& iso2c !='ZG'  & iso2c !='ZH' & iso2c !='ZI'  & iso2c !='ZJ'  & iso2c !='ZQ'  
                      & iso2c !='ZT'  & iso2c !='Z7')  %>% arrange(iso3c, year) 



# 6. Datasett for resterende variabler.

df_rest0<-WDI(
  country = "all",
  indicator = c('poptot'="SP.POP.TOTL", 'gi'="NE.GDI.FTOT.KD.ZG", 'gx'="NE.EXP.GNFS.KD.ZG", 'nry'="NY.ADJ.DRES.GN.ZS", 'p'="SP.POP.GROW" ),  
  start = 2000,
  end = 2019,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)


df_rest0 [df_rest0 == 0]<-NA

df_rest <- df_rest0[complete.cases( df_rest0$iso3c),]  %>%  arrange(iso2c) 


# Ta vekk observasjoner som ikke representerer land.
df_rest <- df_rest  %>%  filter(iso2c!='1A' & iso2c !='1W' & iso2c != '4E' & iso2c != '7E' & iso2c !='8S'
                                & iso2c !='B8' & iso2c !='EU' & iso2c !='F1' & iso2c !='OE' & iso2c !='S1' & iso2c !='S2' & iso2c !="S3" 
                                & iso2c !='S4' & iso2c !='T2' & iso2c !='T3' & iso2c !='T4' & iso2c !='T5' & iso2c !='T6' & iso2c !='T7' 
                                & iso2c !='V1' & iso2c !='V2' & iso2c !='V3' & iso2c !='V4' & iso2c !='XC' & iso2c !='XD' & iso2c !='XE' 
                                & iso2c !='XF' & iso2c !='XG' & iso2c !='XH' & iso2c !='XI' & iso2c !='XJ' & iso2c !='XL' & iso2c !='XM' 
                                & iso2c !='XN' & iso2c !='XO' & iso2c !='XP' & iso2c !='XQ' & iso2c !='XT' & iso2c !='XU' & iso2c !='Z4' 
                                & iso2c !='Z7' & iso2c !='ZF'& iso2c !='ZG'  & iso2c !='ZH' & iso2c !='ZI'  & iso2c !='ZJ'  & iso2c !='ZQ'  
                                & iso2c !='ZT'  & iso2c !='Z7')  %>% arrange(iso3c, year) 

df_rest <- subset(df_rest, select = c("country", "region", "income", "iso3c", "iso2c", "year", "poptot", "p", "nry", "gi", "gx"))
df_all <- left_join(df, df_rest, by=c("country", "region", "income", "iso2c", "iso3c", "year"))


# Lag en rekkefolge til variablene slik at det er enklere a fa en oversikt over datamaterialet.
col_order <- c("country",  "region", "income", "iso3c", "iso2c", "year", "gdppc", "gdppc0", "poptot", "p", "avg_n", "avg_nsy", "nry", "gi", "gx", "avg_educ")
df_all <- df_all[, col_order]





# Tar frem vekstraten og gjennomsnitt for resterende variabler
df_growth0 = df_all %>%  
  arrange(iso3c, year) %>%  
  ddply("iso3c",transform,
        gdpgrowth=c(NA,diff(log(gdppc)))*100) %>%   
  mutate(gdpgrowth = as.numeric(gdpgrowth, na.rm = TRUE)) %>% 
  ddply("iso3c",transform,
        avg_gdpgrowth=mean(gdpgrowth, na.rm = TRUE), 
        avg_gi=mean(gi, na.rm = TRUE), 
        avg_nry=mean(nry, na.rm = TRUE), 
        avg_gx=mean(gx, na.rm = TRUE),  
        avg_p=mean(p, na.rm = TRUE))  




#View(df_growth0)
df_growth0 [df_growth0 == 0]<-NA
df_growth <- df_growth0[complete.cases( df_growth0$country, df_growth0$income, df_growth0$iso3c, df_growth0$avg_gdpgrowth, df_growth0$gdppc0, df_growth0$avg_n, df_growth0$avg_p, df_growth0$avg_nsy, df_growth0$avg_nry,df_growth0$avg_gi, df_growth0$avg_gx, df_growth0$avg_educ),] # Ta vekk land som mangler data 


df_growth <- subset(df_growth, select = c("country",  "region", "income", "iso3c", "iso2c","year", "poptot", "gdppc", "gdppc0", "avg_gdpgrowth", "avg_n", "avg_p", "avg_nsy", "avg_nry", "avg_gi", "avg_gx", "avg_educ"))

df_growth$dppc <-as.numeric(df_growth$gdppc)
df_growth$ln_gdppc<-log(df_growth$gdppc) 
df_growth$ln_gdppc0<-log(df_growth$gdppc0) 




# Lage datasettet du vil bruke til analysen din 
df_growth2019  <- df_growth %>%  
  arrange(iso3c, year) %>% 
  group_by(iso3c) %>% 
  slice(n()) %>% # Behold den SISTE observasjonen for hvert land
  ungroup()


# Logaritmer
df_growth2019$dppc <-as.numeric(df_growth2019$gdppc)
df_growth2019$ln_gdppc<-log(df_growth2019$gdppc) 
df_growth2019$ln_gdppc0<-log(df_growth2019$gdppc0) 





##Grafer: korrelasjon mellom niva pa bnp og variabler 


#Sparerate

ss <- ggplot(df_growth2019, aes(x= avg_nsy, y= ln_gdppc)) + 
  labs(title="Niva pa BNP per innbygger og sparerate") +
  xlab("Gjennomsnittlig sparerate i prosent mellom 2000-2015") + 
  ylab("Niva pa BNP per innbygger i 2019")


ss + geom_jitter(aes(col=region)) + 
  geom_smooth(aes(col=region), method="lm", se=F)



# Befolkningsvekst


ui <- ggplot(df_growth2019, aes(x= avg_p, y= ln_gdppc)) + 
  labs(title="Nivaa pa BNP per innbygger og befolkningsvekst")+
  xlab("Gjennomsnittlig aarlig befolkningsvekst i prosent mellom 2000-2019") + 
  ylab("Niva pa BNP per innbygger i 2019")

ui + geom_jitter(aes(col=region)) + 
  geom_smooth(aes(col=region), method="lm", se=F)



# humankapital 


ui <- ggplot(df_growth2019, aes(x= avg_educ, y= ln_gdppc)) + 
  labs(subtitle="2019",
       title="Niva pa BNP per innbygger og humankapital")+
  xlab("Gjennomsnittlig aarlig vekstrate i humankaptal mellom 2000-2010") + 
  ylab("Niva pa BNP per innbygger i 2019")

ui + geom_jitter(aes(col=region)) + 
  geom_smooth(aes(col=region), method="lm", se=F)



## vekstrate

ug <- ggplot(df_growth2019, aes(x= avg_gdpgrowth, y= ln_gdppc)) + 
  labs(subtitle="2019",
       title="Nivaa pa BNP per innbygger og gjennomsnittlig, aarlig vekstrate")+
  xlab("Gjennomsnittlig aarlig vekstrate i bnp per innbygger") + 
  ylab("Niva pa BNP per innbygger i 2019")

ug + geom_jitter(aes(col=region)) + 
  geom_smooth(aes(col=region), method="lm", se=F)
              

##Vekstrate 

# Sparing 


gy <- ggplot(df_growth2019, aes(x= avg_nsy, y= avg_gdpgrowth)) + 
  labs(title="Vekst i BNP per innbygger og sparerate")+  
  xlab("Gjennomsnittlig aarlig sparerate i prosent mellom 2000-2015") + 
  ylab("Gjennomsnittlig aarlig vekst i BNP per innbygger i prosent mellom 2000-2019 ")

gy + geom_jitter(aes(col=region)) + 
  geom_smooth(aes(col=region), method="lm", se=F)




# Humankapital 


ge <- ggplot(df_growth2019, aes(x= avg_educ, y= avg_gdpgrowth)) + 
  labs(title="Vekst i BNP per innbygger") +
  xlab("Gjennomsnittlig aarlig vekstrate i humankaptal mellom 2000-2010") + 
  ylab("Gjennomsnittlig aarlig vekst i BNP per innbygger i prosent mellom 2000-2019")

ge + geom_jitter(aes(col=region)) + 
  geom_smooth(aes(col=region), method="lm", se=F)



#Deskriptiv statistikk for empirisk analyse

# Variabler til analyse

Reg <- subset(df_growth2019, select = c("avg_gdpgrowth", "avg_nsy", "avg_educ", "avg_gi",
                                        "avg_nry", "avg_n", "avg_p", "gdppc0"))



# Gi beskrivende navn til variablene (i samme rekkefolge som de ligger i datasettet)
la <- c("Gjennomsnittlig vekstrate i BNP 2000-2019 (dollar)", "Gjennomsnittlig sparerate 2000-2019", 
        "Gjennomsnittlig aar i skole 2000-2019", "Gjennomsnittlig arlig vekstrate i investeringer 2000-2019",
        "Gjennomsnittlig aarlig negativ vekstrate i naturressurser 2000-2019",
        "Gjennomsnittlig aarlig vekstrate i arbeidskraft 2000-2019", "Gjennomsnittlig aarlig vekstrate i befolkningen 2000-2019", 
        "Bnp per innbygger i aar 2000") 


# Lag tabellen
st(Reg, labels=la,
   summ = list(
     c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'), # Beskriv hvilken statistikk du ??nsker ?? vise
     c('notNA(x)','mean(x)')
   ),
   summ.names = list(
     c('N','Gjennomsnitt','SD','Min','Maks') # Gi navn til kolonnene
   ))






## Regresjon 
#Gjennomsnittlig, ??rlig vekstrate i bnp, humankapital, naturressurser, investeringer, eksport, bnp pc i ??r 2000


modela <- lm(avg_gdpgrowth ~ avg_nsy + avg_educ+ avg_nry + avg_gi + 
               + avg_p + avg_gx + gdppc0, data= df_growth2019)
summary(modela)
tab_model(modela)





#Lokalisere ekstreme observasjoner

drt <- df_growth2019[complete.cases( df_growth2019$avg_gi, df_growth2019$avg_nry,
                                     df_growth2019$gdppc0),]

Q1gi <- quantile(drt$avg_gi, .25 )
Q3gi <- quantile(drt$avg_gi, .75)
IQRgi <- IQR(drt$avg_gi)

Q1nry <- quantile(drt$avg_nry, .25 )
Q3nry <- quantile(drt$avg_nry, .75)
IQRnry <- IQR(drt$avg_nry)

Q1gdppc <- quantile(drt$gdppc0, .25 )
Q3gdppc <- quantile(drt$gdppc0, .75)
IQRgdppc <- IQR(drt$gdppc0)


no_outliers <- subset(drt, drt$avg_gi > (Q1gi - 1.5*IQRgi) & drt$avg_gi < (Q3gi + 1.5*IQRgi) &  drt$avg_nry > (Q1nry - 1.5*IQRnry) & drt$avg_nry < (Q3nry + 1.5*IQRnry) &  drt$gdppc0 > (Q1gdppc - 1.5*IQRgdppc) & drt$gdppc0 < (Q3gdppc + 1.5*IQRgdppc))  
dim(no_outliers)




## Regresjon 77 observasjoner
#Gjennomsnittlig, arlig vekstrate i bnp per innbygger, humankapital, naturressurser, investeringer, arbeidskraft, eksport,   


modelw <- lm(avg_gdpgrowth ~ avg_nsy + avg_educ+ avg_nry + avg_gi + 
             + avg_p + avg_gx + gdppc0, data= no_outliers)
summary(modelw)

#Tabell av regresjonsanalysen

tab_model(modelw)



#Konvergensteori regresjon




## Regresjon 
#Niva pa bnp i ar 2000, vekst i bnp per innbygger, sparing, befolkningsvekst. 


modelt <- lm(gdppc0 ~ avg_nsy + avg_p + avg_gdpgrowth, data= df_growth2019)
summary(modelt)
tab_model(modelt)









