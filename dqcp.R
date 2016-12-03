
## triggering the script from shell
## go to cmd
## change directory to C:\Program Files\R\R-3.2.1\bin (probably or whereever R is installed)
## go to R by typing R
## change working directory with setwd("/path/to/whereever/the/Rmd/file/is") mind reverted slashes in Windows!! 
## rmarkdown::render("dqcp.Rmd", "pdf_document")
## btw rendering to pdf is quite a hassle because several additional libraries like pandoc need to be installed

require(ggplot2)
require(ggthemes)
require(plyr)
require(rworldmap)

raw <- read.csv("dq_check.dat", sep=";")

# a few remarks to the structure of the script:
# since almost identical computations are done for each section, writing and applying some methods
# comes to mind. However, value properties and their description differ within the output of the software

# visualize name analysis

## overview

nang <- count(!is.na(raw$selected_namensAnalyse)) ## geprüft / nicht geprüft 
nang$column <- c("nicht geprüft", "geprüft")
nang.df = ddply(nang, .(column), transform, percent = round(freq/sum(nang$freq) * 100))
nang.df

## detected gender

knag <- as.data.frame(table(raw$NameAnalysis_gender))
knag$Var1 <- as.character(knag$Var1)
knag <- subset(knag, !knag$Var1 == "")
knag$Var1[knag$Var1 == "0"] <- "Anrede unbestimmt"
knag$Var1[knag$Var1 == "1"] <- "Anrede männlich"
knag$Var1[knag$Var1 == "2"] <- "Anrede weiblich"
knag$Var1[knag$Var1 == "8"] <- "unbekannt"
knag$Var1[knag$Var1 == "9"] <- "nicht analysiert"
names(knag) <- c("Anredeprofile", "Anzahl")
knag = ddply(knag, .(Anredeprofile), transform, Prozentual = Anzahl/sum(knag$Anzahl) * 100)
knag = ddply(knag, .(Anredeprofile), transform, pos = (cumsum(Anzahl) - 0.5 * Anzahl))
knag$label = paste0(sprintf("%.0f", knag$Prozentual), "%")

ggplot(knag, aes(x = Anredeprofile, y = Anzahl, fill = Anredeprofile)) +
  geom_bar(stat = "identity", width = .6) + ggtitle("Anredeprofile") + 
  geom_text(aes(y = pos, label = label), size = 3) +
  theme_gdocs() + scale_fill_pander() + theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank())


# postal validation

## overview

pv <- count(!is.na(raw$POST_class)) ## geprüft / nicht geprüft 
pv$column <- c("nicht geprüft", "geprüft")
pv.df = ddply(pv, .(column), transform, percent = round(freq/sum(pv$freq) * 100))
pv.df

## retrieve result class

pvrc <- as.data.frame(table(raw$POST_class))
pvrc$Var1 <- as.character(pvrc$Var1)
pvrc <- subset(pvrc, !pvrc$Var1 == "")
pvrc$Var1[pvrc$Var1 == "1"] <- "unveränderte Adressen"
pvrc$Var1[pvrc$Var1 == "2"] <- "sicher veränderte Adressen"
pvrc$Var1[pvrc$Var1 == "3"] <- "fehlertolerant veränderte Adressen"
pvrc$Var1[pvrc$Var1 == "4"] <- "mehrdeutige Adressen"
pvrc$Var1[pvrc$Var1 == "5"] <- "nicht veränderte Adressen"
names(pvrc) <- c("Ergebnisklasse", "Anzahl")
pvrc = ddply(pvrc, .(Ergebnisklasse), transform, Prozentual = Anzahl/sum(pvrc$Anzahl) * 100)
pvrc = ddply(pvrc, .(Ergebnisklasse), transform, pos = (cumsum(Anzahl) - 0.5 * Anzahl))
pvrc$label = paste0(sprintf("%.0f", pvrc$Prozentual), "%")

ggplot(pvrc, aes(x = Ergebnisklasse, y = Anzahl, fill = Ergebnisklasse)) +
  geom_bar(stat = "identity", width = .6) + ggtitle("Ergebnisklassen der Zustelladresse") + 
  geom_text(aes(y = pos, label = label), size = 3) +
  theme_gdocs() + scale_fill_pander() + theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank())


## analysis of postal code

pvzca <- as.data.frame(table(raw$POST_res_zip))
pvzca$Var1 <- as.character(pvzca$Var1)
pvzca <- subset(pvzca, !pvzca$Var1 == "")
pvzca$Var1[pvzca$Var1 == "OK"] <- "OK = unverändert"
pvzca$Var1[pvzca$Var1 == "NF"] <- "NF = PLZ nicht gefunden"
pvzca$Var1[pvzca$Var1 == "NI"] <- "NI = keine Eingabe"
pvzca$Var1[pvzca$Var1 == "NC"] <- "NC = nicht geprüft"
pvzca$Var1[pvzca$Var1 == "PI"] <- "PI = unvollständige PLZ in der Eingabe"
pvzca$Var1[pvzca$Var1 == "C"] <- "C = PLZ wurde geändert"
pvzca$Var1[pvzca$Var1 == "SC"] <- "SC = die ersten beiden Stellen wurden geändert"
pvzca$Var1[pvzca$Var1 == "SL"] <- "SL = PLZ ist mehrdeutig"
pvzca$Var1[pvzca$Var1 == "DF"] <- "DF = Default PLZ (kleinste zustellbare PLZ)"
pvzca$Var1[pvzca$Var1 == "DV"] <- "DV = PLZ über Spezial-PLZ gefunden"

pvzca = ddply(pvzca, .(Var1), transform, Prozentual = Freq/sum(pvzca$Freq) * 100)
pvzca = ddply(pvzca, .(Var1), transform, pos = (cumsum(Freq) - 0.5 * Freq))
pvzca$label = paste0(sprintf("%.0f", pvzca$Prozentual), "%")
names(pvzca)[1:2] <- c("PLZ-Analyse", "Anzahl")

ggplot(pvzca, aes(x = pvzca$`PLZ-Analyse`, y = Anzahl, fill = pvzca$`PLZ-Analyse`)) +
  geom_bar(stat = "identity", width = .6) + ggtitle("Ergebnisse der PLZ-Analyse") + 
  geom_text(aes(y = pos, label = label), size = 3) +
  theme_gdocs() + scale_fill_pander() + coord_flip() + scale_x_discrete(labels = element_blank()) + 
  theme(legend.position = "right", legend.title=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

## analysis of city

pvca <- as.data.frame(table(raw$POST_res_city))
pvca$Var1 <- as.character(pvca$Var1)
pvca <- subset(pvca, !pvca$Var1 == "")
pvca$Var1[pvca$Var1 == "OK"] <- "bereits korrekt"
pvca$Var1[pvca$Var1 == "NF"] <- "Ort nicht gefunden"
pvca$Var1[pvca$Var1 == "NI"] <- "keine Eingabe"
pvca$Var1[pvca$Var1 == "NC"] <- "nicht geprüft"
pvca$Var1[pvca$Var1 == "PI"] <- "unvollständige PLZ in der Eingabe"
pvca$Var1[pvca$Var1 == "C"] <- "Ort wurde geändert"
pvca$Var1[pvca$Var1 == "SC"] <- "Ort wurde über Archivort gefunden"
pvca$Var1[pvca$Var1 == "SL"] <- "Ort ist mehrdeutig"
pvca$Var1[pvca$Var1 == "DV"] <- "Ort wurde über Spezial-Ort gefunden"

pvca = ddply(pvca, .(Var1), transform, Prozentual = Freq/sum(pvca$Freq) * 100)
pvca = ddply(pvca, .(Var1), transform, pos = (cumsum(Freq) - 0.5 * Freq))
pvca$label = paste0(sprintf("%.0f", pvca$Prozentual), "%")
names(pvca)[1:2] <- c("Ortsanalyse", "Anzahl")

ggplot(pvca, aes(x = pvca$`Ortsanalyse`, y = Anzahl, fill = pvca$`Ortsanalyse`)) +
  geom_bar(stat = "identity", width = .6) + ggtitle("Ergebnisse der Ortsanalyse") + 
  geom_text(aes(y = pos, label = label), size = 3) +
  theme_gdocs() + scale_fill_pander() + coord_flip() + scale_x_discrete(labels = element_blank()) + 
  theme(legend.position = "right", legend.title=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

## analysis of street

pvstra <- as.data.frame(table(raw$POST_res_str))
pvstra$Var1 <- as.character(pvstra$Var1)
pvstra <- subset(pvstra, !pvstra$Var1 == "")
pvstra$Var1[pvstra$Var1 == "OK"] <- "OK = unverändert"
pvstra$Var1[pvstra$Var1 == "NF"] <- "NF = Straße nicht gefunden"
pvstra$Var1[pvstra$Var1 == "NI"] <- "NI = keine Eingabe"
pvstra$Var1[pvstra$Var1 == "NC"] <- "NC = nicht geprüft"
pvstra$Var1[pvstra$Var1 == "NP"] <- "NP = nicht geprüft wegen Postfachangabe im Straßenfeld"
pvstra$Var1[pvstra$Var1 == "C"] <- "C = Straße wurde geändert"
pvstra$Var1[pvstra$Var1 == "SC"] <- "SC = Straße wurde umgeschlüsselt"
pvstra$Var1[pvstra$Var1 == "SL"] <- "SL = Straße ist mehrdeutig"

pvstra = ddply(pvstra, .(Var1), transform, Prozentual = Freq/sum(pvstra$Freq) * 100)
pvstra = ddply(pvstra, .(Var1), transform, pos = (cumsum(Freq) - 0.5 * Freq))
pvstra$label = paste0(sprintf("%.0f", pvstra$Prozentual), "%")
names(pvstra)[1:2] <- c("Straßenanalyse", "Anzahl")

ggplot(pvstra, aes(x = pvstra$`Straßenanalyse`, y = Anzahl, fill = pvstra$`Straßenanalyse`)) +
  geom_bar(stat = "identity", width = .6) + ggtitle("Ergebnisse der Straßenanalyse") + 
  geom_text(aes(y = pos, label = label), size = 3) +
  theme_gdocs() + scale_fill_pander() + coord_flip() + scale_x_discrete(labels = element_blank()) + 
  theme(legend.position = "right", legend.title=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

## analysis of housenumber

pvhnoa <- as.data.frame(table(raw$POST_res_hno))
pvhnoa$Var1 <- as.character(pvhnoa$Var1)
pvhnoa <- subset(pvhnoa, !pvhnoa$Var1 == "")
pvhnoa$Var1[pvhnoa$Var1 == "OK"] <- "OK = unverändert"
pvhnoa$Var1[pvhnoa$Var1 == "NF"] <- "NF = Hausnummer nicht gefunden"
pvhnoa$Var1[pvhnoa$Var1 == "NI"] <- "NI = keine Eingabe"
pvhnoa$Var1[pvhnoa$Var1 == "NC"] <- "NC = nicht geprüft"
pvhnoa$Var1[pvhnoa$Var1 == "NP"] <- "NP = nicht geprüft wegen Postfachangabe im Straßenfeld"
pvhnoa$Var1[pvhnoa$Var1 == "C"] <- "C = Hausnummer geändert"
pvhnoa$Var1[pvhnoa$Var1 == "SC"] <- "SC = Hausnummer wurde umgeschlüsselt"
pvhnoa$Var1[pvhnoa$Var1 == "SL"] <- "SL = Hausnummer ist mehrdeutig"
pvhnoa$Var1[pvhnoa$Var1 == "HR"] <- "HR = Hausnummer innerhalb von Bereich"
pvhnoa$Var1[pvhnoa$Var1 == "NN"] <- "NN = Hausnummer entspricht nicht der Norm"
pvhnoa$Var1[pvhnoa$Var1 == "SM"] <- "SM = Hausnr. in Hausnummernbereich"

pvhnoa = ddply(pvhnoa, .(Var1), transform, Prozentual = Freq/sum(pvhnoa$Freq) * 100)
pvhnoa = ddply(pvhnoa, .(Var1), transform, pos = (cumsum(Freq) - 0.5 * Freq))
pvhnoa$label = paste0(sprintf("%.0f", pvhnoa$Prozentual), "%")
names(pvhnoa)[1:2] <- c("Hausnummernanalyse", "Anzahl")

ggplot(pvhnoa, aes(x = pvhnoa$`Hausnummernanalyse`, y = Anzahl, fill = pvhnoa$`Hausnummernanalyse`)) +
  geom_bar(stat = "identity", width = .6) + ggtitle("Ergebnisse der Hausnummernanalyse") + 
  geom_text(aes(y = pos, label = label), size = 3) +
  theme_gdocs() + scale_fill_pander() + coord_flip() + scale_x_discrete(labels = element_blank()) + 
  theme(legend.position = "right", legend.title=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

## analysis p.o.box

pvboxa <- as.data.frame(table(raw$POST_res_pobox))
pvboxa$Var1 <- as.character(pvboxa$Var1)
pvboxa <- subset(pvboxa, !pvboxa$Var1 == "")
pvboxa$Var1[pvboxa$Var1 == "OK"] <- "OK = Postfachangaben korrekt"
pvboxa$Var1[pvboxa$Var1 == "CW"] <- "CW = dieser Ort hat keine Postfächer"
pvboxa$Var1[pvboxa$Var1 == "PL"] <- "PL = für diesen Ort müssen Postfachnummern angegeben werden"
pvboxa$Var1[pvboxa$Var1 == "PW"] <- "PW = Postfachnummer ungültig"
pvboxa$Var1[pvboxa$Var1 == "ZL"] <- "ZL = Postfach-PLZ muss angegeben werden, da Postfachnr. nicht eindeutig"
pvboxa$Var1[pvboxa$Var1 == "NC"] <- "NC = nicht geprüft"

pvboxa = ddply(pvboxa, .(Var1), transform, Prozentual = Freq/sum(pvboxa$Freq) * 100)
pvboxa = ddply(pvboxa, .(Var1), transform, pos = (cumsum(Freq) - 0.5 * Freq))
pvboxa$label = paste0(sprintf("%.0f", pvboxa$Prozentual), "%")
names(pvboxa)[1:2] <- c("Postfachanalyse", "Anzahl")

ggplot(pvboxa, aes(x = pvboxa$`Postfachanalyse`, y = Anzahl, fill = pvboxa$`Postfachanalyse`)) +
  geom_bar(stat = "identity", width = .6) + ggtitle("Ergebnisse der Postfachanalyse") + 
  geom_text(aes(y = pos, label = label), size = 3) +
  theme_gdocs() + scale_fill_pander() + coord_flip() + scale_x_discrete(labels = element_blank()) + 
  theme(legend.position = "right", legend.title=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

## accuracy of housenumber-check

pvahno <- as.data.frame(table(raw$POST_hno_check))
pvahno$Var1 <- as.character(pvahno$Var1)
pvahno <- subset(pvahno, !pvahno$Var1 == "")
pvahno$Var1[pvahno$Var1 == "OK"] <- "OK = Hausnummer gefunden"
pvahno$Var1[pvahno$Var1 == "NC"] <- "NC = keine Prüfung möglich"
pvahno$Var1[pvahno$Var1 == "NF"] <- "NF = Hausnummer nicht gefunden"

pvahno = ddply(pvahno, .(Var1), transform, Prozentual = Freq/sum(pvahno$Freq) * 100)
pvahno = ddply(pvahno, .(Var1), transform, pos = (cumsum(Freq) - 0.5 * Freq))
pvahno$label = paste0(sprintf("%.0f", pvahno$Prozentual), "%")
names(pvahno)[1:2] <- c("Hausnummergenauigkeit", "Anzahl")

ggplot(pvahno, aes(x = pvahno$`Hausnummergenauigkeit`, y = Anzahl, fill = pvahno$`Hausnummergenauigkeit`)) +
  geom_bar(stat = "identity", width = .6) + ggtitle("Ergebnisse hausnummergenaue Prüfung") + 
  geom_text(aes(y = pos, label = label), size = 3) +
  theme_gdocs() + scale_fill_pander() + coord_flip() + scale_x_discrete(labels = element_blank()) + 
  theme(legend.position = "right", legend.title=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())


# geo coordinates 

## overview

geoc <- count(!is.na(raw$COOR_class)) ## geprüft / nicht geprüft 
geoc$column <- c("nicht geprüft", "geprüft")
geoc.df = ddply(geoc, .(column), transform, percent = round(freq/sum(geoc$freq) * 100))
geoc.df

## quality of geocoordinates

geocc <- as.data.frame(table(raw$COOR_class))
geocc$Var1 <- as.character(geocc$Var1)
geocc <- subset(geocc, !geocc$Var1 == "")
geocc$Var1[geocc$Var1 == "1"] <- "korrekt gefunden"
geocc$Var1[geocc$Var1 == "2"] <- "unsicher gefunden"
geocc$Var1[geocc$Var1 == "3"] <- "Default-Informationen"
geocc$Var1[geocc$Var1 == "4"] <- "mehrdeutige Adressen"
geocc$Var1[geocc$Var1 == "5"] <- "nicht gefunden"
names(geocc) <- c("Ergebnisklasse", "Anzahl")
geocc = ddply(geocc, .(Ergebnisklasse), transform, Prozentual = Anzahl/sum(geocc$Anzahl) * 100)
geocc = ddply(geocc, .(Ergebnisklasse), transform, pos = (cumsum(Anzahl) - 0.5 * Anzahl))
geocc$label = paste0(sprintf("%.0f", geocc$Prozentual), "%")

ggplot(geocc, aes(x = Ergebnisklasse, y = Anzahl, fill = Ergebnisklasse)) +
  geom_bar(stat = "identity", width = .6) + ggtitle("Ergebnisklassen der Geokoordinaten") + 
  geom_text(aes(y = pos, label = label), size = 3) +
  theme_gdocs() + scale_fill_pander() + theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank())

## geocoordinate level

geol <- as.data.frame(table(raw$COOR_valid_for))
geol$Var1 <- as.character(geol$Var1)
geol <- subset(geol, !geol$Var1 == "")
geol$Var1[geol$Var1 == "0"] <- "keine Koordinaten"
geol$Var1[geol$Var1 == "2"] <- "einzelne Hausnummer"
geol$Var1[geol$Var1 == "3"] <- "Straßenabschnitt"
geol$Var1[geol$Var1 == "4"] <- "ganze Straße"
geol$Var1[geol$Var1 == "5"] <- "Ortsebene"

geol = ddply(geol, .(Var1), transform, Prozentual = Freq/sum(geol$Freq) * 100)
geol = ddply(geol, .(Var1), transform, pos = (cumsum(Freq) - 0.5 * Freq))
geol$label = paste0(sprintf("%.0f", geol$Prozentual), "%")
names(geol)[1:2] <- c("Ebene", "Anzahl")

ggplot(geol, aes(x = geol$`Ebene`, y = Anzahl, fill = geol$`Ebene`)) +
  geom_bar(stat = "identity", width = .6) + ggtitle("Gültigkeitsebene") + 
  geom_text(aes(y = pos, label = label), size = 3) +
  theme_gdocs() + scale_fill_pander() + coord_flip() + scale_x_discrete(labels = element_blank()) + 
  theme(legend.position = "right", legend.title=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

## 

geolc <- as.data.frame(table(raw$COOR_city_valid))
geolc$Var1 <- as.character(geolc$Var1)
geolc <- subset(geolc, !geolc$Var1 == "")
geolc$Var1[geolc$Var1 == "0"] <- "keine zusätzliche Einschränkung"
geolc$Var1[geolc$Var1 == "1"] <- "PLZ"
geolc$Var1[geolc$Var1 == "2"] <- "Ortsteil"
geolc$Var1[geolc$Var1 == "3"] <- "PLZ und Ortsteil"

geolc = ddply(geolc, .(Var1), transform, Prozentual = Freq/sum(geolc$Freq) * 100)
geolc = ddply(geolc, .(Var1), transform, pos = (cumsum(Freq) - 0.5 * Freq))
geolc$label = paste0(sprintf("%.0f", geolc$Prozentual), "%")
names(geolc)[1:2] <- c("Ortsebene", "Anzahl")

ggplot(geolc, aes(x = geolc$`Ortsebene`, y = Anzahl, fill = geolc$`Ortsebene`)) +
  geom_bar(stat = "identity", width = .6) + ggtitle("Qualifizierung der Ortsebene") + 
  geom_text(aes(y = pos, label = label), size = 3) +
  theme_gdocs() + scale_fill_pander() + coord_flip() + scale_x_discrete(labels = element_blank()) + 
  theme(legend.position = "right", legend.title=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())


# duplicate check

## overview

dc <- count(!is.na(raw$DUP_type)) ## geprüft / nicht geprüft 
dc$column <- c("nicht geprüft", "geprüft")
dc.df = ddply(dc, .(column), transform, percent = round(freq/sum(dc$freq) * 100))
dc.df

## duplicate types

dupty <- as.data.frame(table(raw$DUP_type))
dupty[1] <- c("Singles", "Kopfdubletten", "Folgedubletten")
names(dupty) <- c("Dublettentyp", "Anzahl")
dupty = ddply(dupty, .(Dublettentyp), transform, Prozentual = Anzahl/sum(dupty$Anzahl) * 100)
dupty = ddply(dupty, .(Dublettentyp), transform, pos = (cumsum(Anzahl) - 0.5 * Anzahl))
dupty$label = paste0(sprintf("%.0f", dupty$Prozentual), "%")

ggplot(dupty, aes(x = Dublettentyp, y = Anzahl, fill = Dublettentyp)) +
  geom_bar(stat = "identity", width = .6) + ggtitle("Dublettenabgleich") + 
  geom_text(aes(y = pos, label = label), size = 3) +
  theme_gdocs() + scale_fill_pander() + theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank())

## statement validity

dups <- as.data.frame(table(raw$DUP_comp))
dups$Var1 <- as.character(dups$Var1)
dups <- subset(dups, !dups$Var1 == "")
dups$Var1[dups$Var1 == "0"] <- "Singles"
dups$Var1[dups$Var1 == "1"] <- "identisch"
dups$Var1[dups$Var1 == "2"] <- "geringe Toleranz"
dups$Var1[dups$Var1 == "3"] <- "ausgeglichene Toleranz"
dups$Var1[dups$Var1 == "4"] <- "große Toleranz"

dups = ddply(dups, .(Var1), transform, Prozentual = Freq/sum(dups$Freq) * 100)
dups = ddply(dups, .(Var1), transform, pos = (cumsum(Freq) - 0.5 * Freq))
dups$label = paste0(sprintf("%.0f", dups$Prozentual), "%")
names(dups)[1:2] <- c("Dublettensicherheit", "Anzahl")

ggplot(dups, aes(x = dups$`Dublettensicherheit`, y = Anzahl, fill = dups$`Dublettensicherheit`)) +
  geom_bar(stat = "identity", width = .6) + ggtitle("Aussagekraft der Dublettenfindung") + 
  geom_text(aes(y = pos, label = label), size = 3) +
  theme_gdocs() + scale_fill_pander() + coord_flip() + scale_x_discrete(labels = element_blank()) + 
  theme(legend.position = "right", legend.title=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())


# distribution of countries

countries <- as.data.frame(table(raw$SRC_CT_Country))
countries <- subset(countries, !countries$Var1 == "")
countries <- countries[order(-countries$Freq),] 
names(countries) <- c("Länder", "Anzahl")
countries
map <- joinCountryData2Map(countries, joinCode = "ISO2", nameJoinColumn = "Länder")
mapCountryData(map, nameColumnToPlot="Anzahl", catMethod = "fixedWidth", colourPalette = "topo")

