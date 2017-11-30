library(lattice)
library(lubridate)
library(ncdf4)
library(stringr)
library(plyr)
library(pastecs)
library(oce)
library(measurements)

# read the data
an <- Anthogorgia
ac <- Acanthogorgia
ca <- Calcigorgia
cy <- Cyclomuricea
m <- Muricella

# clean names
colnames(an) <- c("ÔªøCatalog_Number","Kind_of_Object","Scientific_Name","Family","Phylum","Class","Order","Identified_By","Date_Identified","Current_Identification","Other_Identifications","Type_Status","Type_Citations","Kind_of_Voucher","Classification","Common_Name","Collection_Name","Specimen_Count","Sex_and_Stage","Preparation","Station_Number","Collector(s)","Date_Collected","Collection_Method","Ocean","Sea/Gulf","Bay/Sound","Country","Province/State","District/County","City/Town","Precise_Locality","Centroid_Latitude","Centroid_Longitude","Elevation_(m)","Expedition_Name","Vessel","Cruise","River_Basin","Microhabitat_Description","Depth_(m)","Depth_Notes","Geologic_Age","Notes","Field_Number(s)","GenBank_Numbers","Other_Numbers_(Type:Value)","Accession_Number","Genetic_Sample_Type","Biorepository_Number","Specimen_Voucher_Number","Bold_ID","Preservation_Method","Embargo?","Depleted?","Record_Last_Modified","EZID")
head <- names(an)
head <- str_replace(head, "\\(.*\\)", "")
head <- str_trim(head)
head <- make.names(head)
head <- tolower(head)
head <- str_replace(head, fixed(".."), ".")

colnames(ac) <- c("ÔªøCatalog_Number","Kind_of_Object","Scientific_Name","Family","Phylum","Class","Order","Identified_By","Date_Identified","Current_Identification","Other_Identifications","Type_Status","Type_Citations","Kind_of_Voucher","Classification","Common_Name","Collection_Name","Specimen_Count","Sex_and_Stage","Preparation","Station_Number","Collector(s)","Date_Collected","Collection_Method","Ocean","Sea/Gulf","Bay/Sound","Country","Province/State","District/County","City/Town","Precise_Locality","Centroid_Latitude","Centroid_Longitude","Elevation_(m)","Expedition_Name","Vessel","Cruise","River_Basin","Microhabitat_Description","Depth_(m)","Depth_Notes","Geologic_Age","Notes","Field_Number(s)","GenBank_Numbers","Other_Numbers_(Type:Value)","Accession_Number","Genetic_Sample_Type","Biorepository_Number","Specimen_Voucher_Number","Bold_ID","Preservation_Method","Embargo?","Depleted?","Record_Last_Modified","EZID")
head <- names(ac)
head <- str_replace(head, "\\(.*\\)", "")
head <- str_trim(head)
head <- make.names(head)
head <- tolower(head)
head <- str_replace(head, fixed(".."), ".")

colnames(ca) <- c("ÔªøCatalog_Number","Kind_of_Object","Scientific_Name","Family","Phylum","Class","Order","Identified_By","Date_Identified","Current_Identification","Other_Identifications","Type_Status","Type_Citations","Kind_of_Voucher","Classification","Common_Name","Collection_Name","Specimen_Count","Sex_and_Stage","Preparation","Station_Number","Collector(s)","Date_Collected","Collection_Method","Ocean","Sea/Gulf","Bay/Sound","Country","Province/State","District/County","City/Town","Precise_Locality","Centroid_Latitude","Centroid_Longitude","Elevation_(m)","Expedition_Name","Vessel","Cruise","River_Basin","Microhabitat_Description","Depth_(m)","Depth_Notes","Geologic_Age","Notes","Field_Number(s)","GenBank_Numbers","Other_Numbers_(Type:Value)","Accession_Number","Genetic_Sample_Type","Biorepository_Number","Specimen_Voucher_Number","Bold_ID","Preservation_Method","Embargo?","Depleted?","Record_Last_Modified","EZID")
head <- names(ca)
head <- str_replace(head, "\\(.*\\)", "")
head <- str_trim(head)
head <- make.names(head)
head <- tolower(head)
head <- str_replace(head, fixed(".."), ".")

colnames(cy) <- c("ÔªøCatalog_Number","Kind_of_Object","Scientific_Name","Family","Phylum","Class","Order","Identified_By","Date_Identified","Current_Identification","Other_Identifications","Type_Status","Type_Citations","Kind_of_Voucher","Classification","Common_Name","Collection_Name","Specimen_Count","Sex_and_Stage","Preparation","Station_Number","Collector(s)","Date_Collected","Collection_Method","Ocean","Sea/Gulf","Bay/Sound","Country","Province/State","District/County","City/Town","Precise_Locality","Centroid_Latitude","Centroid_Longitude","Elevation_(m)","Expedition_Name","Vessel","Cruise","River_Basin","Microhabitat_Description","Depth_(m)","Depth_Notes","Geologic_Age","Notes","Field_Number(s)","GenBank_Numbers","Other_Numbers_(Type:Value)","Accession_Number","Genetic_Sample_Type","Biorepository_Number","Specimen_Voucher_Number","Bold_ID","Preservation_Method","Embargo?","Depleted?","Record_Last_Modified","EZID")
head <- names(cy)
head <- str_replace(head, "\\(.*\\)", "")
head <- str_trim(head)
head <- make.names(head)
head <- tolower(head)
head <- str_replace(head, fixed(".."), ".")

colnames(m) <- c("ÔªøCatalog_Number","Kind_of_Object","Scientific_Name","Family","Phylum","Class","Order","Identified_By","Date_Identified","Current_Identification","Other_Identifications","Type_Status","Type_Citations","Kind_of_Voucher","Classification","Common_Name","Collection_Name","Specimen_Count","Sex_and_Stage","Preparation","Station_Number","Collector(s)","Date_Collected","Collection_Method","Ocean","Sea/Gulf","Bay/Sound","Country","Province/State","District/County","City/Town","Precise_Locality","Centroid_Latitude","Centroid_Longitude","Elevation_(m)","Expedition_Name","Vessel","Cruise","River_Basin","Microhabitat_Description","Depth_(m)","Depth_Notes","Geologic_Age","Notes","Field_Number(s)","GenBank_Numbers","Other_Numbers_(Type:Value)","Accession_Number","Genetic_Sample_Type","Biorepository_Number","Specimen_Voucher_Number","Bold_ID","Preservation_Method","Embargo?","Depleted?","Record_Last_Modified","EZID")
head <- names(m)
head <- str_replace(head, "\\(.*\\)", "")
head <- str_trim(head)
head <- make.names(head)
head <- tolower(head)
head <- str_replace(head, fixed(".."), ".")

cy$`Date_Collected` <- as.Date(as.character(cy$`Date_Collected`), format = "%y/%m/%d")
ac$Date_Collected <- as.Date(as.character(ac$Date_Collected), format = "%y/%m/%d")
an$Date_Collected <- as.Date(as.character(an$Date_Collected), format = "%y/%m/%d")
ca$Date_Collected <- as.Date(as.character(ca$Date_Collected), format = "%y/%m/%d")
m$Date_Collected <- as.Date(as.character(m$Date_Collected), format = "%y/%m/%d")

ac$`Depth_(m)` <- as.numeric(ac$`Depth_(m)`)
an$`Depth_(m)` <- as.numeric(an$`Depth_(m)`)
ca$`Depth_(m)` <- as.numeric(ca$`Depth_(m)`)
cy$`Depth_(m)` <- as.numeric(cy$`Depth_(m)`)
m$`Depth_(m)` <- as.numeric(m$`Depth_(m)`)

#p$LONG <- as.numeric(p$LONG)
#p$LAT <- as.numeric(p$LAT)

#p$LONG <- p$LONG* -1
#p$LAT <- p$LAT* -1

# columns that are all zero are not possible. They are actually missing data. Detect them
totCol <- colSums(an[llply(an, class) == "numeric"])
allZeroCols <- names(totCol)[which(totCol == 0)]
an[,allZeroCols] <- NA # replace the content with NAs

totCol <- colSums(ac[llply(ac, class) == "numeric"])
allZeroCols <- names(totCol)[which(totCol == 0)]
ac[,allZeroCols] <- NA # replace the content with NAs

totCol <- colSums(cy[llply(cy, class) == "numeric"])
allZeroCols <- names(totCol)[which(totCol == 0)]
cy[,allZeroCols] <- NA # replace the content with NAs

totCol <- colSums(ca[llply(ca, class) == "numeric"])
allZeroCols <- names(totCol)[which(totCol == 0)]
ca[,allZeroCols] <- NA # replace the content with NAs

totCol <- colSums(m[llply(m, class) == "numeric"])
allZeroCols <- names(totCol)[which(totCol == 0)]
m[,allZeroCols] <- NA # replace the content with NAs

#remove points without coordinates
ac <- subset(ac, !is.na(Centroid_Latitude))
an <- subset(an, !is.na(Centroid_Latitude))
ca <- subset(ca, !is.na(Centroid_Latitude))
cy <- subset(cy, !is.na(Centroid_Latitude))
m <- subset(m, !is.na(Centroid_Latitude))

#write and export data
write.table(ac, "Acanthogorgia.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(an, "Anthogorgia.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(ca, "Calcigorgia.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(cy, "Cyclomuricea.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(m, "Muricella.txt", sep = "\t", row.names = FALSE, col.names = TRUE)

