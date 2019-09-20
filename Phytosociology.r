## Running phytosociological parameters in R:        ##
## phytosociology from field to table                ##
### Gustavo Heringer <<gustavoheringer@hotmail.com>> ##
### May 19 2019                                      ##
#######################################################

# Woody sampling at line 10 and Ground sampling at line 56.


########## WOODY SAMPLING ##########

# Before start:

# 1- When a tree has bifurcated trunk, you must use abundance as 0 for the second, third, fourth... trunks in the same individual (like in line 6 in the example below)

# 2- Your data frame must look like:
# plot| species| abundance|  cbh| height|
#   81|    sp01|         1| 17.9|    4.0|
#   81|    sp02|         1| 29.8|    5.0|
#   81|    sp01|         1| 31.7|    7.5|
#   81|    sp03|         1| 84.6|   10.8|
#   81|    sp04|         1| 40.6|    7.5|
#   81|    sp04|         0| 21.8|    3.9|
#   81|    sp05|         1| 30.1|    5.1|

# Where, cbh = circumference at breast height

# 3- Here we have a sampling area of 1000 m^2 (or 0.1 hectares) and 10 samples in total
area.wo <- 0.1 
sample.wo <- 10

# Prepearing the data
df.wo$basal_a <- (3.1415926*(df.wo$cbh/(2*3.1415926))^2)/10000
abund_wo <- tapply(df.wo$abundance, df.wo$species, function(x) sum(x))
basal_a_wo <- tapply(df.wo$basal_a, df.wo$species, function(x) sum(x))
freq_wo <- tapply(df.wo$plot, df.wo$species, function(x) length(unique(x)))
  
# Creating a dataframe
table_wo <- data.frame(SPECIES=names(abund_wo), ABUND_WO=abund_wo, BASAL_A_WO=basal_a_wo, FREQ_WO=freq_wo)

# Phytosociological parameters
table_wo$ADo <- table_wo$BASAL_A_WO/area.wo
table_wo$RDo <- 100*(table_wo$BASAL_A_WO/sum(table_wo$BASAL_A_WO))
table_wo$AFr <- 100*(table_wo$FREQ_WO/sample.wo)
table_wo$RFr <- 100*(table_wo$FREQ_WO/sum(table_wo$FREQ_WO))
table_wo$ADe <- table_wo$ABUND_WO/area.wo
table_wo$RDe <- 100*(table_wo$ABUND_WO/sum(table_wo$ABUND_WO))
table_wo$IVI <- table_wo$RDo + table_wo$RF + table_wo$RD

# Where, ADo = Absolute Dominance, RDo = Relative Dominance, AFr = Absolute Frequency, RFr = Relative Frequency, ADe = Absolute Density, RDe = Relative Density, IVI = Importance Value Index.

# Saving the dataframe
write.table(table_wo, "phytosociology_woody.txt", row.names = F)


########## GROUND SAMPLING (or BRAUN-BLANQUET SAMPLING) ##########

# Before start:

# 1- Your data frame must look like:
# plot| species| abundance| cover_class| cover_percent| cover_prop|
#    1|     sp1|         4|           2|          17.5|      0.175|
#    1|     sp2|         1|           R|          00.1|      0.001|
#    1|     sp3|         2|           2|          17.5|      0.175|
#    1|     sp4|         1|           2|          17.5|      0.175|

# Where, cover_class = cover class of Braun-Blanquet (1979), cover_percent = mean of cover in percentage based on cover class of Braun-Blanquet (1979), and cover_prop = mean of cover in proportion based on cover class of Braun-Blanquet (1979). 

# 2- Here we have a sampling area of 10 m^2 and 10 samples in total
area.bb <- 10 
sample.bb <- 10

# Prepearing the data
abund <- tapply(df.bb$abundance, df.bb$species, function(x) sum(x))
cover_prop <- tapply(df.bb$cover_prop, df.bb$species, function(x) sum(x))
freq <- tapply(df.bb$plot, df.bb$species, function(x) length(unique(x)))

# Creating a dataframe
table <- data.frame(SPECIES=names(abund), ABUND=abund, COVER_PROP=cover_prop, FREQ=freq)

# Phytosociological parameters
table$CV <- 100*(table$COVER_PROP/area.bb)
table$RC <- 100*(table$COVER_PROP/sum(table$COVER_PROP))
table$AFr <- 100*(table$FREQ/sample.bb) 
table$RFr <- 100*(table$FREQ/sum(table$FREQ))
table$ADe <- table$ABUND/area.bb
table$RDe <- 100*(table$ABUND/sum(table$ABUND))
table$IVI <- table$RC + table$RF + table$RD

# Where, CV = Cover Value, RC = Relative Cover, AFr = Absolute Frequency, RFr = Relative Frequency, ADe = Absolute Density, RDe = Relative Density, IVI = Importance Value Index.

# Saving the dataframe
write.table(table, "phytosociology_braun_blanquet.txt", row.names = F)
