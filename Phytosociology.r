## Phytosociological parameters in R                 ##
### Gustavo Heringer <<gustavoheringer@hotmail.com>> ##
### May 19 2019                                      ##
#######################################################

########## WOODY SAMPLING ##########

# Before start:

# 1- When a tree has bifurcated trunk, you must use abundance as 0 for the second, third, fourth... trunks in the same individual (like in line 19 in the example below)

# 2- Your data frame must look like:
# plot| species| abundance|  cbh| height|
#   81|    sp01|         1| 17.9|    4.0|
#   81|    sp02|         1| 29.8|    5.0|
#   81|    sp03|         1| 84.6|   10.8|
#   81|    sp04|         1| 40.6|    7.5|
#   81|    sp04|         0| 21.8|    3.9|

# Where, cbh = circumference at breast height

# 3- Here we have a sampling area of 1000 m^2 (or 0.1 hectares) and 10 samples in total
area.wo <- 0.1 
sample.wo <- 10

# Prepearing the data (here cbh in centimeters is also converted to the basal area in meters)
df.wo$basal_a <- (3.1415926*(df.wo$cbh/(2*3.1415926))^2)/10000
basal_a_wo <- tapply(df.wo$basal_a, df.wo$species, function(x) sum(x))
freq_wo <- tapply(df.wo$plot, df.wo$species, function(x) length(unique(x)))
abund_wo <- tapply(df.wo$abundance, df.wo$species, function(x) sum(x))
                   
# If the researcher sampled dbh (diameter at breast height) instead circumference, the code line 27 must be replaced with: df.wo$basal_a <- 3.1415926*(df.wo$dbh/2)^2/10000
                   
# Creating a dataframe
table_wo <- data.frame(SPECIES=names(abund_wo), BASAL_A=basal_a_wo, FREQ=freq_wo, ABUND=abund_wo)

# Phytosociological parameters
table_wo$ADo <- table_wo$BASAL_A/area.wo
table_wo$RDo <- 100*(table_wo$BASAL_A/sum(table_wo$BASAL_A))
table_wo$AFr <- 100*(table_wo$FREQ/sample.wo)
table_wo$RFr <- 100*(table_wo$FREQ/sum(table_wo$FREQ))
table_wo$ADe <- table_wo$ABUND/area.wo
table_wo$RDe <- 100*(table_wo$ABUND/sum(table_wo$ABUND))
table_wo$IVI <- table_wo$RDo + table_wo$RFr + table_wo$RDe

# Where, ADo = Absolute Dominance, RDo = Relative Dominance, AFr = Absolute Frequency, RFr = Relative Frequency, ADe = Absolute Density, RDe = Relative Density, IVI = Importance Value Index.

# Saving the dataframe
write.table(table_wo, "phyto_from_field_to_table_woody.txt", row.names = F)

########## GROUND SAMPLING (or BRAUN-BLANQUET SAMPLING) ##########

# Before start:

# 1- Your data frame must look like:
# plot| species| abundance| cover_class| cover_percent| cover_prop|
#    1|     sp1|         4|           2|          15.0|      0.150|
#    1|     sp2|         1|           R|          00.1|      0.001|
#    1|     sp3|         2|           2|          15.0|      0.150|
#    1|     sp4|         8|           5|          87.5|      0.875|

# Where, cover_class = cover class of Braun-Blanquet (1979), cover_percent = mean of cover in percentage based on cover class of Braun-Blanquet (1979), and cover_prop = mean of cover in proportion based on cover class of Braun-Blanquet (1979). 

# 2- Here we have a sampling area of 10 m^2 and 10 samples in total
area.bb <- 10 
sample.bb <- 10

# Prepering the data
cover_prop <- tapply(df.bb$cover_prop, df.bb$species, function(x) sum(x))
freq <- tapply(df.bb$plot, df.bb$species, function(x) length(unique(x)))
abund <- tapply(df.bb$abundance, df.bb$species, function(x) sum(x))
                
# Creating a dataframe
table_bb <- data.frame(SPECIES=names(abund), COVER=cover_prop, FREQ=freq, ABUND=abund)

# Phytosociological parameters
table_bb$CV <- 100*(table_bb$COVER/area.bb)
table_bb$RC <- 100*(table_bb$COVER/sum(table_bb$COVER))
table_bb$AFr <- 100*(table_bb$FREQ/sample.bb) 
table_bb$RFr <- 100*(table_bb$FREQ/sum(table_bb$FREQ))
table_bb$ADe <- table_bb$ABUND/area.bb
table_bb$RDe <- 100*(table_bb$ABUND/sum(table_bb$ABUND))
table_bb$IVI <- table_bb$RC + table_bb$RFr + table_bb$RDe

# Where, CV = Cover Value, RC = Relative Cover, AFr = Absolute Frequency, RFr = Relative Frequency, ADe = Absolute Density, RDe = Relative Density, IVI = Importance Value Index.

# Saving the dataframe
write.table(table_bb, "phyto_from_field_to_table_braun_b.txt", row.names = F)
