# Consider the User Record data you want
plot_id = 2360

########################################
####Design Functions####################
########################################
mappingStage1andStage2 <- function(WISE_HORIZON_AWC_ORGC_TEXTURE_LIST,WISE_SITE_GDAL_STANDLIZATION_LIST_SELECTED_RESULT){
  wiseSize <- length(WISE_SITE_GDAL_STANDLIZATION_LIST_SELECTED_RESULT[,"WISE3_ID"])
  WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_50_HIGHEST <- subset(WISE_HORIZON_AWC_ORGC_TEXTURE_LIST,(WISE_HORIZON_AWC_ORGC_TEXTURE_LIST$WISE3_ID %in% WISE_SITE_GDAL_STANDLIZATION_LIST_SELECTED_RESULT$WISE3_ID) & as.numeric(WISE_HORIZON_AWC_ORGC_TEXTURE_LIST$HONU) == 1)
  horizonSize <- length(WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_50_HIGHEST[,"WISE3_ID"])
  for(i in 1:horizonSize){
    for(j in 1:wiseSize){
      if (toString(WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_50_HIGHEST[i,"WISE3_ID"]) == toString(WISE_SITE_GDAL_STANDLIZATION_LIST_SELECTED_RESULT[j,"WISE3_ID"])){
        WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_50_HIGHEST[i,"cosine_similarity_index_stage_1"] <- WISE_SITE_GDAL_STANDLIZATION_LIST_SELECTED_RESULT[j,"cosine_similarity_index_stage_1"]
      }
    }
  }
  return(WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_50_HIGHEST)
}
bindTextureAWCData <- function(user_infor_data,user_awc_data,soil_texture_data){
  print("Start Binding data")
  plot_id <- user_infor_data$ID
  text_1 <- user_infor_data$texture_for_soil_horizon_1
  tuple_texture_sand_silt_clay <- subset(soil_texture_data,soil_texture_data$texture == toString(text_1),select = c("sand","silt","clay"))
  tuple_texture_all <- subset(soil_texture_data,soil_texture_data$texture == toString(text_1))
  awc_profile <- as.numeric(user_awc_data[1,"soil_profile_awc"])
  newList <- list("plot_id" = plot_id, "texture_sand_silt_clay" = tuple_texture_sand_silt_clay,"texture_all" = tuple_texture_all, "awc_profile" = awc_profile)
  return(newList)
}
calculation_normalization <- function(WISE_SITE_GDAL_STANDLIZATION_NUMBEROUS,step_3_1_user_plot_data_numberous,check){
  print("Start runing Normalization")
  number_rows <- length(WISE_SITE_GDAL_STANDLIZATION_NUMBEROUS[,check])
  number_columns <- length(WISE_SITE_GDAL_STANDLIZATION_NUMBEROUS)
  print("Number of Rows : ")
  print(number_rows)
  print("Number of Columns : ")
  print(number_columns)
  min_fields <- c()
  max_fields <- c()
  for (i in 1:number_columns){
    min_fields[i] <- min(WISE_SITE_GDAL_STANDLIZATION_NUMBEROUS[i])
    if (min_fields[i] > step_3_1_user_plot_data_numberous[i]){
      min_fields[i] <- step_3_1_user_plot_data_numberous[i]
    }
    max_fields[i] <- max(WISE_SITE_GDAL_STANDLIZATION_NUMBEROUS[i])
    if (max_fields[i] < step_3_1_user_plot_data_numberous[i]){
      max_fields[i] <- step_3_1_user_plot_data_numberous[i]
    }
  }
  #print("Min data : ")
  #print(min_fields)
  #print("Max data : ")
  #print(max_fields)
  
  for(i in 1:number_rows){
    for(j in 1:number_columns){
      WISE_SITE_GDAL_STANDLIZATION_NUMBEROUS[i,j] = (as.numeric(WISE_SITE_GDAL_STANDLIZATION_NUMBEROUS[i,j]) - as.numeric(min_fields[j])) / (as.numeric(max_fields[j]) - as.numeric(min_fields[j]))
    }
  }
  
  for(i in 1:number_columns){
    step_3_1_user_plot_data_numberous[i] = (as.numeric(step_3_1_user_plot_data_numberous[i]) - as.numeric(min_fields[i])) / (as.numeric(max_fields[i]) - as.numeric(min_fields[i]))
  }
  print("Done : Normalization")
  newList <- list("WISE_SITE_GDAL_STANDLIZATION_NUMBEROUS" = WISE_SITE_GDAL_STANDLIZATION_NUMBEROUS,"step_3_1_user_plot_data_numberous" = step_3_1_user_plot_data_numberous)
  return(newList)
}

calculation_similarity <- function(WISE_SITE_GDAL_STANDLIZATION_NUMBEROUS_NORM,step_3_1_user_plot_data_numberous_NORM,method){
   if (method == "cosine_vector_space_model"){
      size <- length(WISE_SITE_GDAL_STANDLIZATION_NUMBEROUS_NORM[,1])
      cosine_similarity_array <- c()
      for(i in 1:size){
        cosine_similarity_array[i] <- cosine.sim(step_3_1_user_plot_data_numberous_NORM,WISE_SITE_GDAL_STANDLIZATION_NUMBEROUS_NORM[i,])
      }
      return(cosine_similarity_array)
  } else {
    print("Does not support now")
    return(NULL)
  }
}

cosine.sim <- function(A,B){
  A <- as.numeric(A)
  B <- as.numeric(B)
  return(sum(A*B)/sqrt(sum(A^2)*sum(B^2)))
}

########################################
####Read input of user record###########
########################################
similarity_method = 'cosine_vector_space_model'
# Read User Data LandInfo
all_user_info_data <- read.csv(file="./USER_INPUT_DATA/User_Input_LandInfo.csv",head=TRUE,sep=",")
# Read USer Data GDAL
all_user_gdal_data <- read.csv(file="./USER_INPUT_DATA/User_Input_Global_GDAL.csv",head=TRUE,sep=",")
# Read USer Data AWC 
all_user_awc_data <- read.csv(file="./USER_INPUT_DATA/User_Input_Soil_AWC.csv",head=TRUE,sep=",")
# Read Soil Tecture Data
soil_texture_data <- read.csv(file="./USER_INPUT_DATA/User_Input_Texture.csv",head=TRUE,sep=",")
# Read all user data follow record_id
user_infor_data <- all_user_info_data[all_user_info_data$ID==plot_id,]
user_gdal_data <- all_user_gdal_data[all_user_gdal_data$record_id==plot_id,]
user_awc_data <- all_user_awc_data[all_user_awc_data$record_id==plot_id,]
########################################
####Read Wise Site Data + GDAL data#####
########################################
WISE_SITE_GDAL_STANDLIZATION_LIST <- read.csv(file="./LANDPKS_WISE_DATA/WISE3_LANDPKS_GDAL_STANDALIZATION.csv",head=TRUE,sep=",")
#################################################################################
####Prepare user input to run similarity=== STAGE 1 == 3_1 Python == GDAL Data###
#################################################################################
step_3_1_user_plot_data_Identify = user_gdal_data[,c("ID","record_id")]
step_3_1_user_plot_data_numberous = user_gdal_data[,c("latitude","longitude","clim_precipitation_data","clim_gdd","clim_aridity_index","topog_elevation","topog_slope_global","topog_twi_global","topog_topi_global","topog_israd_global")]
step_3_1_user_plot_data_category = user_gdal_data[,c("clim_kopgeiger","clim_fao_lgp","topog_geolage","topog_landform_global")]
user_data_surface_craking <- user_infor_data[1,"surface_cracking"]
#####################################################################################
####Prepare WISE GDAL Data to run similarity=== STAGE 1 == 3_1 Python == GDAL Data###
####Preprocessing 1 : Use records that has same Kop-Geiger with user data############
####Preprocessing 2 : If surface craking == TRUE, use only Vc, Vp        ############
#####################################################################################
if (user_data_surface_craking == "true" | user_data_surface_craking == "TRUE"){
   WISE_SITE_GDAL_STANDLIZATION_LIST_SELECTED <- WISE_SITE_GDAL_STANDLIZATION_LIST[as.numeric(WISE_SITE_GDAL_STANDLIZATION_LIST$CLIM_KOPGEIGER)==as.numeric(step_3_1_user_plot_data_category[1]) & (WISE_SITE_GDAL_STANDLIZATION_LIST$WISE3_FAO_74 == "Vp" | WISE_SITE_GDAL_STANDLIZATION_LIST$WISE3_FAO_74 == "Vc"),]
} else {
   WISE_SITE_GDAL_STANDLIZATION_LIST_SELECTED <- WISE_SITE_GDAL_STANDLIZATION_LIST[as.numeric(WISE_SITE_GDAL_STANDLIZATION_LIST$CLIM_KOPGEIGER)==as.numeric(step_3_1_user_plot_data_category[1]),]
}
WISE_SITE_GDAL_STANDLIZATION_NUMBEROUS <- WISE_SITE_GDAL_STANDLIZATION_LIST_SELECTED[,c("LATITUDE_DECIMAL","LONGITUDE_DECIMAL","CLIM_PRECIPITATION_DATA","CLIM_GDD","CLIM_ARIDITY_INDEX","topog_elevation","topog_slope_global","topog_twi_global","topog_topi_global","topog_israd_global")]
WISE_SITE_GDAL_STANDLIZATION_CATEGORY <- WISE_SITE_GDAL_STANDLIZATION_LIST_SELECTED[,c("CLIM_KOPGEIGER","CKIM_FAO_LGP","topog_geolage","topog_landform_global")]
#####################################################################################
####Calculate Normalization=== STAGE 1 == 3_1 Python == GDAL Data###
#####################################################################################
normalizationDataList <- calculation_normalization(WISE_SITE_GDAL_STANDLIZATION_NUMBEROUS,step_3_1_user_plot_data_numberous,"LATITUDE_DECIMAL")
#####################################################################################
####Calculate Similarity  === STAGE 1 == 3_1 Python == GDAL Data###
#####################################################################################
cosine_similarity_object <- calculation_similarity(normalizationDataList$WISE_SITE_GDAL_STANDLIZATION_NUMBEROUS,normalizationDataList$step_3_1_user_plot_data_numberous,method ="cosine_vector_space_model")
#####################################################################################
####Add Cosine value to Data Store###
#####################################################################################
size <- length(WISE_SITE_GDAL_STANDLIZATION_LIST_SELECTED[,1])
for(i in 1:size){
  WISE_SITE_GDAL_STANDLIZATION_LIST_SELECTED[i,"cosine_similarity_index_stage_1"] <- cosine_similarity_object[i]
}
#####################################################################################
####Sort to get 50 highest similarity value###
#####################################################################################
WISE_SITE_GDAL_STANDLIZATION_LIST_SELECTED_RESULT <- WISE_SITE_GDAL_STANDLIZATION_LIST_SELECTED[with(WISE_SITE_GDAL_STANDLIZATION_LIST_SELECTED, order(-cosine_similarity_object)), ]
WISE_SITE_GDAL_STANDLIZATION_LIST_SELECTED_RESULT <- WISE_SITE_GDAL_STANDLIZATION_LIST_SELECTED_RESULT[1:50,]
#####################################################################################
####Write Result to File -- Ending STAGE 1###
#####################################################################################
dir.create("./LANDPKS_WISE_DATA/Private", showWarnings = FALSE, recursive = FALSE, mode = "0777")
privateDataPath <- paste0("./LANDPKS_WISE_DATA/Private/",plot_id)
dir.create(privateDataPath, showWarnings = FALSE, recursive = FALSE, mode = "0777")
privateDataPath_File <- paste0(privateDataPath,"/Wise_3_LandPKS_GDAL_User_Data.csv")
file.create(privateDataPath_File, showWarnings = FALSE, recursive = FALSE, mode = "0777")
write.csv(user_gdal_data[,c("record_id","latitude", "longitude", "clim_precipitation_data","clim_gdd","clim_aridity_index","clim_kopgeiger","clim_fao_lgp","topog_elevation","topog_geolage","topog_slope_global","topog_landform_global","topog_twi_global","topog_topi_global","topog_israd_global")], file = privateDataPath_File)
privateDataPath_File <- paste0(privateDataPath,"/Wise_3_LandPKS_GDAL_Highest_Similarity_Index.csv")
file.create(privateDataPath_File, showWarnings = FALSE, recursive = FALSE, mode = "0777")
write.csv(WISE_SITE_GDAL_STANDLIZATION_LIST_SELECTED_RESULT[,c("WISE3_ID","LATITUDE_DECIMAL", "LONGITUDE_DECIMAL", "cosine_similarity_index_stage_1","CLIM_PRECIPITATION_DATA","CLIM_GDD","CLIM_ARIDITY_INDEX","CLIM_KOPGEIGER","CKIM_FAO_LGP","topog_elevation","topog_geolage","topog_slope_global","topog_landform_global","topog_twi_global","topog_topi_global","topog_israd_global","WISE3_FAO_74")], file = privateDataPath_File)
########################################
########################################
########################################
########################################
########################################
########################################
########################################
#####################################################################################
####Start Stage 2 --- Finding Similarity based AWC and ORGC and Texture Soil Data####
#####################################################################################
#################################################################################
####Prepare user input to run similarity=== STAGE 2 == 5_1 Python == GDAL Data###
#################################################################################
step_5_1_user_plot_texture_awc_LIST <- bindTextureAWCData(user_infor_data,user_awc_data,soil_texture_data)
step_5_1_user_plot_texture_awc_numberous <- step_5_1_user_plot_texture_awc_LIST$texture_sand_silt_clay
step_5_1_user_plot_texture_awc_numberous$soil_profile_awc <- step_5_1_user_plot_texture_awc_LIST$awc_profile
#################################################################################
####Prepare WISE AWC ORGC to run similarity=== STAGE 2 == 5_1 Python == GDAL Data###
#################################################################################
WISE_HORIZON_AWC_ORGC_TEXTURE_LIST <- read.csv(file="./LANDPKS_WISE_DATA/WISE3_LANDPKS_HORIZON_AWC_ORGC_DATA.csv",head=TRUE,sep=",")
#################################################################################
####Build Mapping Data Frame 50 hightest Stage 1 and HORIZON DATA###
#################################################################################
WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_50_HIGHEST <- mappingStage1andStage2(WISE_HORIZON_AWC_ORGC_TEXTURE_LIST,WISE_SITE_GDAL_STANDLIZATION_LIST_SELECTED_RESULT)
WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_50_HIGHEST <- WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_50_HIGHEST[with(WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_50_HIGHEST, order(-cosine_similarity_index_stage_1)), ]
#####################################################################################
####Calculate Normalization=== STAGE 2 == 5_1 Python == AWC Texture Data###
#####################################################################################
WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_50_HIGHEST_NUMBEROUS <- WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_50_HIGHEST[,c("SAND","SILT","CLAY","SOIL_PROFILE_AWC")]
normalizationDataList_Stage2 <- calculation_normalization(WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_50_HIGHEST_NUMBEROUS,step_5_1_user_plot_texture_awc_numberous,"SAND")
#####################################################################################
####Calculate Similarity  === STAGE 2 == 5_1 Python == GDAL Data###
#####################################################################################
cosine_similarity_object_Stage_2 <- calculation_similarity(normalizationDataList_Stage2$WISE_SITE_GDAL_STANDLIZATION_NUMBEROUS,normalizationDataList_Stage2$step_3_1_user_plot_data_numberous,method ="cosine_vector_space_model")
#####################################################################################
####Add Cosine value to Data Store###
#####################################################################################
size <- length(WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_50_HIGHEST[,1])
for(i in 1:size){
  WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_50_HIGHEST[i,"cosine_similarity_index_stage_2"] <- cosine_similarity_object_Stage_2[i]
}
#####################################################################################
####Sort to get 50 highest similarity value###
#####################################################################################
WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_50_HIGHEST <- WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_50_HIGHEST[with(WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_50_HIGHEST, order(-cosine_similarity_index_stage_2)), ]
WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_5_HIGHEST <- WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_50_HIGHEST[1:5,]
#####################################################################################
####BUILD FINAL RESULT###
#####################################################################################
finalSize <- length(WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_5_HIGHEST[,1])
WISE_SITE_DATA <- read.csv(file="./ISRIC_WISE_DATABASE/WISE3_SITE.csv",head=TRUE,sep=",")
WISE_SITE_DATA <- subset(WISE_SITE_DATA,WISE_SITE_DATA$WISE3_id %in% WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_5_HIGHEST$WISE3_ID)
for(i in 1:finalSize){
  for(j in 1:finalSize){
    if (WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_5_HIGHEST[i,"WISE3_ID"] == WISE_SITE_DATA[j,"WISE3_id"]){
      WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_5_HIGHEST[i,"COUNTRY"] = WISE_SITE_DATA[j,"COUNTRY"]
      WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_5_HIGHEST[i,"SOLDEP"] = WISE_SITE_DATA[j,"SOLDEP"]
      WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_5_HIGHEST[i,"FAO_74"] = WISE_SITE_DATA[j,"FAO_74"]
      WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_5_HIGHEST[i,"USCL"] = WISE_SITE_DATA[j,"USCL"]
      WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_5_HIGHEST[i,"SLOPE"] = WISE_SITE_DATA[j,"SLOPE"]
    }
  }
}

WISE_SITE_GDAL <- subset(WISE_SITE_GDAL_STANDLIZATION_LIST_SELECTED_RESULT,WISE_SITE_GDAL_STANDLIZATION_LIST_SELECTED_RESULT$WISE3_ID %in% WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_5_HIGHEST$WISE3_ID)
for(i in 1:finalSize){
  for(j in 1:finalSize){
    if (WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_5_HIGHEST[i,"WISE3_ID"] == toString(WISE_SITE_GDAL[j,"WISE3_ID"])){
      WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_5_HIGHEST[i,"LATITUDE_DECIMAL"] = WISE_SITE_GDAL[j,"LATITUDE_DECIMAL"]
      WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_5_HIGHEST[i,"LONGITUDE_DECIMAL"] = WISE_SITE_GDAL[j,"LONGITUDE_DECIMAL"]
    }
  }
}

privateDataPath_File <- paste0(privateDataPath,"/Wise_3_LandPKS_Final_Result_WISE.csv")
file.create(privateDataPath_File, showWarnings = FALSE, recursive = FALSE, mode = "0777")
write.csv(WISE_HORIZON_AWC_ORGC_TEXTURE_LIST_5_HIGHEST[,c("WISE3_ID","LATITUDE_DECIMAL", "LONGITUDE_DECIMAL", "COUNTRY","SOLDEP","FAO_74","USCL","SLOPE","cosine_similarity_index_stage_1","cosine_similarity_index_stage_2","HONU","TOP_DEP","BOT_DEP","SAND","SILT","CLAY","SOIL_PROFILE_AWC")], file = privateDataPath_File)


privateDataPath_File <- paste0(privateDataPath,"/Wise_3_LandPKS_Final_Result_USER_DATA.csv")
file.create(privateDataPath_File, showWarnings = FALSE, recursive = FALSE, mode = "0777")
write.csv(user_infor_data[,c("ID","name", "recorder_name", "latitude","longitude","slope","texture_for_soil_horizon_1","texture_for_soil_horizon_2","texture_for_soil_horizon_3","texture_for_soil_horizon_4","texture_for_soil_horizon_5","texture_for_soil_horizon_6","texture_for_soil_horizon_7","surface_cracking")], file = privateDataPath_File)