NEON_MycoProp <- function(sites, startdate,enddate){
  #check if dependencies are installed. If not, stop.-
  if (!require('neonUtilities'   ,character.only = TRUE)){
    stop("please install the neonUtilities package.")
  }
  if (!require('plyr'   ,character.only = TRUE)){
    stop("please install the plyr package.")
  }
  if (!require('dplyr'   ,character.only = TRUE)){
    stop("please install the dplyr package.")
  }
  if (!require('tidyr'   ,character.only = TRUE)){
    stop("please install the tidyr package.")
  }
  #check that the input is formatted right. If not, stop, throw an error.
  if (!is.character(sites)){
    stop('You need add sites in character format. Try again.')
  }
  #Download woody plant structure data product from NEON
  library(neonUtilities)
  zipsByProduct(dpID="DP1.10098.001",site=sites,
                startdat=startdate, enddate =enddate,
                package="expanded", check.size=F)
  
  # Make tree identity object
  wd <- getwd()
  library(plyr)
  library(dplyr)
  setwd(gsub(" ","",paste(wd,"/filesToStack10098")))
  dir.create("TreeIdentity") #create directory for NEON unzipped files
  zip_files = list.files(path = getwd(), pattern="*.zip") #create list of unzipped files
  llply(.data=zip_files, .fun=unzip, exdir="TreeIdentity") #Unzip all zip files in TreeIdentity directory
  setwd(wd)
  mydir <- gsub(" ","",paste(wd,"/filesToStack10098/TreeIdentity/"))
  myfiles_TreeIdent = list.files(path=mydir, pattern="*mappingandtagging*")
  setwd(gsub(" ","",paste(wd,"/filesToStack10098/TreeIdentity/")))
  site_Tree <- ldply(myfiles_TreeIdent, .fun=read.csv)
  myfiles_TreeData = list.files(path=mydir, pattern="*.vst_apparentindividual*", full.names=TRUE) # read in other data including basal area
  site_TreeData <- ldply(myfiles_TreeData, read.csv)
  myfiles_class <- list.files(path=mydir, pattern="*perplotperyear*", full.names=TRUE)
  site_class <- ldply(myfiles_class, read.csv) %>%
    dplyr::select(plotID, nlcdClass)
  site_TreeData_sim <- site_TreeData %>%
    dplyr::select(individualID, namedLocation, growthForm, stemDiameter, measurementHeight) %>%
    dplyr::filter(stemDiameter != "NA")
  TreeData <- site_Tree %>% right_join(site_TreeData_sim, by="individualID")
  TreeData_class <- TreeData %>% left_join(site_class, by="plotID")
  TreeData_sep <- TreeData_class %>%
    tidyr::separate(col="scientificName", into = c("Genus", "rest"), sep="\\s") #Separate tree genus to match mycorrhizal type
  setwd(wd) 
  #Tree individuals now have genus separated for addition of mycorrhixal type #
  MycoAssoc <-  readRDS("MycoTree.rds") # FungalRoot Mycorrhizal association information
  MycoTree <- TreeData_sep %>% left_join(MycoAssoc, by = "Genus") # Add mycorrhizal association to tree genus 
  MycoTree <- MycoTree %>%
    dplyr::mutate(BA = (pi*(stemDiameter^2))/40000)#Calculate Basal Area of each tree
  
  PlotTBA <- MycoTree %>%
    dplyr::group_by(domainID,siteID,plotID) %>%
    dplyr::summarise(TBA=sum(BA))#Calculate total basal area of trees per plot

  PlotEBA <- MycoTree %>%
    dplyr::filter(MycorrhizalType == "EcM") %>%
    dplyr::group_by(plotID) %>%
    dplyr::summarise(EBA=sum(BA))
  
  MycoProp <- PlotTBA %>% left_join(PlotEBA, by="plotID")
  MycoProp <- MycoProp %>% replace_na(list(EBA = 0))
  MycoProp <- MycoProp %>%
    dplyr::mutate(MP = EBA/TBA)

  MycoProp <- MycoProp %>%
    mutate(MycoGroup = ifelse(MP <= 0.15, "AM", ifelse(MP >= 0.85, "EM", "Mix")))
  
  MycoProp$MycoGroup <- factor(MycoProp$MycoGroup,
                               levels = c('AM','Mix','EM'),ordered = FALSE)
  unlink(gsub(" ","",paste(wd,"/filesToStack10098/")), recursive = TRUE)
  return(MycoProp)
}



