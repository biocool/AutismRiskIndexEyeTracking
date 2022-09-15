################################################################################
#Description=======
#This function puts all columns that are not "fixed.characteristics" in a single column and adds
#a new column to show each mesurement values belong to which measurement type.
#(for example "diagnosis" is a fixed.characteristics or any 
#other measures that we are not going do startify subjects based on those and are note being used in "facet")
################################################################################
#INPUT====
#"df" a data frame (the first col is SubjectID), 
#"no.of.fixed.characteristics" is the number of cols that show the clinical characteristics
#which we want to repeat them for all Mesearements (e.g. Sex, Dxj, ....). By default these fixed.characteristics MUST be the most right cols
################################################################################
#OUTPUT====
#"ggplot.friendly.df": is the converted version of the imput which is very friendly for ggplot.
################################################################################
#function definition========
convert.df.2.ggplot.friendly <- function(df, no.of.fixed.characteristics)
{
  library(dplyr)
  no.of.cols <- 
    ncol(df)
  #the final df would be 2-column df
  #the first column shows that subjectIDs and the second one shows the measurement (e.g. clinical metrics or eye tracking feature etc.)
  ggplot.friendly.df <- 
    as.data.frame(matrix(ncol = 3 + no.of.fixed.characteristics, nrow = 1))
  #the "fixed.characteristics.col.name" vctr shows the colnames for those "fixed.characteristics"
  if (no.of.fixed.characteristics != 0 ) 
  {
    fixed.characteristics.col.name  <- 
      colnames(df)[c((no.of.cols - no.of.fixed.characteristics + 1):no.of.cols)]
    colnames(ggplot.friendly.df) <- 
      c("SubjectID", fixed.characteristics.col.name, "MesearementValue", "Mesearement")
  }else
  {
    fixed.characteristics.col.name  <- NA
    colnames(ggplot.friendly.df) <- 
      c("SubjectID", "MesearementValue", "Mesearement")
  }
  
  no.of.measurements <- 
    no.of.cols - no.of.fixed.characteristics - 1 # -1==> SubjectID 
  measuresment.name.vctr <-
    colnames(df)[c(2:(no.of.measurements+1))]
  for (j in c(2:(no.of.measurements+1))) 
  {
    #selecting the related cols
    if (no.of.fixed.characteristics != 0 ) 
    {
      df.correspond.to.current.measurement <-
        df[,c(1,c((no.of.cols - no.of.fixed.characteristics + 1):no.of.cols),j)]
    }else
    {
      df.correspond.to.current.measurement <-
        df[,c(1,j)]
    }
    
    #adding measurement name as last column
    df.correspond.to.current.measurement$Mesearement <-
      rep(measuresment.name.vctr[j-1], 
          nrow(df.correspond.to.current.measurement))
    colnames(df.correspond.to.current.measurement) <- 
      colnames(ggplot.friendly.df)
    ggplot.friendly.df <- 
      rbind(ggplot.friendly.df , df.correspond.to.current.measurement)
  }
  
  #remoding the first NA row 
  ggplot.friendly.df <-
    ggplot.friendly.df[-1,]
  return(ggplot.friendly.df)
}



