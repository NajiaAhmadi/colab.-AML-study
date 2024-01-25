library(readr)
library(readxl)
library(dplyr)


file_path <- "/Users/ahmadinai/Documents/GitHub/colab.\ AML\ study/Data/20240112_sal_ohsu_data.xlsx"

aml_df = read_excel(file_path, na = "NA")

# calculate Overall survival (OSTM) after two years in binary form using the 
# death event occurrence (OSSTAT)

calculate_OS2years <- function(OSTM, OSSTAT) {
  if (OSTM > 24) {
    return(1)
  } else if (OSTM < 24 && OSSTAT == 1) {
    return(0)
  } else {
    return(NA)
  }
}


aml_df <- aml_df %>% mutate(OS2Years = mapply(calculate_OS2years, OSTM, OSSTAT))

# OSTM < 2 years, OSSTAT = 0 -> patient has been censored within the first two years,
# we do not know whether the patient has survived up until the 2 year mark 
# -> we have to exclude these patients

aml_df_OS2years<- aml_df %>% 
  filter(!is.na(OS2Years)) %>% 
  select(-c(AGEGRP, "Sex male", "sex female", CGKT)) 

#names(aml_df)


write.csv(aml_df_OS2years, "/Users/ahmadinai/Documents/GitHub/colab.\ AML\ study/AML\ study/20240116_sal_ohsu_data_OS2.csv", row.names = TRUE)
