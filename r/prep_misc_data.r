
# libraries and functions ---------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "functions.r"))


# constants ---------------------------------------------------------------
npfn <- "NamedParties_2022-03-16.csv"
propfn <- "Proposals_2022-03-16.csv"
pcdfn <- "Purchasing_Commodity_Data_2022-03-17.csv"


# ONETIME download proposals ----
# https://data.sfgov.org/City-Management-and-Ethics/SFEC-Form-126f2-Notification-of-Submission-of-Prop/pv99-gzft
# Download: https://data.sfgov.org/api/views/pv99-gzft/rows.csv?accessType=DOWNLOAD
# propurl <- "https://data.sfgov.org/api/views/pv99-gzft/rows.csv?accessType=DOWNLOAD"
# download.file(propurl, here::here("data", propfn), mode="wb")


# ONETIME download named parties ----
# https://data.sfgov.org/City-Management-and-Ethics/SFEC-Form-126f2-Named-Parties/djj2-gvaq
# Download: https://data.sfgov.org/api/views/djj2-gvaq/rows.csv?accessType=DOWNLOAD
# npurl <- "https://data.sfgov.org/api/views/djj2-gvaq/rows.csv?accessType=DOWNLOAD"
# download.file(npurl, here::here("data", npfn), mode="wb")


# ONETIME download purchasing commodity data ----
# https://data.sfgov.org/City-Management-and-Ethics/Purchasing-Commodity-Data/ebsh-uavg
# pcdurl <- "https://data.sfgov.org/api/views/ebsh-uavg/rows.csv?accessType=DOWNLOAD"
# download.file(pcdurl, here::here("data", pcdfn), mode="wb")


# proposals ---------------------------------------------------------------

df <- vroom(here::here("data", propfn),
            col_types = cols(.default = col_character()))
glimpse(df)
names(df)
cnames <- c("docsignid", "puburl", "filingtype", "cofdate", "amend",
            "agname", "agcontact", "agtelno", "agemail", "rfpno",
            "fileno", "nature", "comments", "addlnames", "signer",
            "stitle", "csdate")
df2 <- df %>%
  setNames(cnames) %>%
  mutate(ofdate=cleandate(cofdate),
         sdate=cleandate(csdate))
glimpse(df2)
count(df2, cofdate, ofdate)
count(df2, csdate, sdate)
summary(df2)
df3 <- df2 %>%
  select(-c(cofdate, csdate))
saveRDS(df3, here::here("data", "proposals.rds"))
summary(df3)

# Possible proposals linking variables:
#  proposals: docsignid, agname

#  [1] "DocuSign ID"                   "Public URL"                    "Filing Type"                  
#  [4] "Original Filing Date"          "Amendment Description"         "City Agency Name"             
#  [7] "City Agency Contact Name"      "City Agency Contact Telephone" "City Agency Contact Email"    
# [10] "Bid Rfp Number"                "File Number"                   "Nature of Contract"           
# [13] "Comments"                      "Additional Names Required"     "Signer Name"                  
# [16] "Signer Title"                  "Date Signed"    



# nparties ----------------------------------------------------------------

df <- vroom(here::here("data", npfn),
            col_types = cols(.default = col_character()))
names(df)
glimpse(df)

df2 <- df %>%
  setNames(c("docsignid", "contractor", "contype", "cpropdate")) %>%
  mutate(propdate=cleandate(cpropdate))
summary(df2)

df3 <- df2 %>%
  select(docsignid, contractor, contype, propdate)
summary(df3)
saveRDS(df3, here::here("data", "nparties.rds"))

# Possible linking variables:
#  nparties: docsignid, contractor, contype, propdate
# [1] "DocuSign ID"     "Contractor Name" "Contractor Type" "Proposal Date"  


# commodity purchases -----------------------------------------------------

df <- vroom(here::here("data", pcdfn),
            col_types = cols(.default = col_character()))
# force reading of all data as character, then convert
glimpse(df)
names(df)
cnames <- c("fyear", "cpodate", "purchorder", "poline", "cnum",
            "ctitle", "purchdept", "purchdeptname", "cpostdateorig", "cpostdatecurr",
            "commcode", "commtitle", "supplier", "suppstreet", "suppcity",
            "suppstate", "suppzip", "suppcontact", "suppemail", "suppphone",
            "encumbq", "encumbered")
# possible linking variables: 
#   commodities: fyear, purchorder, cnum, ctitle
# conversions:
# integer: fyear
# date: podate
df2 <- df %>%
  setNames(cnames) %>%
  mutate(fyear=as.integer(fyear),
         podate=cleandate(cpodate), 
         postdateorig=cleandate(cpostdateorig), 
         postdatecurr=cleandate(cpostdatecurr),
         across(c(encumbq, encumbered), as.numeric))
summary(df2)
glimpse(df2)
count(df2, cpodate, podate)
count(df2, cpostdateorig, postdateorig)
count(df2, cpostdatecurr, postdatecurr)
df2 %>% filter(is.na(podate)) %>% pull(cpodate) %>% unique
df2 %>% filter(is.na(postdateorig)) %>% pull(cpostdateorig) %>% unique
df2 %>% filter(is.na(postdatecurr)) %>% pull(cpostdatecurr) %>% unique
df3 <- df2 %>%
  select(-cpodate, -cpostdateorig, -cpostdatecurr)

saveRDS(df3, here::here("data", "commodities.rds"))
# possible linking variables:
#  commodities: fyear, cnum, ctitle, purchdept, purchdeptname, supplier


#  [1] "Fiscal Year"                          "Purchase Order Date"                 
#  [3] "Purchase Order"                       "Purchase Order Line"                 
#  [5] "Contract Number"                      "Contract Title"                      
#  [7] "Purchasing Department"                "Purchasing Department Title"         
#  [9] "Post Date - Original"                 "Post Date - Current"                 
# [11] "Commodity Code"                       "Commodity Title"                     
# [13] "Supplier & Other Non-Supplier Payees" "Supplier Street"                     
# [15] "Supplier City"                        "Supplier State"                      
# [17] "Supplier ZIP Code"                    "Supplier Contact"                    
# [19] "Supplier Email"                       "Supplier Phone"                      
# [21] "Encumbered Quantity"                  "Encumbered Amount"  


# # San Francisco Department information ------------------------------------
# df <- read_excel(here::here("report", "inputs", "san_francisco_inputs.xlsx"),
#                  sheet="departments",
#                  range="A2:G22")
# saveRDS(df, here::here("report", "data", "deptinfo.rds"))



