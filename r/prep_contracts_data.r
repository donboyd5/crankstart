
# There are two sources of contracts data


# libraries and functions ---------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "functions.r"))


# constants ---------------------------------------------------------------

sconfn <- "SupplierContracts_2022-03-14.csv"
rfncon <- "Supplier Contracts_From_Report_2022-03-21.csv"


# ONETIME download and save supplier contracts ----
# https://data.sfgov.org/City-Management-and-Ethics/Supplier-Contracts/cqi5-hm2d
# scurl <- "https://data.sfgov.org/api/views/cqi5-hm2d/rows.csv?accessType=DOWNLOAD"
# download.file(scurl, here::here("data", sconfn), mode="wb")


# get opendata contracts csv file ---------------------------------------------------
df <- read_csv(here::here("data", scfn),
               col_types = cols(.default = col_character()))
glimpse(df)
summary(df)
names(df)
cnames <- c("cnum", "ctitle", "cstartdate", "cenddate", "ctype",
            "purchauth", "solesource", "deptcode", "deptname", "prime",
            "supplier", "lbe", "nonprofit", "suptype", "scope",
            "amt_award", "amt_po", "amt_paid", "amt_left")

df2 <- df %>%
  setNames(cnames) %>%
  mutate(startdate=cleandate(cstartdate),
         enddate=cleandate(cenddate),
         across(starts_with("amt"), as.numeric))
count(df2, cenddate, enddate)
summary(df2)
df2 %>% filter(is.na(startdate)) %>% select(cstartdate)
df2 %>% filter(is.na(enddate)) %>% select(cenddate) %>% unique

df3 <- df2 %>%
  select(-c(cstartdate, cenddate))
saveRDS(df3, here::here("data", "contracts.rds"))

# possible linking variables:
# contracts: cnum, ctitle, ctype, deptcode, deptname, prime, supplier

#  [1] "Contract Number"                  "Contract Title"                   "Term Start Date"                 
#  [4] "Term End Date"                    "Contract Type"                    "Purchasing Authority"            
#  [7] "Sole Source"                      "Department Code"                  "Department"                      
# [10] "Supplier Name (Prime Contractor)" "Supplier Name"                    "Supplier LBE Status"             
# [13] "Non-Profit"                       "Supplier Type"                    "Scope of Work"                   
# [16] "Contract Awarded Amount"          "Purchase Orders Outstanding"      "Payments Made"                   
# [19] "Remaining Contract Award Amount" 



# report-based contracts data ---------------------------------------------

dfcon1 <- vroom(here::here("data", rfncon),
                col_types = cols(.default = col_character()))
glimpse(dfcon1)
summary(dfcon1)
count(dfcon1, Textbox1745)
names(dfcon1)

# downloadable csv file:
# cnames <- c("cnum", "ctitle", "cstartdate", "cenddate", "ctype",
#             "purchauth", "solesource", "deptcode", "deptname", "prime",
#             "supplier", "lbe", "nonprofit", "suptype", "scope",
#             "amt_award", "amt_po", "amt_paid", "amt_left")

# csv from report
cnames <- c("cnum", "ctitle", "cstartdate", "cenddate", "ctype",
            "purchauth", "solesource", "deptname","concur", "supplier",
            "lbe", "nonprofit", "suptype", "scope", "line",
            "prodcat", "covidcode", "amt_award", "amt_po", "amt_paid",
            "amt_left", "txt1745", "sub_award", "sub_po", "sub_paid",
            "sub_left", "tot_award", "tot_po", "tot_paid", "tot_left")

# check parsing
# x <- c("123", "123.45", "$123", "$123,456,789.123", "$  123,456,789.12")
# parse_number(x)

dfcon2 <- dfcon1 %>%
  setNames(cnames) %>%
  select(cnum:amt_left) %>%
  mutate(startdate=cleandate(cstartdate),
         enddate=cleandate(cenddate),
         across(starts_with("amt_"), parse_number))
summary(dfcon2)

dfcon2 %>%
  filter(is.na(enddate)) %>%
  count(startdate, cenddate, sort=TRUE)

count(dfcon2, ctype, sort=TRUE)
count(dfcon2, purchauth, sort=TRUE)
count(dfcon2, solesource, sort=TRUE)
count(dfcon2, deptname, sort=TRUE)
count(dfcon2, concur, sort=TRUE)
count(dfcon2, supplier, sort=TRUE)
count(dfcon2, lbe, sort=TRUE)
count(dfcon2, nonprofit, sort=TRUE)
count(dfcon2, suptype, sort=TRUE)
count(dfcon2, scope, sort=TRUE)
count(dfcon2, line, sort=TRUE)  # all values are NA
count(dfcon2, prodcat, sort=TRUE) # all NA
count(dfcon2, covidcode, sort=TRUE) # all NA
count(dfcon2, startdate, sort=TRUE)
count(dfcon2, enddate, sort=TRUE)

rcontracts <- dfcon2 %>%
  select(-c(line, prodcat, covidcode, cstartdate, cenddate))
summary(rcontracts)

rcontracts %>%
  filter(amt_award >= 1e9)

saveRDS(rcontracts, here::here("data", "rcontracts.rds"))

# [1] "Contract"                             "Contract_Title"                      
#  [3] "Term_Start_Date"                      "Term_End_Date"                       
#  [5] "Contract_Type"                        "Purchasing_Authority"                
#  [7] "Sole_Source"                          "Department"                          
#  [9] "CON_Concurrence"                      "Supplier_Name"                       
# [11] "Supplier_LBE_Status"                  "Non_Profit"                          
# [13] "Supplier_Type"                        "Supplier_Scope_of_Work"              
# [15] "Line_Number"                          "Prod_Cat"                            
# [17] "COVID_Related_Project_Code"           "Contract_Award_Amount"               
# [19] "Purchase_Orders_Outstanding"          "Payments_Made"                       
# [21] "Remaining_Contract_Award_Amount"      "Textbox1745"                         
# [23] "Contract_Award_Amount_Subtotal"       "Purchase_Orders_Outstanding_Subtotal"
# [25] "Payments_Made_Subtotal"               "Remaining_Contract_Amount_Subtotal"  
# [27] "Contract_Award_Amount_Total"          "Purchase_Orders_Outstanding_Total"   
# [29] "Payments_Made_Total"                  "Remaining_Contract_Amount_Total"   

