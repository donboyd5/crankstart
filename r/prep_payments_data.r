
# libraries and functions ---------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "functions.r"))


# constants ---------------------------------------------------------------
vpfn <- "VendorPayments_2022-03-15.csv"
vppofn <- "VendorPaymentsPO_2022-03-16.csv"
rfnpay <- "Supplier Payments_From_Report_2022-03-21.csv"


# links -------------------------------------------------------------------

# https://openbook-report.sfgov.org/OBMiddleware/report.aspx?reportname=5



# ONETIME download vendor payments ----

# https://data.sfgov.org/City-Management-and-Ethics/Vendor-Payments-Vouchers-/n9pm-xkyq
vpurl <- "https://data.sfgov.org/api/views/n9pm-xkyq/rows.csv?accessType=DOWNLOAD"
download.file(vpurl, here::here("data", vpfn), mode="wb")


# ONETIME download vendor payments purchase orders ----
# https://data.sfgov.org/City-Management-and-Ethics/Vendor-Payments-Purchase-Order-Summary-/p5r5-fd7g
vppourl <- "https://data.sfgov.org/api/views/p5r5-fd7g/rows.csv?accessType=DOWNLOAD"
download.file(vppourl, here::here("data", vppofn), mode="wb")


# vendor payments ---------------------------------------------------------

df <- vroom(here::here("data", vpfn),
            col_types = cols(.default = col_character()))
glimpse(df)
names(df)
cnames <- c("fyear", "relgov", "orgcode", "orggroup", "deptcode",
            "deptname", "progcode", "progname", "charcode", "charname",
            "objcode", "objname", "subobjcode", "subobjname", "ftypecode",
            "ftypename", "fundcode", "fundname", "fcatcode", "fcatname",
            "purchorder", "supplier", "vpaid", "vpend", "vpendretain",
            "voucher")
df2 <- df %>%
  setNames(cnames) %>%
  mutate(fyear=as.integer(fyear),
         relgov=str_to_upper(relgov),
         across(c(vpaid, vpend, vpendretain), as.numeric))
summary(df2)
glimpse(ht(df2))
# note: vpendretain appears to have virtually no useful information

saveRDS(df2, here::here("data", "vpayments.rds"))
# possible linking variables:
#  vpayments: fyear, deptcode, deptname, purchorder, supplier, voucher


count(df2, relgov)
count(df2, orgcode, orggroup)
#   orgcode orggroup                                       n
#   <chr>   <chr>                                      <int>
# 1 01      Public Protection                         508823
# 2 02      Public Works, Transportation & Commerce  1694487
# 3 03      Human Welfare & Neighborhood Development  339351
# 4 04      Community Health                         2153216
# 5 05      Culture & Recreation                      369173
# 6 06      General Administration & Finance          839166
# 7 07      General City Responsibilities              85478
count(df2, deptcode, deptname)
# CHF      CHF Children;Youth & Families    67053
# potential linking variables:
#   supplier
#   deptcode, deptname
#   voucher
count(df2, charcode, charname)
# of interest: 
#   538                  City Grant Programs             191479
#   539                  Other Support&Care Of Persons     4276
#   AID_ASSIST           Aid Assistance                   26387
#   AID_PMTS             Aid Payments                       455
#   CHGS_FOR_SERVICES    Charges for Services             19541
#   CITY_GR_PROG         City Grant Program               99725
#   FID_BEN_PMT          Fiduciary-Benefit Payments        8819
#   OTH_SUP_CARE_PERS    Other Support/Care of Persons     2386


#  [1] "Fiscal Year"                          "Related Govt Units"                   "Organization Group Code"             
#  [4] "Organization Group"                   "Department Code"                      "Department"                          
#  [7] "Program Code"                         "Program"                              "Character Code"                      
# [10] "Character"                            "Object Code"                          "Object"                              
# [13] "Sub-object Code"                      "Sub-object"                           "Fund Type Code"                      
# [16] "Fund Type"                            "Fund Code"                            "Fund"                                
# [19] "Fund Category Code"                   "Fund Category"                        "Purchase Order"                      
# [22] "Supplier & Other Non-Supplier Payees" "Vouchers Paid"                        "Vouchers Pending"                    
# [25] "Vouchers Pending Retainage"           "Voucher"           


# vendor payments purchase orders -----------------------------------------

df <- vroom(here::here("data", vppofn),
            col_types = cols(.default = col_character()))
names(df)
cnames <- c("fyear", "relgov", "orgcode", "orggroup", "deptcode",
            "deptname", "progcode", "progname", "charcode", "charname",
            "objcode", "objname", "subobjcode", "subobjname", "ftypecode",
            "ftypename", "fundcode", "fundname", "fcatcode", "fcatname",
            "purchorder", "supplier", "vpaid", "vpend", "encumbal",
            "vpendretain")
df2 <- df %>%
  setNames(cnames) %>%
  mutate(fyear=as.integer(fyear),
         across(c(vpaid, vpend, encumbal, vpendretain), as.numeric))
summary(df2)

saveRDS(df2, here::here("data", "vpaypo.rds"))
# possible linking variables
#   vpaypo: fyear, deptcode, deptname, purchorder, supplier

#  [1] "Fiscal Year"                          "Related Govt Units"                  
#  [3] "Organization Group Code"              "Organization Group"                  
#  [5] "Department Code"                      "Department"                          
#  [7] "Program Code"                         "Program"                             
#  [9] "Character Code"                       "Character"                           
# [11] "Object Code"                          "Object"                              
# [13] "Sub-object Code"                      "Sub-object"                          
# [15] "Fund Type Code"                       "Fund Type"                           
# [17] "Fund Code"                            "Fund"                                
# [19] "Fund Category Code"                   "Fund Category"                       
# [21] "Purchase Order"                       "Supplier & Other Non-Supplier Payees"
# [23] "Vouchers Paid"                        "Vouchers Pending"                    
# [25] "Encumbrance Balance"                  "Vouchers Pending Retainage"          



# report-based payments data ----------------------------------------------

dfpay1 <- vroom(here::here("data", rfnpay),
                col_types = cols(.default = col_character()))
glimpse(dfpay1)
ns(dfpay1)
summary(dfpay1)
names(dfpay1)

# get some counts before deciding what to do
# Textbox29, 36, 37, 39, 40, 74:81
# 29 36 37 39 40 each has a single fiscal year FY 2017-2018..FY 2021-2022
# 74:81 each has a single number
# Completed_Balance_4 3, 2, 1 and Completed_Balance
count(dfpay1, Completed_Balance_4) %>% ht
# looks like we should parse these as accounting numbers but they are a bit odd
"Encumbrance_Document"                   
# [15] "Voucher_Document"                        "Payment_Date"   
count(dfpay1, Encumbrance_Document) # drop it
count(dfpay1, Voucher_Document) # drop
count(dfpay1, Payment_Date) # drop
count(dfpay1, Queued_for_Approval_Routing_and_Payment) # parse as accounting
count(dfpay1, Contract_Retainage) # parse as accounting
count(dfpay1, Remaining_Balance) # parse as accounting
# report does not have voucher or encumbrance documents; data csv DOES
# data does not have contract num, title but report data DOES

dfpay2 <- dfpay1 %>%
  select(-starts_with("Textbox"),
         -c(Encumbrance_Document, Voucher_Document, Payment_Date))
names(dfpay2)

cnames <- c("supplier", "nonprofit", "deptname", "type", "prodcat",
            "cnum", "ctitle", "solesource", 
            "compbal4", "compbal3", "compbal2", "compbal1", "compbal0",
            "queued", "retain", "amt_left")


dfpay3 <- dfpay2 %>%
  setNames(cnames) %>%
  mutate(deptcode=str_sub(deptname, 1, 3)) %>%
  select(deptcode, deptname, cnum, ctitle, supplier, nonprofit, solesource, everything())

dfpay4 <- dfpay3 %>%
  mutate(across(c(starts_with("compbal"),
                  queued, retain, amt_left),
                parse_acctg))
summary(dfpay4)

# map the compbal values to years
compbals <- paste0("compbal", 4:0)
compyears <- 2018:2022
cbind(compbals, compyears)

dfpay5 <- dfpay4 %>%
  pivot_longer(cols=starts_with("compbal"),
               names_to = "comp", values_to = "paid") %>%
  mutate(year=factor(comp,
                     levels=compbals, 
                     labels=compyears),
         year= as.integer(levels(year))[year]) 
count(dfpay5, comp, year)

rvpayments <- dfpay5 %>%
  select(-comp) %>%
  arrange(deptcode, cnum, year)

saveRDS(rvpayments, here::here("data", "rvpayments.rds"))

rvpayments <- readRDS(here::here("data", "rvpayments.rds"))

rvpayments %>%
  filter(str_detect(supplier, "VALENCIA"), str_sub(deptname, 1, 3)=="CHF") %>%
  write_csv(here::here("analysis", "temp.csv"))



#  [1] "Textbox29"                               "Textbox36"                              
#  [3] "Textbox37"                               "Textbox39"                              
#  [5] "Textbox40"                               "Supplier_Name"                          
#  [7] "Non_Profit"                              "Department"                             
#  [9] "Type_of_Goods_and_Services"              "Product_Category"                       
# [11] "Contract_Number"                         "Contract_Title"                         
# [13] "Sole_Source_Flag"                        "Encumbrance_Document"                   
# [15] "Voucher_Document"                        "Payment_Date"                           
# [17] "Completed_Balance_4"                     "Completed_Balance_3"                    
# [19] "Completed_Balance_2"                     "Completed_Balance_1"                    
# [21] "Completed_Balance"                       "Queued_for_Approval_Routing_and_Payment"
# [23] "Contract_Retainage"                      "Remaining_Balance"                      
# [25] "Textbox74"                               "Textbox75"                              
# [27] "Textbox76"                               "Textbox77"                              
# [29] "Textbox78"                               "Textbox79"                              
# [31] "Textbox80"                               "Textbox81"  

# downloadable csv file:
# cnames <- c("fyear", "relgov", "orgcode", "orggroup", "deptcode",
#             "deptname", "progcode", "progname", "charcode", "charname",
#             "objcode", "objname", "subobjcode", "subobjname", "ftypecode",
#             "ftypename", "fundcode", "fundname", "fcatcode", "fcatname",
#             "purchorder", "supplier", "vpaid", "vpend", "vpendretain",
#             "voucher") 




