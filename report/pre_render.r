# subtitle: "Prepared for Crankstart Management, LLC"

print("starting pre-render actions")

# libraries and functions ---------------------------------------------------------------
source(here::here("r", "libraries.r"))
source(here::here("r", "functions.r"))

# get data ----------------------------------------------------------------
rcontracts <- readRDS(here::here("data", "rcontracts.rds"))
rvpayments <- readRDS(here::here("data", "rvpayments.rds"))

# # San Francisco Department information ------------------------------------

deptinfo <- read_excel(here::here("report", "inputs", "san_francisco_inputs.xlsx"),
                       sheet="departments",
                       range="A2:G22")

# conbase -----------------------------------------------------------------
# identify contracts of interest:
#   - nonprofits
#   - contract started no later than end of the fiscal year
#   -   and ended no earlier than start of fiscal year
#   - grant-related: purchasing authority or grant
# glimpse(rcontracts)

conbase <- rcontracts %>%
  filter(nonprofit=="X",
         startdate <= "2021-06-30",
         enddate >= "2020-07-01") %>%
  mutate(grant=ifelse(
    str_detect(ctype, coll("GRANT", ignore_case = TRUE)),
    "grant",
    "nongrant"),
    trimname=str_sub(5)) %>%
  select(-scope) # scope is not useful


# paybase -----------------------------------------------------------------
## individual payments to nonprofits in 2021 ----
paybase <- rvpayments  %>% 
  filter(year==2021, nonprofit=="X", paid!=0) %>%
  left_join(conbase %>%
              select(cnum, supplier, lbe, purchauth, grant, amt_award) %>%
              mutate(incontracts=TRUE),
            by=c("cnum", "supplier")) %>%
  mutate(grant=ifelse(is.na(incontracts), "nocontract", grant),
         trimname=str_sub(deptname, 5, -1)) %>%
  select(-incontracts)

# saveRDS(paybase, here::here("report", "data", "paybase.rds"))


# npedeptsort -------------------------------------------------------------
## payments summarized by nonprofit x department ----
# get dept and npe ranks
npedeptsort <- paybase %>%
  filter(grant=="grant") %>%
  group_by(deptcode, trimname, supplier, grant) %>%
  summarise(paid=sum(paid, na.rm=TRUE), .groups="drop") %>%
  left_join(deptsort %>%
              select(deptcode, deptrank=rank, longname), by = "deptcode") %>%
  left_join(npesort %>%
              select(supplier, nperank=rank), by = "supplier")

saveRDS(npedeptsort, here::here("report", "data", "npedeptsort.rds"))


# npedept_long ------------------------------------------------------------
npedeptsort <- readRDS(here::here("report", "data", "npedeptsort.rds"))
# summary(npedeptsort)

# add the totals
deptsums <- npedeptsort %>%
  group_by(deptrank, longname) %>%
  summarise(paid=sum(paid, na.rm=TRUE), .groups="drop") %>%
  mutate(nperank=1e9, supplier="Total")

npesums <- npedeptsort %>%
  group_by(nperank, supplier) %>%
  summarise(paid=sum(paid, na.rm=TRUE), .groups="drop") %>%
  mutate(deptrank=1e9, longname="Total")

allsums <- deptsums %>%
  summarise(paid=sum(paid, na.rm=TRUE), .groups="drop") %>%
  mutate(nperank=1e9, supplier="Total", deptrank=1e9, longname="Total")

npedept_long <- bind_rows(npedeptsort, deptsums, npesums, allsums) %>%
  select(nperank, supplier, deptrank, longname, paid) %>%
  arrange(deptrank, nperank)
# summary(npedept_long)

saveRDS(npedept_long, here::here("report", "data", "npedept_long.rds"))


# npestats and deptstats ----
# for each npe get: # depts, largest dept, and largest as % of total
npestats <- npedept_long |> 
  group_by(nperank, supplier) |> 
  summarise(nna=sum(paid[longname!="Total"] > 0),
            maxdept=max(paid[longname!="Total"]),
            maxdeptpct=maxdept / paid[longname=="Total"],
            .groups="drop")
saveRDS(npestats, here::here("report", "data", "npestats.rds"))

# for each dept get: # npes, largest npe, and largest as % of total
deptstats <- npedept_long |> 
  group_by(deptrank, longname) |> 
  summarise(nna=sum(paid[supplier!="Total"] > 0),
            maxnpe=max(paid[supplier!="Total"]),
            maxnpepct=maxnpe / paid[supplier=="Total"],
            .groups="drop")
saveRDS(deptstats, here::here("report", "data", "deptstats.rds"))


# npedept_wide ------------------------------------------------------------

# npedept_long |> 
#   filter(str_detect(longname, "Police")) 

# now construct a wide table with the values we want
# first, the paid dollars
npedept_wide <- npedept_long %>%
  select(nperank, supplier, longname, paid) %>%
  pivot_wider(names_from = longname, values_from = paid) %>%
  relocate(Total, .after = last_col()) %>%
  arrange(nperank) %>%
  mutate(nperank=ifelse(nperank==1e9, NA_real_, nperank))

saveRDS(npedept_wide, here::here("report", "data", "npedept_wide.rds"))


# deptsort ----------------------------------------------------------------
## payments summarized by department ----
# count(paybase, type, sort = TRUE)
# count(paybase, prodcat, sort = TRUE)

deptsort1 <- paybase %>%
  filter(grant=="grant") %>%
  group_by(deptcode, trimname, grant) %>%
  summarise(paid=sum(paid, na.rm=TRUE), .groups="drop") %>%
  arrange(desc(paid)) %>%
  mutate(rank=row_number(),
         paidpct=paid / sum(paid, na.rm=TRUE),
         paidcumpct=cumsum(paidpct))

deptsort <- deptsort1 |> 
  left_join(deptinfo, by=c("deptcode")) |> 
  mutate(longname=ifelse(is.na(longname), trimname, longname)) |> 
  left_join(deptstats, by="longname")

saveRDS(deptsort, here::here("report", "data", "deptsort.rds"))


# npesort -----------------------------------------------------------------
## payments summarized by nonprofit ----
npesort1 <- paybase %>%
  filter(grant=="grant") %>%
  group_by(supplier, grant) %>%
  summarise(paid=sum(paid, na.rm=TRUE), .groups="drop") %>%
  arrange(desc(paid)) %>%
  mutate(rank=row_number(),
         paidpct=paid / sum(paid, na.rm=TRUE),
         paidcumpct=cumsum(paidpct))

npesort <- npesort1 |> 
  left_join(npestats |> select(-nperank), by="supplier")
  

saveRDS(npesort, here::here("report", "data", "npesort.rds"))


# dcyf wide ---------------------------------------------------------------

dcyf_funding <- readRDS(here::here("data", "dcyf_funding.rds"))

dcyf_wide1 <- dcyf_funding %>%
  pivot_wider(names_from = planfy, values_from = funding)

dcyf_sums <- dcyf_wide1 %>%
  summarise(`2020-21`=sum(`2020-21`, na.rm=TRUE),
            `2021-22`=sum(`2021-22`, na.rm=TRUE)) %>%
  mutate(agency="Total")

dcyf_wide <- bind_rows(dcyf_wide1, dcyf_sums) %>%
  mutate(id=row_number()) %>%
  select(id, everything())

saveRDS(dcyf_wide, here::here("report", "data", "dcyf_wide.rds"))


# Homelessness Services over time ------------------------------------------------------

homeless <- rvpayments  %>% 
  filter(deptcode=="HOM", nonprofit=="X", paid!=0) %>%
  left_join(conbase %>%
              select(cnum, supplier, lbe, purchauth, grant, amt_award) %>%
              mutate(incontracts=TRUE),
            by=c("cnum", "supplier")) %>%
  mutate(grant=ifelse(is.na(incontracts), "nocontract", grant),
         trimname=str_sub(deptname, 5, -1)) %>%
  filter(grant=="grant") %>%
  select(-incontracts)
count(homeless, supplier)

saveRDS(homeless, here::here("report", "data", "homeless.rds"))


# end ---------------------------------------------------------------------

print("done with pre-render actions...")


# # add a largest-npe row
# npe_row <- deptstats |> 
#   pivot_longer(cols = c(nna, maxnpe, maxnpepct)) |> 
#   select(-deptrank) |> 
#   pivot_wider(names_from = longname) |> 
#   rename(supplier=name)
# 
# npedept_wide_paid2 <- bind_rows(npedept_wide_paid, npe_row)
# 
# npedept_wide1 <- npedept_long %>%
#   select(nperank, supplier, longname, paid) %>%
#   pivot_wider(names_from = longname, values_from = paid) %>%
#   relocate(Total, .after = last_col()) %>%
#   arrange(nperank) %>%
#   mutate(nperank=ifelse(nperank==1e9, NA_real_, nperank))
# 
# # calculate largest dept as % of total, for each nonprofit
# fmax <- function(df, cols){
#   m <- df |> select(all_of(cols)) |> as.matrix()
#   m[is.na(m)] <- 0
#   imaxcol <- max.col(m)
#   imaxvals <- cbind(1:nrow(m), imaxcol)
#   maxvals <- m[imaxvals]
#   maxvals
# }
# fmax(npedept_wide1, 3:5)
# 
# npedept_wide <- npedept_wide1 |> 
#   mutate(maxdept = fmax(npedept_wide, 3:(ncol(npedept_wide1) - 1)),
#          maxpct=maxdept / Total) %>%
#   rename(`Largest payment`=maxdept,
#          `Largest as % of Total`=maxpct)