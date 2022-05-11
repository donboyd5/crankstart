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
  mutate(longname=ifelse(is.na(longname), trimname, longname))

saveRDS(deptsort, here::here("report", "data", "deptsort.rds"))


# npesort -----------------------------------------------------------------
## payments summarized by nonprofit ----
npesort <- paybase %>%
  filter(grant=="grant") %>%
  group_by(supplier, grant) %>%
  summarise(paid=sum(paid, na.rm=TRUE), .groups="drop") %>%
  arrange(desc(paid)) %>%
  mutate(rank=row_number(),
         paidpct=paid / sum(paid, na.rm=TRUE),
         paidcumpct=cumsum(paidpct))

saveRDS(npesort, here::here("report", "data", "npesort.rds"))


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


# npedept_wide ------------------------------------------------------------

npedeptsort <- readRDS(here::here("report", "data", "npedeptsort.rds"))

# add the totals
deptsums <- npedeptsort %>%
  group_by(longname) %>%
  summarise(paid=sum(paid, na.rm=TRUE), .groups="drop") %>%
  mutate(nperank=2e9, supplier="Total")

npesums <- npedeptsort %>%
  group_by(nperank, supplier) %>%
  summarise(paid=sum(paid, na.rm=TRUE), .groups="drop") %>%
  mutate(deptrank=1e9, longname="Total")

allsums <- deptsums %>%
  summarise(paid=sum(paid, na.rm=TRUE), .groups="drop") %>%
  mutate(nperank=2e9, supplier="Total", longname="Total")

npedept_long <- bind_rows(npedeptsort, deptsums, npesums, allsums) %>%
  select(nperank, supplier, deptrank, longname, paid) %>%
  arrange(deptrank, nperank)

npedept_wide <- npedept_long %>%
  select(nperank, supplier, longname, paid) %>%
  pivot_wider(names_from = longname, values_from = paid) %>%
  relocate(Total, .after = last_col()) %>%
  arrange(nperank) %>%
  mutate(nperank=ifelse(nperank==2e9, NA_real_, nperank))

saveRDS(npedept_wide, here::here("report", "data", "npedept_wide.rds"))


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


# end ---------------------------------------------------------------------

print("done with pre-render actions...")