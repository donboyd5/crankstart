
proposals <- readRDS(here::here("data", "proposals.rds"))
nparties <- readRDS(here::here("data", "nparties.rds"))
contracts <- readRDS(here::here("data", "contracts.rds"))
vpayments <- readRDS(here::here("data", "vpayments.rds"))
vpaypo <- readRDS(here::here("data", "vpaypo.rds"))
commodities <- readRDS(here::here("data", "commodities.rds"))

rcontracts <- readRDS(here::here("data", "rcontracts.rds"))
rvpayments <- readRDS(here::here("data", "rvpayments.rds"))










con <- "1000009176"

df1 <- contracts %>%
  filter(cnum==con)

df2 <- rcontracts %>%
  filter(cnum==con)  # new name, new end date

df3 <- rvpayments %>%
  filter(cnum==con, year==2021)

df4 <- vpayments %>%
  filter(deptcode=="CHF", 
         str_detect(supplier, coll("826 Val", ignore_case = TRUE)),
         fyear >= 2018)

tmp <- df4 %>%
  filter(fyear==2021)

tmp %>%
  summarise(vpaid=sum(vpaid))

chfchk <- rvpayments %>%
  filter(deptcode=="CHF", year==2021, nonprofit=="X") %>%
  left_join(rcontracts %>% 
              select(-c(solesource)) %>%
              rename(rcctitle=ctitle), by=c("cnum", "supplier", "nonprofit")) %>%
  arrange(supplier, cnum)

chfchk %>%
  write_csv(here::here("analysis", "chfchk.csv"))



glimpse(proposals)
glimpse(contracts)
glimpse(vpayments)

glimpse(rcontracts)

tmp <- contracts %>%
  group_by(cnum) %>%
  mutate(nctitle=length(unique(ctitle))) %>%
  ungroup %>%
  filter(deptcode=="CHF", nctitle > 1)

tmp <- contracts %>%
  filter(deptcode=="CHF") %>%
  arrange(cnum)

tmp <- rcontracts %>%
  filter(deptcode=="CHF") %>%
  arrange(cnum)



con <- "1000009176"


rcontracts %>%
  filter(cnum==con)

npecon <- rcontracts %>%
  filter(startdate <= "2021-06-30",
         is.na(enddate) | enddate >= "2021-06-30") %>%
  filter(nonprofit=="X")

npecon %>%
  filter(str_detect(ctype, coll("grant", ignore_case = TRUE))) %>%
  group_by(deptname) %>%
  summarise(n=n(), award=sum(amt_award), .groups="drop") %>%
  arrange(desc(award))

npecon %>%
  filter(str_detect(ctype, coll("grant", ignore_case = TRUE))) %>%
  arrange(desc(amt_award)) %>%
  select(deptname, cnum, ctitle, supplier, startdate, enddate, amt_award)

int_length(interval("2017-07-03", "2022-06-30"))
interval("2017-07-03", "2022-06-30") / years(1)

npecon %>%
  filter(str_sub(deptname, 1, 3)=="CHF") %>%
  arrange(desc(amt_award)) %>%
  select(ctitle, supplier, startdate, enddate, amt_award, amt_paid) %>% # cnum
  mutate(length=interval(startdate, enddate) / years(1),
         ltd=interval(startdate, "2022-03-12") / years(1),
         dpy=amt_paid / ltd)

npecon %>%
  filter(str_sub(deptname, 1, 3)=="CHF", str_detect(supplier, coll("valencia", ignore_case = TRUE))) %>%
  arrange(desc(amt_award)) %>%
  select(cnum, ctitle, supplier, startdate, enddate, amt_award)

npecon %>%
  filter(str_sub(deptname, 1, 3)=="CHF") %>%
  group_by(ctype) %>%
  summarise(n=n(), award=sum(amt_award))

npecon %>%
  filter(str_sub(deptname, 1, 3)=="CHF") %>%
  group_by(scope) %>% # all unspecified
  summarise(n=n(), award=sum(amt_award))

npecon %>%
  filter(str_sub(deptname, 1, 3)=="CHF") %>%
  group_by(purchauth) %>% # same breakdown as ctype - almost all grants
  summarise(n=n(), award=sum(amt_award))