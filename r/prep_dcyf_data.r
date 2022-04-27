
# libraries and functions ---------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "functions.r"))


# constants ---------------------------------------------------------------
dcyffn <- "FY2020-2022 DCYF funded Program List.xlsx"


# get data ----------------------------------------------------------------
# FY2020-2021

df21 <- read_excel(here::here("data", "dcyf", dcyffn), "FY2020-2021")
ns(df21)

df22 <- read_excel(here::here("data", "dcyf", dcyffn), "FY2021-2022")
ns(df22)

names(df21)
names(df22)

cnames <- c("planfy", "agency", "program", "funding", "svcarea", "strategy", "description")


# DCYF funding ------------------------------------------------------------

dcyf_funding <- bind_rows(
  df21 %>% setNames(cnames) %>%
    mutate(planfy="2020-21"),
  df22 %>% setNames(cnames) %>%
    mutate(planfy="2021-22"))
dcyf_funding
saveRDS(dcyf_funding, here::here("data", "dcyf_funding.rds"))

dcyf_funding %>%
  group_by(planfy) %>%
  summarise(funding=sum(funding, na.rm=TRUE))


# DCYF programs -----------------------------------------------------------

dcyf_programs <- dcyf_funding %>%
  select(agency, program) %>%
  distinct()
saveRDS(dcyf_programs, here::here("data", "dcyf_programs.rds"))

