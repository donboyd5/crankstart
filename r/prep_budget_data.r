
# BETTER than the data: https://openbook.sfgov.org/webreports/search.aspx?searchString=&year=2019&year2=2022&type=CityBudgets&index=33&index2=3&index3=0
# https://openbook.sfgov.org/webreports/details3.aspx?id=2982
# https://sfcontroller.org/sites/default/files/Documents/Budget/AAO%20FY2021-22%20%26%20FY2022-23%20-%20B%26F%20PROPOSED%20-%20FINAL%20210714.pdf

# https://data.sfgov.org/City-Management-and-Ethics/Budget/xdgd-c79v/data


source(here::here("r", "libraries.r"))
# Budget_2022-05-08.csv
fn <- "Budget_2022-05-08.csv"

df <- vroom(here::here("data", "opendata", fn))
glimpse(df)

temp <- df %>%
  rename(fy=`Fiscal Year`,
         deptcode=`Department Code`,
         deptname=Department,
         fintype=`Revenue or Spending`,
         fundtype=`Fund Type`) %>%
  lcnames() |> 
  filter(fy>=2021, fintype=="Spending") 

count(temp, Program)

temp |> 
  filter(deptcode=="CHF") |> 
  group_by(fy) |> 
  summarise(bud=sum(Budget))

temp |> 
  filter(deptcode=="DBI") |> 
  group_by(fy, Program) |> 
  summarise(bud=sum(Budget))

temp |> 
  filter(deptcode=="HSA") |> 
  group_by(deptcode, fy, fundtype) |> 
  summarise(bud=sum(budget))

temp |> 
  filter(deptcode=="HSA") |> 
  group_by(deptcode, fy, Fund) |> 
  summarise(bud=sum(Budget))



temp %>%
  filter(str_detect(fundtype, coll("work", ignore_case = TRUE)))


doi <- c("HSA", "HOM", "CHF", "MYR", "ECN", "CFC", "DPW", 
         "ADM", "ADP", "WOM", "ART", "DPH", "DBI", "SHF",
         "HRC", "PUC", "POL", "REC", "PRT", "ENV")

temp |> 
  filter(deptcode %in% doi, fy==2021) |> 
  group_by(deptcode, deptname) |> 
  summarise(bud=sum(budget)) |> 
  kbl(format.args = list(big.mark=","))



tcfc <- df %>%
  rename(fy=`Fiscal Year`,
         deptcode=`Department Code`,
         deptname=Department,
         fintype=`Revenue or Spending`,
         fundtype=`Fund Type`) %>%
  lcnames() |> 
  filter(fy==2021, deptcode=="CFC") 
count(tcfc, fintype)

tcfc |> 
  group_by(deptcode, deptname, fintype) |> 
  summarise(bud=sum(budget)) 
