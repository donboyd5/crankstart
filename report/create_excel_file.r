

# libraries ---------------------------------------------------------------
library(openxlsx)


# constants ---------------------------------------------------------------
outpath <- here::here("report", "results", "nonprofits.xlsx")


# functions ---------------------------------------------------------------

top_sheet <- function(wb, data, sname, first_data_row,
                      comma_cols, dollar_cols, pct_cols){
  last_data_row <- nrow(data) + first_data_row - 1
  totalrow <- last_data_row
  last_col <- ncol(data)
  
  writeData(wb, sheet = sname, x=data,
            startCol=1, startRow=first_data_row, rowNames = FALSE, colNames = FALSE)
  
  # apply styles
  addStyle(wb, sheet = sname, style = num0, rows=first_data_row:last_data_row, cols = comma_cols, gridExpand = TRUE)
  addStyle(wb, sheet = sname, style = pct1, rows=first_data_row:last_data_row, cols = pct_cols, gridExpand = TRUE)
  addStyle(wb, sheet = sname, style = curr0, rows=c(first_data_row, last_data_row), cols = dollar_cols)
  
  addStyle(wb, sheet = sname, style = curr0, rows=totalrow, cols = dollar_cols, stack=TRUE)
  addStyle(wb, sheet = sname, style = totstyle, rows = totalrow, cols = 1:last_col, stack=TRUE)
  wb
}


npes_by_depts <- function(wb, data, sname, header_row){
  
  cnames <- c("Rank", "Nonprofit Organization", names(data)[-c(1:2)])
  # create an empty data frame with these names
  cnamesdf <- as_tibble(matrix(nrow=0, ncol=length(cnames), dimnames=list(NULL, cnames)))
  
  first_data_row <- header_row + 2
  last_data_row <- nrow(data) + first_data_row - 1
  totalrow <- last_data_row
  
  last_col <- ncol(data)
  comma_cols <- 3:last_col
  dollar_cols <- comma_cols
  
  # clear all data
  # deleteData(wb, sheet = sname, rows = header_row:totalrow, cols = 1:last_col, gridExpand = TRUE)
  # removeCellMerge(wb, sheet = sname, rows = 1:totalrow, cols = 1:last_col)
  
  # insert column names
  writeData(wb, sheet = sname, x=cnamesdf,
            startCol=1, startRow=header_row, rowNames = FALSE, colNames = TRUE)
  # data
  writeData(wb, sheet = sname, x=data,
            startCol=1, startRow=first_data_row, rowNames = FALSE, colNames = FALSE)
  
  # apply styles
  setColWidths(wb, sheet=sname, cols = comma_cols, widths = rep(15, length(comma_cols)))
  mergeCells(wb, sheet = sname, rows = 3, cols = 1:last_col)
  mergeCells(wb, sheet = sname, rows = 4, cols = 1:last_col)
  
  addStyle(wb, sheet = sname, style = headerstyle, rows = header_row, cols = 1:last_col, stack=TRUE)
  
  addStyle(wb, sheet = sname, style = num0, rows=first_data_row:last_data_row, cols = comma_cols, gridExpand = TRUE)
  addStyle(wb, sheet = sname, style = curr0, rows=c(first_data_row, last_data_row), cols = dollar_cols, gridExpand = TRUE)
  
  addStyle(wb, sheet = sname, style = curr0, rows=totalrow, cols = dollar_cols, stack=TRUE)
  addStyle(wb, sheet = sname, style = totstyle, rows = totalrow, cols = 1:last_col, stack=TRUE)
  wb
}


dcyf_sheet <- function(wb, data, sname, header_row, comma_cols, wrap_cols){
  first_data_row <- header_row + 2
  last_data_row <- nrow(data) + first_data_row - 1
  totalrow <- last_data_row
  
  last_col <- ncol(data)
  dollar_cols <- comma_cols
  
  writeData(wb, sheet = sname, x=data,
            startCol=1, startRow=first_data_row, rowNames = FALSE, colNames = FALSE)
  
  # apply styles
  addStyle(wb, sheet = sname, style = headerstyle, rows = (header_row - 1), cols = 1:last_col, stack=TRUE)
  addStyle(wb, sheet = sname, style = headerstyle, rows = header_row, cols = 1:last_col, stack=TRUE)
  
  addStyle(wb, sheet = sname, style = num0, rows=first_data_row:last_data_row, cols = comma_cols, gridExpand = TRUE, stack=TRUE)
  addStyle(wb, sheet = sname, style = curr0, rows=c(first_data_row, last_data_row), cols = dollar_cols, stack=TRUE)
  addStyle(wb, sheet = sname, style = wrap_style, rows=first_data_row:last_data_row, cols = wrap_cols, gridExpand = TRUE, stack=TRUE)
  addStyle(wb, sheet = sname, style = vcenter_style, rows=first_data_row:last_data_row, cols = 1:last_col, gridExpand = TRUE, stack=TRUE)
  
  addStyle(wb, sheet = sname, style = curr0, rows=totalrow, cols = dollar_cols, stack=TRUE)
  addStyle(wb, sheet = sname, style = totstyle, rows = totalrow, cols = 1:last_col, stack=TRUE)
  wb
}


# define styles -----------------------------------------------------------
num0 <- createStyle(numFmt = "#,##0")
num1 <- createStyle(numFmt = "#,##0.0")
num2 <- createStyle(numFmt = "#,##0.00")
num3 <- createStyle(numFmt = "#,##0.000")

curr0 <- createStyle(numFmt = "$#,##0")
curr2 <- createStyle(numFmt = "$#,##0.00")
curr3 <- createStyle(numFmt = "$#,##0.000")

pct1 <- createStyle(numFmt = "0.0%")
pct2 <- createStyle(numFmt = "0.00%")

# for totals rows
totstyle <- createStyle(fontSize = 12, fgFill = "#f0f0f0", border = "TopBottom", borderColour = "#f0f0f0")
headerstyle <- createStyle(fontSize = 12, fgFill = "#f0f0f0", 
                           border = "TopBottom", borderColour = "#f0f0f0", 
                           wrapText = TRUE, textDecoration = "bold")
bold_style <- createStyle(fontSize = 12, textDecoration = "bold")
wrap_style <- createStyle(wrapText = TRUE, halign = "left", valign = "center")
vcenter_style <- createStyle(valign = "center")


# get empty workbook shell ------------------------------------------------------
shellfile <- "nonprofits_shell.xlsx"
snames <- getSheetNames(here::here("report", "inputs", shellfile))

wb <- loadWorkbook(file = here::here("report", "inputs", shellfile))

# global workbook settings
modifyBaseFont(wb, fontSize = 12, fontColour = "black", fontName = "Calibri")


# get the sheets we want --------------------------------------------------
# sname <- "TaxTable"
# cloneWorksheet(wb, sheetName=snames[1], clonedSheet = "TableShell")
# purrr::walk(snames, function(x) removeWorksheet(wb, x))

# top departments --------------------------------------------------------
# get data
topndepts <- readRDS(here::here("report", "data", "topndepts.rds"))
# names(topndepts)
wb <- top_sheet(wb, topndepts, "top_departments", first_data_row=8,
                comma_cols=4, dollar_cols=4, pct_cols=5:6)

# top nonprofits ----------------------------------------------------------
topnpes <- readRDS(here::here("report", "data", "topnpes.rds"))
names(topnpes)
wb <- top_sheet(wb, topnpes, "top_nonprofits", first_data_row=8,
                comma_cols=3, dollar_cols=3, pct_cols=4:5)


# top nonprofits by top departments ---------------------------------------
topnpesdepts <- readRDS(here::here("report", "data", "topnpesdepts.rds"))

wb <- npes_by_depts(wb, data=topnpesdepts, sname="topnonprofits_by_topdepts", header_row=6)


# allnonprofits_by_alldepts -----------------------------------------------
# this is a little more complicated
npedept_wide <- readRDS(here::here("report", "data", "npedept_wide.rds"))

wb <- npes_by_depts(wb, data=npedept_wide, sname="allnonprofits_by_alldepts", header_row=6)


# dcyf -----------------------------------------------
dcyf <- readRDS(here::here("report", "data", "dcyf_wide.rds"))

wb <- dcyf_sheet(wb, data=dcyf, sname="dcyf_allocations", header_row=7,
                 comma_cols=7:8, wrap_cols = 3:6)


# view workbook -----------------------------------------------------------

# openXL(wb)  ## opens a temp version


# save workbook -----------------------------------------------------------
saveWorkbook(wb, file = outpath, overwrite = TRUE)

