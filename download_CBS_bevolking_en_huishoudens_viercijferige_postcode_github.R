#
# Init
#

# Load packages
library(cbsodataR)
library(readxl)
library(dplyr)
library(stringr)

# Function to download and convert CBS "BevolkingPerPostcode_1januari" tables
download_pc4 <- function(year, id) {
  # year = Year (for file name)
  # id = url to xls file or CBS Open data ID

  # Excel file (TRUE) or CBS open data (FALSE)
  excel <- id %>% str_detect(pattern = "xls")

  # Download table
  if (excel) {
    filename <- str_c("BevolkingPerPostcode_1januari", year, ".xls")
    download.file(url = id, destfile = filename)
    x <- read_excel(path = filename, skip = 5, col_names = FALSE) %>% as.data.frame
    file.remove(filename)
  } else {
    x <- get_data(id = id) %>% as.data.frame
  }

  # First row and column cleanup
  if (excel) {
    # Remove rows and columns with all NA
    x <- x %>%
      filter(X__1 %>% is.na %>% `!`) %>%
      select(-(x %>% lapply(FUN = is.na) %>% sapply(FUN = all) %>% which))
  } else {
    # Remove ID column
    x <- x %>%
      select(-ID)
  }

  # Set column names by hand (because they differ between years)
  names(x) <- c(
    "pc4",
    outer(
      X = c("mannen_vrouwen", "mannen", "vrouwen"),
      Y = c("totaal", str_c(seq(from = 0, to = 95, by = 5), "tot", seq(from = 5, to = 100, by = 5))),
      FUN = str_c, sep = "_") %>% t %>% as.vector,
    str_c("allochtonen", c("totaal", "westers", "nietwesters"), sep = "_"),
    str_c("hh", c("totaalparticulier", "eenpersoons", "meerpersoonszonderkinderen", "meerpersoonsmetkinderen", "gemiddeldegrootte"), sep = "_"),
    str_c("gemeente", c("code", "naam"), sep = "_"))

  # Manipulations on data
  if (excel) {
    x <- x %>%
      # Only keep records with <=4 pc4 characters
      filter(str_count(pc4) <= 4)
  } else {
    x <- x %>%
      # Strip gemeentenaam from pc4
      mutate(pc4 = pc4 %>% str_sub(start = 1, end = 4)) %>%
      # Remove records with "Tota" and "Over" from pc4
      filter(!pc4 %in% c("Tota", "Over"))
  }

  # Covert variabels to their correct datatype
  x <- x %>%
    mutate_at(
      .vars = vars(pc4:hh_meerpersoonsmetkinderen),
      .funs = funs(as.integer)) %>%
    mutate(
      hh_gemiddeldegrootte = hh_gemiddeldegrootte %>% as.numeric,
      gemeente_code = str_c("GM", gemeente_code %>% str_trim %>% str_pad(width = 4, pad = "0")) %>% factor,
      gemeente_naam = gemeente_naam %>% str_trim %>% factor)

  # Write table as tab delimited text file
  x %>% write.table(file = str_c("BevolkingPerPostcode_1januari", year, ".txt"),
    quote = FALSE, sep = "\t", row.names = FALSE)
}

#
# Download PC4 data
#

# Set download directory
setwd(".../pc4")

# Get list CBS Statline tables with ID and title
statline.data <- get_table_list(select = c("Identifier", "ShortTitle"))

# Identify tables with "Bevolking" & "postcode" in title
(id.data <- statline.data[
  grepl(x = statline.data$ShortTitle, pattern = "Bevolking") &
    grepl(x = statline.data$ShortTitle, pattern = "postcode"), ])

# Download PC4 data from StatLine
download_pc4(year = 1998, id = "37602ned")
download_pc4(year = 1999, id = "37339ned")
download_pc4(year = 2000, id = "37724ned")
download_pc4(year = 2001, id = "37680")
download_pc4(year = 2002, id = "70005ned")
download_pc4(year = 2003, id = "70654ned")
download_pc4(year = 2004, id = "70819ned")
download_pc4(year = 2005, id = "71012ned")
download_pc4(year = 2006, id = "71273ned")
download_pc4(year = 2007, id = "71491ned")
download_pc4(year = 2008, id = "71899ned")
download_pc4(year = 2009, id = "80280ned")
download_pc4(year = 2010, id = "80667ned")
download_pc4(year = 2011, id = "81310ned")
download_pc4(year = 2012, id = "81922NED")
download_pc4(year = 2013, id = "82245NED")
download_pc4(year = 2014, id = "https://www.cbs.nl/-/media/imported/documents/2014/52/bevolking-postcode.xls")
download_pc4(year = 2015, id = "https://www.cbs.nl/-/media/imported/documents/2016/53/bevolkingperpostcode_1januari2015.xls")
download_pc4(year = 2016, id = "https://www.cbs.nl/-/media/_excel/2016/51/bevolking-per-postcode-1-januari-2016.xls")
download_pc4(year = 2017, id = "https://www.cbs.nl/-/media/_excel/2017/48/bevolkingperpostcode_1januari2017.xls")
