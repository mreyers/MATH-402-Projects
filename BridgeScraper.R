# http://www.th.gov.bc.ca/trafficdata/tradas/reports/AllYears/2005/02/MV03/MV03%20-%20Site%20Lions%20Gate%20-%20P-15-1NS%20-%20N%20on%2002-01-2005.xls
# http://www.th.gov.bc.ca/trafficdata/tradas/reports/AllYears/2006/02/MV03/MV03%20-%20Site%20Lions%20Gate%20-%20P-15-1NS%20-%20N%20on%2002-01-2006.xls
# http://www.th.gov.bc.ca/trafficdata/tradas/reports/AllYears/2007/02/MV03/MV03%20-%20Site%20Lions%20Gate%20-%20P-15-1NS%20-%20N%20on%2002-01-2007.xls
# http://www.th.gov.bc.ca/trafficdata/tradas/reports/AllYears/2010/02/MV03/MV03%20-%20Site%20Lions%20Gate%20-%20P-15-1NS%20-%20N%20on%2002-01-2010.xls
# http://www.th.gov.bc.ca/trafficdata/tradas/reports/AllYears/2011/02/MV03/MV03%20-%20Site%20Lions%20Gate%20-%20P-15-1NS%20-%20N%20on%2002-01-2011.xls
# http://www.th.gov.bc.ca/trafficdata/tradas/reports/AllYears/2011/01/MV03/MV03%20-%20Site%20Lions%20Gate%20-%20P-15-1NS%20-%20N%20on%2001-01-2011.xls
# http://www.th.gov.bc.ca/trafficdata/tradas/reports/AllYears/2012/02/MV03/MV03%20-%20Site%20Lions%20Gate%20P-15-1NS%20-%20NY%20on%2002-01-2012.xls
# http://www.th.gov.bc.ca/trafficdata/tradas/reports/AllYears/2012/01/MV03/MV03%20-%20Site%20Lions%20Gate%20P-15-1NS%20-%20NY%20on%2001-01-2012.xls
# http://www.th.gov.bc.ca/trafficdata/tradas/reports/AllYears/2014/02/MV03/MV03%20-%20Site%20Lions%20Gate%20P-15-1NS%20-%20NY%20on%2002-01-2014.xls


# Differences between the links
# /AllYears/YYYY/MM/........../%20on%20MM-01-YYYY.xls

years <- as.character(c(2005:2016))
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

# Create a double loop that iterates over each of the links, should cause me to download
for( i in 1:length(years)){
  for( j in 1:12){
    if( i <= 7){
      browseURL(url = paste0("http://www.th.gov.bc.ca/trafficdata/tradas/reports/AllYears/", years[i], "/", months[j],
                         "/MV03/MV03%20-%20Site%20Lions%20Gate%20-%20P-15-1NS%20-%20N%20on%20", months[j], "-01-", years[i], ".xls"))
    }
    else{
      browseURL(url = paste0("http://www.th.gov.bc.ca/trafficdata/tradas/reports/AllYears/", years[i], "/", months[j],
                             "/MV03/MV03%20-%20Site%20Lions%20Gate%20P-15-1NS%20-%20NY%20on%20", months[j], "-01-", years[i], ".xls"))
    }
  }
}
