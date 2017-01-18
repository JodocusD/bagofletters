
#testing outlier removal for multiple columns

#TEST1 :  http://stackoverflow.com/questions/27255761/remove-outliers-for-all-columns-with-r

calcul.mad <- function(x) {
  mad <- median(abs(x-median(x, na.rm=TRUE))) 
  mad}

uper.interval <- function(x,y) {
  up.inter <- median(x, na.rm=TRUE)+3*(y) 
  up.inter}

lower.interval <- function(x,y) {
  low.inter <- median(x, na.rm=TRUE)-3*(y)
  low.inter}

functionData <- function(x,h,l) {
  out <- ifelse(x > h, NA, ifelse(x < l, NA, x))
  out}

outlier.fun <- function(column1) {
  med_data <- median(column1, na.rm=TRUE)
  cal_mad <- calcul.mad(column1)
  up_data <- uper.interval(med_data, cal_mad)
  low_data <- lower.interval(med_data, cal_mad)
  column_without_outliers <- functionData(column1, up_data, low_data)
  
  return(column_without_outliers)
}

Testset <- engag16[, c("alexa_pv", "uniques1000x", "pages_per_unique", "comments_article"), with = FALSE]
df <- as.data.table(engag16)
df2 <- df[ ,"alexa_pv", with = FALSE]
Testhmm <- lapply(engag16[ ,c(2:4) , with = FALSE], outlier.fun)

engag16or <- engag16
engag16or[ , c("alexa_pv", "uniques1000x", "pages_per_unique", "comments_article")] <- lapply(engag16or[ , c("alexa_pv", "uniques1000x", "pages_per_unique", "comments_article"), with = FALSE], outlier.fun)
