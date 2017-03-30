library(data.table)

flights <- fread("small_tests/5 Stanford_data/flights14.csv")
flights

DT = data.table(ID = c("b","b","b","a","a","c"), a = 1:6, b = 7:12, c = 13:18)

getOption("datatable.print.nrows")


# 1 rows manipulate --------------------------------------------------------

## Get all the flights with “JFK” as the origin airport in the month of June.
ans = flights[origin == "JFK" & month == 6L]
ans

## Get the first two rows from flights
ans <- flights[1:2]
ans


## ordering
ans <- flights[order(origin, -dest)]
ans


## The different ordering speed:
odt = data.table(col = sample(1e7))
(t1 <- system.time(ans1 <- odt[base::order(col)]))
(t2 <- system.time(ans2 <- odt[order(col)]))
(identical(ans1, ans2))

# 2 columns manipulate -----------------------------------------------------
## Select arr_delay column, but return it as a vector.
ans <- flights[, arr_delay]
head(ans)

## Select arr_delay column, but return as a data.table instead.
ans <- flights[, list(arr_delay)]
head(ans)
## or 
ans <- flights[, .(arr_delay)]
head(ans)

## Select both column:
ans <- flights[, .(arr_delay, dep_delay)]
ans

## columns reneme:
ans <- flights[, .(delay_arr = arr_delay, delay_dep = dep_delay)]
ans

## columns comuting:
ans <-  flights[, sum((arr_delay + dep_delay) < 0)]
ans

ans <- flights[origin == "JFK" & month == 6L,
							 .(m_arr = mean(arr_delay), m_dep = mean(dep_delay))]	
ans

ans <- flights[origin == "JFK" & month == 6L,
							 length(dest)]
ans

ans <- flights[origin == "JFK" & month == 6L,.(.N)]
ans

ans <- flights[,c("origin", "month"), with=F]
ans

## delete columns:
ans <- flights[, !c("arr_delay","dep_delay"), with = F]
ans
ans <- flights[, -c("arr_delay","dep_delay"), with = F]
ans


## we can also select by specifying start and end column names:
ans <- flights[, year:day]
ans
ans <- flights[, !(year:day)]
ans



# 3 aggregations ------------------------------------------------------------

ans = flights[, .(.N), by = .(origin)]
ans

# where there is only 1 column in j and by
ans = flights[,.N, by = origin]
ans

ans = flights[carrier == "AA",.N, by = origin]
ans

ans = flights[carrier == "AA", .N, by = .(origin, dest)]
ans

ans = flights[carrier == "AA",
							.(m_arr = mean(arr_delay),m_dep = mean(dep_delay)), 
							by = .(origin, dest, month)]
ans


ans = flights[carrier == "AA",
							.(m_arr = mean(arr_delay),m_dep = mean(dep_delay)), 
							keyby = .(origin, dest, month)]
ans


# 4 Chaining ----------------------------------------------------------------
ans = flights[carrier == "AA",
							.(m_arr = mean(arr_delay),m_dep = mean(dep_delay)), 
							keyby = .(origin, dest, month)
						][
								order(origin,-dest)
						]
							



# 5 expressions in by -------------------------------------------------------
ans = flights[,
							.N,
							.(dep_delay>0, arr_delay>0)]
ans

## play with dplyr
ans = flights[,
							.N,
							.(dep_delay%%10, dep_delay%/%10)
						][order(-dep_delay)] %>% 
	rename(int = dep_delay.1)
ans

class(ans)


# 5 .SD

DT
DT[, .SD]
DT[, .SD, by = ID]
DT[, print(.SD), by = ID]
DT[, lapply(.SD, mean), by = ID]


ans = flights[carrier == "AA",
							lapply(.SD, mean),
							by = .(origin, dest, month),
							.SDcols = c("arr_delay", "dep_delay")]

ans <- flights[, head(.SD, 2), by = month]

##
DT
DT[,
	 .(c(a,b)), by = ID]
DT[,
	 .(list(c(a,b))), by = ID]
