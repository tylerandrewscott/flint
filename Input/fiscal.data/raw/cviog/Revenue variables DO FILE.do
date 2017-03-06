// read in cleaned.fiscal_temp on 5/18/2016 from 
// https://www.dropbox.com/s/gbff1ecaq5nz4xf/cleaned.fiscal.csv?dl=0
// of find at: https://github.com/tylerascott/flint/blob/master/Input/fiscal.data/cleaned.fiscal.csv

rename p3n314 interest_exp
rename p3n315 saleofassets
rename p3n316 other nonop_rev
rename p3n316 other_nonop_rev

replace totaloprev = "" if totaloprev=="NA"
replace fromfed = "" if totaloprev=="NA"
replace fromfed = "" if fromfed =="NA"
replace fromloc = "" if fromloc =="NA"
replace fromstate = "" if fromstate =="NA"

destring fromstate, replace
destring fromloc, replace
destring fromfed, replace
destring totaloprev , replace

***	Note: Check for duplicates
egen id = group(p2authname)
duplicates report id year
duplicates list id year

***	Note: Revised means that interest expense, sale of assets, and other non-operating revenues and expenses
*** Note: Revised non-operating revenue and revised total revenue include some negative numbers
gen revised_nonoprev = fromstate + fromfed + fromloc 
gen revised_total_rev = totaloprev + revised_nonoprev

*** Note: Zeroed variables = 1 to indicate observations with negative values
sort totaloprev
edit p2authname year p3n301 p3n302 p3n303 totaloprev
gen zeroed_totaloprev = 1 if totaloprev<0
replace zeroed_totaloprev=0 if missing(zeroed_totaloprev)
sort revised_nonoprev
gen zeroed_revised_nonoprev = 1 if fromfed<0 | fromstate<0 | fromloc<0
replace zeroed_revised_nonoprev =0 if missing(zeroed_revised_nonoprev)

save "C:\Users\Rgreer1\Dropbox\Data\GA Public Authorities\special_dist_finance_revenue.dta"
drop if year<2010
collapse (sum) totaloprev revised_nonoprev fromloc, by(id)
gen perc_ownsource = (revised_nonoprev / (totaloprev + revised_nonoprev))*100
gen perc_fromloc = (fromloc / (totaloprev + revised_nonoprev))*100

*** Note: POS variables are zeroed-out versions of the revised non-operating revenue and revised total revenue variables
gen pos_op_rev = totaloprev						
replace pos_op_rev = 0 if pos_op_rev<0  	
gen pos_nonop_rev = revised_nonoprev		
replace pos_nonop_rev = 0 if pos_nonop_rev<0
gen pos_fromloc = fromloc						
replace pos_fromloc = 0 if pos_fromloc<0

gen pos_perc_ownsource = (pos_nonop_rev / (pos_op_rev + pos_nonop_rev))*100
gen pos_perc_fromloc = (pos_fromloc / (pos_op_rev + pos_nonop_rev))*100

*** Note: To get authority names
*** use "C:\Users\Rgreer1\Dropbox\Data\GA Public Authorities\special_dist_finance_revenue.dta", replace
*** keep id p2authname year
*** collapse (p50) id, by( p2authname)
