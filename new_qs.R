source("assets/r/libs_data.R")
library(ggplot2)


###
### Settings
###
options(scipen = 999)
path.data <- file.path("/Users/joetidwell/Documents/git/freidenker/data")

############
### Data ###
############

### State Homestead Exemption Propositions
### Sources: https://www.texaspolicyresearch.com/texas-2025-constitutional-amendments-explained-ballot-guide-vote-recommendations/
###          https://www.sos.state.tx.us/elections/forms/november-2025-ballot-language-17.pdf
state_homestead_exemptions <- data.table(
  type = c("All","Over 65"),
  current = c(100000, 10000),
  prop_11 = c(0, 50000),
  prop_13 = c(40000, 0),
  if_passed = c(140000, 60000)

)

### Boerne ISD Tax Rate Public Notice
### Source: Boerne Star
### Image: /resources/boerne_star_tax_rate_announcement.jpg
bisd_rate_notice <- read.csv(file.path(path.data,'bisd_rate_proposal.csv')) %>%
  data.table


#################
### Questions ###
#################

###
### If passed, how will the state propositions affect my property taxes?
###


# "Average" taxable value

# Under-65

# Over-65



finance_summary <- read_excel(file.path(path.data,'2008-2024-summarized-financial-data-03-17-2025.xlsx')) %>%
  data.table
levies_2024 <- read_excel(file.path(path.data,'2024-school-district-rates-levies.xlsx')) %>%
  data.table 
levies_2024 <- levies_2024[,.(`TAXING UNIT NAME`,
                              `MARKET VALUE`,
                              `TAXABLE VALUE M&O`,
                              `TAXABLE VALUE I&S`,
                              `NO-NEW-REVENUE RATE`,
                              `VOTER-APPROVAL RATE`,
                              `M&O RATE`,
                              `I&S RATE`,
                              `TOTAL TAX RATE`,
                              `CALCULATED LEVY`)]
levies_2024 <- levies_2024[,
                           .(`MARKET VALUE` = sum(`MARKET VALUE`),
                             `TAXABLE VALUE M&O` = sum(`TAXABLE VALUE M&O`),
                             `TAXABLE VALUE I&S` = sum(`TAXABLE VALUE I&S`),
                             `M&O RATE` = `M&O RATE`[1],
                             `I&S RATE` = `I&S RATE`[1],
                             `TOTAL TAX RATE` = `TOTAL TAX RATE`[1],
                             `CALCULATED LEVY` = sum(`CALCULATED LEVY`)
                             ),
                           by=`TAXING UNIT NAME`]
# levies_2024[,`DISTRICT NUMBER`:=gsub("'","",`DISTRICT NUMBER`)]

tax_rates <- read_excel(file.path(path.data,'school-district-adopted-tax-rates.xlsx')) %>%
  data.table
fast_growth <- read.csv(file.path(path.data,'fast_growth_districts.csv')) %>%
  data.table

neighbor_districts <- data.table(CDN = c("130901", # Boerne ISD
                                         "016902", # Blanco ISD
                                         "046902", # Comal ISD
                                         "015915", # North Side ISD
                                         "015910", # North East ISD
                                         "010902", # Bandera ISD
                                         "130902") # Comfort ISD
)






# finance_summary[`DISTRICT NAME`=="BOERNE ISD" & YEAR=="2024",.(`FALL SURVEY ENROLLMENT`)]

# names(finance_summary)
# finance_summary[`FALL SURVEY ENROLLMENT`>8000 & `FALL SURVEY ENROLLMENT`<12000,.N]
# ggplot(finance_summary[YEAR==2024 & `FALL SURVEY ENROLLMENT` < 50000], aes(x=`FALL SURVEY ENROLLMENT`)) +
  # geom_histogram(binwidth=1000)

n.under.8000 <- finance_summary[YEAR==2024 & `FALL SURVEY ENROLLMENT` < 8000,.N]
n.over.13000 <- finance_summary[YEAR==2024 & `FALL SURVEY ENROLLMENT` > 13000,.N]
n.total <- finance_summary[YEAR==2024,.N]
n.over.10500 <- finance_summary[YEAR==2024 & `FALL SURVEY ENROLLMENT` > 8000,.N]

# finance_summary[YEAR==2024 & `FALL SURVEY ENROLLMENT` < 1000,.N]
# n.total-n.under.8000-n.over.13000
# (n.total-n.under.8000-n.over.13000)/n.total
# finance_summary[YEAR==2024 & `FALL SURVEY ENROLLMENT` < 10849,.N]
# 1086/n.total

comp_districts <- finance_summary[YEAR==2024 & 
                                  `FALL SURVEY ENROLLMENT` > 8000 &
                                  `FALL SURVEY ENROLLMENT` < 13000 ,
                                  .(`DISTRICT NUMBER`,
                                    `DISTRICT NAME`,
                                    `FALL SURVEY ENROLLMENT`,
                                    `ALL FUNDS-TOTAL OPERATING REVENUE`,
                                    `GEN FUNDS-TOTAL OPERATING REVENUE`,
                                    `GEN FUNDS-TOTAL DISBURSEMENTS`,
                                    `ALL FUNDS-TOTAL DISBURSEMENTS`)]

# names(finance_summary)
# levies_2024
# names(levies_2024)


tmp <- finance_summary[YEAR==2024,
                      .(`DISTRICT NUMBER`,
                        `DISTRICT NAME`,
                        `FALL SURVEY ENROLLMENT`,
                        `ALL FUNDS-TOTAL OPERATING REVENUE`,
                        `GEN FUNDS-TOTAL OPERATING REVENUE`,
                        `GEN FUNDS-TOTAL DISBURSEMENTS`,
                        `ALL FUNDS-TOTAL DISBURSEMENTS`)]
setkey(tmp,`DISTRICT NAME`)
levies_2024[,`DISTRICT NAME`:=toupper(`TAXING UNIT NAME`)]
setkey(levies_2024,`DISTRICT NAME`)
levies_2024 <- tmp[levies_2024]

levies_2024[,`LEVY PER STUDENT`:=`CALCULATED LEVY`/`FALL SURVEY ENROLLMENT`]
levies_2024[,`GF REVENUE PER STUDENT`:=`GEN FUNDS-TOTAL OPERATING REVENUE`/`FALL SURVEY ENROLLMENT`]
levies_2024[,`AF REVENUE PER STUDENT`:=`ALL FUNDS-TOTAL OPERATING REVENUE`/`FALL SURVEY ENROLLMENT`]

ecdf_levy_per_student <- ecdf(levies_2024$`LEVY PER STUDENT`)
ecdf_levy_per_student(10582.77)
ecdf_levy_per_student_comp <- ecdf(levies_2024[`FALL SURVEY ENROLLMENT` > 9000 &
                                               `FALL SURVEY ENROLLMENT` < 12000 ]$`LEVY PER STUDENT`)
ecdf_levy_per_student_comp(10582.77)
ecdf_levy_per_student_neighbor <- ecdf(levies_2024[`DISTRICT NUMBER` %in% neighbor_districts$CDN]$`LEVY PER STUDENT`)
ecdf_levy_per_student_neighbor(10582.77)

##

ecdf_rate <- ecdf(levies_2024$`TOTAL TAX RATE`)
ecdf_rate(.9909)
ecdf_rate_comp <- ecdf(levies_2024[`FALL SURVEY ENROLLMENT` > 9000 &
                                   `FALL SURVEY ENROLLMENT` < 12000 ]$`TOTAL TAX RATE`)
ecdf_rate_comp(.9909)
ecdf_rate_neighbor <- ecdf(levies_2024[`DISTRICT NUMBER` %in% neighbor_districts$CDN]$`TOTAL TAX RATE`)
ecdf_rate_neighbor(.9909)



levies_2024_collapsed = levies_2024[,.(`M&O RATE`=`M&O RATE`[1],
                                       `I&S RATE`=`I&S RATE`[1],
                                       `TOTAL TAX RATE`=`TOTAL TAX RATE`[1],
                                       `CALCULATED LEVY`=sum(`CALCULATED LEVY`)),by=.(`TAXING UNIT NAME`)]
levies_2024_collapsed[,`DISTRICT NAME`:=toupper(`TAXING UNIT NAME`)]
levies_2024_collapsed[,`TAXING UNIT NAME`:=NULL]
setkey(levies_2024_collapsed,`DISTRICT NAME`)
setkey(comp_districts,`DISTRICT NAME`)
levies_2024_collapsed <- levies_2024_collapsed[comp_districts]

names(levies_2024_collapsed)


## Comparable Districts Tax Rates
ecdf_total <- ecdf(levies_2024_collapsed$`TOTAL TAX RATE`)
ecdf_MO <- ecdf(levies_2024_collapsed$`M&O RATE`)
ecdf_IS <- ecdf(levies_2024_collapsed$`I&S RATE`)
ecdf_total(levies_2024_collapsed[`DISTRICT NAME`=="BOERNE ISD"]$`TOTAL TAX RATE`)
ecdf_MO(levies_2024_collapsed[`DISTRICT NAME`=="BOERNE ISD"]$`M&O RATE`)
ecdf_IS(levies_2024_collapsed[`DISTRICT NAME`=="BOERNE ISD"]$`I&S RATE`)
hist(levies_2024_collapsed$`TOTAL TAX RATE`)

## Comparable Districts Tax Levies
ecdf_levy_per_student <- ecdf(levies_2024_collapsed$`LEVY PER STUDENT`)
ecdf_levy_per_student(levies_2024_collapsed[`DISTRICT NAME`=="BOERNE ISD"]$`LEVY PER STUDENT`)

## Comparable Districts Revenue
ecdf_revenue_per_student <- ecdf(levies_2024_collapsed$`GF REVENUE PER STUDENT`)
ecdf_revenue_per_student(levies_2024_collapsed[`DISTRICT NAME`=="BOERNE ISD"]$`GF REVENUE PER STUDENT`)
hist(levies_2024_collapsed$`AF REVENUE PER STUDENT`)

## Compare to Comfort ISD
comp_comfort <- finance_summary[`DISTRICT NAME` %in% c("BOERNE ISD","COMFORT ISD","NORTHSIDE ISD") & YEAR=="2024" & `DISTRICT NUMBER` != "'244905",]
comp_comfort[,`GF REVENUE PER STUDENT`:=`GEN FUNDS-TOTAL OPERATING REVENUE`/`FALL SURVEY ENROLLMENT`]
comf.add.GF.rev.per.student <- comp_comfort[,`GF REVENUE PER STUDENT`][3] - comp_comfort[,`GF REVENUE PER STUDENT`][2]
comf.add.GF.rev <- comf.add.GF.rev.per.student * comp_comfort[`DISTRICT NAME` == "BOERNE ISD",`FALL SURVEY ENROLLMENT`]
ns.add.GF.rev.per.student <- comp_comfort[,`GF REVENUE PER STUDENT`][1] - comp_comfort[,`GF REVENUE PER STUDENT`][2]
ns.add.GF.rev <- ns.add.GF.rev.per.student * comp_comfort[`DISTRICT NAME` == "BOERNE ISD",`FALL SURVEY ENROLLMENT`]

comp_comfort$`DISTRICT NUMBER`

#BISD says 37% comes in addition to taxes
# 2024 copper penny rate $49.28 per penny per weighted student
# 2024 golden pennies $98.56 yield for "golden" pennies

