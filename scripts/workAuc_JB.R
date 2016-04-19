## packages
require(MESS)
require(zoo)
require(caTools)

require(Bolstad2)

###NOTE : Use the vendorDistribution Chart variable as produced after repairs

###Note : There is no curve to calculate AUC if there is only one point or the value is NA.


chart.test1 <- vendor.funding %>% filter(!is.na(pctFund))


## Calculate using trapezoid method (less desired according to SO)

thing.trap <- chart.test1 %>% ungroup () %>%
  group_by(Funding.Department.Name) %>%
  summarize(test=caTools::trapz(pctVendor,pctFund)) 


#Calculate using Spline Method

thing.spline <- chart.test1 %>% ungroup () %>%
  group_by(Funding.Department.Name) %>%
  summarize(test=MESS::auc(pctVendor,pctFund,type = 'spline'))

#Calculate Using Simpsons

thing.simp <- chart.test1 %>% ungroup () %>%
  group_by(Funding.Department.Name) %>%
  filter(n()>1) %>%
summarize(test=Bolstad2::sintegral(pctVendor,pctFund)$int)


