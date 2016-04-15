## AUC example
x <- 1:10
y <- 3*x+25
id <- order(x)

require(zoo)
rollmean(y[id],2)
diff(x[id])

AUC <- function(x) {
    sum(diff(x$pctVendor)*rollmean(x$pctFund,2))
}

chart %>% do(AUC(.))
chart %>% 
    group_by(Funding.Department.Name) %>%
    summarize(test = AUC(.))

test <- chart %>%
    filter(Funding.Department.Name == 'COMMERCE, DEPARTMENT OF')

AUC(test)
