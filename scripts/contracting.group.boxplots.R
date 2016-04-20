require(dplyr)
require(ggplot2)
require(gridExtra)
require(grid)


region.frame <- fpds %>%
  select(Contracting.Group.ID,Fiscal.Year, Contracting.Office.Region,
         uniqueId, vendorId, congressId, NAICS.Code,
         Action.Obligation, Number.of.Offers.Received, 
         Extent.Competed, Reason.Not.Awarded.To..Small.Business, 
         catAward, compCat, naicsTwo, naicsThree, naicsFour, 
         Award.or.IDV.Type, Type.of.Contract, Type.of.Set.Aside,
         Fair.Opportunity.Limited.Sources, Other.Than.Full.and.Open.Competition,
         Effective.Date, Completion.Date,
         Funding.Department.ID, Funding.Department.Name,
         PIID.Agency.ID, PIID, Referenced.IDV.Agency.ID, 
         Referenced..IDV.PIID, Modification.Number, 
         Principal.Place.of.Performance.State.Code,
         Congressional.District.Place.of..Performance,histDisAdv
  )


high <- c("R3 - PUBLIC BUILDINGS SERVICE","R2 - FEDERAL ACQUISITION SERVICE")
low <- c("R2 - PUBLIC BUILDINGS SERVICE","R9 - FEDERAL ACQUISITION SERVICE")

region.box.data <- region.frame %>% 
  mutate(holdOffers = ifelse(Number.of.Offers.Received == 999, NA,
                             Number.of.Offers.Received)) %>% 
  group_by(Contracting.Group.ID,uniqueId,catAward) %>%
  select(holdOffers, Number.of.Offers.Received, uniqueId, Action.Obligation, Contracting.Group.ID,catAward) %>%
  summarize(numberOffers = max(holdOffers, na.rm = TRUE),
            amountDollars = sum(Action.Obligation,na.rm = T),
            numberActions = n()) %>%
  filter(catAward == 'Award')


ggplot(data=region.box.data,aes(x=Contracting.Group.ID,y=numberOffers)) + 
  geom_boxplot() + facet_wrap(facets = catAward,scales="free")


grouplist <- unique(region.box.data$Contracting.Group.ID) %>% sort(decreasing = F)
colorlist <- rep("black",length(grouplist))
colorlist [grouplist %in% high] <- "red"
colorlist [grouplist %in% low] <- "turquoise3"


png(filename = "fig/1_offers_received_dist_regional_agencies_BW.png",height = 1000,width = 1400)

region.box.data %>% 
ggplot(data=.,aes(x=Contracting.Group.ID,y=numberOffers)) +
  geom_boxplot() + coord_flip() +xlab("Regional Contracting Agency") + 
  ylab("Offers") + ggtitle("Offers Received by Regional Contracting Agency")+
  annotate("text", label = "* Only Non-Vehicle Awards Shown", x = 2.5, y = 90, size = 3, colour = "black") +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=13,face="bold"),title=element_text(size=14,face="bold"))
  
dev.off()



naics.box.data <- region.frame %>% 
  mutate(holdOffers = ifelse(Number.of.Offers.Received == 999, NA,
                             Number.of.Offers.Received)) %>% 
  group_by(naicsTwo,uniqueId,catAward) %>%
  select(holdOffers, Number.of.Offers.Received, uniqueId, Action.Obligation, Contracting.Group.ID,catAward) %>%
  summarize(numberOffers = max(holdOffers, na.rm = TRUE),
            amountDollars = sum(Action.Obligation,na.rm = T),
            numberActions = n())



png(filename = "fig/2_offers_received_dist_NAICS_box.png",height = 1000,width = 1400)
naics.box.data %>% filter(catAward == 'Award') %>% 
  ggplot(data=.,aes(x=naicsTwo,y=numberOffers)) +
  geom_boxplot() + coord_flip() +xlab("Industry Sector") + 
  ylab("Offers") + ggtitle("Offers Received by Industry Sector")+
  annotate("text", label = "* Only Non-Vehicle Awards Shown", x = 1.5, y = 90, size = 3, colour = "black") +
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13,face="bold"),title=element_text(size=14,face="bold"))

dev.off()

## This is going nowhere right now
png(filename = "fig/offers_received_dist_NAICS_freqpoly.png",height = 1000,width = 1400)
naics.box.data %>% filter(catAward == 'Award') %>% 
  ggplot(data=.,aes(x=naicsTwo,y=numberOffers)) +
  geom_freqpoly(stat = "identity") + coord_flip() +xlab("Number of Offers") + 
  ylab("NAICS") + ggtitle("Distribution of Offers Received by NAICS") +
  annotate("text", label = "* Only Non-Vehicle Awards Shown", x = 1.5, y = 90, size = 3, colour = "black")


thing <- naics.box.data %>% filter(catAward == 'Award')
qplot(x = numberOffers, y = ..count..,
      data = thing,
      geom = 'freqpoly',
      color = naicsTwo) +
  scale_x_continuous(lim = c(0, 100)) + 
  scale_y_continuous(lim = c(0,2500))

dev.off()


####
## NAICS color axis labels by High / Low contracting groups

grouplist <- unique(region.box.data$Contracting.Group.ID) %>% sort(decreasing = F)
colorlist <- rep("black",length(grouplist))
colorlist [grouplist %in% high] <- "red"
colorlist [grouplist %in% low] <- "turquoise3"

boldlist <- rep("plain",length(grouplist))
boldlist[grouplist %in% c(high,low)] <- "bold"

high <- c("R3 - PUBLIC BUILDINGS SERVICE","R2 - FEDERAL ACQUISITION SERVICE")
low <- c("R2 - PUBLIC BUILDINGS SERVICE","R9 - FEDERAL ACQUISITION SERVICE")

png(filename = "fig/3_offers_received_dist_regional_agencies_COLOR.png",height = 1000,width = 1400)

region.box.data %>% 
  ggplot(data=.,aes(x=Contracting.Group.ID,y=numberOffers)) +
  geom_boxplot() + coord_flip() +xlab("Regional Contracting Agency") + 
  ylab("Offers") + ggtitle("Offers Received by Regional Contracting Agency")+
  annotate("text", label = "* Only Non-Vehicle Awards Shown", x = 2.5, y = 90, size = 3, colour = "black") +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=13,face="bold"),
        title=element_text(size=14,face="bold"), axis.text.y = element_text(colour = colorlist,face=boldlist))

dev.off()


#groups chosen by visual inspection



comb.scatter.data <- region.frame %>% 
  mutate(holdOffers = ifelse(Number.of.Offers.Received == 999, NA,
                             Number.of.Offers.Received)) %>% 
  group_by(naicsTwo,Contracting.Group.ID,uniqueId,catAward) %>%
  select(holdOffers, Number.of.Offers.Received, uniqueId, Action.Obligation, Contracting.Group.ID,catAward) %>%
  summarize(numberOffers = max(holdOffers, na.rm = TRUE),
            amountDollars = sum(Action.Obligation,na.rm = T),
            numberActions = n()) %>%
  mutate(Grouping=ifelse( Contracting.Group.ID %in% low, "Low",
                          ifelse(Contracting.Group.ID %in% high,"High","None" ))) %>%
  filter(catAward == 'Award', Grouping != "None")




png(filename = "fig/4_offers_received_dist_NAICS_HiLow.png",height = 1000,width = 1400)
comb.scatter.data  %>% ungroup() %>%
  ggplot(data=.,aes(x=naicsTwo,y=numberOffers,color=Grouping)) +
  geom_point() + coord_flip() +
  ylab("Offers") + 
  xlab("Industry Sector") + ggtitle("Offers Received by Industry Sector") +
  annotate("text", label = "* Only Non-Vehicle Awards Shown", x = 1.5, y = 90, size = 3, colour = "black") +
  scale_colour_discrete( name="",breaks=c("High", "Low"),
                          labels=c("HIGH: R3 - PBS & R2 - FAS","LOW:  R2 - PBS & R9 - FAS")) +
  theme(axis.text=element_text(size=11),axis.title=element_text(size=13,face="bold"),
        title=element_text(size=14,face="bold"))
dev.off()



