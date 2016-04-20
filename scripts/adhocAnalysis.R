setaside <- fpds %>%
    
    
ggplot(data = fpds, aes(x = Type.of.Set.Aside, y = Action.Obligation)) +
    geom_bar(stat = "identity") +
    facet_wrap(~catAward) +
    coord_flip()

ggplot(data = fpds, aes(x = Type.of.Set.Aside)) +
    geom_bar() +
    facet_wrap(~compCat) +
    coord_flip()