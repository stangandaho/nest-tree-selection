tree_sn <- function(tree_name) {
  case_when(
    tree_name == "Nyala Berry" ~ "Xanthocercis zambesiaca",
    tree_name == "Schotia brachypetala" ~ "Schotia brachypetala",
    tree_name == "Marula" ~ "Sclerocarya birrea",
    tree_name == "Ledwood" ~ "Combretum imberbe",
    tree_name == "False Marula" ~ "Lannea schweinfurthii",
    tree_name == "Jackal berry" ~ "Diospyros mespiliformis",
    tree_name == "Euphorbia" ~ "Euphorbia tirucalli",
    tree_name == "Tambotie" ~ "Spirostachys africana",
    tree_name == "Olive tree" ~ "Olea europaea",
    tree_name == "Acacia robusta" ~ "Vachellia robusta",
    tree_name == "Pod Mahogany" ~ "Afzelia quanzensis",
    tree_name == "Knob Thorn" ~ "Senegalia nigrescens",
    tree_name == "Silver cluster leaf" ~ "Terminalia sericea",
    tree_name == "Sycamore fig" ~ "Ficus sycomorus",
    tree_name == "Weeping boerbeen" ~ "Schotia brachypetala",
    tree_name == "Variable bush Willow" ~ "Combretum collinum",
    tree_name == "Albezia" ~ "Albizia adianthifolia",
    tree_name == "Apple leaf tree" ~ "Philenoptera violacea",
    tree_name == "Nyala berry" ~ "Xanthocercis zambesiaca",
    tree_name == "Jackle berry" ~ "Diospyros mespiliformis",
    tree_name == "Apple leaf" ~ "Philenoptera violacea",
    tree_name == "Acacia nig" ~ "Senegalia nigrescens",#"Acacia nigrescens",
    tree_name == "Bohemia calfene" ~ "Bohemia calfene",#?
    tree_name == "Scotia" ~ "Schotia brachypetala",
    tree_name == "Knob thorne" ~ "Senegalia nigrescens",
    tree_name == "Silver cluster" ~ "Terminalia sericea",
    tree_name == "F. Marula" ~ "Lannea schweinfurthii",
    tree_name == "Belenilees" ~ "Balanites maughamii",
    T ~ tree_name
  )
}
