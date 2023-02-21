
## ######################################################################## ##
## This script collects all the search terms for SDGs at the target levels  ##
## ######################################################################## ##

#' **Last update on**:  3/31/2022
#'
#' **New changes**:
#'
#' Compare to the earlier version, we made the following changes
#'
#' 1. Instead of combining multiple term lists by `OR` for one particular target,
#'    it is more intuitive and accurate to add each alternative term list to the search
#'    term table or database directly.
#'
#' 2. Added `Look around` function to more accurately match SDG targets.
#'
#'
#'
#'
#'

###
# *   --> blank
# ()  --> ""
# AND --> ,




#' Use `AND` to Concatenate a Vector of Terms
#'
#' @param v a vector of characters
#'
#' @importFrom magrittr %>%
#'
#' @return A character
#' @export
#'
#' @examples
#' words <- c('apple', 'bean', 'food')
#' func_AND_vector(v= words)
func_AND_vector <- function(v){
  pat <- paste0("(?=.*(?:", v, "))", collapse="")
  return(pat)
}


#' Use `OR` to Concatenate a Vector of Terms
#'
#' @param v a vector of characters
#'
#' @importFrom magrittr %>%
#'
#' @return A character
#' @export
#'
#' @examples
#' words <- c('apple', 'bean', 'food')
#' func_AND_vector(v= words)
func_OR_vector <- function(v){
  pat <- paste0(v, collapse = "|")
  pat <- paste0("(", pat, ")")
  # print(pat)
  return(pat)
}


## similar to `func_AND_vector`, and mainly used for the final `regular expressions` for a target
func_AND_plus <- function(v){
  pat <- func_AND_vector(v)
  target <- paste0("^", pat, ".+")
  # print(target)
  return(target)
}


## To exclude certain terms -- used when exclude terms for a `AND` pattern by `func_AND_plus` function
## - 1. this is an ealier version
# func_to_exclude_terms <- function(which_sdg_term, terms_to_exclude) {
#   terms_replacement <- paste0("(?!.*", paste(terms_to_exclude, collapse = ")(?!.*"), ")")
#
#   if ( stringr::str_detect(which_sdg_term, "\\.\\+") )  {
#     which_sdg_term <- gsub(pattern = "\\.\\+", replacement = terms_replacement, which_sdg_term) ## to exclude these keywords
#   } else {
#     which_sdg_term <- paste(which_sdg_term, terms_replacement)
#   }
#   return(which_sdg_term)
# }

## - 2. this is an updated version
func_to_exclude_terms <- function(which_sdg_term, terms_to_exclude) {
  pat_exclude <- paste0("^(?!.*(?:", paste(terms_to_exclude, collapse = "|"), ")).*") ## this is more concise and efficient
  pat_and     <- which_sdg_term

  ## put `exclude` pattern before the `AND` pattern
  pat_exclude_and <- paste0(pat_exclude, pat_and)
  ## remove the ".*" and "^" between `exclude` pattern and the `AND` pattern
  pat_exclude_and <- gsub('\\.\\*\\^', '', pat_exclude_and)

  return(pat_exclude_and)
}
### test code
# exclude <- c('a', 'b')
# terms_to_exclude <- exclude
# func_to_exclude_terms(which_sdg_term  = SDG1_4, terms_to_exclude = exclude)



## Call the `look around function`
# source('./Code/function_lookaround_nearby_n.R') ## the following code is copied from this source
condition1 <- "\\s"                 ## a space after the 1st word
condition2 <- "(?:\\w+\\s)"         ## a word character with a space after it

#' @title Look Around
#'
#' @description Look around to match pattern in a sentence
#'
#' @param word_ls1 is a string, which includes a list of words connected by "|" that indicates 'OR'
#' @param word_ls2 is a string, which includes a list of words connected by "|" that indicates 'OR'
#' @param n        is a number, indicates the number of words to look around
#' @param exclude  is a vector, including a list of words to be excluded from match
#' @param third_AND_string similar to word_ls1 or word_ls2, it is a string that includes a list of words connected by "|" that indicates 'OR'
#'
#' @importFrom magrittr %>%
#'
#' @return A regex string
#' @export
#'
#' @examples
#' con1 <- c('apple', 'bean', 'food')
#' con2 <- c('big', 'delicious')
#' lookaround_nearby_n(word_ls1 = con1, word_ls2 = con2, n = 2, exclude = "", third_AND_string = "")
lookaround_nearby_n <- function(word_ls1, word_ls2, n, exclude = "", third_AND_string = "") {
  pat1 <- paste0(
    paste0('(?:', word_ls1, ')'),  ## the 1st word list
    condition1,                    ## a space after the 1st word
    condition2,                    ## other strings after the space (`condition2`)
    "{0,", n, "}",                 ## Matches `condition2` at most n times
    "(?=(?:", word_ls2, "))"       ## the 2nd word list
  );

  pat2 <- paste0(
    paste0('(?:', word_ls2, ')'),
    condition1,
    condition2,
    "{0,", n, "}",
    "(?=(?:", word_ls1, "))")

  ## format the 'pat_exclude' ---------------------------------------------------------- #
  nchar_exclude <- paste(exclude, sep = "", collapse = "")
  nchar_exclude <- nchar(nchar_exclude)
  ## words to be excluded, default is ''
  if (nchar_exclude < 1) {
    pat_exclude <- exclude
  } else {
    # pat_exclude <- paste0("(?!.*", paste(exclude, collapse = ")(?!.*"), ")")
    pat_exclude <- paste0("^(?!.*(?:", paste(exclude, collapse = "|"), ")).*") ## more concise and efficient
  }


  ## format the 3rd `AND` PATTERN ------------------------------------------------------ #
  if (nchar(third_AND_string) < 1) {
    pat_and <- third_AND_string
  } else {
    pat_and <- paste0('^(?=.*(?:', third_AND_string, ')).+')
  }



  ## combine `pat_exclude` and `pat_and` ----------------------------------------------- #
  pat_exclude_and <- paste0(pat_exclude, pat_and)
  if( nchar(pat_exclude)<1 ) {
    pat_exclude_and <- pat_exclude_and
  } else {
    pat_exclude_and <- gsub('\\.\\*\\^', '', pat_exclude_and); ## remove the ".*^" between `pat_exclude` and `pat_and`
    # pat_exclude_and
  }

  ## assemble the pattern -------------------------------------------------------------- #
  pat <- paste0(
    pat_exclude_and,
    '(',                      ## put "pat1|pat2" in a pair of large brackets
    '(', pat1, ')', '|',
    '(', pat2, ')', ')');

  return(pat)
}




################################################################################################## #
# Indirect mention                                                                              ####
################################################################################################## #


## Auxiliary list ------



### ag_ls ----
ag_ls = paste("agricultur\\S*|\\bagro\\S*|agronom\\S*|agrarian\\S*|\\bcrop\\S*|food\\S*|farm\\S*",
              "forestry|pastoral|pasture|\\bfisher\\S*|horticulture|cultivation\\S*|husbandry|grazing",
              "\\bgrain\\S*|planting|\\btillage|\\btilling", sep = "|")


### child_ls ----
child_ls = paste("child\\S*|\\bteen.?\\b|teenage\\S*|juvenile\\S*|youth|young|\\bminor.?\\b|under 5|under five|underfive|before fifth|adolescen\\S*",
                 "\\bgirl\\S*|\\bboy\\b|\\bboys\\b|\\bkid\\b|\\bkids\\b|\\bkiddo|\\bkiddy|\\bkiddies|offspring\\S*",
                 sep = "|")


### company_ls ----
company_ls <- "\\bcompany\\b|\\bcompanies|\\bfirm.?\\b|\\bcorporate\\b|\\bcorporation\\S*|business|enterprise\\S*"


### death_ls ----
death_ls = "mortal\\S*|death|\\bdead|\\bdie\\b|\\bdies\\b|\\bdied\\b|\\bdying|fatal|deceas\\S*|\\bdemis\\S*|\\bkilled|lethal|perish\\S*|lose.*life|pass away"


### disaster_ls ----
disaster_ls = paste(
  "shock\\S*|disaster.?|catastroph\\S*|cataclysm|casualt\\S*|calamit\\S*|hazard\\S*|mishap\\S*",
  "hurricane\\S*|tornado\\S*|cyclone\\S*|flood\\S*|drought\\S*|\\bharm.?\\b|\\bharmed\\b",
  "wild.?fire|fire.?storm|Extreme event.?|extreme climate|extreme weather|extreme temperature|extreme heat|heat.?wave|cold wave",
  "extreme precipitation|lightning|thunderstorm.?|ice storm.?|blizzard\\S*|hailstorm\\S*|tropical storm.?|earthquake.?",
  "volcanic eruption.?|limnic eruption.?|exploding lake.?|landslide.?|mudslide.?|debris flow.?|tsunami\\S*|sinkhole\\S*|subsidence\\S*",
  sep = "|")
## -> avoid "risk", which is too broad


### disability_ls ----
disability_ls = "Disabled|disabilit\\S*|handicapped|incapacitated|crippled|\\bblind\\b"


### economic_ls ----
economic_ls = paste("econom\\S*|\\bGDP\\b|gross domestic product",
                    "fiscal|monetary",
                    sep = "|")
## taking away these synonyms due to excessive matches: (financ|profit|revenue|income|\\bgain|proceed|yield|dividend|\\bearning|\\bmonies|budget|return|\\bvalu)


### emission_ls ----
emission_ls = paste("\\bemission.?|\\bCO2\\b|carbon|green.?house|\\bGHG\\b|G\\.H\\.G\\.|Halocarbons",
                    "chlorofluorocarbon.?|\\bcfc\\b|c\\.f\\.c\\.|\\bCFCs\\b",
                    "water vapour|methane|\\bCH4\\b|nitrous oxide|\\bN2O\\b|Nitrogen Oxides|\\bNOx\\b",
                    "\\bozone.?|\\bO3\\b|fluorinated gas",
                    "perfluorocarbon.?|\\bPFCs\\b|hydrofluorocarbon.?|\\bHFCs\\b|sulphur hexafluoride|\\bSF6\\b",
                    "nitrogen trifluoride|\\bNF3\\b",
                    sep = "|")

### equal_ls ----
equal_ls = paste("equal\\S*|\\bequitab\\S*|\\bequalit\\S*|\\bun.?bias\\S*|unprejudic\\S*",
                 "\\bfair\\b|\\bfairly|\\bfairness\\S*|impartial\\S*|\\bparity",
                 sep = "|")


### discrimination_ls ----
discrimination_ls = "discrimin\\S*|\\bbias\\S*|prejudice\\S*|\\bunfair\\S*|inequal\\S*|inequit\\S*|disparit\\S*|imparit\\S*|stereotype\\S*"


### female_ls ----
female_ls = "woman|women|\\bgirl.?|female.?|\\blady\\b|\\bladies\\b|\\bmaid.?"

### increase_ls ----
increase_ls = paste("accelerat\\S*|ascend\\S*|\\badvanc\\S*|augment\\S*|\\bamplif\\S*|magnif\\S*|burgeon\\S*",
                    "\\bclimb\\S*|better|boost\\S*|\\bboom\\S*|\\bdouble\\S*|escalat\\S*",
                    "expansion\\S*|\\bexpand\\S*|\\benlarge\\S*|\\bgrow\\S*|\\bhike|\\bhiking|heighten\\S*|improv\\S*",
                    "increas\\S*|foster\\S*|\\bfortif\\S*|facilitat\\S*|increment\\S*|jump\\S*|\\blift\\S*",
                    "\\brais\\S*|\\brise.?\\b|rising|\\brose\\S*|risen|\\brocket\\S*|skyrocket\\S*",
                    "\\bsurg\\S*|\\bsoar\\S*|promot\\S*|proliferat\\S*",
                    "strengthen\\S*|enhanc\\S*|aggrandiz\\S*|reinforc\\S*|upsurg\\S*|upward\\S*",
                    sep = "|")

### infrastructure_ls ----
infrastructure_ls = paste(
  "infrastructure.?|\\bhouse\\b|\\bhouses\\b|\\bhousing\\b|architecture.?|construction.?|freight\\S*|transport\\S*",
  "\\broad.?\\b|\\broadway\\S*|highway|expressway|freeway|speed.?way|motorway|\\brail\\S*|\\bport.?\\b|power plant\\S*",
  "steam network.?|gas network.?|\\bdams",
  "bridge.?|airport.?|aviation\\S*|sewer\\S*|broadband\\S*|cellular infrastructure\\S*|internet\\S*|telecommunication.?",
  "electricity|power grid\\S*|electrical grid\\S*|\\bpark.?\\b",
  "tunnel\\S*|water supply|\\bCanal\\S*|Hospital\\S*|Irrigation scheme\\S*|Levee\\S*|Lighthouse\\S*",
  "Pipeline.?|\\btransit.?\\b|Public space\\S*|Sewage treatment\\S*|Sewerage\\S*|Sluice\\S*|Solid waste\\S*|Utilities",
  "\\bWeir\\S*|waterway.?|harbor\\S*|harbour\\S*|\\bdock\\S*|\\bdike\\S*|\\bdyke\\S*",
  sep = "|")


### illegal_ls ----
illegal_ls = "illegal|criminal|illegitimate|illicit|unauthorized|unlawful|unlicensed|unconstitutional|prohibited|forbidden|banned"

### LGBT_ls ----
LGBT_ls = "\\bLGBT\\S*|\\blesbian|\\bgay\\b|bisexual|transgender|\\bQueer\\b|Questioning|Intersex|\\bAllies\\b|\\bAsexual\\b|Pansexual"



### renewable_ls ----
renewable_ls_x = c(
  paste("\\badvanc\\S*|alcohol|modern|\\bclean\\b|\\brenewable|alternative\\b|ethanol|biorefinery|\\bwind\\b|solar|biomass|nuclear",
        "\\bsun\\b|sunlight.?\\b|\\btide.?\\b|tidal",
        "\\bwave.?\\b|\\bwood.?\\b|thermal|geothermal|algae|\\balgal|\\bHydro\\S*|hybrid|hydrogen|synthetic\\S*",
        "\\bgreen\\b|\\bgreener\\b|sustainable",
        "enviro.?friendly|eco.?friendly|Environmentally.?Friendly|Environment.?Friendly",
        sep = "|"),
  "\\benergy|\\benergies|\\bfuel.?\\b|\\bpower\\b")
renewable_ls_y = paste("(\\brenewables|biofuel|bioenergy|biodiesel|bio.?gas|bioethanol|hydropower",
                       "hydroelectric|electrofuel|\\be.?fuel|photovoltaic|wind farm\\S*|offshore wind|wind turbine\\S*|solar panel\\S*)",
                       sep = "|")
renewable_ls_x <- func_AND_vector(renewable_ls_x) ## not suitable to use `func_AND_plus` here
renewable_ls   <- func_OR_vector(c(renewable_ls_y, renewable_ls_x))



### finance_ls ----
finance_ls <- paste("financ\\S*|\\bfund.?\\b|\\bfunding\\b|\\bassist\\S*|budget\\S*|\\baid.? to",
                    "government aid|state aid|federal aid|national aid",
                    "invest\\S*|compensation\\S*|donor\\S*|donat\\S*",
                    "expenditure.?|\\bgrant.?\\b|\\bloan.?\\b|official flow.?|\\bsubsidy|\\bsubsidies|\\bsubsidiz\\S*|\\bsubsidis",
                    "Corporate sponsorship.?",
                    sep = "|")
## -> be careful when using invest*, might mistakenly match "investigation|investigate"


financial_exclude_ls =
  paste("Financial risk\\S*|Financial Disclosure\\S*|Financial Stability|financial sector\\S*|financial group\\S*",
        "financial crisis|financial condition\\S*|financial impact\\S*|financial inclusive\\S*|financial statement\\S*",
        "Financial Times|financial press|financial result\\S*|financial instrument\\S*|Financial Review\\S*",
        "financial assumption\\S*|finance corporation\\S*",
        sep = "|")
financial_exclude_ls <- strsplit(x = financial_exclude_ls, split = "\\|")
financial_exclude_ls <- unlist(financial_exclude_ls)


### support_ls ----
support_ls  = paste(finance_ls,
                    "support\\S*|transfer.?\\b|boost\\S*|bolster\\S*|\\bstimulat\\S*|\\bfortif\\S*|help\\S*",
                    "strengthen\\S*|enhanc\\S*|aggrandiz\\S*|reinforce\\S*|uphold",
                    sep = "|")

### urban_ls -----
urban_ls = paste("urbaniz\\S*|\\burban|\\bcity|\\bcities|Megacit\\S*|human settlement.?",
                 "land consumption.?|land.?use|metropoli\\S*|town\\S*|municipal\\S*|oppidan\\S*",
                 sep = "|")

### policy_ls ----
policy_ls = paste("action.?|activit\\S*|administ\\S*|agreement.?|approach\\S*|arrangement.?|blueprint.?|\\bdeal.?|\\bguide\\S*|govern\\S*",
                  "method.?|manag\\S*|\\bpolicy|\\bpolicies|\\bplan\\b|\\bplans\\b|planning|proced\\S*|program\\S*|project.?\\b",
                  "practice.?|procedure.?|propos\\S*|protocol.?|\\brule.?\\b|\\bregulat\\S*",
                  "\\blaw.?|\\btreat\\S*|\\bright.?|constitution.?|initiative.?|scheme.?|strateg\\S*", sep = "|")

### poverty_ls ----
poverty_ls = "poverty|\\bpoor\\S*|impover\\S*|destitution.?|underprivileg\\S*|necessitous\\S*|homeless|\\bhobo.?|low.?income|lower.?income|underclass\\S*"

### reduce_ls ----
reduce_ls = paste("alleviat\\S*|\\babate\\S*|abridge\\S*|\\bavert\\S*|abolish\\S*|eradicat\\S*|eliminat\\S*",
                  "\\beras\\S*|\\bease\\S*|\\bend.?\\b|\\bended\\b|rid of|phase out|\\bwipe out",
                  "\\breduc\\S*|\\bcut.?\\b|\\bcurb\\S*|curtail\\S*|declin\\S*|decreas\\S*|diminish\\S*|diminution\\S*|\\bdrop\\S*|dwindl\\S*",
                  "\\bfade\\b|fall off|go down|goes down|went down|tone down|die down|cut down",
                  "\\bhalt\\S*|hinder\\S*|\\bhamper\\S*|impede\\S*",
                  "\\binhibit.?\\b|\\binhibited\\b|\\binhibiting",
                  "lessen\\S*|mitigat\\S*|slash\\S*|shrink\\S*|\\bstop\\S*|slump\\S*|\\btrim\\S*|weaken\\S*",
                  "\\bno\\b|\\bzero|prevent\\S*|prohibit\\S*|minimis\\S*|minimiz\\S*", sep = "|")

### ls_hazardous_waste_chemicals ----
ls_hazardous_waste_chemicals = paste(
  "\\bPhenol|\\bEthane|\\bEthene|\\bCresol|\\bIndeno|\\bEndrin|\\bDibenz|\\bSilvex|\\bArsine|\\bPropen|\\bCumene|\\bXylene|\\bAldrin|\\bIsolan",
  "\\bOxamyl|\\bKepone|\\bThiram|\\bBarban|\\bBenzene|\\bEthanol|\\bDiethyl|\\bPropane|\\bMercury|\\bMethane|\\bOxirane|\\bDibenzo|\\bAniline",
  "\\bEthanal|\\bAcetone|\\bBrucine|\\bDinoseb|\\bIsodrin|\\bPhorate|\\bFamphur|\\bTirpate|\\bChloral|\\bLindane|\\bSafrole|\\bToluene|\\bBenomyl",
  "\\bPropham|\\bacetone|\\bbenzene|\\bPyridine|\\bThiourea|\\bPlumbane|\\bThallium|\\bEthanone|\\bPentanol|\\bNicotine|\\bFurfuran|\\bFurfural",
  "\\bMethanol|\\bAcrolein|\\bCyanogen|\\bDieldrin|\\bFluorine|\\bMethomyl|\\bAldicarb|\\bPhosgene|\\bAmitrole|\\bAuramine|\\bChrysene|\\bCreosote",
  "\\bDiallate|\\bCarbaryl|\\bPropoxur|\\bmethanol|\\bpyridine|\\bBenzamide|\\bManganese|\\bArgentate|\\bGuanidine|\\bAcetamide|\\bChlordane|\\bHydrazine",
  "\\bAziridine|\\bThiofanox|\\bEndothall|\\bParathion|\\bPhosphine|\\bToxaphene|\\bMetolcarb|\\bDimetilan|\\bPromecarb|\\bAzaserine|\\bBenzidine",
  "\\bMelphalan|\\bPronamide|\\bReserpine|\\bBromoform|\\bTriallate|\\bVinylamine|\\bEthanamine|\\bPiperidine|\\bStrychnine|\\bThiophenol|\\bDisulfoton",
  "\\bDimethoate|\\bEndosulfan|\\bHeptachlor|\\bSelenourea|\\bCarbofuran|\\bMethiocarb|\\bAcrylamide|\\bChloroform|\\bDaunomycin|\\bIsosafrole",
  "\\bPhenacetin|\\bResorcinol|\\bBendiocarb|\\bThiodicarb|\\bisobutanol|\\bAcetic acid|\\bBenzenamine|\\bStrychnidin|\\bNitric acid|\\bMethanamine",
  "\\bPyrrolidine|\\bNaphthalene|\\bThiophanate|\\bFormic acid|\\bCyclohexane|\\bEthyl ether|\\bEpinephrine|\\bMexacarbate|\\bCarbosulfan|\\bMitomycin C",
  "\\bParaldehyde|\\bTrypan blue|\\bCarbendazim|\\bethyl ether|\\bVanadic acid|\\bMethanethiol|\\bAcetaldehyde|\\bArsinic acid|\\bAcetonitrile",
  "\\bThiomethanol|\\bNitrobenzene|\\bAcrylic acid|\\bBenzenethiol|\\bBromoacetone|\\bDithiobiuret|\\bNitric oxide|\\bSodium azide|\\bZinc cyanide",
  "\\bFormparanate|\\bAcetophenone|\\bChlorambucil|\\bFluoranthene|\\bFormaldehyde|\\bLasiocarpine|\\bLead acetate|\\bMethoxychlor|\\bProsulfocarb",
  "\\bnitrobenzene|\\bGlucopyranose|\\bCarbamic acid|\\bHydroperoxide|\\bFulminic acid|\\bCarbonic acid|\\bSulfuric acid|\\bCyclohexanone|\\bDimethylamine",
  "\\bDipropylamine|\\bEthyl acetate|\\bAllyl alcohol|\\bDiethylarsine|\\bEthyleneimine|\\bEthyl cyanide|\\bThallic oxide|\\bPhysostigmine|\\bAcrylonitrile",
  "\\bChlornaphazin|\\bChlorobenzene|\\bMethyl iodide|\\bMalononitrile|\\bMethapyrilene|\\bThioacetamide|\\bTriethylamine|\\bchlorobenzene|\\bethyl acetate",
  "\\bethyl benzene|\\bcyclohexanone|\\bPropanoic acid|\\bSelenious acid|\\bPropanenitrile|\\bBenzenediamine|\\bEthylene oxide|\\bNitroglycerine",
  "\\bEthyl acrylate|\\bMethyl alcohol|\\bBarium cyanide|\\bCopper cyanide|\\bNickel cyanide|\\bPhenylthiourea|\\bSilver cyanide|\\bSodium cyanide",
  "\\bMethyl bromide|\\bVinyl chloride|\\bCrotonaldehyde|\\bDihydrosafrole|\\bCacodylic acid|\\bLead phosphate|\\bStreptozotocin|\\bToluenediamine",
  "\\bUracil mustard|\\bPhosphoric acid|\\bNaphthalenamine|\\bDiphosphoramide|\\bEthyl carbamate|\\bAcetyl chloride|\\bMethyl chloride|\\bZinc cyanide Zn",
  "\\bTetrahydrofuran|\\bCalcium cyanide|\\bBenzyl chloride|\\bEthanedinitrile|\\bFluoroacetamide|\\bNickel carbonyl|\\bTetraethyl lead|\\bBenzal chloride",
  "\\bChlorobenzilate|\\bEpichlorohydrin|\\bHexachlorophene|\\bLead subacetate|\\bEthanethioamide|\\btrifluoroethane|\\bBenzotrichloride|\\bCyanogen bromide",
  "\\bIsobutyl alcohol|\\bAmmonium picrate|\\bSulfur phosphide|\\bArsenic trioxide|\\bBeryllium powder|\\bCarbon disulfide|\\bHydrogen cyanide",
  "\\bHydrocyanic acid|\\bMethyl hydrazine|\\bMethyl parathion|\\bNitrogen dioxide|\\bOsmium tetroxide|\\bAldicarb sulfone|\\bCalcium chromate",
  "\\bCyclophosphamide|\\bDimethyl sulfate|\\bEthylenethiourea|\\bGlycidylaldehyde|\\bHexachloroethane|\\bHydrogen sulfide|\\bMaleic anhydride",
  "\\bMaleic hydrazide|\\bPropanedinitrile|\\bMethylthiouracil|\\bSelenium dioxide|\\bSelenium sulfide|\\bcarbon disulfide|\\bBenzeneethanamine",
  "\\bDiphosphoric acid|\\bFluoroacetic acid|\\bDiethylene glycol|\\bCarbamic chloride|\\bOsmium oxide OsO4|\\bCyanogen chloride|\\bMercury fulminate",
  "\\bHydrogen fluoride|\\bHydrofluoric acid|\\bMethacrylonitrile|\\bNickel cyanide Ni|\\bCopper cyanide Cu|\\bSilver cyanide Ag|\\bSodium cyanide Na",
  "\\bTetranitromethane|\\bArsenic pentoxide|\\bMethyl isocyanate|\\bNitrogen oxide NO|\\bPotassium cyanide|\\bPropargyl alcohol|\\bThiosemicarbazide",
  "\\bAmmonium vanadate|\\bPentachlorophenol|\\bMethylene bromide|\\bDibutyl phthalate|\\bDiethyl phthalate|\\bHexachlorobenzene|\\bPentachloroethane",
  "\\bMethyl chloroform|\\bTrichloroethylene|\\bHexachloropropene|\\bBendiocarb phenol|\\bCarbofuran phenol|\\btrichloroethylene|\\bCarbamothioic acid",
  "\\bNickel carbonyl Ni|\\bAluminum phosphide|\\bCarbon oxyfluoride|\\bCalcium cyanide Ca|\\bPhosphorus sulfide|\\bChloroacetaldehyde",
  "\\bNitrogen oxide NO2|\\bHydrogen phosphide|\\bVanadium pentoxide|\\bEthylene dibromide|\\bMethylene chloride|\\bDimethyl phthalate",
  "\\bEthyl methacrylate|\\bPentachlorobenzene|\\bPhthalic anhydride|\\bmethylene chloride|\\bMethyl ethyl ketone|\\bArsonous dichloride",
  "\\bMethyl methacrylate|\\bPotassium cyanide K|\\bCarbonic dichloride|\\bVanadium oxide V2O5|\\bDichloroethyl ether|\\bCarbonic difluoride",
  "\\bEthylene dichloride|\\bDiethylstilbesterol|\\bHexachlorobutadiene|\\bTetrachloroethylene|\\bmethyl ethyl ketone|\\bCarbamodithioic acid",
  "\\bPhosphorothioic acid|\\bBenzenebutanoic acid|\\bTetraphosphoric acid|\\bChromic acid H2 CrO4|\\bMethanesulfonic acid|\\bToluene diisocyanate",
  "\\bArsenic acid H3 AsO4|\\bArsenic oxide As2 O5|\\bArsenic oxide As2 O3|\\bDichloromethyl ether|\\bDichlorophenylarsine|\\bPropylene dichloride",
  "\\bHydrogen sulfide H2S|\\bCarbon tetrachloride|\\bcarbon tetrachloride|\\bCarbonochloridic acid|\\bThiodiphosphoric acid|\\bPhenylmercury acetate",
  "\\bThallium oxide Tl2 O3|\\bTrichloromethanethiol|\\bEthylidene dichloride|\\bPhosphorodithioic acid|\\bMethyl chlorocarbonate|\\bSelenium sulfide SeS2",
  "\\bMethyl isobutyl ketone|\\bDichloromethoxy ethane|\\bDiethylhexyl phthalate|\\bEthyl methanesulfonate|\\bOxiranecarboxyaldehyde",
  "\\bThallium chloride TlCl|\\btrichlorofluoromethane|\\bmethyl isobutyl ketone|\\bPhosphorofluoridic acid|\\bPentachloronitrobenzene",
  "\\bAcetic acid ethyl ester|\\bHydrazinecarbothioamide|\\bDichloroisopropyl ether|\\bDichlorodifluoromethane|\\bBenzenesulfonyl chloride",
  "\\bHexaethyl tetraphosphate|\\bPotassium silver cyanide|\\bTetraethyl pyrophosphate|\\bPhysostigmine salicylate|\\bFormetanate hydrochloride",
  "\\bChloromethyl methyl ether|\\bHexachlorocyclopentadiene|\\bchlorinated fluorocarbons|\\bCresols and cresylic acid|\\bDiisopropylfluorophosphate",
  "\\bDimethylcarbamoyl chloride|\\bTrichloromonofluoromethane|\\bThioimidodicarbonic diamide|\\bOctamethylpyrophosphoramide",
  "\\bMethyl ethyl ketone peroxide|\\bBenzenesulfonic acid chloride|\\bTetraethyldithiopyrophosphate|\\bEthylenebisdithiocarbamic acid",
  "\\bEthylene glycol monoethyl ether|\\bManganese dimethyldithiocarbamate",
  "\\bDDD\\b|\\bDDT\\b",
  sep = "|")

### developing_country_name_ls ----
developing_country_name_ls = paste(
  "Aruba|Afghanistan|Angola|Anguilla|Albania|United Arab Emirates|Argentina|Armenia|American Samoa",
  "French Southern Territories|Antigua and Barbuda|Azerbaijan|Burundi|Benin|Burkina Faso|Bangladesh|Bahrain|Bahamas",
  "Bosnia and Herzegovina|Belarus|Belize|Bolivia|Brazil|Barbados|Brunei Darussalam|Bhutan|Botswana|Central African Republic",
  "Chile|China|C.?te d.?Ivoire|Cameroon|Congo\\b|Cook Islands|Colombia|Comoros|Cabo Verde|Costa Rica|\\bCuba\\b|Cura.?ao|Cayman Islands",
  "Cyprus|Cyprus|Djibouti|Dominica|Dominican Republic|Algeria|Ecuador|Egypt|Eritrea|Western Sahara|Ethiopia|Fiji|Micronesia",
  "Gabon|Georgia|Ghana|Guinea|Gambia|Guinea.?Bissau|Equatorial Guinea|Grenada|Guatemala|Guam|Guyana|Heard Island and McDonald Islands",
  "Honduras|Haiti|Indonesia|\\bIndia\\b|\\bIran\\b|\\bIraq\\b|Jamaica|Jordan|Kazakhstan|Kenya|Kyrgyzstan|Cambodia|Kiribati|Saint Kitts and Nevis|Kuwait|\\bLao.?\\b",
  "Lebanon|Liberia|Libya|Saint Lucia|Sri Lanka|Lesotho|Macao|Saint Martin|Morocco|Moldova|Madagascar|Maldives|Mexico|Marshall Islands",
  "North Macedonia|\\bMali\\b|Myanmar|Montenegro|Mongolia|Northern Mariana Islands|Mozambique|Mauritania|Montserrat|Mauritius|Malawi|Malaysia",
  "Namibia|New Caledonia|Niger|Norfolk Island|Nigeria|Nicaragua|Niue|Nepal|Nauru|Oman|Pakistan|Panama|Pitcairn|Peru|Philippines|Palau",
  "Papua New Guinea|Puerto Rico|Paraguay|Palestine|French Polynesia|Qatar|Russia|Rwanda|Sudan|Senegal|Singapore|South Georgia and the South Sandwich Islands",
  "Saint Helena|Solomon Islands|Sierra Leone|El Salvador|Somalia|Serbia|South Sudan|Sao Tome and Principe|Suriname|Eswatini|Sint Maarten|Seychelles|Syrian",
  "Turks and Caicos Islands|Chad|Togo|Thailand|Tajikistan|Turkmenistan|Timor.?Leste|Tonga|Trinidad and Tobago|Tunisia|Turkey|Tanzania|Uganda|Ukraine|Uruguay",
  "Uzbekistan|Saint Vincent and the Grenadines|Venezuela|Virgin Islands|Viet Nam|Vanuatu|Wallis and Futuna|Samoa|Yemen|South Africa|Zambia|Zimbabwe|Somaliland",
  "Kosovo|Ashmore|Cartier|Siachen Glacier|North Korea",
  # func_AND_vector(c('Democratic', 'Korea')), ## this might cause errors
  sep = "|")


## "ARE" might match the common word "are" in a sentence - thus removed.
## "MAR" might match "Mar" (e.g., March 1st, Mar 1st)
## "PRE" might match "per year"
developing_country_iso3_ls = paste(
  "ABW|AFG|AGO|AIA|ALB|ARG|ARM|ASM|ATF|ATG|AZE|BDI|BEN|BFA|BGD|BHR|BHS|BIH|BLR|BLZ|BOL|BRA|BRB|BRN|BTN|BWA",
  "CAF|CHL|CHN|CIV|CMR|COD|COG|COK|COL|COM|CPV|CRI|CUB|CUW|CYM|CYP|CYP|DJI|DMA|DOM|DZA|ECU|EGY|ERI|ESH|ETH|FJI|FSM|GAB|GEO|GHA|GIN|GMB|GNB|GNQ|GRD|GTM|GUM|GUY",
  "HMD|HND|HTI|IDN|IND|IRN|IRQ|JAM|JOR|KAZ|KEN|KGZ|KHM|KIR|KNA|KOR|KWT|LAO|LBN|LBR|LBY|LCA|LKA|LSO|MAF|MDA|MDG|MDV|MEX|MHL|MKD|MLI|MMR|MNE|MNG|MNP",
  "MOZ|MRT|MSR|MUS|MWI|MYS|NAM|NCL|NER|NFK|NGA|NIC|NIU|NPL|NRU|OMN|PAK|PAN|PCN|PHL|PLW|PNG|PRK|PRY|PSE|PYF|QAT|RUS|RWA|SDN|SEN|SGP|SGS|SHN|SLB|SLE|SLV",
  "SOM|SRB|SSD|STP|SUR|SWZ|SXM|SYC|SYR|TCA|TCD|TGO|THA|TJK|TKM|TLS|TON|TTO|TUN|TUR|TZA|UGA|UKR|URY|UZB|VCT|VEN|VIR|VNM|VUT|WLF|WSM|YEM|ZAF|ZMB|ZWE",
  sep = "|")
developing_country_iso3_ls <- gsub('\\|', '\\\\b|\\\\b', developing_country_iso3_ls)
developing_country_iso3_ls <- paste0('\\b', developing_country_iso3_ls, '\\b')
developing_country_iso3_ls


### The 2-letter country code can match many relevant texts, so we would rather not use this.
# developing_country_iso2_ls = paste(
#   "AW|AF|AO|AI|AL|AE|AR|AM|AS|TF|AG|AZ|BI|BJ|BF|BD|BH|BS|BA|BY|BZ|BO|BR|BB|BN|BT|BW|CF|CL|CN|CI|CM|CD|CG|CK|CO|KM|CV|CR|CU|CW|KY|CY|CY|DJ|DM|DO|DZ|EC|EG|ER|EH",
#   "ET|FJ|FM|GA|GE|GH|GN|GM|GW|GQ|GD|GT|GU|GY|HK|HM|HN|HT|ID|IN|IR|IQ|JM|JO|KZ|KE|KG|KH|KI|KN|KR|KW|LA|LB|LR|LY|LC|LK|LS|MO|MF|MA|MD|MG|MV|MX|MH|MK|ML|MM|ME|MN",
#   "MP|MZ|MR|MS|MU|MW|MY|NA|NC|NE|NF|NG|NI|NU|NP|NR|OM|PK|PA|PN|PE|PH|PW|PG|PR|KP|PY|PS|PF|QA|RU|RW|SD|SN|SG|GS|SH|SB|SL|SV|SO|RS|SS|ST|SR|SZ|SX|SC|SY|TC|TD|TG",
#   "TH|TJ|TM|TL|TO|TT|TN|TR|TZ|UG|UA|UY|UZ|VC|VE|VI|VN|VU|WF|WS|YE|ZA|ZM|ZW",
#   sep = "|")
# developing_country_iso2_ls <- gsub('\\|', '\\\\b|\\\\b', developing_country_iso2_ls)
# developing_country_iso2_ls <- paste0('\\b', developing_country_iso2_ls, '\\b')
# developing_country_iso2_ls


developing_country_group_ls =
  c(paste("developing|least.?develop\\S*|less.?develop\\S*|underdevel\\S*|\\bpoor|low.?income|lower.?income|small island\\S*",
          "africa\\S*|\\bimpover\\S*|\\bpover\\S*|\\bemergent\\S*",
          sep = "|"),
    "countr\\S*|\\bnation.?|\\bstate.?\\b")
developing_country_group_ls = func_AND_vector(developing_country_group_ls)

developing_country_ls = func_OR_vector(v = c(developing_country_name_ls,
                                             developing_country_iso3_ls,
                                             # developing_country_iso2_ls,
                                             'global south|Third World|Pacific Alliance'
                                             # developing_country_group_ls
))
developing_country_ls <- paste0('(', developing_country_ls, ')|(', developing_country_group_ls, ')')## was used




## 1. No Poverty -----------------------------------------------------------------------------------

SDG1_1 = c(poverty_ls,
           reduce_ls,
           "extreme|\\bmost\\b|excess\\S*|exceeding|\\bhigh\\b|\\bhuge\\b|great|\\bover\\b|remarkabl\\S*|striking|\\bsevere|serious")

SDG1_2 = c(poverty_ls,
           reduce_ls)

SDG1_3 = c(paste(poverty_ls, "the vulnerable.?", sep = "|"),
           "assure|\\bcare.?\\b|\\bcover\\S*|preserve\\S*|protect\\S*|safeguard\\S*|secure|shelter.?|shield\\S*|support\\S*")
SDG1_1 <- func_AND_plus(SDG1_1)
SDG1_2 <- func_AND_plus(SDG1_2)
SDG1_3 <- func_AND_plus(SDG1_3)



SDG1_4 = c("access to|\\brights to",
           paste("resource.?\\b|basic service.?|property|inheritance|ownership|\\bland|financial service.?",
                 "\\bestate\\S*|\\bhome\\b|\\bhouse\\b|\\bhouses\\b|\\bhousing\\b|\\bfarms", sep = "|"))
SDG1_4 <- func_AND_plus(SDG1_4)
SDG1_4 <- func_to_exclude_terms(which_sdg_term = SDG1_4,
                                terms_to_exclude = c(financial_exclude_ls, "Access Bank", "voting right\\S*", "human resource\\S*"))


SDG1_5 = c(paste(poverty_ls, "vulnerab\\S*", sep = "|"),
           disaster_ls,
           reduce_ls)
SDG1_5 <- func_AND_plus(SDG1_5)


SDG1_a_x = c(poverty_ls,
             "econom\\S*|financ\\S*|develop\\S*",
             paste(support_ls, "empower\\S*|relie\\S*|program\\S*|\\bpolicy|\\bpolicies", sep = "|"))
SDG1_a_y = c('government\\S*',
             'expenditure\\S*|\\bspend\\S*',
             "essential service.?|education\\S*|health|social protection.?")
SDG1_a_z = c('development cooperation.?',
             'program\\S*|\\bpolicy|\\bpolicies',
             reduce_ls,
             poverty_ls)
SDG1_a_x <- func_AND_plus(SDG1_a_x)
SDG1_a_y <- func_AND_plus(SDG1_a_y)
SDG1_a_z <- func_AND_plus(SDG1_a_z)


SDG1_b_x = c(poverty_ls,
             reduce_ls,
             paste(policy_ls, support_ls, sep = '|'))
SDG1_b_y = c('government\\S*',
             'expenditure\\S*|\\bspend\\S*',
             'women|\\bpoor|vulnerable.?')
SDG1_b_x <- func_AND_plus(SDG1_b_x)
SDG1_b_y <- func_AND_plus(SDG1_b_y)


##### #
# SDG1_a   <- func_OR_vector(c(SDG1_a_x, SDG1_a_y))
# SDG1_b   <- func_OR_vector(c(SDG1_b_x, SDG1_b_y))









## 2. Zero Hunger ----------------------------------------------------------------------------------

SDG2_1_x = c(reduce_ls, "hunger|undernourish\\S*|undernutrion|starv\\S*|famine\\S*|malnourish\\S*|malnutrition\\S*")
SDG2_1_y = c(reduce_ls, "food|nutrition.?", "insecurity|desert.?\\b|deprivation.?|deficien\\S*")
SDG2_1_z = c("food|nutrition.?", "access|safe|secur\\S*|nutritious|sufficient|\\bample|plentiful|abundant")

SDG2_1_x <- func_AND_plus(SDG2_1_x)
# SDG2_1_y <- func_AND_plus(SDG2_1_y)
SDG2_1_z <- func_AND_plus(SDG2_1_z)

temp <- SDG2_1_y
SDG2_1_y <- lookaround_nearby_n(word_ls1 = temp[2], word_ls2 = temp[3], third_AND_string = temp[1], n = 5)


temp    <- paste("malnutrition.?|malnourish|undernourish|undernutrition|under nourished|stunting",
                 "wasting|overweight\\S*|underweight\\S*|\\bpolio\\S*|paralysis\\S*|tephromyelitis\\S*",
                 "nutrition\\S*|\\banaem\\S*|\\banem\\S*",
                 sep = "|")
SDG2_2_x = c(temp,
             paste(child_ls, "infant\\S*|neonate\\S*|newborn|baby|babies|toddler|older", sep = '|'))
SDG2_2_y = c(temp,
             "pregnan\\S*|lactat\\S*",
             "women|woman")
SDG2_2_x <- func_AND_plus(SDG2_2_x)
SDG2_2_y <- func_AND_plus(SDG2_2_y)



SDG2_3_x = c(increase_ls,
             ag_ls,
             "productiv\\S*|production.?|income")
SDG2_3_y = c("access to",
             ag_ls,
             "\\bland|land tenure right.?|land right.?|land own\\S*|resource.?|\\binput.?|knowledge\\S*|financial service.?|market.?|opportunit\\S*|employ\\S*")
SDG2_3_z = c("\\breform\\S*",
             ag_ls,
             "\\bland\\S*")
SDG2_3_x <- func_AND_plus(SDG2_3_x)
SDG2_3_y <- func_AND_plus(SDG2_3_y)
SDG2_3_z <- func_AND_plus(SDG2_3_z)



SDG2_4_x = c(ag_ls,
             "sustain\\S*|resilien\\S*|productiv\\S*|\\borganic|ecological")
SDG2_4_y = c(ag_ls,
             "adapt\\S*",
             paste(disaster_ls, "climate change|global warming|\\bwarm\\S*", sep = '|'))
SDG2_4_z = c("adapt\\S*",
             'temperat\\S*|\\bsea level',
             "\\bris\\S*")
SDG2_4_w = c("\\bland\\S*|\\bsoil\\b",
             "quality|fertility")

SDG2_4_x <- func_AND_plus(SDG2_4_x)
SDG2_4_y <- func_AND_plus(SDG2_4_y)
SDG2_4_z <- func_AND_plus(SDG2_4_z)
SDG2_4_w <- func_AND_plus(SDG2_4_w)



SDG2_5_x = c("genetic\\S*",
             "manag\\S*|maintain\\S*|diversif\\S*|diversity|conserv\\S*|secur\\S*",
             "cultivated|farmed|domesticated",
             "seed\\S*|plant bank.?|animal|wild species")
SDG2_5_y = c("manag\\S*|maintain\\S*|diversif\\S*|diversity|conserv\\S*|secur\\S*|\\butiliz\\S*",
             "local|\\btraditional\\b|indigen\\S*|aboriginal|native|endemic",
             "breed\\S*|bred")
SDG2_5_z = c("genetically modified|\\bGMO\\b",
             "food|agricultur\\S*")

SDG2_5_x <- func_AND_plus(SDG2_5_x)
SDG2_5_y <- func_AND_plus(SDG2_5_y)
SDG2_5_z <- func_AND_plus(SDG2_5_z)




SDG2_a_x = c(paste(support_ls, "\\badvanc\\S*|cooperat\\S*|collaborat\\S*|joint effort.?|\\bflow\\b|\\bflows\\b", sep = '|'),
             "rural|agricult\\S*",
             "infrastructure.?|research\\S*|investig\\S*|extension.?\\b|technolog\\S*")
SDG2_a_y = c(paste(support_ls, "\\badvanc\\S*|cooperat\\S*|collaborat\\S*|joint effort.?|\\bflow\\b|\\bflows\\b", sep = '|'),
             "\\bplant\\S*|\\bsoy\\S*|livestock\\S*|animal.?|cattle.?|\\bcow.?|\\bpig.?|sheep|\\bhog.?|horse.?|oxen|\\bherd\\S*|swine",
             "gene bank.?")

# SDG2_a_x <- func_AND_plus(SDG2_a_x)
temp <- SDG2_a_x
SDG2_a_x <- lookaround_nearby_n(word_ls1 = temp[2], word_ls2 = temp[3], n = 5, third_AND_string = temp[1])
SDG2_a_y <- func_AND_plus(SDG2_a_y)




SDG2_b_x = c(ag_ls,
             "export\\S*|\\btrade\\b|supply chain.?|value chain.?|market.?|business\\S*|commerce\\S*",
             paste("\\bsubsidy|\\bsubsidies|\\bsubsidiz|\\bsubsidis|restrict\\S*|distort\\S*|allowance\\S*", support_ls, sep = '|'))
SDG2_b_y = "Doha Development Round|Doha Round|\\bDDR\\b|D\\.D\\.R\\."
SDG2_b_x <- func_AND_plus(SDG2_b_x)

SDG2_c   = c(ag_ls, "market.?|price",
             "volatil\\S*|anomal\\S*|change.?|unstable|unsettled|elastic|elusive")
SDG2_c   <- func_AND_plus(SDG2_c)


### may no need to put them together ------------ #

# SDG2_1   <- func_OR_vector(c(SDG2_1_x, SDG2_1_y, SDG2_1_z))
# SDG2_2   <- func_OR_vector(c(SDG2_2_x, SDG2_2_y))
# SDG2_3   <- func_OR_vector(c(SDG2_3_x, SDG2_3_y, SDG2_3_z))
# SDG2_4   <- func_OR_vector(c(SDG2_4_x, SDG2_4_y, SDG2_4_z, SDG2_4_w))
# SDG2_5   <- func_OR_vector(c(SDG2_5_x, SDG2_5_y, SDG2_5_z))
# SDG2_a   <- func_OR_vector(c(SDG2_a_x, SDG2_a_y))
# SDG2_b   <- func_OR_vector(c(SDG2_b_x, SDG2_b_y))










## 3. Good Health and Well-being  ------------------------------------------------------------------


SDG3_1 = c(paste("matern\\S*|antenatal\\S*|birth|gestational|parturient", func_AND_plus(c("post","natal|partum")), sep = "|"),
           paste(death_ls, "health.?care|complica\\S*|depress\\S*", sep = "|"),
           reduce_ls)

SDG3_2 = c(paste(child_ls, "newborn\\S*|foetal|premature|preterm|pediatric\\S*|perinatal|infant.?|neonat\\S*", sep = '|'),
           paste(death_ls, "syndrome|wellness|well.?being", sep = '|'),
           reduce_ls)

SDG3_1 <- func_AND_plus(SDG3_1)
SDG3_2 <- func_AND_plus(SDG3_2)


temp <- paste(
  "epidemic\\S*|pandemic|outbreak.?|communicable disease.?|\\binfect\\S*|contagious|endemic",
  "\\bSARS\\b|acute respiratory syndrome|zika|Zikv\\S*",
  "dengue|schistosomiasis|ebola|measles|cholera|Chagas\\S*|\\bAIDS\\b|\\bHIV\\b|Acquired immunodeficiency syndrome",
  "Human immunodeficiency virus",
  "Yellow fever|Middle East respiratory syndrome|MERS-CoV|Antiretroviral|tuberculos\\S*|malaria",
  "tropical disease.?|trypanosom\\S*|hepatit\\S*|Lyme disease\\S*",
  "COVID|2019.?nCoV|SARS.?CoV.?2|SARS.?CoV2|HCoV.?2019|\\bHCOV\\b|NCOVID.?19|coronavirus|corona virus",
  "water borne|water.?borne",
  "sexual\\S* transmi\\S*", sep = "|"
)
SDG3_3 = c(temp, reduce_ls)
SDG3_3 <- func_AND_plus(SDG3_3)



SDG3_4_x1 = c(
  paste("non.?communicable",
        "\\bNCD\\b|cardiovascular|heart attack\\S*|myocard\\S* infarct\\S*|cerebrovascular accident\\S*|\\bCVA\\b",
        "cancer|carcinoma\\S*|lung emphysema\\S*|neoplasm\\S*|tumor\\S*|tumour\\S*",
        "diabet\\S*|copd|coad|asthma\\S*|diarrhea|diarrhoea|dysentery|\\bobes\\S*|suicid\\S*|stroke", sep = '|'),
  death_ls,
  reduce_ls);

SDG3_4_x2 = c(
  "chronic\\S*",
  "bronchitis|disease\\S*|hepatitis\\S*|pulmonary|respiratory|\\bstress",
  reduce_ls);

SDG3_4_y = c(
  "\\bmental\\S*|mood|psychological|psychiatric|psychosis|psychotic|\\bcogniti\\S*|emotion\\S*",
  "disorder|disease\\S*|\\bhealth\\b|wellness|illness\\S*|\\bsick\\S*|disabilit\\S*",
  reduce_ls)
SDG3_4_z = c('\\bCV\\b', "death|mortality", reduce_ls)

# SDG3_4_x1 <- func_AND_plus(SDG3_4_x1)
# SDG3_4_x2 <- func_AND_plus(SDG3_4_x2)
# SDG3_4_y  <- func_AND_plus(SDG3_4_y)

temp <- SDG3_4_x1
SDG3_4_x1 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, third_AND_string = temp[3])
temp <- SDG3_4_x2
SDG3_4_x2 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, third_AND_string = temp[3])
temp <- SDG3_4_y
SDG3_4_y  <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, third_AND_string = temp[3])
temp <- SDG3_4_z
SDG3_4_z  <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 2, third_AND_string = temp[3])



SDG3_5 = c("substance|drug|alcohol|drinking|drunk|ethanol|liquor|liqueur|booze|\\bwine|\\bbeer.?|Narcotic",
           "\\babus\\S*|misuse|misconduct.?|obsessive|addict\\S*|\\bharm.?\\b|\\bharmed\\b|hazardous|disorder\\S*",
           paste('\\btreatment.?', reduce_ls, sep = "|"))
SDG3_5 <- func_AND_plus(SDG3_5)


SDG3_6_x = c(
  paste("\\broad.?\\b|\\broadway|\\blane\\S*|avenue|street|\\bdrive\\b|\\bcar\\b|automobile|\\bmotor|vehicle.?",
        "highway|expressway|freeway|speed.?way|motorway|\\brail\\S*|congest\\S*|collision\\S*|collid\\S*",
        "\\bjam.?|traffic|transport.?\\b|transportation.?|\\btransit.?\\b|travel\\S*",
        sep = "|"),
  paste(death_ls,
        "fatal|injur\\S*|accident.?|adversit\\S*|bottleneck\\S*|damag\\S*|wound|catastroph\\S*",
        "calamit\\S*|casualt\\S*|crash|disaster.?",
        "\\bharm.?\\b|\\bharmed\\b|hazard\\S*|emergenc\\S*|traged\\S*",
        sep = "|"))
SDG3_6_y = "\\bTZD program\\S*|\\bTZD National Strateg\\S*|\\bTZD Strateg\\S*|Toward Zero Deaths|Road to Zero"
SDG3_6_x <- func_AND_plus(SDG3_6_x)


SDG3_7_x1 = c(
  "family planning|contracept\\S*|condom.?|diaphragm\\S*|Birth Control|intrauterine device\\S*|\\bIUD\\b|conception control",
  "access|availab\\S*|modern|information|informing|\\binform\\b|\\binforms\\b|\\binformed\\b|educat\\S*")

SDG3_7_x2 = c(
  "\\bsex|reproductive",
  "\\bcare\\b",
  "access|availab\\S*|modern|information|informing|\\binform\\b|\\binforms\\b|\\binformed\\b|educat\\S*")

SDG3_7_x3 = c(
  "unintended|unwanted",
  "pregnanc\\S*|motherhood",
  "access|availab\\S*|modern|information|informing|\\binform\\b|\\binforms\\b|\\binformed\\b|educat\\S*")

SDG3_7_y = c(child_ls,
             "birth|pregnan\\S*")

SDG3_7_x1 <- func_AND_plus(SDG3_7_x1)
SDG3_7_x2 <- func_AND_plus(SDG3_7_x2)
SDG3_7_x3 <- func_AND_plus(SDG3_7_x3)
SDG3_7_y  <- func_AND_plus(SDG3_7_y)



SDG3_8_x = c("health|Medical|\\bhearing|\\bDental|\\bVision\\b",
             "coverage|\\bcare\\b|service.?|benefit.?",
             "universal|access|availab\\S*|affordable|\\bpublic|essential")
SDG3_8_y = c("health|medicine\\S*|vaccin\\S*|pharmac\\S*",
             "household",
             "expenditure\\S*|expense|income|financ\\S*")
SDG3_8_x <- func_AND_plus(SDG3_8_x)
SDG3_8_y <- func_AND_plus(SDG3_8_y)
SDG3_8_y <- func_to_exclude_terms(which_sdg_term = SDG3_8_y, terms_to_exclude = financial_exclude_ls)


SDG3_9_x = c("hazard\\S*|unsafe|unintentional|inadequate|contamin\\S*|pollut\\S*",
             "chemical\\S*|\\bair\\b|\\bwater.?\\b|\\bsoil\\b|sanita\\S*|hygien\\S*|poison\\S*",
             paste(death_ls, "illness|\\bsick", sep = "|"))
SDG3_9_y = c("\\black",
             "sanita\\S*|hygien\\S*|poison\\S*",
             paste(death_ls, "illness\\S*|\\bsick\\S*", sep = "|"))
SDG3_9_x <- func_AND_plus(SDG3_9_x)
SDG3_9_y <- func_AND_plus(SDG3_9_y)


SDG3_a = c("tobacco|nicotine|cigar\\S*|\\bvap\\S*|smok\\S*" ,
           paste("control|\\bregulat\\S*|administer\\S*|\\bban\\b|\\bbans\\b|\\bbanned\\b|govern\\S*|manag\\S*|cessation\\S*|\\bquit\\S*",
                 reduce_ls, sep = "|"))
SDG3_a   <- func_AND_plus(SDG3_a)
SDG3_a   <- gsub("\\.\\+", "(?!.*government)(?!.*Governance)(?!.*Management)(?!.*manager)", SDG3_a) ## to exclude these


SDG3_b_x = c(
  paste("vaccin\\S*|medicin\\S*|medication\\S*|antibiotic", "health facilit\\S*|health sector\\S*", sep = '|'),
  "research\\S*|develop\\S*|\\bR&D\\b",
  developing_country_ls,
  support_ls)
SDG3_b_y = c(
  paste("vaccin\\S*|medicin\\S*|medication\\S*|antibiotic|pharmac\\S*", "health facilit\\S*", sep = '|'),
  "access|\\bavailab\\S*|afford\\b|affordabl\\S*|covered by|TRIPS Agreement.?|Trade.?Related Aspects of Intellectual Property Rights")
SDG3_b_x <- func_AND_plus(SDG3_b_x)
SDG3_b_y <- func_AND_plus(SDG3_b_y)
SDG3_b_x <- gsub("\\.\\+", "(?!.*development expense)(?!.*investigat\\S*)(?!.*Invest Ltd)", SDG3_b_x) ## to exclude these
SDG3_b_y <- func_to_exclude_terms(which_sdg_term = SDG3_b_y,
                                  terms_to_exclude =  c("available data", "extension", "market access", "access to equity"))


## ------------------------------------------------------------------------------------- ##
## For this case, we used two options to find match
SDG3_c_x = c("\\bhealth\\b|well.?being",
             paste("financ\\S*|\\bfund.?\\b|\\bfunding\\b|\\bassistance.?|\\baid.? to",
                   "government aid|state aid|federal aid|national aid|invest\\b|investment\\S*|donor\\S*|donat\\S*|\\bgrant.?\\b|official flow.?",
                   sep = "|"))
temp <- SDG3_c_x
ex_ls  <- c(financial_exclude_ls,
            'statement\\S*', 'granted', 'Global Grants Programme',
            'investigat\\S*', 'First Aid', 'Animal Health', 'funded')

### Option 1: use `func_AND_plus` function
# SDG3_c_x <- func_AND_plus(SDG3_c_x)
# SDG3_c_x <- func_to_exclude_terms(SDG3_c_x, terms_to_exclude = ex_ls) ## to exclude these

### Option 2: to use look around function ----
SDG3_c_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, exclude = ex_ls)
## ------------------------------------------------------------------------------------- ##


SDG3_c_y = c("\\bhealth\\b",
             "workforce|\\bworker.?|employ\\S*|\\blabor|\\blabour",
             "recruit\\S*|develop\\S*|\\btrain\\S*|retention.?")
SDG3_c_y <- func_AND_plus(SDG3_c_y)


SDG3_d_x = c("\\bhealth\\b",
             "early warning|manag\\S*|risk reduction|emergency|\\burgen\\S*",
             paste("preparedness", increase_ls, sep = "|"))
SDG3_d_y = c("International Health Regulations|\\bIHR\\b|health risk",
             "capacity")
SDG3_d_z1 = c("blood\\S*",
              "\\binfect\\S*",
              "resist\\S*")
SDG3_d_z2 = c("antibiotic|antimicr\\S*",
              "resist\\S*")

# SDG3_d_x  <- func_AND_plus(SDG3_d_x)
temp <- SDG3_d_x
SDG3_d_x  <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6, third_AND_string = temp[3])
SDG3_d_y  <- func_AND_plus(SDG3_d_y)
SDG3_d_z1 <- func_AND_plus(SDG3_d_z1)
SDG3_d_z2 <- func_AND_plus(SDG3_d_z2)



SDG3_general_x = c('health|good|quality',
                   paste("\\blife\\b|\\blives\\b", child_ls, female_ls, sep = "|"))
SDG3_general_y = "well.?being"
SDG3_general_x <- func_AND_plus(SDG3_general_x)



### may no need to put them together ------------ #

# SDG3_4   <- func_OR_vector(c(SDG3_4_x1, SDG3_4_x2, SDG3_4_y,  SDG3_4_z))
# SDG3_7   <- func_OR_vector(c(SDG3_7_x1, SDG3_7_x2, SDG3_7_x3, SDG3_7_y))
# SDG3_8   <- func_OR_vector(c(SDG3_8_x, SDG3_8_y))
# SDG3_9   <- func_OR_vector(c(SDG3_9_x, SDG3_9_y))
# SDG3_b   <- func_OR_vector(c(SDG3_b_x, SDG3_b_y))
# SDG3_c   <- func_OR_vector(c(SDG3_c_x, SDG3_c_y))
# SDG3_d   <- func_OR_vector(c(SDG3_d_x, SDG3_d_y, SDG3_d_z1, SDG3_d_z2))
# SDG3_general <- func_OR_vector(c(SDG3_general_x, SDG3_general_y))







## 4. Quality Education -----------------------------------------------------------------------------


SDG4_1_x1 = c(child_ls,
              "\\bprimary|secondary|middle",
              "educat\\S*|school\\S*",
              "\\bcomplet\\S*|conclud\\S*|\\bend\\b|\\bfinish\\S*|Finaliz\\S*|\\bFulfill\\S*|accomplish\\S*|\\bachiev\\S*|wrap up")
SDG4_1_x2 = c(child_ls,
              "\\bgrade",
              "\\b2\\b|\\b2nd\\b|second\\S*|\\btwo|\\b3\\b|third|\\b3rd\\b|three",
              "\\bcomplet\\S*|conclud\\S*|\\bend\\b|\\bfinish\\S*|Finaliz\\S*|\\bFulfill\\S*|accomplish\\S*|\\bachiev\\S*|wrap up")
SDG4_1_x3 = c(child_ls,
              "reading|\\bmath\\S*|educat\\S*|school\\S*",
              "proficiency|competence|skill\\S*")
SDG4_1_y = c(child_ls,
             "inclusi\\S*|quality|equitab\\S*|\\bfree\\b|access|affordab\\S*",
             "educat\\S*|school\\S*")

SDG4_1_x1 <- func_AND_plus(SDG4_1_x1)
SDG4_1_x2 <- func_AND_plus(SDG4_1_x2)
SDG4_1_x3 <- func_AND_plus(SDG4_1_x3)
SDG4_1_y <- func_AND_plus(SDG4_1_y)



SDG4_2_x = c(paste(child_ls, "infant|toddler", sep = "|"),
             "\\bcare.?\\b|pre.?primary educat\\S*|health|well.?being|psycho\\S*")
SDG4_2_x <- func_AND_plus(SDG4_2_x)
SDG4_2_x <- gsub("\\.\\+", "(?!.*(?:young|Children.?s Centre|Children.?s Center))", SDG4_2_x) ## to exclude these

SDG4_2_y <- "pre.?school|early childhood development"
SDG4_2_z <- c("primary entry age|\\bbefore\\b|ready for",
              "educat\\S*|learning\\S*")
temp   <- SDG4_2_z
SDG4_2_z <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)


temp     <- paste("access|availab\\S*|equitab\\S*|equal\\S*|\\bfree\\b|affordab\\S*|quality|particip\\S*",
                  "\\bjoin\\b|\\bjoins|\\bjoined|\\bjoining|attend\\S*|lifelong|adult\\S*|non.?traditional", sep = "|")
SDG4_3_x <- c("technical|vocational|tertiary",
              "educat\\S*|\\bcollege.?\\b|universit\\S*|\\btraining\\S*|learning\\S*|high school.?",
              temp)
SDG4_3_y <- c("\\bGED\\b|G\\.E\\.D\\.|General Educational Development",
              temp)
SDG4_3_x <- func_AND_plus(SDG4_3_x)
SDG4_3_y <- func_AND_plus(SDG4_3_y)
SDG4_3_x <- func_to_exclude_terms(which_sdg_term = SDG4_3_x, terms_to_exclude = c('Access Bank', 'Information is available', 'job fair')) ## to exclude these
SDG4_3_y <- func_to_exclude_terms(which_sdg_term = SDG4_3_y, terms_to_exclude = c('Access Bank', 'Information is available', 'job fair')) ## to exclude these


SDG4_4_x = c("technical|vocational|information and communications|\\bICT\\b",
             "skill\\S*|competenc\\S*|technique\\S*|expert\\S*|certificate.?|diploma",
             "\\bemployment.?|\\bjob.?|entrepreneur\\S*")
SDG4_4_y = c("financial literacy",
             "\\bemployment\\S*|\\bjob\\S*|entrepreneur\\S*")
SDG4_4_x <- func_AND_plus(SDG4_4_x)
SDG4_4_y <- func_AND_plus(SDG4_4_y)



SDG4_5 = c(
  "education|vocational|\\btraining\\S*|school\\S*",
  "disparit\\S*|parit\\S*|equal access|equit\\S*|equalit\\S*|inclusi\\S*|exclus\\S*|inequalit\\S*|discrimin\\S*",
  paste(disability_ls,
        "wounded|indigen\\S*|aboriginal|vulnerable|\\bgender|female|women|rural",
        # urban_ls,              ## -> can be too broad --> removed
        # "bottom|top|conflict", ## -> can be too broad
        "underrepresented|wealth\\S*|low.?income",
        sep = '|'))
SDG4_5 <- func_AND_plus(SDG4_5)

SDG4_6 = "literacy|numeracy|literate\\S*|reading skill\\S*|writing skill\\S*"
SDG4_6 = paste0('^(?=.*(?:', SDG4_6, '))',  "(?!.*financial literacy)")


SDG4_7_x = c("\\bknowledge|skill\\S*|educat\\S*|\\btraining\\S*|curricul\\S*|teach\\S*|student assessment.?",
             "sustainable development|sustainable lifestyle\\S*|human right.?|\\bethics|gender equality|peace|non.?violence|cultural diversity")
SDG4_7_y = c("global citizenship|international|Universal|\\bpolicy|\\bpolicies|Teacher.?\\b|trainer.?\\b|educator.?\\b",
             "education")
SDG4_7_x <- func_AND_plus(SDG4_7_x)
# SDG4_7_y <- func_AND_plus(SDG4_7_y)

temp   <- SDG4_7_y
SDG4_7_y <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6)



SDG4_a = c("educat\\S*|learn\\S*|school\\S*",
           "facilit\\S*|infrastructure\\S*|environment|basic service.?|electricity|internet|computer.?|drinking water|sanitation|hand.?washing",
           "\\bbuild|\\bupgrad\\S*|\\boffer\\S*|\\bprovid\\S*")

SDG4_b = c(paste("scholarship.?|fellowship.?|official development assistance",
                 "financ\\S*|\\bfund.?\\b|\\bfunding\\b|\\baid.? to|government aid|state aid|invest\\S*|\\bgrant.?\\b|official flow.?",
                 sep = "|"),
           paste("knowledge|skill\\S*|educat\\S*|vocational training.?|information technology|communications technology",
                 "information and communications technology",
                 "\\btechnical\\b|engineer\\S*|scien\\S*|mathemat\\S*|technolo\\S*|\\bSTEM\\b|S\\.T\\.E\\.M\\.", sep = "|"),
           developing_country_ls)
SDG4_b <- func_AND_plus(SDG4_b)
SDG4_b <- gsub("\\.\\+", "(?!.*Corporate Governance)", SDG4_b) ## to exclude these



SDG4_c = c(paste('teacher\\S*|educator\\S*|faculty|instructor\\S*|lecturer\\S*|professor\\S*|supervisor\\S*",
                 "mentor.?\\b|tutor\\S*|trainer\\S*|adviser\\S*|advisor\\S*|\\bcoach\\b|\\bcoaches\\b|caregiver\\S*', sep = "|"),
           "supply|attrition|qualif|\\btrain|capab")


#### other related but not included in the above
SDG4_general = c("school\\S*|educat\\S*|learning\\S*",
                 "inclusive|attend\\S*|enroll\\S*|basic|\\bequity|\\bequitab\\S*|safe|lifelong|lifetime|continuing")

SDG4_a <- func_AND_plus(SDG4_a)
SDG4_c <- func_AND_plus(SDG4_c)
SDG4_general <- func_AND_plus(SDG4_general)


# SDG4_1   <- func_OR_vector(c(SDG4_1_x1, SDG4_1_x2, SDG4_1_x3, SDG4_1_y))
# SDG4_2   <- func_OR_vector(c(SDG4_2_x, SDG4_2_y))
# SDG4_3   <- func_OR_vector(c(SDG4_3_x, SDG4_3_y))
# SDG4_4   <- func_OR_vector(c(SDG4_4_x, SDG4_4_y))
# SDG4_7   <- func_OR_vector(c(SDG4_7_x, SDG4_7_y))







## -5. Gender Equality -----------------------------------------------------------------------------


SDG5_1_x = c(paste("equality|\\bparity|justice|segregat\\S*|\\banti\\S*|hostile|marginali\\S*",
                   discrimination_ls, sep = "|"),
             paste(female_ls, "\\bsex\\S*|\\bgender", sep = "|"))
SDG5_1_y = "misogyn\\S*|feminis\\S*"
SDG5_1_x <- func_AND_plus(SDG5_1_x)




SDG5_2_x = c("violence|assault\\S*|attack\\S*|aggression|offens\\S*|\\babuse|traffick\\S*|exploit\\S*|\\bforc\\S*|misconduct\\S*",
             paste(female_ls, "domestic|\\bsex\\S*|prostitution\\S*", sep = "|"))
SDG5_2_y = "prostitut\\S*|\\brape\\S*|harass\\S*"


SDG5_3_x = c(paste(child_ls, "\\bharm.?\\b|\\bharmed\\b|\\bearly|forced|before age 15|before age 18", sep = "|"),
             "marr\\S*")
SDG5_3_y = c(paste(female_ls, '\\bgender', sep = "|"),
             'genital',
             "mutilat\\S*|\\bcut\\S*")
SDG5_3_z = c(female_ls,
             "circumcision\\S*")

SDG5_2_x <- func_AND_plus(SDG5_2_x)
SDG5_3_x <- func_AND_plus(SDG5_3_x)
SDG5_3_y <- func_AND_plus(SDG5_3_y)
SDG5_3_z <- func_AND_plus(SDG5_3_z)



SDG5_4 = c("unpaid|shared responsibility|allocation|equity|equitab\\S*|fairness",
           paste("\\bcare.?\\b|domestic work|household work|family work\\S*", female_ls, sep = "|"))
temp   <- SDG5_4
SDG5_4 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)


SDG5_5 = c(paste("leadership|leader\\S*|\\bseat\\S*|decision.?making|national parliament\\S*|government.?|autonomy",
                 "managerial position.?|political participation.?|politician|manager.?|in middle management|in senior management",
                 "in executive position.?|under.?representation|entrepreneurship|\\bpower|glass ceiling", sep = "|"),
           female_ls)

# SDG5_4 <- func_AND_plus(SDG5_4)
SDG5_5 <- func_AND_plus(SDG5_5)



SDG5_6_x = c("access|availab|guarantee\\S*|\\bright.?|decision.?|decide.? on|autonomy|\\brights|\\blaw\\S*|regulation.?|information|educat\\S*",
             "sexual|intercourse|\\breproduct\\S*|\\bcontracept\\S*|family planning|divorc\\S*|parenting plan.?|\\babort\\S*")
SDG5_6_y = c("\\bSex\\b|health.?care",
             "information|educat\\S*",
             "\\bfull\\b|\\bequal\\b")
SDG5_6_z = c("decision.?|\\breproduct\\S*",
             "health.?care",
             female_ls
             )
SDG5_6_w = paste("parent.? leave|Parental leave|paternity leave|family leave|adoption leave",
                 "Adoptive leave|maternity leave|Childbirth Leave|child.?care leave|Maternity care",
                 "paid maternity|Contraception service\\S*|Emergency contraception|abortion care",
                 "HIV test\\S*|HIV counsel\\S*|HIV care|HIV treatment|HPV vaccine|CSE curriculum|CSE law",
                 sep = "|")
SDG5_6_x <- func_AND_plus(SDG5_6_x)
SDG5_6_y <- func_AND_plus(SDG5_6_y)
SDG5_6_z <- func_AND_plus(SDG5_6_z)


SDG5_a_x = c("access|availab\\S*|\\bright.?\\b|\\bcontrol|\\breform\\S*|equal|\\blegal\\S*|\\blaw\\S*",
             "resource.?|\\bfund|\\bland\\b|propert\\S*|financial service.?|finance|owner\\S*|budget.?|\\bquota\\b",
             paste(female_ls, "\\bgender", sep = "|"))
SDG5_a_y = c(paste("ownership|inheritance right.?|right.? of inheritance|right.? to inherit\\S*",
                   "Mandatory quota.?|land tenure|land management|rights.?bearer\\S*", sep = "|"),
             paste(female_ls, "\\bgender", sep = "|"))
SDG5_a_z = "agrarian feminism"

SDG5_a_x <- func_AND_plus(SDG5_a_x)
SDG5_a_y <- func_AND_plus(SDG5_a_y)



SDG5_b = c("technolog\\S*|information|communications|mobile.?phone.?|telephone.?|smart.?phone.?|Cell.?phone.?|\\bICT\\b",
           "\\bempower\\S*|promot\\S*",
           female_ls)

SDG5_c = c(paste("\\bpolicy|\\bpolicies|program.?|legislation.?|\\bregulation.?|\\blaw\\b|\\blaws",
                 "government.?|authorit\\S*|national budget\\S*|budget allocation.?|resource allocation.?",
                 "public financ\\S*|public expenditure.?|fiscal system.?|monitoring system.?",
                 sep = "|"),
           paste("\\bgender|\\bsex\\S*",
                 female_ls,
                 sep = "|"),
           paste(equal_ls,
                 "equit\\S*|promot\\S*|empower\\S*",
                 sep = "|"))

SDG5_general = c(paste("\\bgender|\\bsex", female_ls, sep = "|"),
                 "equal\\S*|inclusi\\S*")

SDG5_b <- func_AND_plus(SDG5_b)
SDG5_c <- func_AND_plus(SDG5_c)
SDG5_general <- func_AND_plus(SDG5_general)



##### #
# SDG5_1   <- func_OR_vector(c(SDG5_1_x, SDG5_1_y))
# SDG5_2   <- func_OR_vector(c(SDG5_2_x, SDG5_2_y))
# SDG5_3   <- func_OR_vector(c(SDG5_3_x, SDG5_3_y))
# SDG5_6   <- func_OR_vector(c(SDG5_6_x, SDG5_6_y))
# SDG5_a   <- func_OR_vector(c(SDG5_a_x, SDG5_a_y))






## 6. Clean Water and Sanitation --------------------------------------------------------------------

SDG6_1 = c("access|availab\\S*|afford\\S*|safe|secure|\\bclean|manag\\S*|equitab\\S*",
           "drinking water|drinkable water")

SDG6_2_x = c("access|availab\\S*|safe|manag\\S*|adequate\\S*|equitab\\S*|equal|facilit\\S*|service.?",
             paste("sanita\\S*|hygien\\S*|soap|cleanliness", "hand wash\\S*|wash.*hand\\S*", sep = '|'))
SDG6_2_y = c("access|availab\\S*|safe|\\bopen|hygien\\S*|adequate|equal|health|\\bclean|manag\\S*",
             "bathroom.?|defecation|toilet.?|restroom.?|lavatory|latrine|water closet.?|sewerage\\S*")

SDG6_1   <- func_AND_plus(SDG6_1)
SDG6_2_x <- func_AND_plus(SDG6_2_x)
SDG6_2_y <- func_AND_plus(SDG6_2_y)



SDG6_3_x = c("\\bwater.?\\b|aquatic|aquifer",
             "pollut\\S*|black|contamination.?",
             reduce_ls)
SDG6_3_y = c('\\bwater.?\\b|aquatic', 'recycl\\S*|\\bre.?us\\S*')
SDG6_3_z = c('\\bwater.?\\b|aquatic', 'quality', paste('good', increase_ls, sep = "|"))
SDG6_3_w = c("dump\\S*|releas\\S*|recycl\\S*|\\bre.?us\\S*|untreated|\\btreated\\b|sustainab\\S*",
             "hazard\\S*|chemical\\S*|toxicology|ecotoxicology|wastewater|sewer\\S*")
temp   <- SDG6_3_y
SDG6_3_y <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6)
SDG6_3_x <- func_AND_plus(SDG6_3_x)
# SDG6_3_y <- func_AND_plus(SDG6_3_y)
SDG6_3_z <- func_AND_plus(SDG6_3_z)
SDG6_3_w <- func_AND_plus(SDG6_3_w)



SDG6_4_x = c("\\bwater.?\\b|freshwater|groundwater",
             "\\befficien\\S*|sustainab\\S*|secur\\S*|availab\\S*|conservat\\S*|recycl\\S*|\\bre.?us\\S*|re.?utiliz\\S*|adequate|abundan\\S*|plentiful|plentiful|sufficient\\S*")
SDG6_4_y = c("\\bwater.?\\b|freshwater|groundwater",
             paste("footprint.?|withdraw\\S*|consumption\\S*|consumed|\\busage\\S*|\\bgreen\\b|blue\\b|grey",
                   "scarcity|scarce|scant|shortage|\\bstress|dearth|deficit|\\black|shortfall",
                   "insufficien\\S*|inadequa\\S*|deficienc\\S*", sep = "|"),
             paste(reduce_ls, 'address\\S*|tackl\\S*|response to|cope.?with|coping with|deal.?with|dealing with|handl\\S*', sep = "|"))
SDG6_4_z = "water balancing|water.?scarce|water.?stress"

SDG6_4_x <- func_AND_plus(SDG6_4_x)

# SDG6_4_y <- func_AND_plus(SDG6_4_y)
temp     <- SDG6_4_y
SDG6_4_y <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6, third_AND_string = temp[3])


SDG6_5 = c("water resource.?|watershed.?",
           paste("manag\\S*|transboundary|transnational|international|interbasin|cooperat\\S*|collaborat\\S*|joint effort.?|co.?operation|coping",
                 policy_ls, sep = "|"))

SDG6_6 = c("\\bwater.?\\b|mountain\\S*|forest|wetland|river|aquifer|lake.?",
           "eco.?system.?",
           "conserv\\S*|\\brestor\\S*|reserv\\S*|mainten\\S*|protect\\S*|preserv\\S*|safeguard\\S*")

SDG6_5 <- func_AND_plus(SDG6_5)
SDG6_6 <- func_AND_plus(SDG6_6)



SDG6_a_x = c("\\bwater.?\\b|sanita\\S*",
             paste("development assistance|harvesting|desalina|\\befficien\\S*|waste\\S*|\\btreatment.?",
                   "recycl\\S*|\\bre.?us\\S*|re.?utiliz\\S*|technolog\\S*|purification|infrastructure.?",
                   support_ls, sep = "|"),
             developing_country_ls)
SDG6_a_y = c("\\bwater.?\\b|sanita\\S*",
             "international co.?op\\S*|international collab\\S*",
             developing_country_ls)
SDG6_a_z = c("\\bwater.?\\b|sanita\\S*",
             "capacity", "build\\S*",
             developing_country_ls)

SDG6_a_x <- func_AND_plus(SDG6_a_x)
# SDG6_a_y <- func_AND_plus(SDG6_a_y)
SDG6_a_z <- func_AND_plus(SDG6_a_z)
temp     <- SDG6_a_y
SDG6_a_y <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 8, third_AND_string = temp[3])



SDG6_b =  c("\\bwater.?\\b|sanita\\S*|hygiene|wastewater treatment",
            paste("\\blocal communit\\S*|local administrat\\S*",
                  "local entit\\S*|ocal government\\S*|local authorit\\S*|local Stakeholder\\S*|each location",
                  "sub.?district\\S*|municipalit\\S*|\\bcommune\\S*",
                  "regional|resident\\S*|indigen\\S*|aboriginal|\\btraditional\\b|native|endemic",
                  sep = "|"),
            paste("participat\\S*", policy_ls, sep = "|"))
SDG6_b <- func_AND_plus(SDG6_b)




###### #
# SDG6_2   <- func_OR_vector(c(SDG6_2_x, SDG6_2_y))
# SDG6_3   <- func_OR_vector(c(SDG6_3_x, SDG6_3_y, SDG6_3_z, SDG6_3_w))
# SDG6_4   <- func_OR_vector(c(SDG6_4_x, SDG6_4_y, SDG6_4_z))
# SDG6_a   <- func_OR_vector(c(SDG6_a_x, SDG6_a_y, SDG6_a_z))








## 7. Affordable and Clean Energy ----------------------------------------------------------------

SDG7_1_x = c(paste('energy|\\bfuel|\\bpower\\b', "electricity", sep = "|"),
             "reliable|affordab\\S*|modern|\\clean\\b|sustainable",
             "access\\S*|availab\\S*")
SDG7_1_y = "Power outage.?"


SDG7_2_w = c(renewable_ls,
             paste("\\bshare|proportion|\\bpercent\\S*|\\bmix\\b|consum\\S*|transiti\\S*|shift\\S*", increase_ls, sep = "|"))
### Option 1: use `func_AND_plus` function
# SDG7_2_w <- func_AND_plus(SDG7_2_w)

### Option 2: to use look around function ----
temp <- SDG7_2_w
SDG7_2_w <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)


SDG7_2_x = c(renewable_ls,
             "\\bmeet\\S*|fulfill\\S*",
             "\\demand\\S*|requirement.?")
SDG7_2_x <- func_AND_plus(SDG7_2_x)


SDG7_2_y = paste("energy transition.?", "hybrid vehicle.?|electric vehicle.?", sep = "|")


SDG7_2_z = c(paste("fossil.?fuel.?|fossil.?gas|\\bcoal\\b|\\bcoals\\b|petrol|natural gas|methane|crude oil.?",
                   "gasoline|kerosene|non.?renewable.?|traditional energy|Conventional energy",
                   sep = "|"),
             "consumption\\S*|consumed|\\busage\\S*|\\busing\\b|\\buse\\b|\\buses\\b",
             reduce_ls)
### Option 1: use `func_AND_plus` function
# SDG7_2_z <- func_AND_plus(SDG7_2_z)

### Option 2: to use look around function ----
temp <- SDG7_2_z
ex <- strsplit(x = "year end|coal price\\S*|coal cost\\S*", split = "\\|")
ex <- unlist(ex)
SDG7_2_z <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, exclude = ex, third_AND_string = temp[3])


SDG7_3_x = c("energy|\\belectric\\S*|\\bfuel.?|\\bpower\\b|\\butilit\\S*",
             "\\befficien\\S*|\\bintens\\S*|optimization tech\\S*|productivity|pollut\\S*|footprint.?|\\bleak.?\\b|\\bleakage.?|\\bper GDP|\\bper capita")
SDG7_3_y = c("energy consumption|energy use\\S*|energy usage|energy inefficiency|energy.?intensive|energy burden|energy waste",
             reduce_ls)
SDG7_3_z = "smart grid.?|smart meter.?|Energy smart"


# SDG7_3_x <- func_AND_plus(SDG7_3_x)
temp <- SDG7_3_x
SDG7_3_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6)


SDG7_a_x = c(renewable_ls,
             "cooperat\\S*|collaborat\\S*|joint effort.?",
             paste("research\\S*|\\btech\\S*", finance_ls, sep = "|"))
SDG7_a_x <- func_AND_plus(SDG7_a_x)
ex       <- c(financial_exclude_ls, "statement.?", "granted", "investigat\\S*", "electricity trading")
SDG7_a_x <- func_to_exclude_terms(which_sdg_term = SDG7_a_x, terms_to_exclude = ex)


SDG7_a_y = c('energy|\\belectricit\\S*|\\bfuel.?|electric vehicle.?|hybrid vehicle.?',
             'research\\S*|technolog\\S*|infrastructure\\S*|hybrid system\\S*',
             paste(increase_ls, support_ls, "access|availab\\S*", sep = "|"))
# SDG7_a_y <- func_AND_plus(SDG7_a_y)
# SDG7_a_y <- gsub("\\.\\+", "(?!.*electricity trading)", SDG7_a_y) ## to exclude these keywords
temp <- SDG7_a_y
SDG7_a_y <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 8, exclude = c("electricity trading"), third_AND_string = temp[3])



SDG7_b = c(renewable_ls,
           developing_country_ls,
           support_ls)

SDG7_general <- renewable_ls
SDG7_general <- gsub("\\.\\+", "(?!.*Renewables segment\\S*)", SDG7_general) ## to exclude these keywords


####
SDG7_1_x   <- func_AND_plus(SDG7_1_x)

SDG7_3_y <- func_AND_plus(SDG7_3_y)

SDG7_b <- func_AND_plus(SDG7_b)


# SDG7_1   <- func_OR_vector(c(SDG7_1_x, SDG7_1_y))
# SDG7_2   <- func_OR_vector(c(SDG7_2_x, SDG7_2_y, SDG7_2_z, SDG7_2_w))
# SDG7_3   <- func_OR_vector(c(SDG7_3_x, SDG7_3_y))
# SDG7_a   <- func_OR_vector(c(SDG7_a_x, SDG7_a_y))














## 8. Decent Work and Economic Growth --------------------------------------------------------------

SDG8_1 = c(economic_ls,
           "per capita",
           paste("Sustain\\S*|develop\\S*|prosper\\S*", increase_ls, sep = "|")) ## ,"nation|countr|state"

SDG8_2 = c(economic_ls,
           paste("\\bstimulat\\S*", increase_ls, sep = "|"),
           "Productiv\\S*|\\befficien\\S*|technolo\\S*|innovation\\S*|moderniz\\S*|modernis\\S*|diversif\\S*|per employ\\S*|per work\\S*|per person")
SDG8_2 <- func_AND_plus(SDG8_2)
SDG8_2 <- func_to_exclude_terms(which_sdg_term = SDG8_2, terms_to_exclude = financial_exclude_ls)


SDG8_3_x = c("development",
             '\\bpolicy|\\bpolicies',
             paste(economic_ls, "employ\\S*", sep = "|"),
             increase_ls)

SDG8_3_y = c("\\bemploy\\S*|\\bjob.?|\\bwork\\S*",
             "\\bdecent\\b|\\bseemly\\b|respectable|quality|stable|stabili\\S*|inclusiv\\S*",
             paste("\\bcreate|\\bcreati\\S*|support\\S*", increase_ls, sep = "|"))
# SDG8_3_y  <- func_AND_plus(SDG8_3_y)
### Option 3: to use look around + AND function ----
temp <- SDG8_3_y
SDG8_3_y <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 5, third_AND_string = temp[3])


SDG8_3_z1 = c("productive activit\\S*|entrepreneurship\\S*",
              paste("support\\S*", increase_ls, sep = "|"))
temp <- SDG8_3_z1
### Option 1
### Option 2 ----
SDG8_3_z1 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)


SDG8_3_z2 = c("micro.?|small.?|medium.?|starting|start.?up.?|kickstart|inclusiv\\S*",
              paste(company_ls, "entrepren\\S*", sep = "|"),
              increase_ls)
# SDG8_3_z2 <- func_AND_plus(SDG8_3_z2)
### Option 3: to use look around + AND function ----
temp <- SDG8_3_z2
SDG8_3_z2 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 3, third_AND_string = temp[3])


SDG8_3_z3 = c("inclusive",
              "economic growth")

SDG8_3_z4 = c("informal employment",
              reduce_ls)



SDG8_4_x1 = c("sustain\\S*|\\befficien\\S*",
              "resource.?",
              "consumption.?|production.?")
temp <- SDG8_4_x1
SDG8_4_x1 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)


SDG8_4_x2 = c("resource.?efficien\\S*|efficient resource.?",
              increase_ls)

SDG8_4_y = c("consumption.?|footprint.?",
             "per GDP|per capita|\\befficien\\S*",
             "material.?\\b")
SDG8_4_z = c("environmental degradation",
             paste(reduce_ls, "de.?coupl\\S*|informed", sep = "|"))
temp <- SDG8_4_z
SDG8_4_z <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)


SDG8_5_x1 = c(paste("\\bfull\\b|\\bdecent\\b|\\bseemly\\b|respectable|productiv\\S*",
                    "inclusi\\S*|medical benefit.?|health benefit.?|healthcare benefit.?|insurance benefit.?",
                    sep = "|"),
              "\\bemployment.?|\\bhiring|\\bemployee.?|\\bjob.?|\\bwork\\b|\\bworker.?|\\bstaff\\S*")
### Option 1: use `func_AND_plus` function
# SDG8_5_x1 <- func_AND_plus(SDG8_5_x1)
### Option 2: to use look around function ----
temp <- SDG8_5_x1
ex_ls <- c('full meeting.?', 'full potential', 'full Supervisory', 'full implement\\S*', 'full year', 'full integration.?')
SDG8_5_x1 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, exclude = ex_ls)


SDG8_5_x2 = c(paste(discrimination_ls, "informal\\b|precarious", sep = "|"),
              "\\bemployment|\\bhiring|\\bemployee\\S*|\\bjob\\S*|\\bworker\\S*|\\bwork\\b|\\bworking")
# SDG8_5_x2 <- func_AND_plus(SDG8_5_x2)
temp      <- SDG8_5_x2
SDG8_5_x2 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)


SDG8_5_x3 = c(equal_ls,
              "\\bwage|\\bpay.?\\b|\\bpaying|\\bpaid|\\bsalary|\\bsalaries|\\bearning|\\bstipend")
temp <- SDG8_5_x3
### Option 1
# SDG8_5_x3 <- func_AND_plus(SDG8_5_x3)
### Option 2 ----
ex_ls <- "instalment\\S*|retained earning\\S*|pay.?out|payable"
ex_ls <- strsplit(ex_ls, split = "\\|")
ex_ls <- unlist(ex_ls)
SDG8_5_x3 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, exclude = ex_ls)


SDG8_5_x4 = c(equal_ls,
              "opportunit\\S*",
              paste("people|person|\\bemployee\\S*|\\bstaff\\S*|\\bworker.?|\\bwork\\b|\\bworking|\\bgender|\\bsex|ethni\\S*|\\brace|racial",
                    "\\breligion",
                    disability_ls,
                    "disadvantaged",
                    female_ls,
                    LGBT_ls,
                    sep = "|"))
SDG8_5_x4 <- func_AND_plus(SDG8_5_x4)


SDG8_5_x5 = c("\\bwage|\\bpay.?\\b|\\bpaying|\\bpaid|\\bsalary|\\bsalaries|\\bearning|\\bstipend",
              paste("\\bgap\\b|difference|discrepan\\S*|dissimilarit\\S*|disproport\\S*|divergen\\S*|imbalance\\S*",
                    discrimination_ls, sep = "|"),
              reduce_ls)
# SDG8_5_x5 <- func_AND_plus(SDG8_5_x5)
temp <- SDG8_5_x5
SDG8_5_x5 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, third_AND_string = temp[3])


SDG8_5_x6 = c("Share of|proportion|\\bpercent\\S*|discrimination",
              paste(disability_ls,
                    "disadvantaged|\\breligion|\\bgender",
                    female_ls,
                    LGBT_ls,
                    sep = "|"),
              "workforce|employment|\\bemployee.?")
SDG8_5_x7 = c("unemployment|dismissal",
              paste("\\baddress\\S*|tackl\\S*|response to|cope.?with|coping with|deal.?with|dealing with|handl\\S*",
                    "\\bmeasure.?\\b|\\bbelow\\b|\\blower\\b|\\bless\\b",
                    reduce_ls, sep = "|"))
SDG8_5_x8 = c("employment.?|\\bhiring|\\bemployee.?|\\bjob\\S*|\\bworker.?|\\blabor\\b|\\blabour\\b|coverage",
              disability_ls)
SDG8_5_x9 = "disability benefit.?"

SDG8_5_x6 <- func_AND_plus(SDG8_5_x6)
SDG8_5_x7 <- func_AND_plus(SDG8_5_x7)
SDG8_5_x8 <- func_AND_plus(SDG8_5_x8)


SDG8_6 = c("youth|young people|\\bteen.?\\b|teenage\\S*",
           "\\bin\\b|\\bwith\\b",
           "educat\\S*|employment.?|\\btraining.?")
# SDG8_6   <- func_AND_plus(SDG8_6)
temp   <- SDG8_6
SDG8_6 <- lookaround_nearby_n(word_ls1 = temp[2], word_ls2 = temp[3], n = 4, third_AND_string = temp[1])


SDG8_7_x = c(paste("\\bforced", child_ls, sep = "|"),
             "\\blabour|\\blabor|\\bwork\\S*|soldier.?|\\bslave\\S*|traffick\\S*")
SDG8_7_y = "modern slave\\S*|human traffick\\S*"

# SDG8_7_x <- func_AND_plus(SDG8_7_x)
temp   <- SDG8_7_x
SDG8_7_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)


SDG8_8_x1 = "\\blabour right.?|\\blabor right.?|employee right.?"
SDG8_8_x2 = c("human right.?|compensation law",
              "employ\\S*|\\blabor|\\blabour|\\bworker.?")
# SDG8_8_x2 <- func_AND_plus(SDG8_8_x2)
# SDG8_8_x2 <- gsub("\\.\\+", "(?!.*(?:voting right|Rights issue|Annual Report))", SDG8_8_x2) ## to exclude
temp <- SDG8_8_x2
w_ex <- c('voting right.?', 'Rights issue.?', 'Annual Report.?')
SDG8_8_x2 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, exclude = w_ex)
## ------------------------------------------------------------------------------------ ##

SDG8_8_x3 = c("\\bsafe|\\bsecure",
              "\\bwork\\b|\\bworker.?|\\bworking|\\bjob.?\\b|occupational",
              "environment\\b")
# SDG8_8_x3 <- func_AND_plus(SDG8_8_x3)
temp <- SDG8_8_x3
SDG8_8_x3 <- lookaround_nearby_n(word_ls1 = temp[2], word_ls2 = temp[3], n = 4, third_AND_string = temp[1])


SDG8_8_x4 <- paste("zero incident.?",
                   "retirement fund\\S*|secure retirement.?", sep = "|")
SDG8_8_x4 <- paste0("^(?!.*\\breport.?\\b).*", "(", SDG8_8_x4, ")")

SDG8_8_x5 = c(paste(death_ls, "injur\\S*|\\bharm.?\\b|\\bharmed\\b|danger\\S*|\\bincident\\S*", sep = "|"),
              "\\bjob.?\\b|\\bwork\\S*|\\bemploy\\S*|occupation\\S*",
              reduce_ls)
temp <- SDG8_8_x5
w_ex <- c('alcohol', 'Drinking', '\\bNo\\.')
SDG8_8_x5 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 8, exclude = w_ex, third_AND_string = temp[3])


SDG8_8_x6 <- c(paste("ensure|prioritiz\\S*|\\bstimulat\\S*|management\\S*", increase_ls, sep = "|"),
               "employee benefit.?|employment benefit.?|occupational health|occupational safe\\S*")
# SDG8_8_x6 <- func_AND_plus(SDG8_8_x6)
temp <- SDG8_8_x6
SDG8_8_x6 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 10)



SDG8_9_x = c("touris\\S*",
             paste("sustainab\\S*|community.?based|\\bemployment.?", func_AND_plus(c('\\bcreat\\S*', '\\bjob.?')), sep = "|"))
SDG8_9_x <- func_AND_plus(SDG8_9_x)
SDG8_9_y <- func_AND_plus(c("\\btour\\S*",  "\\bpolicy|\\bpolicies|\\bGDP\\b|Gross Domestic Product"))
SDG8_9_z <- 'ecotourism'

SDG8_10 = c("access to|inclusi\\S*",
            "financial service.?|financial institution|\\bbanks|\\banking|bank branch\\S*|insurance|automated teller machine|\\bATMs\\b")
SDG8_10 <- func_AND_plus(SDG8_10)
SDG8_10 <- func_to_exclude_terms(which_sdg_term = SDG8_10, terms_to_exclude = c(financial_exclude_ls, "Access Bank"))


SDG8_a = c(support_ls,
           '\\btrade\\b')
temp <- SDG8_a
ex_words <- c('receivable|on-trade')
SDG8_a <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, exclude = ex_words)

SDG8_b = c(child_ls,
           'employ\\S*',
           policy_ls)
SDG8_b <- func_AND_plus(SDG8_b)




SDG8_1 <- func_AND_plus(SDG8_1)
SDG8_2 <- (SDG8_2)

SDG8_3_x  <- func_AND_plus(SDG8_3_x)
# SDG8_3_z1 <- func_AND_plus(SDG8_3_z1)
SDG8_3_z3 <- func_AND_plus(SDG8_3_z3)
SDG8_3_z4 <- func_AND_plus(SDG8_3_z4)

# SDG8_4_x1 <- func_AND_plus(SDG8_4_x1)
SDG8_4_x2 <- func_AND_plus(SDG8_4_x2)
SDG8_4_y  <- func_AND_plus(SDG8_4_y)
# SDG8_4_z  <- func_AND_plus(SDG8_4_z)



# SDG8_3   <- func_OR_vector(c(SDG8_3_x,  SDG8_3_y, SDG8_3_z1, SDG8_3_z2, SDG8_3_z3))
# SDG8_4   <- func_OR_vector(c(SDG8_4_x,  SDG8_4_y, SDG8_4_z))
# SDG8_5   <- func_OR_vector(c(SDG8_5_x1, SDG8_5_x2, SDG8_5_y1, SDG8_5_y2, SDG8_5_z))
# SDG8_7   <- func_OR_vector(c(SDG8_7_x,  SDG8_7_y))
# SDG8_8   <- func_OR_vector(c(SDG8_8_x1, SDG8_8_x2, SDG8_8_x3, SDG8_8_x4, SDG8_8_x5))
# SDG8_9   <- func_OR_vector(c(SDG8_9_x,  SDG8_9_y, SDG8_9_z))







## 9. Industry, Innovation, and Infrastructure ------------------------------------------

SDG9_1   = c("quality|reliab\\S*|sustainable|resilien\\S*|afford\\S*|equitab\\S*|inclus\\S*",
             infrastructure_ls)
temp <- SDG9_1
SDG9_1 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6)




SDG9_2_x = c("inclusiv\\S*|sustainab\\S*|cradle to cradle|circular",
             "\\bindustr\\S*")
temp <- SDG9_2_x
SDG9_2_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)



SDG9_2_y = c('manufactur\\S*|\\bindustr\\S*',
             '\\bvalue',
             'per capita|\\bGDP\\b|gross domestic product')
SDG9_2_z = c('manufactur\\S*|\\bindustr\\S*',
             '\\bemployment.?|\\bhiring|\\bemployee.?|\\bworker.?|\\bstaff.?',
             "Share of|proportion\\S*|\\bpercent\\S*")

SDG9_2_y <- func_AND_plus(SDG9_2_y)
# SDG9_2_z <- func_AND_plus(SDG9_2_z)
temp <- SDG9_2_z
SDG9_2_z <- lookaround_nearby_n(word_ls1 = temp[2], word_ls2 = temp[3], n = 4, third_AND_string = temp[1])


SDG9_3_x = c("\\bindustr\\S*|enterprise\\S*|entrepreneur\\S*",
             "access|availab\\S*|\\breach",
             "financial service.?|fiscal|\\bcredit.?|\\bloan.?|value chain.?|\\bbanking|\\binsurance|\\bfund.?\\b|\\bfunding\\b|micro.?credit.?|micro.?financ\\S*")
SDG9_3_y = c("small.?scale industr\\S*|micro.?enterprise\\S*|small.?sized business",
             "Share of|proportion\\S*|\\bpercent\\S*")
SDG9_3_z = c("\\bsmall|\\bmicro.?\\b|starting|start.?up.?|kickstart",
             paste("entrepren\\S*", company_ls, sep = "|"),
             paste("Share of|proportion\\S*|\\bpercent\\S*|financial service.?|affordable credit.?",
                   "credit union.?|\\bbanking|\\bInsurance|\\bbank.?\\b|\\bleasing|Retail deposit.?",
                   support_ls, sep = "|"))

SDG9_3_x <- func_AND_plus(SDG9_3_x)
SDG9_3_x <- gsub("\\.\\+", "(?!.*Access Bank)", SDG9_3_x) ## to exclude
SDG9_3_y <- func_AND_plus(SDG9_3_y)
# SDG9_3_z <- func_AND_plus(SDG9_3_z)
temp <- SDG9_3_z
SDG9_3_z <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, third_AND_string = temp[3])


SDG9_4_x = c("upgrad\\S*|retrofit\\S*",
             paste(infrastructure_ls, "\\bindustr\\S*", sep = '|'))
SDG9_4_y = c("resource.?",
             "\\buse\\b|\\buses\\b|\\busage.?|consumption.?",
             "\\befficien\\S*")
SDG9_4_z = c("\\bindustr\\S*",
             emission_ls,
             paste(reduce_ls, "manage\\S*|\\btreat\\S*", sep = '|'))
SDG9_4_w = c("\\bclean|environmentally sound|\\bgreen\\b|\\bgreener\\b",
             "technolog\\S*|\\bindustr\\S*")

# SDG9_4_x <- func_AND_plus(SDG9_4_x)
SDG9_4_y <- func_AND_plus(SDG9_4_y)
SDG9_4_z <- func_AND_plus(SDG9_4_z)
# SDG9_4_w <- func_AND_plus(SDG9_4_w)

temp     <- SDG9_4_x
SDG9_4_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6)
temp     <- SDG9_4_w
ex_ls    <- c("Industrial Investment")
SDG9_4_w <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, exclude = ex_ls)


SDG9_5_x = c("technolog\\S*|innov\\S*|\\binvention|research\\S*",
             paste(increase_ls,
                   "upgrad\\S*|\\bencourag\\S*|prioritiz\\S*|Dedicated to|focus\\S*|\\bfurther\\b",
                   "\\bfortifie\\S*|drive.?\\b|driving|leverag\\S*|\\bcontinue\\S*|\\binculcat\\S*",
                   "\\bobtain\\S*|\\badapt\\S*|\\bdevelop\\S*",
                   sep = "|"),
             paste("\\bindustr\\S*|manufactur\\S*|business\\S*|\\boperation.?|\\bspending.?|\\bGDP\\b|gross domestic product",
                   "\\bproduction.?|\\bemployee\\S*|\\bjob.?|\\bworker.?|\\blabor\\b|\\blabour\\b",
                   finance_ls,
                   sep = "|"))
temp <- SDG9_5_x
w_ex <- "incremental cost|Technology Development Zone|COMBINED MANAGEMENT REPORT"
w_ex <- strsplit(w_ex, split = "\\|")
w_ex <- unlist(w_ex)
SDG9_5_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6, exclude = w_ex, third_AND_string = temp[3])


SDG9_5_y = c(paste("scientific research|Researcher.?|scientist.?|\\bscholar.?\\b|specialist\\S*|\\bexpert.?\\b",
                   "\\bsavant.?\\b|academician|professor.?|Research and development",
                   "\\bR&D\\b|\\bR & D\\b|\\bR-D\\b|\\bRTD\\b|\\bR\\+D\\b", sep = "|"),
             paste("support.?", increase_ls, sep = "|"))
temp <- SDG9_5_y
ex_ls <- "Research Network|Research Alliance"
ex_ls <- strsplit(ex_ls, split = "\\|") %>% unlist()
SDG9_5_y <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 8, exclude = ex_ls)


SDG9_a <- c("resilient|sustainable",
            infrastructure_ls,
            support_ls,
            developing_country_ls)

SDG9_a <- func_AND_plus(SDG9_a)


SDG9_b_x = c("technolog\\S*|innovation.?|research|medium.?tech\\S*|high.?tech\\S*|\\bR&D\\b|\\bR & D\\b|\\bR-D\\b|industrial diversification",
            paste(increase_ls, support_ls, sep = "|"),
            paste("operational efficiency|domestic", developing_country_ls, sep = "|"))
ex_ls <- "Award.?|Financial Report.?|Annual Report.?|University of Science and Technology" %>%
  strsplit(., split = "\\|") %>% unlist()
SDG9_b_x <- func_AND_plus(SDG9_b_x)
SDG9_b_x <- func_to_exclude_terms(which_sdg_term = SDG9_b_x, terms_to_exclude = ex_ls)


SDG9_b_y <- c("medium.?tech\\S*|high.?tech\\S*",
              "value added")
SDG9_b_y <- func_AND_plus(SDG9_b_y)





SDG9_c_x = c("access\\S*|covered by",
             "internet|mobile network.?|cyberspace|information highway|information superhighway|wireless|Phone service.?|broadband|cellular network|\\bICT\\b|\\b5G\\b|\\b4G\\b")
SDG9_c_y = c("access\\S*|covered by",
             'information|communication.?',
             '\\btech\\S*')
# SDG9_c_x <- func_AND_plus(SDG9_c_x)
# SDG9_c_y <- func_AND_plus(SDG9_c_y)

ex_ls    <- "Access Bank|Access Line.?|inappropriate access|broadband accesses retail|Internet of Thing" %>%
  strsplit(., split = "\\|") %>% unlist()
temp     <- SDG9_c_x
SDG9_c_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 8, exclude = ex_ls)
temp     <- SDG9_c_y
SDG9_c_y <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 8, exclude = ex_ls, third_AND_string = temp[3])




# SDG9_2   <- func_OR_vector(c(SDG9_2_x, SDG9_2_y, SDG9_2_z))
# SDG9_3   <- func_OR_vector(c(SDG9_3_x, SDG9_3_y, SDG9_3_z))
# SDG9_4   <- func_OR_vector(c(SDG9_4_x, SDG9_4_y, SDG9_4_z, SDG9_4_w))
# SDG9_5   <- func_OR_vector(c(SDG9_5_x, SDG9_5_y))
# SDG9_c   <- func_OR_vector(c(SDG9_c_x, SDG9_c_y))






## 10. Reduce inequality within and among countries ------------------------------------------------

SDG10_1 = c("income|livelihood|expenditure|earning|\\bpay\\b|\\bwage",
            "household|per capita|population",
            paste("empower\\S*", increase_ls, sep = "|"))
####
SDG10_1 <- func_AND_plus(SDG10_1)


SDG10_2_x = c("inclusi\\S*",
              paste("empower\\S*|\\breform\\S*|Prioritiz\\S*|\\bstimulat\\S*|strengthen\\S*|support\\S*|\\bcommitment\\S*|provid\\S*",
                    increase_ls,
                    sep = "|"),
              paste("social|economic|socio.?economic|political|\\bage\\b|\\bsex|\\bgender",
                    female_ls,
                    LGBT_ls,
                    disability_ls,
                    "disadvantaged|\\brace|racial|ethnic|origin|religi\\S*|low.?income|lower.?income|demographic",
                    sep = "|"))
SDG10_2_y = c("financial inclusion.?",
              paste("empower\\S*|\\breform\\S*|Prioritiz\\S*|\\bstimulat\\S*|strengthen\\S*|support\\S*|\\bcommitment\\S*|provid\\S*",
                    increase_ls,
                    sep = "|"))
SDG10_2_x <- func_AND_plus(SDG10_2_x)
SDG10_2_x <- gsub("\\^", "^(?!.*(?:financial inclusion.?))", SDG10_2_x) ## exclude "financial inclusion"

SDG10_2_y <- func_AND_plus(SDG10_2_y)



SDG10_3_x = c(paste(discrimination_ls, "harass\\S*|homophobia\\S*|racism|sexism", sep = "|"),
              reduce_ls)
SDG10_3_y = c("equal opportunit\\S*|equal treatment.?|\\bequitab\\S*|\\bequality|\\bun.?bias\\S*|unprejudic\\S*|impartial\\S*|fairness",
              paste("\\bensure|\\bassur\\S*|guarantee\\b|promot\\S*|empower\\S*|support\\S*|\\bstimulat\\S*|strengthen\\S*",
                    increase_ls, sep = "|"))

# SDG10_3_x <- func_AND_plus(SDG10_3_x)
SDG10_3_y <- func_AND_plus(SDG10_3_y)

temp      <- SDG10_3_x
SDG10_3_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 10)


SDG10_4_x = c("equal\\S*|inequalit\\S*",
              "\\bpolicy|\\bpolicies|fiscal|economic|financ\\S*|\\bwage|income|social|socio.?economic")
SDG10_4_y = "\\bGINI\\b|social protection polic\\S*|Labo.?r share of GDP"

# SDG10_4_x <- func_AND_plus(SDG10_4_x)
# SDG10_4_x <- gsub("\\.\\+", "(?!.*\\bequal to)(?!.*equalization)", SDG10_4_x)  ## to exclude 'financial statement'    # NOT WORK
# SDG10_4_x <- gsub("\\.\\+", "(?!.*equal to)(?!.*equalization)", SDG10_4_x)     ## to exclude 'financial statement'    # WORK
# SDG10_4_x <- gsub("\\.\\+", "(?!.*\\\\bequal to)(?!.*equalization)(?!.*\\\\bprice)(?!.*fixed income)(?!.*gender equality)", SDG10_4_x)  ## to exclude 'financial statement'    # WORK (use this, note that must add more \\ to the replacement)

# SDG10_4_x <- gsub("\\^", "^(?!.*\\bequal to)(?!.*equalization)", SDG10_4_x) ## to exclude 'financial statement'   # NOT WORK
# SDG10_4_x <- func_AND_vector(SDG10_4_x)
# SDG10_4_x <- paste0("^(?!.*\\bequal to)(?!.*equalization)", SDG10_4_x) ## to exclude 'financial statement'        # WORK (this also work, but not the same as others)


temp <- SDG10_4_x
ex_ls<- c('\\bequal to', 'equalization', '\\bprice', 'fixed income', 'gender equality')
SDG10_4_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 10, exclude = ex_ls)


SDG10_5 = c("\\bregulat\\S*|monitor\\S*|administer\\S*|govern.?\\b|governed|governing|\\bsound\\b",
            "financial market.?|financial institution.?")
ex_ls   <- "regulatory|regulator.?|General government|Corporate Governance|Financial Institutions Act|Pension Fund\\S*" %>%
  strsplit(., split = "\\|") %>% unlist()
SDG10_5 <- func_AND_plus(SDG10_5)
SDG10_5 <- func_to_exclude_terms(which_sdg_term = SDG10_5, terms_to_exclude = ex_ls) ## to exclude



SDG10_6_x = c("representation.?|\\bvoic\\S*|\\bright.?|\\bvot\\S*|effective|credib\\S*|accountabl\\S*|legitimate|justif\\S*|lawful",
              "econom\\S*|financ\\S*|\\bfund.?\\b|\\bfunding\\b",
              "institution.?|organization.?|organisation.?|association.?|\\bunion.?|\\bsystem.?",
              developing_country_ls)
SDG10_6_x <- func_AND_plus(SDG10_6_x)
SDG10_6_x <- func_to_exclude_terms(which_sdg_term = SDG10_6_x, terms_to_exclude = financial_exclude_ls)


SDG10_6_y <- c("\\bno \\b|\\bnot\\b|\\none\\b",
               "casting vot\\S*")
SDG10_6_z <- c("\\beach\\b",
               "one vote")
SDG10_6_y <- func_AND_plus(SDG10_6_y)

# SDG10_6_z <- func_AND_plus(SDG10_6_z)
temp      <- SDG10_6_z
SDG10_6_z <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6)


SDG10_7_x = c("migrat\\S*",
              "orderly|safe|regular|responsible|\\bpolicy|\\bpolicies")
SDG10_7_y = c("mobili\\S*",
              "people|population|employee\\S*",
              "orderly|safe|regular|responsible|\\bpolicy|\\bpolicies")
SDG10_7_z = "refugee.?"

SDG10_7_x <- func_AND_plus(SDG10_7_x)

temp <- SDG10_7_y
### Option 1
# SDG10_7_y <- func_AND_plus(SDG10_7_y)
### Option 3 ----
SDG10_7_y <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, third_AND_string = temp[3])


SDG10_a = c(paste(reduce_ls, "\\bspecial|differential|\\bfree\\b", sep = '|'),
            'tariff.?|duty|\\btax\\b|\\bWTO\\b',
            developing_country_ls)
SDG10_a   <- func_AND_plus(SDG10_a)




SDG10_b_x = c("financ\\S*|\\bfunding|\\bassistance|budget.?|\\baid.? to|government aid.?\\b|state aid.?\\b|invest\\S*|donor|donat\\S*|official flow.?",
              "develop\\S*|resource.?|capabilit\\S*|national plan\\S*|program\\S*",
              developing_country_ls)
SDG10_b_y = c("north-south divide\\S*|financial flow.?|resource flow.?|foreign direct investment.?|\\bFDI\\b|\\bODA\\b",
              developing_country_ls)

SDG10_b_y <- func_AND_plus(SDG10_b_y)

temp <- SDG10_b_x
w_ex <- financial_exclude_ls
### Option 1
# SDG10_b_x <- func_AND_plus(SDG10_b_x)
# SDG10_b_x <- gsub("\\.\\+", "(?!.*(?:financial statement|financial group|Financial Report|Financial Review))", SDG10_b_x) ## to exclude
### Option 4 ----
SDG10_b_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 5, third_AND_string = temp[3], exclude = w_ex)


SDG10_c_x = c("remittance\\S*",
              "\\bcost\\b|\\bcosts\\b",
              reduce_ls)
SDG10_c_y = c("remittance\\S*",
              "migrant\\S*|emigrant\\S*|immigrant\\S*|corridor\\S*")
SDG10_c_x <- func_AND_plus(SDG10_c_x)
SDG10_c_y <- func_AND_plus(SDG10_c_y)


SDG10_general = c("\\bequal\\S*|inequalit\\S*|unequal\\S*",
                  "\\bSocial|societal|societ\\S*|\\bSocio\\S*|environm\\S*|health\\S*")
SDG10_general <- func_AND_plus(SDG10_general)




# SDG10_3   <- func_OR_vector(c(SDG10_3_x, SDG10_3_y))
# SDG10_4   <- func_OR_vector(c(SDG10_4_y, SDG10_4_x))
# SDG10_7   <- func_OR_vector(c(SDG10_7_x, SDG10_7_y))
# SDG10_b   <- func_OR_vector(c(SDG10_b_x, SDG10_b_y))
# SDG10_c   <- func_OR_vector(c(SDG10_c_x, SDG10_c_y))







## 11. Sustainable Cities and Communities --------------------------------------------------------

waste_ls = "waste\\S*|rubbish|garbage\\S*|junk|debris|trash|litter.?|sewage\\S*|sludge|scrap metal|scrap lumber|solvent\\S*|product residue\\S*|kiln dust|slag|fly ash"


SDG11_1_x = c("access|availab\\S*|affordab\\S*|adequate|formal|safe|insufficient|deficient|\\black|scarce|scant|unsatisfactory",
              paste("\\bhouse\\b|\\bhouses\\b|\\bhousing\\b|\\bhome\\b|apartment.?|condo|dwelling\\S*|residence|accommodation\\S*|\\bshelter",
                    "basic service.?|sanitation.?|hygiene|electricity|slum\\b|slums\\b|slummy|Shanty|\\bliving", sep = "|"))
SDG11_1_y = c(paste(urban_ls, reduce_ls, "upgrad\\S*|enhanc\\S*", sep = "|"),
              "slum\\b|slums\\b|slummy")
SDG11_1_z = "formal settlement.?|housing finance"

# SDG11_1_x <- func_AND_plus(SDG11_1_x)
SDG11_1_y <- func_AND_plus(SDG11_1_y)

temp <- SDG11_1_x
w_ex <- c("Access Bank")
SDG11_1_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 5, exclude = w_ex)


SDG11_2 = c("access|availab\\S*|affordable",
            "transport\\S*|\broad.?\\b|congestion\\S*|shipment.?|\\btransit.?\\b|\\bbus\\b|\\bbuses\\b|\\bbusses\\b|shuttle.?")
temp <- SDG11_2
w_ex <- c("suppl\\S*", "equipment\\S*", "Access Bank")
### Option 1
# SDG11_2 <- func_AND_plus(SDG11_2)
# SDG11_2 <- gsub("\\.\\+", "(?!.*suppl)(?!.*equipment)(?!.*Access Bank)", SDG11_2) ## to exclude
### Option 3 ----
SDG11_2 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6, exclude = w_ex)


SDG11_3_x = c(urban_ls,
              "planning|management.?",
              "participat\\S*|democrat\\S*|integrated")
SDG11_3_y = c(paste("buildings", urban_ls, sep = "|"),
              "inclusi\\S*|sustainab\\S*|resilien\\S*|zero.?carbon|zero.?energy|low.?carbon|low.?energy")

SDG11_3_x <- func_AND_plus(SDG11_3_x)
# SDG11_3_y <- func_AND_plus(SDG11_3_y)
temp <- SDG11_3_y
SDG11_3_y <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6)


SDG11_4_x = c(paste("protect\\S*|safeguard\\S*|preserv\\S*|conserv\\S*|assure|retain\\S*",
                    "maintenance|preventive method\\S*|preventive measure.?",
                    support_ls,
                    sep = "|"),
              paste("heritage|archaeological site.?|geological site.?|monumental sculpture\\S*|monumental painting\\S*",
                    "monument.?|museum.?|Architectural|cultural landscape\\S*|local cultur\\S*|Rare breed\\S*",
                    "historic\\S{0,4} building\\S*|historic\\S{0,4} site\\S*",
                    "historic\\S{0,4} place.?|\\btradition.?\\b|\\btraditional music|\\bcustom.?\\b",
                    "aesthetic belief|spiritual belief.?",
                    "artistic expression\\S*|African music",
                    "\\blanguage.?|performing art\\S*|social practice\\S*|\\britual.?\\b|festive event\\S*",
                    "traditional craftsmanship",
                    "nature reserve.?|natural habitat.?|protected natural area.?|marine ecosystem.?|sanctuar\\S*",
                    "zoological garden.?|Botanical garden.?",
                    sep = "|"))
SDG11_4_y = c(paste("protect\\S*|safeguard\\S*|preserv\\S*|conserv\\S*|assure|retain\\S*",
                    "maintenance|preventive method\\S*|preventive measure.?",
                    support_ls,
                    sep = "|"),
              paste("Cultural\\S*|symbolic\\S*|historic\\S*|artistic\\S*|aesthetic\\S*|ethnological\\S*",
                    "anthropological\\S*|scientific\\S*|social\\S*",
                    sep = "|"),
              "significance.?|diversity")

temp    <- SDG11_4_x
SDG11_4_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6)
temp    <- SDG11_4_y
SDG11_4_y <- lookaround_nearby_n(word_ls1 = temp[2], word_ls2 = temp[3], n = 2, third_AND_string = temp[1])





SDG11_5_x = c(reduce_ls,
              paste(death_ls, "economic loss|\\bmissing|affected|damag\\S*|\\bdisrupt\\S*", sep = '|'),
              disaster_ls)
SDG11_5_x <- func_AND_plus(SDG11_5_x)


SDG11_6_x = c(paste("contaminat\\S*|environment impact.?|environmental impact.?|impact.? environment\\S*",
                    "\\bair.?quality|\\bsmog.?|\\bhaze|pm.?2.?5|pm.?10|particulate matter.?",
                    waste_ls,
                    "spill\\S*|\\bsoil\\b|footprint.?|pollution\\S*|pollutant\\S*|recycl\\S*|circular|toxin|toxic", sep = "|"),
              urban_ls)
SDG11_6_x <- func_AND_plus(SDG11_6_x)
SDG11_6_y = "waste generat\\S*|waste manag\\S*|waste collected|waste controlled|alternative waste disposal.?"


SDG11_7_x = c("safe|secure|inclusi\\S*|accessible|available|reachable|\\bgreen\\b|\\bpublic|\\bnatur\\S*",
              "\\bspace.?\\b|bikeway")
## -> 'area' is removed to avoid noise
SDG11_7_y = c("physical|sexual",
              "harass\\S*",
              paste(urban_ls, "public space.?|Work.?place.?", sep = "|"))
SDG11_7_x <- func_AND_plus(SDG11_7_x)
SDG11_7_y <- func_AND_plus(SDG11_7_y)


SDG11_a_x = c("econom\\S*|social|socio\\S*|environment\\S*|socio.?economic|ecolog\\S*",
              "\\blink\\S*|balanc\\S*|\\bbind\\S*|bridg\\S*|connect\\S*|network\\S*",
              urban_ls,
              "rural|Countryside| peri.?urban|\\bsuburb|\\boutskirt")
SDG11_a_y = c(urban_ls,
              paste("development plan.?|fiscal", policy_ls, sep = '|'),
              "\\bnational|regional|balanced territorial development|local fiscal space")
SDG11_a_x <- func_AND_plus(SDG11_a_x)
SDG11_a_y <- func_AND_plus(SDG11_a_y)



SDG11_b_x = c(paste(urban_ls, "local government", sep = "|"),
              policy_ls,
              "inclusi\\S*|resource efficien\\S*")
SDG11_b_y = c(paste(urban_ls, "local government", sep = "|"),
              policy_ls,
              "mitigation\\S*|adaptation\\S*|resilien\\S*",
              paste("climate change", disaster_ls, sep = "|"))
SDG11_b_z <- c("risk management\\S*|risk reduction.?",
               paste('climate change', disaster_ls, sep = "|"))

SDG11_b_x <- func_AND_plus(SDG11_b_x)
SDG11_b_y <- func_AND_plus(SDG11_b_y)
# SDG11_b_z <- func_AND_plus(SDG11_b_z)

temp      <- SDG11_b_z
SDG11_b_z <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)



SDG11_c = c(support_ls,
            "resilient building|sustainable building",
            developing_country_ls)
SDG11_c <- func_AND_plus(SDG11_c)


####
# SDG11_1   <- func_OR_vector(c(SDG11_1_x, SDG11_1_y, SDG11_1_z))
# SDG11_3   <- func_OR_vector(c(SDG11_3_x, SDG11_3_y))
# SDG11_7   <- func_OR_vector(c(SDG11_7_x, SDG11_7_y))
# SDG11_a   <- func_OR_vector(c(SDG11_a_x, SDG11_a_y))









## 12. Responsible Consumption and Production ---------------------------------------------------


SDG12_1_x1 = c("sustainab\\S*|\\bgreen\\b|\\bgreener\\b|\\bclean\\b|Responsible|Eco.?Friendly|Environmentally.?Friendly|Environment.?Friendly|Recyclable|\\befficien\\S*",
               "consum\\S*|\\bproduction.?|products|manufactur\\S*")
temp <- SDG12_1_x1
SDG12_1_x1 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 8,
                                  exclude = c("consumer.?", "responsible for\\b", "demand for products"))
SDG12_1_x2 = "\\bSCP\\b|\\b10YFP\\b"



SDG12_2_x = c("sustainab\\S*|\\befficien\\S*",
              "resource.?|material.?\\b|\\bland\\b",
              "manag\\S*|\\busing\\b|\\buse\\b|\\buses\\b|consumption.?|production.?")
SDG12_2_y = c("manag\\S*|conserv\\S*|protocol.?",
              paste("natural.?resource.?|natural capital|natural asset.?|biological resource.?|Renewable resource.?|raw material.?",
                    "basic material.?|primary material.?",
                    "\\btimber.?|land resource.?|forest resource.?|mineral resource.?", sep = "|"))
SDG12_2_z = c("resource.?efficien\\S*|efficient resource.?",
              increase_ls)
SDG12_2_w = c("material.?\\b|resource.?|\\bland\\b|biomass|fossil.?fuel.?|Fossil.?energy|metal ores|industrial mineral.?",
              "\\bcoal\\b|\\bpetroleum\\b|natural gas|oil shale|\\bbitumen\\S*|\\btar sand.?|heavy oil.?",
              "footprint.?|\\bintensity|\\bflow\\b|\\bflows\\b|consum\\S*|depletion\\S*|overconsumption",
              paste("manag\\S*|per GDP|per capita|\\befficien\\S*", reduce_ls, sep = "|"))

SDG12_2_x <- func_AND_plus(SDG12_2_x)
SDG12_2_x <- func_to_exclude_terms(which_sdg_term = SDG12_2_x, terms_to_exclude = c("Human Resource.?"))
SDG12_2_y <- func_AND_plus(SDG12_2_y)
SDG12_2_z <- func_AND_plus(SDG12_2_z)
SDG12_2_w <- func_AND_plus(SDG12_2_w)



SDG12_3 = c(paste(reduce_ls, "halve|\\bhalf", sep = "|"),
            "food|harvest\\S*|supply chain.?",
            "wast\\S*|\\bloss\\S*|spoil\\S*")
# SDG12_3   <- func_AND_plus(SDG12_3)
temp    <- SDG12_3
SDG12_3 <- lookaround_nearby_n(word_ls1 = temp[2], word_ls2 = temp[2], n = 4, third_AND_string = temp[1])



waste_ls_plus <-
  paste(waste_ls,
        "chemical.?|contaminat\\S*|pesticide\\S*|pollut\\S*|Heavy metal.?|Mercury|air emission.?",
        "\\bpoison\\S*|hazardous\\S*|\\btoxin\\S*|\\btoxic\\S*",
        "virulent|radioactive|Chlorinated aliphatic hydrocarbons|\\bWood preserv\\S*",
        "Multisource leachate\\S*|Petroleum refin\\S*|Explosives manufactur\\S*",
        "\\bPFOA\\b|\\bPFOS\\b|\\bPFAS\\b|olatile organic compound.?|\\bVOC\\b",
        ls_hazardous_waste_chemicals,
        sep = "|")
SDG12_4_x = c(paste("manag\\S*|\\btreat\\S*|agreement.?|responsib\\S*|collected|controlled|handled|removal|life cycle|per capita",
                    "environmentally.?sound|Eco.?Friendly|Environmentally.?Friendly",
                    reduce_ls, sep = '|'),
              waste_ls_plus)
# SDG12_4_x <- func_AND_plus(SDG12_4_x)
temp      <- SDG12_4_x
ex_ls <- "chemical.? industr\\S*|Chemical.? division.?|Chemical.? segment\\S*|chemical.? business\\S*|Chemical.? Index|treatment plant.?" %>%
  strsplit(., split = "\\|") %>% unlist()
SDG12_4_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 8, exclude = ex_ls)



SDG12_4_x2 = c("\\bsafe\\S*|responsib\\S*|sustainab\\S*|manag\\S*",
               "disposal.?|handl\\S*|incinerat\\S*|land.?fill\\S*",
               waste_ls_plus)
temp <- SDG12_4_x2
SDG12_4_x2 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, third_AND_string = temp[3])




SDG12_4_y = "environmental permit.?|Basel Convention|Rotterdam Convention|Stockholm Convention|Montreal Protocol|Minamata Convention"


SDG12_4_z = c("environmental impact.?|environmental issue.?|environmental risk.?",
             paste(reduce_ls, "de.?coupl\\S*|informed",
                   "\\baddress\\S*|tackl\\S*|response to|cope.?with|coping with|deal.?with|dealing with",
                   "handl\\S*|\\bmeasure.?\\b|\\blower\\b|\\bless\\b",
                   sep = "|"))
temp <- SDG12_4_z
w_ex <- c("Limiting Our Environmental Impact")
SDG12_4_z <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6, exclude = w_ex)




SDG12_5_x = c(paste("recycl\\S*|\\bre.?us\\S*|re.?utiliz\\S*|re.?possess\\S*|\\bregenerat\\S*|circular",
                    "circulat\\S*|circulating|reclaimed|reclamation.?",
                    "\\breduc",
                    sep = "|"),
              paste(waste_ls,
                    "\\bpaper\\b|cardboard|packaging|\\bfibre.?|\\bfiber.?|\\bpulp\\b|plastic\\S*|\\bglass\\b|\\bresidue.?\\b",
                    "by.?product\\S*|material.?\\b|metal.?|resource.?|furniture.?|mattresse.?",
                    sep = "|"))
temp <- SDG12_5_x
### Option 1
# SDG12_5_x <- func_AND_plus(SDG12_5_x)
### Option 2 ----
SDG12_5_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6)


SDG12_5_y = c("\\brepair\\S*|\\brefurbish\\S*",
              "material.?\\b|resource.?|products")
# SDG12_5_y <- func_AND_plus(SDG12_5_y)
temp      <- SDG12_5_y
SDG12_5_y <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6)

SDG12_5_z = "cradle to cradle|Life cycle assessment.?|Circular economy|recycling rate.?|refurbishment.?"



## ------------------------------------------------------------------------------------ ##
SDG12_6_x = c("sustainab\\S*",
              "practic\\S*|action.?|reporting|\\bpublish\\S*|information",
              company_ls)
temp <- SDG12_6_x

w_ex <- c('Annual Report.?', 'Corporate Information', 'Initiatives for Sustainable Growth',
          'Sustainability Meeting.?', 'Corporate Governance Sustainability', 'Company Overview')

### Option 1
# SDG12_6_x <- func_AND_plus(SDG12_6_x)
# SDG12_6_x <- gsub("\\.\\+",
#                   "(?!.*Annual Report)(?!.*Corporate Information)(?!.*Initiatives for Sustainable Growth)(?!.*Sustainability Meeting)(?!.*Corporate Governance Sustainability)(?!.*Company Overview)",
#                   SDG12_6_x) ## to exclude
### Option 4: to use look around + AND function + exclude ----
SDG12_6_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, third_AND_string = temp[3], exclude = w_ex)
## ------------------------------------------------------------------------------------ ##


SDG12_6_y = c("\\bsoci\\S*",
              "responsibilit\\S*",
              company_ls,
              increase_ls)
SDG12_6_y <- func_AND_plus(SDG12_6_y)
SDG12_6_y <- func_to_exclude_terms(which_sdg_term = SDG12_6_y, terms_to_exclude = c("Social Responsibility Committee"))



SDG12_6_z <- c("commitment\\S*",
               "social responsibilit\\S*|environmental stewardship.?")
SDG12_6_z <- func_AND_plus(SDG12_6_z)


SDG12_6_z1 <- c(company_ls,
                "\\bdisclos\\S*|\\bmanage|share",
                "environmental information")
temp       <- SDG12_6_z1
SDG12_6_z1 <- lookaround_nearby_n(word_ls1 = temp[2], word_ls2 = temp[3], n = 4, third_AND_string = temp[1])




SDG12_6_z2 <- c("\\bdisclos\\S*|reporting",
                "information|data",
                "emission.?")
temp       <- SDG12_6_z2
SDG12_6_z2 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6, third_AND_string = temp[3])



SDG12_7_x = c("sustainab\\S*|\\bGreen\\b",
              "procurement\\S*|acquisition.?")
SDG12_7_y = c("procurement\\S*|acquisition.?",
              "national polic\\S*|national priorit\\S*|national action plan.?",
              "in accordance with|\\bfollow\\S*|align.? with|according to|in line with")
SDG12_7_x <- func_AND_plus(SDG12_7_x)
SDG12_7_y <- func_AND_plus(SDG12_7_y)



SDG12_8 = c("sustainable development|harmony with nature",
            "information|awareness|educat\\S*|curricul\\S*")

SDG12_a = c("sustainab\\S*|\\bgreen\\b|\\bgreener\\b|\\bclean",
            "consum\\S*|\\bproduc\\S*|manufactur\\S*|\\buse|\\busing\\b",
            "scientific|\\bscience|tech\\S*|\\brenewable.?",
            developing_country_ls)

SDG12_b = c("sustainab\\S*|\\bgreen\\b",
            "touris\\S*",
            "monitor\\S*|manag\\S*|accounting|\\btrack\\S*|\\bregulat\\S*|administer\\S*|govern\\S*")

SDG12_c = c("fossil|\\bcoal\\b|\\bcoals\\b|petrol|natural gas|crude oil.?|gasoline|kerosene|non.?renewable.?|traditional energy|Conventional energy",
            "\\bsubsidy|\\bsubsidies|\\bsubsidiz\\S*|\\bsubsidis\\S*|expenditure|expens\\S*")

SDG12_8 <- func_AND_plus(SDG12_8)
SDG12_a <- func_AND_plus(SDG12_a)
SDG12_b <- func_AND_plus(SDG12_b)
SDG12_c <- func_AND_plus(SDG12_c)

####
# SDG12_1   <- func_OR_vector(c(SDG12_1_x1, SDG12_1_x2))
# SDG12_2   <- func_OR_vector(c(SDG12_2_x, SDG12_2_y, SDG12_2_z, SDG12_2_w))
# SDG12_4   <- func_OR_vector(c(SDG12_4_x, SDG12_4_y))
# SDG12_5   <- func_OR_vector(c(SDG12_5_x, SDG12_5_y, SDG12_5_z))
# SDG12_6   <- func_OR_vector(c(SDG12_6_x, SDG12_6_y, SDG12_6_z))











## 13. Climate Action -----------------------------------------------------------------------------


### climate_ls ----
climate_ls = paste("\\bclimat\\S*|global change|Global dimming|global environment change|\\bENSO\\b|\\bEl Ni|Southern Oscillation",
                   "extreme weather|weather pattern.?|warming|\\bwarmer|warmest",
                   # "temperature ris\\S*|temperature increas\\S*|extreme temperature|Temperature overshoot",
                   "temperature",
                   "hotter|hottest|heat.?up|heat.?wave.?",
                   "cloudburst|aridity|drought.?|rainfall shortfall|flood\\S*|\\bstorm\\S*",
                   "extreme heat\\S*|Heat Island",
                   "cold wave.?|extreme precipitation.?|extreme rainfall|heavy rain.?|heavy downpour\\S*",
                   "thunder.?storm.?|ice storm.?",
                   "blizzard\\S*|hailstorm.?|tropical storm.?",
                   "Human activit\\S*|anthropogenic\\S*|wild.?fire\\S*|fire.?storm\\S*",
                   "\\bsea.?level|\\bsea.?ice|\\bmelt\\S*|Ocean acidification",
                   sep = "|")

climate_good_ls = paste(
  "\\bCOP\\b|\\bCOP.?\\d+|\\bIPCC\\b|\\bUNFCC\\b|Paris Agreement\\S*|Paris Climate Agreement.?|Montreal Protocol",
  "nationally determined contribution.?|national adaptation plan.?|Rio Convention.?",
  "1\\.5.?C|2.?C|1\\.5 degrees Celsius|2 degrees Fahrenheit\\S*|two degrees Celsius|net.?zero",
  "negative emission\\S*|Carbon Neutral|carbon sink\\S*|Carbon negative",
  sep = "|")

####
SDG13_1 = c(paste(climate_ls, 'kyoto protocol', sep = "|"),
            paste(disaster_ls, death_ls, "\\bmissing person|human loss", sep = "|"),
            paste("resilien\\S*|adapt\\S*|adjust\\S*|\\bcombat\\S*|\\blimit.?\\b", reduce_ls, sep = '|'))
SDG13_1   <- func_AND_plus(SDG13_1)


SDG13_2_x = paste(climate_good_ls, 'kyoto protocol|tipping point\\S*', sep = '|')
SDG13_2_x = paste0("^(?!.*(?:data.?base\\S*|data.?base\\S*)).*",  ## to exclude this
                   "(", SDG13_2_x, ").+")

SDG13_2_y = c(
  paste("ocean acidification|deforest\\S*|desertificati\\S*|animal farming|farming livestock\\S*|fertilizer\\S*",
        "air pollut\\S*|typhoon\\S*|hurricane\\S*",
        sep = "|"),
  reduce_ls)
temp <- SDG13_2_y
SDG13_2_y <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)

SDG13_2_y2 = c(
  "fossil.?fuel\\S*|fossil.?gas|\\bcoal\\b|\\bcoals\\b|petrol\\S*|natural gas|methane|crude oil.?|traditional energy|Conventional energy",
  "consumption.?|consumed|\\busage.?|\\busing\\b|\\buse\\b|\\buses\\b",
  reduce_ls
)
temp <- SDG13_2_y2
SDG13_2_y2 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, third_AND_string = temp[3])


SDG13_2_z = c(paste(climate_ls, climate_good_ls, sep = "|"),
              paste("adapt\\S*|Mitigation\\S*|action\\S*|\\bact\\b|legislation\\S*|contribut\\S*|convention\\S*|prepar\\S*|fight\\S*|\\bcombat\\S*|tackl\\S*",
                    "response to|cope.?with|coping with|deal.?with|dealing with|handl\\S*|\\bmeasure.?\\b",
                    policy_ls, reduce_ls, sep = '|'))
# SDG13_2_z  <- func_AND_plus(SDG13_2_z)
temp <- SDG13_2_z
SDG13_2_z <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 8,
                                 exclude = c("business climate", "economic climate"))



SDG13_2_w1 <- c(emission_ls,
                paste("captur\\S*|sequest\\S*|\\blimit.?\\b|\\bremov\\S*|absorb\\S*|stabiliz\\S*", reduce_ls, sep = "|"))
SDG13_2_w1 <- func_AND_plus(SDG13_2_w1)


SDG13_2_w2 <- paste("energy transition\\S*|decarboni\\S*", renewable_ls, sep = "|")
SDG13_2_w2 <- paste0("^(?!.*(?:Power segment\\S*|Power Green Camp\\S*|Renewables & Power|wind up|",
                     "Clean Energy Fuels Corp|\\bltd\\b|Total Solar|alternative approach|",
                     "Renewables segment\\S*|energy forward agreement\\S*)).*",
                     "(", SDG13_2_w2, ").+") ## to exclude
###



SDG13_3 = c(paste(climate_ls, 'kyoto protocol', emission_ls, sep = "|"),
            paste("adapt\\S*|captur\\S*|contribut\\S*|convention\\S*", reduce_ls, policy_ls, sep = '|'),
            "educat\\S*|aware\\S*|engag\\S*|outreach|communicat\\S*|\\binforming|\\binform\\b|\\binforms\\b|\\binformed\\b|\\btrain\\S*|tutor\\S*|instruct\\S*|teach\\S*|learn\\S*")
SDG13_3   <- func_AND_plus(SDG13_3)


SDG13_a_x = c(paste(climate_ls, climate_good_ls, 'kyoto protocol', sep = '|'),
              paste(finance_ls, "dollar.?\\b|capital.?|mobiliz\\S*|convention\\S*", sep = '|'))
# SDG13_a_x <- func_AND_plus(SDG13_a_x)
temp <- SDG13_a_x
ex_ls <- c(financial_exclude_ls, 'Martin storm.?', 'economic climate', 'temperature sensor.?', 'section page.?')
SDG13_a_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6, exclude = ex_ls)


SDG13_a_y = "United Nations Framework Convention on Climate Change|\\bunfccc\\b|U\\.N\\.F\\.C\\.C\\.C\\.|Green Climate Fund|\\bGCF\\b|G\\.C\\.F\\."


SDG13_b  = c(paste(climate_ls, climate_good_ls, 'kyoto protocol', sep = '|'),
             paste(policy_ls, "capacity|\\bscope", support_ls, sep = '|'),
             developing_country_ls)
SDG13_b   <- func_AND_plus(SDG13_b)


SDG13_general <- paste(climate_ls, climate_good_ls, 'kyoto protocol', emission_ls, sep = '|')


####
# SDG13_2   <- func_OR_vector(c(SDG13_2_x, SDG13_2_y, SDG13_2_z, SDG13_2_w1, SDG13_2_w2))
# SDG13_a   <- func_OR_vector(c(SDG13_a_x, SDG13_a_y))









## 14. Life Below Water -------------------------------------------------------------------------

ocean_ls = "marine|maritime|ocean|oceanography|\\bsea\\b|seawater\\S*|sublittoral|littoral|coast|tidal|aquatic|coral"


SDG14_1 = c(ocean_ls,
            "pollut\\S*|nutrient\\S*|eutroph\\S*|Kelp|\\balga\\S*|plastic\\S*|micro.?plastic\\S*|debris\\S*|run.?off\\S*|chemical\\S*|arsenic\\S*|contaminat\\S*|fertiliz\\S*|waste")

SDG14_2 = c(paste(ocean_ls, 'coral', sep = "|"),
            "sustainab\\S*|resilien\\S*|\\brestor\\S*|manag\\S*|mitigat\\S*|health|productiv\\S*|habitat\\S*|bleach\\S*")

SDG14_3 = c(ocean_ls,
            "acidi\\S*|\\bpH\\b")

SDG14_1 <- func_AND_plus(SDG14_1)
SDG14_2 <- func_AND_plus(SDG14_2)
SDG14_3 <- func_AND_plus(SDG14_3)



SDG14_4_x = c("\\bfish\\S*|seafood|blue food|\\bcatch\\b|\\bcatches\\b|bycatch|harvest\\S*|overfish\\S*",
              paste(illegal_ls,
                    "unreported|underreport|unregulated|destruct\\S*|destro\\S*|diminish\\S*|exploit\\S*",
                    "sustainab\\S*|\\brestor\\S*|conserv\\S*|manag\\S*|mitig\\S*|\\bregulat\\S*|monitor\\S*|restrict\\S*|enforc\\S*|prohibit\\S*|\\bquota\\b|monitor\\S*",
                    sep = "|"))
SDG14_4_y = "overfish\\S*|maximum sustainable yield|\\bMSY\\b"

SDG14_4_x <- func_AND_plus(SDG14_4_x)



SDG14_5 = c(ocean_ls,
            "\\brestor\\S*|conserv\\S*|protect\\S*|safeguard\\S*|preserv\\S*|\\breduc\\S*|\\bplan\\b|\\bplans\\b|planning|mitig\\S*|restrict\\S*|enforc\\S*|monitor\\S*|prohibit\\S*|reporting")
temp <- SDG14_5
w_ex <- c("Marine and Protective Coatings", "Volvo Ocean Race")
### Option 1
# SDG14_5 <- func_AND_plus(SDG14_5)
### Option 3 ----
SDG14_5 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6, exclude = w_ex)


SDG14_6_x = c("\\bfish\\S*|seafood|\\bcatch\\b|\\bcatches\\b|bycatch|harvest\\S*|aquaculture|blue food|mariculture",
              "\\bsubsidy|\\bsubsidies|\\bsubsidiz|\\bsubsidis")
SDG14_6_y = c("\\bfish\\S*|seafood|\\bcatch\\b|\\bcatches\\b|bycatch|harvest\\S*|aquaculture|blue food|mariculture",
              paste(illegal_ls, "unreported|unregulated|destructive|destro\\S*|diminish\\S*|exploit\\S*|poach\\S*|traffick\\S*", sep = "|"),
              paste("instrument\\S*|\\blaw\\S*|enforc\\S*|restrict\\S*|prohibit\\S*|\\bquota\\b|\\bregulat\\S*|monitor\\S*", policy_ls, sep = '|'))

SDG14_6_x <- func_AND_plus(SDG14_6_x)
SDG14_6_y <- func_AND_plus(SDG14_6_y)



SDG14_7_x = c("econom\\S*|benefi\\S*|sustain\\S*",
              "\\bfish\\S*|aquacultur\\S*|seafood|\\bcatch\\b|\\bcatches\\b|bycatch|blue food|mariculture",
              developing_country_ls)
SDG14_7_y = c("econom\\S*|benefi\\S*|sustain\\S*",
              ocean_ls, "resource\\S*|touris\\S*",
              developing_country_ls)
SDG14_7_x <- func_AND_plus(SDG14_7_x)
SDG14_7_y <- func_AND_plus(SDG14_7_y)



temp <- "scient\\S*|\\bknowledge|research\\S*|technolog\\S*|budget.?|\\bspending|health\\S*"
SDG14_a_x1 = c(ocean_ls,
               temp,
               increase_ls)

SDG14_a_x2 = c(ocean_ls,
               temp,
               "mammal\\S*|species|\\bplant\\S*|animal\\S*|bird.?|biodivers\\S*")
SDG14_a_x3 = c(temp,
               "\\bfish\\S*|cetacean\\S*|\\bwhale.?\\b")
SDG14_a_x4 = "Intergovernmental Oceanographic Commission|Guidelines on the Transfer of Marine Technology"

SDG14_a_x1 <- func_AND_plus(SDG14_a_x1)
SDG14_a_x2 <- func_AND_plus(SDG14_a_x2)
SDG14_a_x3 <- func_AND_plus(SDG14_a_x3)




SDG14_b = c(paste("artisan\\S*|tradition\\S*", "small.?scale", sep = '|'),
            "\\bfish\\S*|seafood|\\bcatch\\b|\\bcatches\\b|bycatch|harvest\\S*",
            "resource\\S*|market.?|\\blaw.?|legal|\\bpolicy|\\bpolicies|institutional|\\bregulat\\S*|jurisdiction\\S*")

SDG14_c = c(ocean_ls,
            "restor\\S*|conserv\\S*|protect\\S*|safeguard\\S*|sustain\\S*|manag\\S*|mitig\\S*|monitor\\S*",
            "\\blaw\\S*|legal\\S*|\\bpolicy|\\bpolicies|institutional|\\bregulat\\S*|jurisdiction\\S*|instrument\\S*")

SDG14_b <- func_AND_plus(SDG14_b)
SDG14_c <- func_AND_plus(SDG14_c)


####
# SDG14_4   <- func_OR_vector(c(SDG14_4_x, SDG14_4_y))
# SDG14_6   <- func_OR_vector(c(SDG14_6_x, SDG14_6_y))
# SDG14_7   <- func_OR_vector(c(SDG14_7_x, SDG14_7_y))
# SDG14_a   <- func_OR_vector(c(SDG14_a_x1, SDG14_a_x2, SDG14_a_x3, SDG14_a_x4))








## 15. Life On Land --------------------------------------------------------------------------------

ecosystem_ls = "eco.?system.?|\\becolog\\S*|environment\\S*|\\bnatur\\S*|environs\\S*|biosphere\\S*"


SDG15_1_x = c("terrestrial|inland|fresh.?water|water eco.?system.?|\\bforest\\S*|wood.?land|wetland|marsh|mountain.?|dryland|rain.?forest\\S*|agroforest\\S*|tundra",
              "conserv\\S*|\\brestor\\S*|reserv\\S*|rehabilitat\\S*|mainten\\S*|protect\\S*|preserv\\S*|safeguard\\S*|secur\\S*|sustain\\S*")
SDG15_1_y = c(ecosystem_ls,
              "service\\S*|\\bgood.?\\b|\\bproduct.?\\b|\\bsupply|\\bsupplies|contribution\\S*",
              "conserv\\S*|\\brestor\\S*|reserv\\S*|rehabilitat\\S*|mainten\\S*|protect\\S*|preserv\\S*|safeguard\\S*|secur\\S*|sustain\\S*")

SDG15_1_x <- func_AND_plus(SDG15_1_x)

temp <- SDG15_1_y
w_ex <- c("financial service.?", "nature reserve.?")
### Option 1
# SDG15_1_y <- func_AND_plus(SDG15_1_y)
# SDG15_1_y <- gsub("\\.\\+", "(?!.*financial service)(?!.*nature reserve)", SDG15_1_y) ## to exclude
### Option 4 ----
SDG15_1_y <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 3, third_AND_string = temp[3], exclude = w_ex)


SDG15_2_x = c("\\btree.?plant\\S*|plant.? tree\\S*|planting tree\\S*|deforest\\S*|afforest\\S*|reforest\\S*|silvicultur\\S*|mangrove\\S*|silviculture|shrub",
              paste("conserv\\S*|\\brestor\\S*|sustain\\S*|manag\\S*|mitig\\S*|health\\S*|resilien\\S*",
                    "stewardship\\S*|certifi\\S*|audit\\S*|account\\S*", increase_ls, sep = "|"))
temp <- SDG15_2_x
### Option 1: use `func_AND_plus` function
# SDG15_2_x <- func_AND_plus(SDG15_2_x)
### Option 2: to use look around function ----
SDG15_2_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)

SDG15_2_y <- c("deforestation|forest product\\S*|timber\\S*|degraded forest\\S*",
               paste("lower", reduce_ls, sep = "|"))
temp <- SDG15_2_y
SDG15_2_y <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 9)
SDG15_2_z <- "Preventive silviculture|\\bREDD\\b|R\\.E\\.D\\.D\\.|Reducing Emissions from Deforestation and forest Degradation"

SDG15_2_w <- c("forest\\S*",
               "management\\S*",
               "sustainable")
temp <- SDG15_2_w
SDG15_2_w <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, third_AND_string = temp[3])


SDG15_3 = c("degrad\\S*|desertif\\S*|contamin\\S*|pollut\\S*|de.?nitrificat\\S*|health\\S*|nitro\\S*|drought\\S*|flood\\S*",
            "\\bland|\\bsoil\\b|sediment\\S*",
            paste("\\bcombat\\S*|conserv\\S*|\\brestor\\S*|sustain\\S*|manag\\S*", reduce_ls, sep = "|"))
SDG15_3   <- func_AND_plus(SDG15_3)


SDG15_4_x = c("mountain\\S*|alpine\\S*",
              paste(ecosystem_ls, "biodiver\\S*|vegetation\\S*|protected", sep = '|'))
SDG15_4_y = "Mountain Green Cover Index"
SDG15_4_x <- func_AND_plus(SDG15_4_x)



SDG15_5_x = c("nature|natural",
              "habitat\\S*|\\breserve.?\\b|\\bpreserve|\\bpark.?\\b")
SDG15_5_x <- func_AND_plus(SDG15_5_x)
SDG15_5_x <- func_to_exclude_terms(which_sdg_term = SDG15_5_x, terms_to_exclude = "natural gas")

SDG15_5_y = paste("Protected Area\\S*|protection area\\S*|conservation area\\S*|conservation park\\S*",
                  "biodivers\\S*|biological diversity|Red List|habitat conversion|habitat degradation\\S*|habitat fragmentation\\S*|habitat loss",
                  sep = "|")
SDG15_5_z = c(paste("habitat|\\breserve.?\\b|sanctuar\\S*|threaten\\S*|endanger\\S*|extinct\\S*|Refuge\\b|\\bcorridor.?",
                    "\\bdiversity|richness|evenness|abundance|vulnerab\\S*|population fragmentation",
                    "extirpate\\S*|environment\\S*|terrestrial|inland|fresh.?water|forest\\S*|grassland|Wetland|Mountain\\S*",
                    sep = "|"),
              "species|Wildlife|animal\\S*|vertebrate\\S*|mammal\\S*|amphibian\\S*|\\bBird.?|Flora|Fauna")
temp <- SDG15_5_z
SDG15_5_z <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, exclude = c("habitat for human", "human habitat"))


SDG15_6 = c("genetic resource\\S*|genetic|\\bgene\\b",
            "\\bfair\\S*|equitab\\S*|access\\S*|availab\\S*|equal\\S*|\\bshare.?\\b|sharing")

SDG15_7 = c(paste("poach\\S*|traffic\\S*", illegal_ls, sep = "|"),
            "species|flora|fauna|wildlife|logg\\S*|animal\\S*|mammal\\S*|\\bhunt\\S*|cultivat\\S*|\\bcatch\\b|\\bcatches\\b")

SDG15_6 <- func_AND_plus(SDG15_6)
SDG15_7 <- func_AND_plus(SDG15_7)


SDG15_8 = "\\binvasive|\\binvasion|\\balien\\S*|\\binvade\\S*|nonindigenous|non.?indigenous|non.?native"



## ------------------------------------------------------------------------------------ ##
SDG15_9 = c(paste("ecosystem\\S*", "biodiver\\S*", sep = '|'),
            "\\bnation\\S*|countr\\S*|region\\S*|local",
            paste("development|poverty reduction|accounting|reporting|Aichi|action plan\\S*|target\\S*|planning|strateg\\S*", policy_ls, sep = "|"))
temp <- SDG15_9
w_ex <- c('business environment\\S*', 'political environment\\S*', 'social environment\\S*')

### Option 1
# SDG15_9 <- func_AND_plus(SDG15_9)
# SDG15_9 <- gsub("\\.\\+", "(?!.*business environment)(?!.*political environment)(?!.*social environment)", SDG15_9) ## to exclude 'financial statement'

### Option 4: to use look around + AND function + exclude ----
SDG15_9 <- lookaround_nearby_n(word_ls1 = temp[2], word_ls2 = temp[3], n = 3, third_AND_string = temp[1], exclude = w_ex)
## ------------------------------------------------------------------------------------ ##



SDG15_a = c(paste(ecosystem_ls, "biodiver\\S*", sep = '|'),
            paste(finance_ls, "revenue\\S*|\\bvalue|incentiv\\S*|\\bspending\\S*", sep = '|'),
            "conserv\\S*|preserv\\S*|sustainable use|sustainably use")
SDG15_a <- func_AND_plus(SDG15_a)
SDG15_a <- gsub("\\.\\+", "(?!.*business environment)(?!.*political environment)(?!.*social environment)", SDG15_a) ## to exclude either of the three terms

SDG15_b_x = c("forest\\S*|biodiversity",
              "conserv\\S*|preserv\\S*|manag\\S*|\\bsustain\\S*",
              paste(support_ls, "revenue\\S*|\\bvalue|incentiv\\S*|\\bspending\\S*", sep = '|'))
SDG15_b_y = c("reforestation\\S*",
              paste(support_ls, "revenue\\S*|\\bvalue|incentiv\\S*|\\bspending\\S*", sep = '|'))

SDG15_b_x <- func_AND_plus(SDG15_b_x)
SDG15_b_y <- func_AND_plus(SDG15_b_y)



SDG15_c = c("poach\\S*|traffic\\S*|illicit\\S*|illegal",
            "species|flora|fauna|wildlife|logg\\S*|animal\\S*|mammal\\S*|\\bhunt\\S*|cultivat\\S*|\\bcatch\\b|\\bcatches\\b",
            "global support\\S*|\\blocal communit\\S*|local entit\\S*")
SDG15_c <- func_AND_plus(SDG15_c)


SDG15_general_x = paste(func_AND_vector(c(ecosystem_ls, "resilien\\S*")),
                        func_AND_vector(c('human', 'wildlife', 'conflict\\S*')),
                        func_AND_vector(c("community", "conserv\\S*")),
                        "eco.?touris\\S*",
                        sep = "|")
SDG15_general_x = paste0("^(", SDG15_general_x, ").+")

SDG15_general_y = c(ecosystem_ls,
                    paste("impact.?\\b|conserv\\S*|\\brestor\\S*|sustaina\\S*|manag\\S*|mitig\\S*",
                          "health.?\\b|\\bwell\\b|resilien\\S*|stewardship\\S*|audit\\S*|account\\S*",
                          sep = "|"))
SDG15_general_y <- func_AND_plus(SDG15_general_y)

####
# SDG15_1   <- func_OR_vector(c(SDG15_1_x, SDG15_1_y))
# SDG15_4   <- func_OR_vector(c(SDG15_4_x, SDG15_4_y))
# SDG15_5   <- func_OR_vector(c(SDG15_5_x, SDG15_5_y, SDG15_5_z))
# SDG15_general   <- func_OR_vector(c(SDG15_general_x, SDG15_general_y))








## 16. Peace, Justice, and Strong Institutions ----------------------------------------------------

SDG16_1_x = c("violen\\S*|victim\\S*|homicid\\S*|murder\\S*|\\bkill\\S*|assault\\S*|assassination\\S*",
              paste(reduce_ls,
                    "\\bcombat\\S*|\\bavoid\\S*|\\baddress\\S*|fight|tackl\\S*",
                    "response to|cope.?with|coping with|deal.?with|dealing with|handl\\S*",
                    sep = "|"))
SDG16_1_y = c("verbal|physical|domestic|psychological|child|\\bsex\\S*",
              "\\babus\\S*|violen\\S*|assault\\S*|torture",
              paste(reduce_ls,
                    "\\bcombat\\S*|\\bavoid\\S*|\\baddress\\S*|fight|tackl\\S*",
                    "response to|cope.?with|coping with|deal.?with|dealing with|handl\\S*",
                    sep = "|"))
SDG16_1_z = "safe walking|peaceful societ\\S*"

SDG16_1_x <- func_AND_plus(SDG16_1_x)
SDG16_1_y <- func_AND_plus(SDG16_1_y)



SDG16_2 = c(child_ls,
            "\\babus\\S*|bully|kidnap\\S*|exploit\\S*|traffick\\S*|victim\\S*|violen\\S*|tortur\\S*|punish\\S*|aggress\\S*|\\brap\\S*|\\bporn\\S*",
            reduce_ls)
SDG16_2 <- func_AND_plus(SDG16_2)


SDG16_3_x = c("access\\S*|resort to|promot\\S*",
              paste("\\bcode.?\\b|\\bnorm.?\\b|justice|authorit\\S*|\\bpolice\\S*|prosecutor.?|\\brule.?\\b",
                    "\\blaw.?\\b|Bylaw.?\\b|\\blegal|legislation|litigation|conflict resolution\\S*|dispute resolution\\S*",
                    sep = "|"))
temp <- SDG16_3_x
w_ex <- c("Access Bank")
SDG16_3_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6, exclude = w_ex)


SDG16_3_y = c("victim\\S*",
              "competent authorit\\S*|violence|detain\\S*|prison|inmate|disput\\S*|officially recognized conflict\\S*")
SDG16_3_y <- func_AND_plus(SDG16_3_y)
SDG16_3_y <- gsub("\\.\\+", "(?!.*Annual Report)", SDG16_3_y) ## to exclude


SDG16_3_z = "actual innocence|false confession|Un.?sentenced detain\\S*|pre.?sentence detention.?|Arbitrary detent\\S*|Enforced disappearance|crime reporting"





SDG16_4_x <- c(paste(illegal_ls, "crime|cybercrime|irregular\\S*|prohibited|smuggl\\S*|unwarranted",
                     "stolen|\\bsteal|theft|seized|traced|conflict\\S*|traffic\\S*|terroris\\S*", sep = "|"),
               paste(finance_ls, "\\barm\\S*|\\bgun\\S*|weapon\\S*|ammunition\\S*|rifle\\S*|pistol\\S*|firearm\\S*|shotgun\\S*|\\basset\\S*", sep = '|'),
               paste("\\brecovery|\\breturn|\\bagainst\\b|\\bAnti.?\\b|\\bcombat\\S*", reduce_ls, sep = "|"))
temp <- SDG16_4_x
w_ex <- c('statement\\S*', '\\bgranted', 'investigat\\S*', financial_exclude_ls)
SDG16_4_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6, third_AND_string = temp[3], exclude = w_ex)



SDG16_4_y <- "unauthorized acquisition\\S*|money.?launder|money.?washing|launder money|laundering money"



SDG16_5_x = c(paste("corrupt\\S*|\\bbrib\\S*|lobbying|extortion\\S*|cronyism|nepotism|parochialism|patronage\\S*",
                    "influence peddling|\\bgraft\\S*|embezzl\\S*|fraud\\S*|\\bextort\\S*|blackmail|Favoritism|clientelism",
                    "Tax evasion\\S*", sep = "|"),
              paste("identif\\S*|takes into account|aware of|\\bpolicy|\\bpolicies|legislation\\S*|\\bregulat\\S*",
                    "manag\\S*|\\brule.?\\b",
                    "\\bagainst\\b|\\bAnti.?\\b|\\bcombat\\S*|\\bavoid\\S*",
                    "\\baddress\\S*|fight\\S*|tackl\\S*|cope.?with|coping with|deal.?with|dealing with|handl\\S*|\\bmeasure.?\\b",
                    reduce_ls, sep = "|"))
SDG16_5_y = c("contact\\S*|asked|\\bpay\\b|\\bpaid|extort\\S*|\\bentrust\\S*",
              "\\bofficial.?")
SDG16_5_z = c('miscarriage', 'justice')
SDG16_5_w = "anticorruption|Abuse of discretion"

# SDG16_5_x <- func_AND_plus(SDG16_5_x)
SDG16_5_y <- func_AND_plus(SDG16_5_y)
SDG16_5_z <- func_AND_plus(SDG16_5_z)

temp <- SDG16_5_x
SDG16_5_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6)



SDG16_6_x = c(paste("institut\\S*|\\bgovern\\S*|legislat\\S*|judicia\\S*",
                    "judicial system.?|penal system.?|legal system.?|criminal tribunal|management system.?",
                    "internal control system.?|Supervisory Board.?|\\bBoard\\b",
                    "public service\\S*|civil service\\S*",
                    "health.?care service.?|health.?care system.?",
                    "education service.?",
                    "Education system.?|curriculum|School facilit\\S*",
                    "government service.?|social welfare service.?",
                    "organization.?\\b|association.?\\b",
                    "society|societies|peer group.?|social group.?|\\breligion\\S*|communit\\S*|Chairman",
                    sep = "|"),
              paste("effective|accountab\\S*|transparent\\S*|satisf\\S*|availability|accessib\\S*|Waiting time",
                    "acceptability|adequate standard|\\bAfford\\S*|Quality of facilit\\S*",
                    "Equal treatment.?|treated equally|Courtesy|courteous",
                    sep = "|"))
temp <- SDG16_6_x
### Option 1
# SDG16_6_x <- func_AND_plus(SDG16_6_x)
### Option 2 ----
SDG16_6_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)


SDG16_6_y = "budget reliability|efficient service delivery|actual.?to.?budget.?"

SDG16_6_z1 <- c("peace\\S*|inclusive|\\bfair\\S*",
                "societ\\S*")
SDG16_6_z2 <- c("public financ\\S*|Public Expenditure.?|government budget\\S*|government expenditure.?",
                "management.?|Accountab\\S*")
SDG16_6_z3 <- c("Actual expenditure.?|Aggregate expenditure.?|final budget\\S*",
                "approved budget\\S*|budgeted expenditure.?")
SDG16_6_z4 <- c("expenditure.?|spending.?",
                "budget.?\\b",
                "ratio|share of|proportion.?|\\bpercent\\S*")

temp <- SDG16_6_z1
SDG16_6_z1 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)
temp <- SDG16_6_z2
SDG16_6_z2 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)
SDG16_6_z3 <- func_AND_plus(SDG16_6_z3)
temp <- SDG16_6_z4
SDG16_6_z4 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4, third_AND_string = temp[3])



temp      <-  "responsive|inclusive|participatory|representative.?|independen\\S*"
SDG16_7_x = c("decision.?|legislature|institution\\S*|public service\\S*|judiciary",
              temp)
SDG16_7_y = "democracy"
SDG16_7_z = c('separat\\S*',
              "\\bpower\\S*")
SDG16_7_w = c("all members",
              "meeting\\S*|discussion\\S*",
              "\\binformed|\\bengag\\S*|\\bparticipat\\S*")
SDG16_7_w2 = c("\\bBoard\\b|Committee\\b|leadership",
               "inclusive|participatory|representative.?",
               paste("\\bgender|\\bsex\\S*|ethni\\S*|\\brace|racial|\\breligion",
                     disability_ls, 'disadvantaged',
                     female_ls, LGBT_ls, sep = "|"))

SDG16_7_x  <- func_AND_plus(SDG16_7_x)
SDG16_7_z  <- func_AND_plus(SDG16_7_z)
SDG16_7_w  <- func_AND_plus(SDG16_7_w)
SDG16_7_w2 <- func_AND_plus(SDG16_7_w2)



SDG16_8_x = c("institution\\S*|global govern\\S*|international organization\\S*",
              "participat\\S*|\\bvote|\\bvoting|\\bvoice|suffrag\\S*|representation|\\belection|electoral",
              developing_country_ls)
SDG16_8_x <- func_AND_plus(SDG16_8_x)
SDG16_8_y = SDG10_6_y
SDG16_8_z = SDG10_6_z

SDG16_8_w = c("\\beffective",
              "participation\\S*|\\bteam.?\\b")
temp <- SDG16_8_w
SDG16_8_w <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 2)


SDG16_9_x = func_AND_plus(c("legal", "identity"))
SDG16_9_y = func_AND_plus(c("birth", "regist\\S*|certifi\\S*"))


SDG16_10_x = c("public access|availab\\S*|attack\\S*|freedom|\\bkill\\S*|kidnap\\S*|murder\\S*|enforc\\S*|disappear\\S*|deten\\S*|detain\\S*|tortur\\S*",
               "journalis\\S*|media|\\bpress|unionis\\S*|human rights advocate\\S*")
SDG16_10_y = 'Aarhus Convention'

SDG16_10_x <- func_AND_plus(SDG16_10_x)



temp <- paste("\\bagainst\\b|\\bAnti.?\\b|\\bcombat\\S*", reduce_ls, sep = "|")
SDG16_a_x1 = c("\\bnation\\S*|countr\\S*|\\bstate.?\\b|domestic|international",
               "institution\\S*|cooperat\\S*|collaborat\\S*|joint effort\\S*|partnership\\S*",
               "violen\\S*|terroris\\S*|\\bcrime|criminal|genocid\\S*|murder\\S*|human traffick\\S*|refugee\\S*|extremis\\S*|insurgen\\S*|\\bwar\\b|\\bwars\\b|warfare\\S*",
               temp)
SDG16_a_x2 = c("\\bnation\\S*|countr\\S*|\\bstate.?\\b|domestic|international",
               "institution\\S*|cooperat\\S*|collaborat\\S*|joint effort\\S*|partnership\\S*",
               "\\barm\\S*|\\bgun\\S*|weapon\\S*",
               "conflict\\S*",
               temp)
SDG16_a_y1 = c("\\bnation\\S*|countr\\S*|\\bstate.?\\b|domestic",
               "independen\\S*",
               "institution\\S*|human right\\S*|civil right\\S*|democracy|treaty|\\bcivil",
               increase_ls)

SDG16_a_y2 = c("\\bpeace",
               "\\bkeep\\S*",
               increase_ls)
SDG16_a_z  = "Paris Principle\\S*|Geneva Convention"


SDG16_a_x1 <- func_AND_plus(SDG16_a_x1)
SDG16_a_x2 <- func_AND_plus(SDG16_a_x2)
SDG16_a_y1 <- func_AND_plus(SDG16_a_y1)

temp <- SDG16_a_y2
SDG16_a_y2 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 3, third_AND_string = temp[3])





SDG16_b = c("\\blaw\\S*|legislat\\S*|\\bpolicy|\\bpolicies|human right\\S*|protect\\S*|safeguard\\S*",
            "discrimin\\S*|harass\\S*")
SDG16_b <- func_AND_plus(SDG16_b)


SDG16_general = paste("ethnic conflict\\S*|exonerat\\S*|justice system\\S*",
                      func_AND_plus(c("environment\\S*", "\\blaw\\S*|govern\\S*|\\benact\\S*|legistlat\\S*")),
                      sep = "|")

SDG16_general_x1 <-
  paste(LGBT_ls,
        "\\bDEI\\b|\\bDEIJ\\b|Social Justice|inclusive societ\\S*|diversity and inclusion",
        sep = "|")

SDG16_general_x2 <- c("\\bDiversity|Inclusion",
                      "\\bEquity")
temp <- SDG16_general_x2
SDG16_general_x2 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 3)



SDG16_general_x3 <- c("\\bdata|\\binformation|\\bcyber\\b|\\bcloud\\b|\\bInternet\\b|\\bnetwork\\b|\\bmobile\\b",
                      paste("privacy|security|protection|sensitive|confidential\\S*|\\battack\\S*",
                            "\\bhack\\S*|\\bcrime|\\bterroris\\S*|warfare\\S*",
                            sep = "|"))
temp <- SDG16_general_x3
SDG16_general_x3 <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)


SDG16_general_x4 <- paste(
  "unauthorized access|\\bGDPR\\b|Privacy Protection|Privacy Act|\\bCOPPA\\b|\\bHIPPA\\b|\\bECPA\\b",
  "\\bInfoSec|Cybercrime|Cyber.?sex\\S*|Computer fraud|Cybergeddon|Cyberterrorism|Cyber.?warfare",
  "Data breach|IT risk.?|cybersecurity|Identity theft",
  sep = "|"
)



####
# SDG16_1   <- func_OR_vector(c(SDG16_1_x, SDG16_1_y, SDG16_1_z))
# SDG16_3   <- func_OR_vector(c(SDG16_3_x, SDG16_3_y, SDG16_3_z))
# SDG16_4   <- func_OR_vector(c(SDG16_4_x, SDG16_4_y))
# SDG16_5   <- func_OR_vector(c(SDG16_5_x, SDG16_5_y, SDG16_5_z))
# SDG16_6   <- func_OR_vector(c(SDG16_6_x, SDG16_6_y, SDG16_6_z))
# SDG16_7   <- func_OR_vector(c(SDG16_7_x, SDG16_7_y, SDG16_7_z))
# SDG16_9   <- func_OR_vector(c(SDG16_9_x, SDG16_9_y))
# SDG16_10  <- func_OR_vector(c(SDG16_10_x, SDG16_10_y))
# SDG16_a   <- func_OR_vector(c(SDG16_a_x1, SDG16_a_x2, SDG16_a_y1, SDG16_a_y2, SDG16_a_z))









## 17. Partnerships for the Goals -----------------------------------------------------------------

### Finance

SDG17_1 = c("domestic resource\\S*|domestic capacity|domestic budget\\S*",
            "revenue\\S*|budget\\S*|\\btax|expenditure\\S*")
SDG17_1 <- func_AND_plus(SDG17_1)
SDG17_1 <- gsub("\\.\\+", "(?!.*governmental charge)", SDG17_1) ## to exclude
SDG17_1_y <- c('government revenue\\S*',
               '\\bGDP\\b')
SDG17_1_y <- func_AND_plus(SDG17_1_y)


SDG17_2 = paste("\\bODA\\b",
                func_AND_plus(c("development assistance", developing_country_ls)), sep = "|")


SDG17_3_x = c(paste("financial resource\\S*|\\bfund.?\\b|\\bfunding\\b|financial assist\\S*",
                    "financial aid|\\baid.? to|government aid|state aid|invest\\S*",
                    "\\bdonat\\S*|official flow\\S*",
                    sep = "|"),
              "\\bto\\b|\\bfor\\b",
              developing_country_ls)
temp <- SDG17_3_x
w_ex <- c('statement\\S*', '\\bgrant.?\\b', 'subsid\\S*', 'budget\\S*', 'investigat\\S*',
          financial_exclude_ls,
          'Principles for Responsible Investment')
### Option 1
# SDG17_3_x <- func_AND_plus(SDG17_3_x)
# SDG17_3_x <- gsub("\\.\\+", "(?!.*statement)(?!.*grant)(?!.*subsid)(?!.*budget)(?!.*investigat)(?!.*financial assumption)(?!.*finance corporation)", SDG17_3_x) ## to exclude these keywords
### Option 4 ----
SDG17_3_x <- lookaround_nearby_n(word_ls1 = temp[2], word_ls2 = temp[3], n = 3, third_AND_string = temp[1], exclude = w_ex)


SDG17_3_y = c("foreign direct investment|\\bFDI\\b|official development assistance|South.?South cooperation.?|remittance\\S*",
              developing_country_ls)
SDG17_3_y <- func_AND_plus(SDG17_3_y)



SDG17_4 = c('debt\\S*',
            "financ\\S*|relief\\S*|restructuring|sustainab\\S*|service\\S*|distress\\S*",
            developing_country_ls)
SDG17_4 <- func_AND_plus(SDG17_4)
SDG17_4 <- func_to_exclude_terms(which_sdg_term = SDG17_4, terms_to_exclude = financial_exclude_ls) ## to exclude 'financial statement'


SDG17_5 = c(finance_ls,
            'promot\\S*',
            developing_country_ls)

### Technology

SDG17_6 = c("access\\S*|availab\\S*|facilit\\S*",
            "\\bscience|\\btech\\S*|innovation\\S*|knowledge.?sharing|Internet|broadband",
            paste("North.?South|South.?South|triangular region\\S*", developing_country_ls, sep = "|"))

SDG17_7 = c("environment\\S*",
            "technolog\\S*",
            "development\\S*|transfer.?|dissemination\\S*|diffusion\\S*",
            developing_country_ls)

SDG17_8 = c("technolog\\S*|\\bscience|innovation\\S*|information|communication.?|internet",
            "capacity",
            developing_country_ls)

SDG17_5 <- func_AND_plus(SDG17_5)
SDG17_6 <- func_AND_plus(SDG17_6)
SDG17_7 <- func_AND_plus(SDG17_7)
SDG17_8 <- func_AND_plus(SDG17_8)



### Capacity-building
SDG17_9_x = c("capacity building|international support.?",
              developing_country_ls)

SDG17_9_y = c("financ\\S*|\\btechnical|technolog\\S*",
              "assistance|\\baid\\b",
              developing_country_ls)
SDG17_9_z = c("North.?South|South.?South|triangular",
              "cooperat\\S*|collaborat\\S*|joint effort\\S*|partnership\\S*",
              developing_country_ls)

SDG17_9_x <- func_AND_plus(SDG17_9_x)
SDG17_9_y <- func_AND_plus(SDG17_9_y)
SDG17_9_y <- gsub("\\.\\+", "(?!.*financial statement\\S*)", SDG17_9_y) ## to exclude 'financial statement'
SDG17_9_z <- func_AND_plus(SDG17_9_z)




### Trade
SDG17_10_x = c("\\btrade\\b|\\btrading|\\bWTO\\b",
               "universal|rules.?based|\\bopen\\b|non.?discrimin\\S*|equitab\\S*|equal\\S*|multilateral")
SDG17_10_y = paste("Doha Development Agenda",
                   func_AND_plus(c('tariff\\S*', 'average')),
                   sep = "|")
SDG17_10_x <- func_AND_plus(SDG17_10_x)
SDG17_10_x <- gsub("\\.\\+", "(?!.*equity method\\S*)(?!.*equity instrument\\S*)(?!.*liabilities and equity)(?!.*trading income)(?!.*open group)(?!.*Private Limited)", SDG17_10_x) ## to exclude



SDG17_11 = c(increase_ls,
             "export\\S*",
             developing_country_ls)

SDG17_12_x = c("market access",
               developing_country_ls)
SDG17_12_y = c("duty|duties|\\bquota\\b|quotum",
               "\\bfree\\b",
               developing_country_ls)

SDG17_11   <- func_AND_plus(SDG17_11)
SDG17_12_x <- func_AND_plus(SDG17_12_x)
SDG17_12_y <- func_AND_plus(SDG17_12_y)



### Systemic issues
SDG17_13_x = c("macroeconomic\\S*|macro.?econom\\S*",
               "stability")
SDG17_13_y = c("macroeconomic\\S*|macro.?econom\\S*",
               "\\bpolicy|\\bpolicies",
               "coordination|coherence")
SDG17_14 = c(policy_ls,
             "coheren\\S*|coordinat\\S*|consistent|systematic",
             "sustainab\\S*|\\bSDG.?")

SDG17_13_x <- func_AND_plus(SDG17_13_x)
SDG17_13_y <- func_AND_plus(SDG17_13_y)
SDG17_14   <- func_AND_plus(SDG17_14)



SDG17_15_x = c("respect",
               "leadership|initiative|\\blead\\S*|direction\\S*|\\bpolicy|\\bpolicies|framework\\S*|planning",
               "countr\\S*|\\bnation\\S*|\\bState\\b|government\\S*")
SDG17_15_y = c('\\bpolicy|\\bpolicies|framework\\S*|\\bplan\\b|\\bplans\\b|planning|objective\\S*|priorit\\S*|goal.?|target.?',
               "country.?owned|country.?led|nation.?owned|nation.?led|\\bState.?owned|\\bState.?led|government.?owned|government.?led")

SDG17_15_x <- func_AND_plus(SDG17_15_x)
SDG17_15_x <- func_to_exclude_terms(which_sdg_term = SDG17_15_x, terms_to_exclude = c("with respect to"))
SDG17_15_y <- func_AND_plus(SDG17_15_y)




SDG17_16_x = c("\\bmulti.?",
               "stakeholder\\S*|partner\\S*|cooperat\\S*|collaborat\\S*",
               "Sustainable|\\bSDG.?|goal.?")
SDG17_16_y = c("\\bshare.?\\b|\\bsharing|mobiliz\\S*|assembl\\S*|marshal\\S*",
               paste("\\bknowledge|expertise|\\btech\\S*|automation", "financial resource\\S*", sep = '|'),
               "Sustainable|\\bSDG.?|goal.?\\b")
SDG17_16_z = c("Global Partnership|joint effort\\S*",
               "sustainable|\\bSDG.?|goal.?\\b")

temp <- SDG17_16_x
SDG17_16_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 3, third_AND_string = temp[3])
SDG17_16_y <- func_AND_plus(SDG17_16_y)
SDG17_16_z <- func_AND_plus(SDG17_16_z)



SDG17_17 = c("partnership\\S*",
             "\\bpublic|private|civil societ\\S*|government\\S*")

SDG17_18 = c("\\bdata|statistic\\S*|indicator\\S*",
             "high.?quality|timely|reliable|availab\\S*",
             developing_country_ls)

SDG17_17   <- func_AND_plus(SDG17_17)
SDG17_18   <- func_AND_plus(SDG17_18)


SDG17_19_x = c("measur\\S*|monitor\\S*",
               "sustainable develop\\S*|\\bSDG.?")
SDG17_19_x <- func_AND_plus(SDG17_19_x)
SDG17_19_y <- func_AND_plus(c("statistic\\S*", "capacity|strengthen\\S*"))
SDG17_19_z <- func_AND_plus(c("census", "population|\\bhousing"))
SDG17_19_w <- func_AND_plus(c("regist\\S*", "birth|death"))



SDG17_general_x = c("sustainab\\S*|global",
                    "partner\\S*|cooperat\\S*|collaborat\\S*|joint effort\\S*|Alliance|coordinat\\S*|\\bstabil\\S*|stabl\\S*")
SDG17_general_y = c("partner\\S*|cooperat\\S*|collaborat\\S*|joint effort\\S*|Alliance",
                    "between",
                    developing_country_ls)

# SDG17_general_x <- func_AND_plus(SDG17_general_x)
# SDG17_general_y <- func_AND_plus(SDG17_general_y)
temp <- SDG17_general_x
SDG17_general_x <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 4)
temp <- SDG17_general_y
SDG17_general_y <- lookaround_nearby_n(word_ls1 = temp[1], word_ls2 = temp[2], n = 6, third_AND_string = temp[3])




####
# SDG17_1    <- func_OR_vector(c(SDG17_1,   SDG17_1_y))
# SDG17_3    <- func_OR_vector(c(SDG17_3_x, SDG17_3_y))
# SDG17_9    <- func_OR_vector(c(SDG17_9_x, SDG17_9_y, SDG17_9_z))
# SDG17_10   <- func_OR_vector(c(SDG17_10_x, SDG17_10_y))
# SDG17_12   <- func_OR_vector(c(SDG17_12_x, SDG17_12_y))
# SDG17_13   <- func_OR_vector(c(SDG17_13_x, SDG17_13_y))
# SDG17_15   <- func_OR_vector(c(SDG17_15_x, SDG17_15_y))
# SDG17_16   <- func_OR_vector(c(SDG17_16_x, SDG17_16_y, SDG17_16_z))
# SDG17_19   <- func_OR_vector(c(SDG17_19_x, SDG17_19_y, SDG17_19_z, SDG17_19_w))







################################################################################################## #
# Direct mention -----------------------------------------------------------------------------------
################################################################################################## #
## Terms for this type are fairly straightforward and easy to do ...

# source('./Code/helper_UN_SDG_Target_list.R') ## --> ls_un; ls_un_id; goals_ls ## a copy of the code

goals_ls <- paste('SDG', seq(1,17), sep = ''); goals_ls
goals_df <- data.frame(goal = goals_ls)


# ls_un <- read.csv('./data/_ls_un_goal_target.csv', stringsAsFactors = F) %>%
#   dplyr::filter(!is.na(GoalID)) %>%
#   tidyr::separate(Targets, c('target_id_un', 'target_desc_un'), sep = ' ', extra = 'merge', remove = T)
# str(ls_un)

#load the rda file
load(file = "./data/list_of_un_goals_targets.rda")
ls_un <- list_of_un_goals_targets

ls_un_id <- ls_un %>%
  dplyr::select(GoalID, target_id_un) %>%
  tidyr::separate(target_id_un, c('target_id_un1', 'target_id_un2'), sep = '\\.', remove = F) %>%
  dplyr::select(-target_id_un1) %>%
  dplyr::mutate(goalname = paste0('SDG', GoalID),
                goalname = factor(goalname, levels = goals_ls))

## Goal level -----------------
# SDG1	 = ('SDG 1\\b'|'SDG1\\b'|Goal 1\\b|Goal1\\b)
# SDG1_1 = ('SDG 1.1\\b'|'SDG1.1\\b')

# goal_ls1 <- data.frame(term = goals_ls)
# goal_ls2 <- data.frame(term = paste0('SDG ', seq(1, 17)))
# goal_ls3 <- ls_un %>% dplyr::distinct(GoalName) %>% dplyr::rename(term = GoalName)
# goal_ls <- rbind(goal_ls1, goal_ls2, goal_ls3)


## 1.1 create a list of SDG id names, such as SDG 1, goal 17, ...
goal_ls_x <- paste0(
  '(sdg|goal)',
  # '.{0,2}',     ## `.` matches any character (e.g., a space or `s`); `{0,2}` matches the previous token between 0 and 2 times
  '[^0-9]{0,2}',  ## the above one can be problematic if `.` can be a number. Here we change it to match only non-numeric character
  '(?=',          ## Positive lookahead: e.g., in "SDG(?=17)", "17" immediately follows the "SDG".
  seq(1, 17),
  '\\b',          ## \\b assert position at a word boundary
  ')'
)

## 1.2 get a list of SDG descriptions for each Goal
goal_ls_y <- ls_un %>% dplyr::distinct(GoalName)
goal_ls_y <- goal_ls_y$GoalName

## 1.3 put 1 and 2 together
goal_keys <- data.frame(SDG_id = paste0('SDG', seq(1, 17), '_general'),
                        goal_ls_x,
                        goal_ls_y) %>%
  dplyr::mutate(SDG_keywords = paste(goal_ls_x, goal_ls_y, sep = "|")) %>%
  dplyr::select(SDG_id, SDG_keywords)



## Target level ------------------------------------------------------------------------------------

### get a list of target ids
targ_df  <- ls_un %>% dplyr::distinct(target_id_un)
targ_ids <- targ_df$target_id_un; targ_ids
targ_ids_ <- gsub('\\.', '\\\\.', targ_ids); targ_ids_ ## '.' can be matched with any character

target_ls <- paste0(
  '(sdg|goal|target|indicator)',
  # '.{0,2}',        ##
  '[^0-9]{0,2}',
  '(?=', targ_ids_,  ##
  '[\\.]{0,1}',      ## there might be 0 or 1 period follows, e.g., if targ_ids = 17.1, then this can match '17.1' and '17.1.1' but not '17.11'
  ')'                ##
)

target_keys <- data.frame(SDG_id = paste0('SDG', targ_ids),
                          SDG_keywords = target_ls)%>%
  dplyr::mutate(SDG_id = gsub('\\.', '_', SDG_id))









#### ################################################################################### #
####                                                                                     #
####                                  Test                                            ####
####                                                                                     #
#### ################################################################################### #

# # pat <- '(sdg|goal|target|indicator)[^0-9]{0,2}(?=17[\\.]{0,1})|No Poverty'       ## 1. works
# pat <- '(sdg|goal|target|indicator)[^0-9]{0,2}(?=7\\.a[\\.]{0,1})|No Poverty'      ## 2. best
# # pat <- '(sdg|goal|target|indicator)(?:s|\\s){0,2}(?=7\\.a[\\.]{0,1})|No Poverty' ## 3. same as 2
#
# data.frame(term = c(
#   'i love SDGs and sdg 7 and sdg 7 and goal 7',
#   'i love SDGs and sdg 7.1 and and goal 7.1, indicator7.12, target7.1.1, indicator 7.1.2',
#   'I like sdg17 and you',
#   'I like sdg-17 and you',
#   'I like sdgs 17 and you',
#   'I like goal 7 and you',
#   'I like goals 7 and you',
#   'I like goal 7.a and you',
#   'I like goals 7.a and you',
#   'I like goal 7.a.1 and you',
#   'I like goal 17.a.1 and you',
#   'I like goals 7.a.1 and you',
#   'I like sdg1 and 17 and you',
#   'I like 17 sdgs and you?',
#   'when will be no Poverty?'
# )) %>%
#   dplyr::mutate(
#     match = ifelse(
#       grepl(pattern = pat, x = term, ignore.case = T, perl = T), 1, 0),
#     n = str_count(string = term, regex(pattern = pat, ignore_case = T))) %>%
#   # arrange(desc(match)) %>%
#   as.data.frame()



### ref:  https://stackoverflow.com/questions/41802272/understanding-lookahead-in-r-regexp
###       https://tpristavec.github.io/regex/#26
###       https://users.cs.cf.ac.uk/Dave.Marshall/PERL/node79.html
