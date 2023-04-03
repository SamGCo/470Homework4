#########################################################################
## Read and clean data basic premium and deductible information
#########################################################################  

## Raw 2007 data
ma.path.2007=paste0("data/input/ma-plan-characteristics/2007 MA Landscape Source File 11-16-06.csv")
ma.data.2007=read_csv(ma.path.2007,
                      skip=4,
                      col_names=c("state","county","org_name","plan_name","plan_type","premium","drug_premium","partd_deductible",
                                  "drug_type","gap_coverage","variable_drug_copay","drug_type_detail","demo_type","contractid",
                                  "planid","segmentid"),
                      col_types = cols(
                        state = col_character(),
                        county = col_character(),
                        org_name = col_character(),
                        plan_name = col_character(),
                        plan_type = col_character(),
                        premium = col_number(),
                        drug_premium = col_number(),
                        partd_deductible = col_number(),
                        drug_type = col_character(),
                        gap_coverage = col_character(),
                        variable_drug_copay = col_character(),
                        drug_type_detail = col_character(),
                        demo_type = col_character(),
                        contractid = col_character(),
                        planid = col_double(),
                        segmentid = col_double()
                      ))

macd.path.2007=paste0("data/input/ma-plan-characteristics/Medicare Part D 2007 Plan Report 12-06-06.xls")
macd.data.2007=read_xls(macd.path.2007,
                        range="A5:AC49252",
                        col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                    "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                    "national_pdp","partd_rein_demo","partd_rein_demo_type","premium_partc",
                                    "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                    "partd_assist_full","nothing","partd_assist_75","partd_assist_50","partd_assist_25",
                                    "partd_deductible","increase_coverage_limit","gap_coverage","gap_coverage_type"))

## Raw 2008 data
ma.path.2008a=paste0("data/input/ma-plan-characteristics/2008LandscapeSourceData_MA_09_25_07(A-M).csv")
ma.data.2008a=read_csv(ma.path.2008a,
                       skip=5,
                       col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                   "drug_type","gap_coverage","drug_type_detail","demo_type","contractid",
                                   "planid","segmentid"),
                       col_types = cols(
                         state = col_character(),
                         county = col_character(),
                         org_name = col_character(),
                         plan_name = col_character(),
                         plan_type = col_character(),
                         premium = col_number(),
                         partd_deductible = col_number(),
                         drug_type = col_character(),
                         gap_coverage = col_character(),
                         drug_type_detail = col_character(),
                         demo_type = col_character(),
                         contractid = col_character(),
                         planid = col_double(),
                         segmentid = col_double()
                       ))


ma.path.2008b=paste0("data/input/ma-plan-characteristics/2008LandscapeSourceData_MA_09_25_07(N-W).csv")
ma.data.2008b=read_csv(ma.path.2008b,
                       skip=5,
                       col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                   "drug_type","gap_coverage","drug_type_detail","demo_type","contractid",
                                   "planid","segmentid"),
                       col_types = cols(
                         state = col_character(),
                         county = col_character(),
                         org_name = col_character(),
                         plan_name = col_character(),
                         plan_type = col_character(),
                         premium = col_number(),
                         partd_deductible = col_number(),
                         drug_type = col_character(),
                         gap_coverage = col_character(),
                         drug_type_detail = col_character(),
                         demo_type = col_character(),
                         contractid = col_character(),
                         planid = col_double(),
                         segmentid = col_double()
                       ))


ma.data.2008 = rbind(ma.data.2008a,ma.data.2008b)


macd.path.2008a=paste0("data/input/ma-plan-characteristics/Medicare Part D 2008 Plan Report 11-06-07.xls")
macd.data.2008a=read_xls(macd.path.2008a,
                         range="A5:AC39471",
                         sheet="Alabama to Montana",
                         col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                     "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                     "national_pdp","partd_rein_demo","partd_rein_demo_type","premium_partc",
                                     "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                     "pard_assist_full","nothing","partd_assist_75","partd_assist_50","partd_assist_25",
                                     "partd_deductible","increase_coverage_limit","gap_coverage","gap_coverage_type"))

macd.path.2008b=paste0("data/input/ma-plan-characteristics/Medicare Part D 2008 Plan Report 11-06-07.xls")
macd.data.2008b=read_xls(macd.path.2008b,
                         range="A5:AC44708",
                         sheet="Nebraska to Wyoming",
                         col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                     "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                     "national_pdp","partd_rein_demo","partd_rein_demo_type","premium_partc",
                                     "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                     "pard_assist_full","nothing","partd_assist_75","partd_assist_50","partd_assist_25",
                                     "partd_deductible","increase_coverage_limit","gap_coverage","gap_coverage_type"))

macd.data.2008 = rbind(macd.data.2008a,macd.data.2008b)



## Raw 2009 data
ma.path.2009a=paste0("data/input/ma-plan-characteristics/2009LandscapeSourceData_MA_11_05_08_A_to_M.csv")
ma.data.2009a=read_csv(ma.path.2009a,
                       skip=5,
                       col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                   "drug_type","gap_coverage","drug_type_detail","demo_type","contractid",
                                   "planid","segmentid"),
                       col_types = cols(
                         state = col_character(),
                         county = col_character(),
                         org_name = col_character(),
                         plan_name = col_character(),
                         plan_type = col_character(),
                         premium = col_number(),
                         partd_deductible = col_number(),
                         drug_type = col_character(),
                         gap_coverage = col_character(),
                         drug_type_detail = col_character(),
                         demo_type = col_character(),
                         contractid = col_character(),
                         planid = col_double(),
                         segmentid = col_double()
                       ))


ma.path.2009b=paste0("data/input/ma-plan-characteristics/2009LandscapeSourceData_MA_11_05_08_N_to_W.csv")
ma.data.2009b=read_csv(ma.path.2009b,
                       skip=5,
                       col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                   "drug_type","gap_coverage","drug_type_detail","demo_type","contractid",
                                   "planid","segmentid"),
                       col_types = cols(
                         state = col_character(),
                         county = col_character(),
                         org_name = col_character(),
                         plan_name = col_character(),
                         plan_type = col_character(),
                         premium = col_number(),
                         partd_deductible = col_number(),
                         drug_type = col_character(),
                         gap_coverage = col_character(),
                         drug_type_detail = col_character(),
                         demo_type = col_character(),
                         contractid = col_character(),
                         planid = col_double(),
                         segmentid = col_double()
                       ))

ma.data.2009 = rbind(ma.data.2009a,ma.data.2009b)


macd.path.2009a=paste0("data/input/ma-plan-characteristics/Medicare Part D 2009 Plan Report 11-03-08.xls")
macd.data.2009a=read_xls(macd.path.2009a,
                         range="A5:AB33304",
                         sheet="Alabama to Montana",
                         col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                     "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                     "national_pdp","partd_rein_demo","partd_rein_demo_type","premium_partc",
                                     "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                     "pard_assist_full","partd_assist_75","partd_assist_50","partd_assist_25",
                                     "partd_deductible","increase_coverage_limit","gap_coverage","gap_coverage_type"))

macd.path.2009b=paste0("data/input/ma-plan-characteristics/Medicare Part D 2009 Plan Report 11-03-08.xls")
macd.data.2009b=read_xls(macd.path.2009b,
                         range="A5:AB40219",
                         sheet="Nebraska to Wyoming",
                         col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                     "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                     "national_pdp","partd_rein_demo","partd_rein_demo_type","premium_partc",
                                     "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                     "pard_assist_full","partd_assist_75","partd_assist_50","partd_assist_25",
                                     "partd_deductible","increase_coverage_limit","gap_coverage","gap_coverage_type"))
macd.data.2009 = rbind(macd.data.2009a,macd.data.2009b)


## Raw 2010 data
ma.path.2010a=paste0("data/input/ma-plan-characteristics/2010LandscapeSourceData_MA_12_01_09_A_to_M.csv")
ma.data.2010a=read_csv(ma.path.2010a,
                       skip=5,
                       col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                   "drug_type","gap_coverage","drug_type_detail","demo_type","contractid",
                                   "planid","segmentid","moop"),
                       col_types = cols(
                         state = col_character(),
                         county = col_character(),
                         org_name = col_character(),
                         plan_name = col_character(),
                         plan_type = col_character(),
                         premium = col_number(),
                         partd_deductible = col_number(),
                         drug_type = col_character(),
                         gap_coverage = col_character(),
                         drug_type_detail = col_character(),
                         demo_type = col_character(),
                         contractid = col_character(),
                         planid = col_double(),
                         segmentid = col_double(),
                         moop = col_character()
                       ))



ma.path.2010b=paste0("data/input/ma-plan-characteristics/2010LandscapeSourceData_MA_12_01_09_N_to_W.csv")
ma.data.2010b=read_csv(ma.path.2010b,
                       skip=5,
                       col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                   "drug_type","gap_coverage","drug_type_detail","demo_type","contractid",
                                   "planid","segmentid","moop"),
                       col_types = cols(
                         state = col_character(),
                         county = col_character(),
                         org_name = col_character(),
                         plan_name = col_character(),
                         plan_type = col_character(),
                         premium = col_number(),
                         partd_deductible = col_number(),
                         drug_type = col_character(),
                         gap_coverage = col_character(),
                         drug_type_detail = col_character(),
                         demo_type = col_character(),
                         contractid = col_character(),
                         planid = col_double(),
                         segmentid = col_double(),
                         moop = col_character()
                       ))
ma.data.2010 = rbind(ma.data.2010a,ma.data.2010b)


macd.path.2010a=paste0("data/input/ma-plan-characteristics/Medicare Part D 2010 Plan Report 09-14-09.xls")
macd.data.2010a=read_xls(macd.path.2010a,
                         range="A5:AC26372",
                         sheet="Alabama to Montana",
                         col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                     "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                     "national_pdp","partd_rein_demo","partd_rein_demo_type","premium_partc",
                                     "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                     "pard_assist_full","partd_assist_75","partd_assist_50","partd_assist_25",
                                     "partd_deductible","deductible_exclusions","increase_coverage_limit",
                                     "gap_coverage","gap_coverage_type"))

macd.path.2010b=paste0("data/input/ma-plan-characteristics/Medicare Part D 2010 Plan Report 09-14-09.xls")
macd.data.2010b=read_xls(macd.path.2010b,
                         range="A5:AC31073",
                         sheet="Nebraska to Wyoming",
                         col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                     "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                     "national_pdp","partd_rein_demo","partd_rein_demo_type","premium_partc",
                                     "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                     "pard_assist_full","partd_assist_75","partd_assist_50","partd_assist_25",
                                     "partd_deductible","deductible_exclusions","increase_coverage_limit",
                                     "gap_coverage","gap_coverage_type"))
macd.data.2010 = rbind(macd.data.2010a,macd.data.2010b)



## Raw 2011 data
ma.path.2011a=paste0("data/input/ma-plan-characteristics/2011LandscapeSourceData_MA_12_17_10_AtoM.csv")
ma.data.2011a=read_csv(ma.path.2011a,
                       skip=6,
                       col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                   "drug_type","gap_coverage","drug_type_detail","contractid",
                                   "planid","segmentid","moop","free_preventive_care"),
                       col_types = cols(
                         state = col_character(),
                         county = col_character(),
                         org_name = col_character(),
                         plan_name = col_character(),
                         plan_type = col_character(),
                         premium = col_number(),
                         partd_deductible = col_number(),
                         drug_type = col_character(),
                         gap_coverage = col_character(),
                         drug_type_detail = col_character(),
                         contractid = col_character(),
                         planid = col_double(),
                         segmentid = col_double(),
                         moop = col_character(),
                         free_preventive_care = col_character()
                       ))


ma.path.2011b=paste0("data/input/ma-plan-characteristics/2011LandscapeSourceData_MA_12_17_10_NtoW.csv")
ma.data.2011b=read_csv(ma.path.2011b,
                       skip=6,
                       col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                   "drug_type","gap_coverage","drug_type_detail","contractid",
                                   "planid","segmentid","moop","free_preventive_care"),
                       col_types = cols(
                         state = col_character(),
                         county = col_character(),
                         org_name = col_character(),
                         plan_name = col_character(),
                         plan_type = col_character(),
                         premium = col_number(),
                         partd_deductible = col_number(),
                         drug_type = col_character(),
                         gap_coverage = col_character(),
                         drug_type_detail = col_character(),
                         contractid = col_character(),
                         planid = col_double(),
                         segmentid = col_double(),
                         moop = col_character(),
                         free_preventive_care = col_character()
                       ))
ma.data.2011 = rbind(ma.data.2011a,ma.data.2011b)


macd.path.2011a=paste0("data/input/ma-plan-characteristics/Medicare Part D 2011 Plan Report 09-15-10.xls")
macd.data.2011a=read_xls(macd.path.2011a,
                         range="A5:AA18105",
                         sheet="Alabama to Montana",
                         col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                     "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                     "national_pdp","premium_partc",
                                     "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                     "pard_assist_full","partd_assist_75","partd_assist_50","partd_assist_25",
                                     "partd_deductible","deductible_exclusions","increase_coverage_limit",
                                     "gap_coverage","gap_coverage_type"))

macd.path.2011b=paste0("data/input/ma-plan-characteristics/Medicare Part D 2011 Plan Report 09-15-10.xls")
macd.data.2011b=read_xls(macd.path.2011b,
                         range="A5:AA20402",
                         sheet="Nebraska to Wyoming",
                         col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                     "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                     "national_pdp","premium_partc",
                                     "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                     "pard_assist_full","partd_assist_75","partd_assist_50","partd_assist_25",
                                     "partd_deductible","deductible_exclusions","increase_coverage_limit",
                                     "gap_coverage","gap_coverage_type"))
macd.data.2011 = rbind(macd.data.2011a,macd.data.2011b)





## Raw 2012 data
ma.path.2012a=paste0("data/input/ma-plan-characteristics/2012LandscapeSourceData_MA_3_08_12_AtoM.csv")
ma.data.2012a=read_csv(ma.path.2012a,
                       skip=6,
                       col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                   "drug_type","gap_coverage","drug_type_detail","contractid",
                                   "planid","segmentid","moop","star_rating"),
                       col_types = cols(
                         state = col_character(),
                         county = col_character(),
                         org_name = col_character(),
                         plan_name = col_character(),
                         plan_type = col_character(),
                         premium = col_number(),
                         partd_deductible = col_number(),
                         drug_type = col_character(),
                         gap_coverage = col_character(),
                         drug_type_detail = col_character(),
                         contractid = col_character(),
                         planid = col_double(),
                         segmentid = col_double(),
                         moop = col_character(),
                         star_rating = col_character()
                       ))



ma.path.2012b=paste0("data/input/ma-plan-characteristics/2012LandscapeSourceData_MA_3_08_12_NtoW.csv")
ma.data.2012b=read_csv(ma.path.2012b,
                       skip=6,
                       col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                   "drug_type","gap_coverage","drug_type_detail","contractid",
                                   "planid","segmentid","moop","star_rating"),
                       col_types = cols(
                         state = col_character(),
                         county = col_character(),
                         org_name = col_character(),
                         plan_name = col_character(),
                         plan_type = col_character(),
                         premium = col_number(),
                         partd_deductible = col_number(),
                         drug_type = col_character(),
                         gap_coverage = col_character(),
                         drug_type_detail = col_character(),
                         contractid = col_character(),
                         planid = col_double(),
                         segmentid = col_double(),
                         moop = col_character(),
                         star_rating = col_character()
                       ))
ma.data.2012 = rbind(ma.data.2012a,ma.data.2012b)


macd.path.2012a=paste0("data/input/ma-plan-characteristics/Medicare Part D 2012 Plan Report 09-08-11.xls")
macd.data.2012a=read_xls(macd.path.2012a,
                         range="A5:AA18521",
                         sheet="Alabama to Montana",
                         col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                     "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                     "national_pdp","premium_partc",
                                     "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                     "pard_assist_full","partd_assist_75","partd_assist_50","partd_assist_25",
                                     "partd_deductible","deductible_exclusions","increase_coverage_limit",
                                     "gap_coverage","gap_coverage_type"))

macd.path.2012b=paste0("data/input/ma-plan-characteristics/Medicare Part D 2012 Plan Report 09-08-11.xls")
macd.data.2012b=read_xls(macd.path.2012b,
                         range="A5:AA21182",
                         sheet="Nebraska to Wyoming",
                         col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                     "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                     "national_pdp","premium_partc",
                                     "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                     "pard_assist_full","partd_assist_75","partd_assist_50","partd_assist_25",
                                     "partd_deductible","deductible_exclusions","increase_coverage_limit",
                                     "gap_coverage","gap_coverage_type"))
macd.data.2012 = rbind(macd.data.2012a,macd.data.2012b)




## Raw 2013 data
ma.path.2013a=paste0("data/input/ma-plan-characteristics/2013LandscapeSource file MA_AtoM 11212012.csv")
ma.data.2013a=read_csv(ma.path.2013a,
                       skip=6,
                       col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                   "drug_type","gap_coverage","drug_type_detail","contractid",
                                   "planid","segmentid","moop","star_rating"),
                       col_types = cols(
                         state = col_character(),
                         county = col_character(),
                         org_name = col_character(),
                         plan_name = col_character(),
                         plan_type = col_character(),
                         premium = col_number(),
                         partd_deductible = col_number(),
                         drug_type = col_character(),
                         gap_coverage = col_character(),
                         drug_type_detail = col_character(),
                         contractid = col_character(),
                         planid = col_double(),
                         segmentid = col_double(),
                         moop = col_character(),
                         star_rating = col_character()
                       ))


ma.path.2013b=paste0("data/input/ma-plan-characteristics/2013LandscapeSource file MA_NtoW 11212012.csv")
ma.data.2013b=read_csv(ma.path.2013b,
                       skip=6,
                       col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                   "drug_type","gap_coverage","drug_type_detail","contractid",
                                   "planid","segmentid","moop","star_rating"),
                       col_types = cols(
                         state = col_character(),
                         county = col_character(),
                         org_name = col_character(),
                         plan_name = col_character(),
                         plan_type = col_character(),
                         premium = col_number(),
                         partd_deductible = col_number(),
                         drug_type = col_character(),
                         gap_coverage = col_character(),
                         drug_type_detail = col_character(),
                         contractid = col_character(),
                         planid = col_double(),
                         segmentid = col_double(),
                         moop = col_character(),
                         star_rating = col_character()
                       ))

ma.data.2013 = rbind(ma.data.2013a,ma.data.2013b)


macd.path.2013a=paste0("data/input/ma-plan-characteristics/Medicare Part D 2013 Plan Report 04252013v1.xls")
macd.data.2013a=read_xls(macd.path.2013a,
                         range="A5:AA20940",
                         sheet="Alabama to Montana",
                         col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                     "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                     "national_pdp","premium_partc",
                                     "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                     "pard_assist_full","partd_assist_75","partd_assist_50","partd_assist_25",
                                     "partd_deductible","deductible_exclusions","increase_coverage_limit",
                                     "gap_coverage","gap_coverage_type"))

macd.path.2013b=paste0("data/input/ma-plan-characteristics/Medicare Part D 2013 Plan Report 04252013v1.xls")
macd.data.2013b=read_xls(macd.path.2013b,
                         range="A5:AA23812",
                         sheet="Nebraska to Wyoming",
                         col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                     "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                     "national_pdp","premium_partc",
                                     "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                     "pard_assist_full","partd_assist_75","partd_assist_50","partd_assist_25",
                                     "partd_deductible","deductible_exclusions","increase_coverage_limit",
                                     "gap_coverage","gap_coverage_type"))
macd.data.2013 = rbind(macd.data.2013a,macd.data.2013b)



## Raw 2014 data
ma.path.2014a=paste0("data/input/ma-plan-characteristics/2014LandscapeSource file MA_AtoM 05292014.csv")
ma.data.2014a=read_csv(ma.path.2014a,
                       skip=6,
                       col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                   "drug_type","gap_coverage","drug_type_detail","contractid",
                                   "planid","segmentid","moop","star_rating"),
                       col_types = cols(
                         state = col_character(),
                         county = col_character(),
                         org_name = col_character(),
                         plan_name = col_character(),
                         plan_type = col_character(),
                         premium = col_number(),
                         partd_deductible = col_number(),
                         drug_type = col_character(),
                         gap_coverage = col_character(),
                         drug_type_detail = col_character(),
                         contractid = col_character(),
                         planid = col_double(),
                         segmentid = col_double(),
                         moop = col_character(),
                         star_rating = col_character()
                       ))


ma.path.2014b=paste0("data/input/ma-plan-characteristics/2014LandscapeSource file MA_NtoW 05292014.csv")
ma.data.2014b=read_csv(ma.path.2014b,
                       skip=6,
                       col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                   "drug_type","gap_coverage","drug_type_detail","contractid",
                                   "planid","segmentid","moop","star_rating"),
                       col_types = cols(
                         state = col_character(),
                         county = col_character(),
                         org_name = col_character(),
                         plan_name = col_character(),
                         plan_type = col_character(),
                         premium = col_number(),
                         partd_deductible = col_number(),
                         drug_type = col_character(),
                         gap_coverage = col_character(),
                         drug_type_detail = col_character(),
                         contractid = col_character(),
                         planid = col_double(),
                         segmentid = col_double(),
                         moop = col_character(),
                         star_rating = col_character()
                       ))

ma.data.2014 = rbind(ma.data.2014a,ma.data.2014b)


macd.path.2014a=paste0("data/input/ma-plan-characteristics/Medicare Part D 2014 Plan Report 05292014.xls")
macd.data.2014a=read_xls(macd.path.2014a,range="A5:AA15859",
                         sheet="Alabama to Montana",
                         col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                     "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                     "national_pdp","premium_partc",
                                     "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                     "pard_assist_full","partd_assist_75","partd_assist_50","partd_assist_25",
                                     "partd_deductible","deductible_exclusions","increase_coverage_limit",
                                     "gap_coverage","gap_coverage_type"))

macd.path.2014b=paste0("data/input/ma-plan-characteristics/Medicare Part D 2014 Plan Report 05292014.xls")
macd.data.2014b=read_xls(macd.path.2014b,
                         range="A5:AA20305",
                         sheet="Nebraska to Wyoming",
                         col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                     "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                     "national_pdp","premium_partc",
                                     "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                     "pard_assist_full","partd_assist_75","partd_assist_50","partd_assist_25",
                                     "partd_deductible","deductible_exclusions","increase_coverage_limit",
                                     "gap_coverage","gap_coverage_type"))
macd.data.2014 = rbind(macd.data.2014a,macd.data.2014b)



## Raw 2015 data
ma.path.2015a=paste0("data/input/ma-plan-characteristics/2015LandscapeSource file MA_AtoM 11042014.csv")
ma.data.2015a=read_csv(ma.path.2015a,
                       skip=6,
                       col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                   "drug_type","gap_coverage","drug_type_detail","contractid",
                                   "planid","segmentid","moop","star_rating"),
                       col_types = cols(
                         state = col_character(),
                         county = col_character(),
                         org_name = col_character(),
                         plan_name = col_character(),
                         plan_type = col_character(),
                         premium = col_number(),
                         partd_deductible = col_number(),
                         drug_type = col_character(),
                         gap_coverage = col_character(),
                         drug_type_detail = col_character(),
                         contractid = col_character(),
                         planid = col_double(),
                         segmentid = col_double(),
                         moop = col_character(),
                         star_rating = col_character()
                       ))

ma.path.2015b=paste0("data/input/ma-plan-characteristics/2015LandscapeSource file MA_NtoW 11042014.csv")
ma.data.2015b=read_csv(ma.path.2015b,
                       skip=6,
                       col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                   "drug_type","gap_coverage","drug_type_detail","contractid",
                                   "planid","segmentid","moop","star_rating"),
                       col_types = cols(
                         state = col_character(),
                         county = col_character(),
                         org_name = col_character(),
                         plan_name = col_character(),
                         plan_type = col_character(),
                         premium = col_number(),
                         partd_deductible = col_number(),
                         drug_type = col_character(),
                         gap_coverage = col_character(),
                         drug_type_detail = col_character(),
                         contractid = col_character(),
                         planid = col_double(),
                         segmentid = col_double(),
                         moop = col_character(),
                         star_rating = col_character()
                       ))
ma.data.2015 = rbind(ma.data.2015a,ma.data.2015b)


macd.path.2015a=paste0("data/input/ma-plan-characteristics/Medicare Part D 2015 Plan Report 03182015.xls")
macd.data.2015a=read_xls(macd.path.2015a,
                         range="A5:Z16666",
                         sheet="Alabama to Montana",
                         col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                     "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                     "national_pdp","premium_partc",
                                     "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                     "pard_assist_full","partd_assist_75","partd_assist_50","partd_assist_25",
                                     "partd_deductible","deductible_exclusions","increase_coverage_limit",
                                     "gap_coverage"))

macd.path.2015b=paste0("data/input/ma-plan-characteristics/Medicare Part D 2015 Plan Report 03182015.xls")
macd.data.2015b=read_xls(macd.path.2015b,
                         range="A5:Z17038",
                         sheet="Nebraska to Wyoming",
                         col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                     "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                     "national_pdp","premium_partc",
                                     "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                     "pard_assist_full","partd_assist_75","partd_assist_50","partd_assist_25",
                                     "partd_deductible","deductible_exclusions","increase_coverage_limit",
                                     "gap_coverage"))
macd.data.2015 = rbind(macd.data.2015a,macd.data.2015b)


for (y in 2007:2015) {

  ############ CLEAN MA-Only Data #####################
  ma.data=get(paste0("ma.data.",y))
  ma.data = ma.data %>%
    select(contractid, planid, state, county, premium)

  ## Fill in missing plan info (by contract, plan, state, and county)
  ma.data = ma.data %>%
    group_by(contractid, planid, state, county) %>%
    fill(premium)

  ## Remove duplicates
  ma.data = ma.data %>%
    group_by(contractid, planid, state, county) %>%
    mutate(id_count=row_number())
  
  ma.data = ma.data %>%
    filter(id_count==1) %>%
    select(-id_count)

    
  ############ CLEAN MA-PD Data #####################
  macd.data=get(paste0("macd.data.",y))
  macd.data = macd.data %>% 
    select(contractid, planid, state, county, premium_partc, premium_partd_basic, 
           premium_partd_supp, premium_partd_total, partd_deductible) %>%
    mutate(planid=as.numeric(planid))
  
  macd.data = macd.data %>%
    group_by(contractid, planid, state, county) %>%
    fill(premium_partc, premium_partd_basic, premium_partd_supp, premium_partd_total, partd_deductible)
  
  ## Remove duplicates
  macd.data = macd.data %>%
    group_by(contractid, planid, state, county) %>%
    mutate(id_count=row_number())
  
  macd.data = macd.data %>%
    filter(id_count==1) %>%
    select(-id_count)

  ## Merge Part D info to Part C info
  ma.macd.data = ma.data %>%
    full_join(macd.data, by=c("contractid", "planid", "state", "county")) %>%
    mutate(year=y)
  
  if (y==2007) {
    plan.premiums=ma.macd.data
  } else {
    plan.premiums=rbind(plan.premiums,ma.macd.data)
  }
  
  
}

write_rds(plan.premiums,"data/output/plan_premiums.rds")
