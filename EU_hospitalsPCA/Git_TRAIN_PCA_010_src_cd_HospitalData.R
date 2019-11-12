

##### ---- 1. All relevant Hospital Data ------ #####

### For bulk reading
data_list = c("hosp_dis_t17", "hosp_dis_m17", "hosp_dis_f17",
              "hosp_dis_day_t17", "hosp_dis_day_m17", "hosp_dis_day_f17",
              "hosp_length_tot17", "hosp_length_male17", "hosp_length_female17")


extension_list = paste0("1_RawData/", data_list, ".csv")

### Bulk reading
df_hosp_list = map(extension_list, read_csv)


# ### Bulk extracting elements as single df!
# hosp_dis_t17  =  df_hosp_list[[1]]     
# hosp_dis_m17    =  df_hosp_list[[2]]      
# hosp_dis_f17        =  df_hosp_list[[3]]  
# hosp_dis_day_t17    =  df_hosp_list[[4]]  
# hosp_dis_day_m17    =  df_hosp_list[[5]]  
# hosp_dis_day_f17    =  df_hosp_list[[6]]  
# hosp_length_tot17   =  df_hosp_list[[7]]  
# hosp_length_male17  =  df_hosp_list[[8]]  
# hosp_length_female17=  df_hosp_list[[9]]  


### Enframing to list-columns
### Nested full data for Batch operations
hosp_list_nest = df_hosp_list %>% 
  set_names(data_list) %>% 
  enframe() %>% 
  rename(Dataframe = name,
         AgeGroupPop = value) 

# ### Test 1 data
# test = hosp_list_nest %>% 
#   pluck("AgeGroupPop", 1)

ages = hosp_list_nest %>% 
  pluck("AgeGroupPop", 1) %>% 
  mutate_if(is.character, as.factor) %>% 
  select_if(is.factor) %>% 
  map(levels) %>% 
  pluck(1)


### ---- 2. First by country then by age_group! ------

hosp_list_country = hosp_list_nest %>% 
  mutate(Country = map(AgeGroupPop,
                       ~.x %>% 
                         mutate(len = nchar(geo)) %>% 
                         filter(len == 2) %>% 
                         select(-len)))


fun_hosp_country_age = function(arg){
hosp_list_country %>% 
  mutate(AgeGroup = map(Country,
                   ~.x %>% filter(age == arg)))
}
  
hosp_country_age = map(ages, fun_hosp_country_age) %>% 
  set_names(ages) %>% 
  enframe()

# # Testing
# map(ages, fun_hosp_country_age) %>% 
#   set_names(ages) %>% 
#   enframe() %>% 
#   pluck("value", 2) %>% 
#   pluck("AgeGroup", 1) %>% 
#   pull(icd10) %>% 
#   unique


### PBM
# Testing - PBM with class 0!!!
# Widening each AgeGroup Dataframe!

# Testing - PBM with class 0!!!
# hosp_country_age %>% 
#   pluck("value", 2) %>% # 7 age groups
#   # pluck("AgeGroup", 1) %>% # 9 variables
#   mutate(Wide = map(AgeGroup,
#                     ~.x %>% 
#                       select(geo, icd10, values) %>% 
#                       spread(icd10, values))) %>% 
#   pluck("Wide",1)
#   

# fun_hosp_country_age_wide = function(agegroup){
# hosp_country_age %>% 
#   pluck("value", agegroup) %>% # 7 age groups
#   # pluck("AgeGroup", 1) %>% # 9 variables
#   mutate(Wide = map(AgeGroup,
#                     ~.x %>% 
#                       select(geo, icd10, values) %>% 
#                       spread(icd10, values))) 
# }
# 
# 
# map(ages, fun_hosp_country_age_wide)

# SOLUTION: Alternative
# Widening each AgeGroup Dataframe!
# Testing - PBM with class 0!!!
fun_hosp_country_age_wide = function(agegroup, variable){
hosp_country_age %>% 
  pluck("value", agegroup) %>% # 7 num of age groups
  filter(Dataframe == variable)  %>% # 9 variables "hosp_dis_t17"
  pluck("AgeGroup", 1) %>% 
  select(geo, icd10, values) %>% 
  spread(icd10, values)
} 

Variables = modify(as.list(data_list), ~rep(.x, 7)) %>% 
  unlist()

combi_age_variable = tibble(Ages = rep(1:7, 9),
                            Variables = Variables,
                            Groups = rep(ages, 9))

# ages 

combi_age_variable = combi_age_variable %>% 
  unite(AgesVariables, Variables, Groups, 
        sep = "*", 
        remove = FALSE)

combi_ages = combi_age_variable %>% 
  pull(Ages)

combi_variables = combi_age_variable %>% 
  pull(Variables)

ages_variables =  combi_age_variable %>% 
  pull(AgesVariables)

# map2(combi_ages[1:2], combi_variables[1:2], fun_hosp_country_age_wide) %>% 
#   set_names(ages_variables[1:2])


PCA_variables_ages_country_list = map2(combi_ages, 
                                  combi_variables, 
                                  fun_hosp_country_age_wide) %>% 
  set_names(ages_variables)


PCA_variables_ages_country = PCA_variables_ages_country_list %>% 
  enframe() %>% 
  separate(name, into = c("Variable", "Age"), 
           sep = "[*]",
           remove = FALSE)




  # 9 variables
#  pluck("value", 2) %>% # 7 age groups
  # pluck("AgeGroup", 1) %>% # 9 variables
  # map(~.x %>% 
  #       select(Dataframe, AgeGroup)) %>% 
  # map()
  #       select(geo, icd10, values) %>% 
  #                     spread(icd10, values) %>% 
  # enframe()
  # pluck("Wide",1)


### ---- 2.1 Country Imputations ------

# Testing
# all(c(T, T, T), T)

# # Testing 
# PCA_variables_ages_country %>% 
#   pluck("value",1) %>% 
#   complete.cases() %>% 
  # all(TRUE)

# Checking for missings! MALE and OTHER situation causing PBM ...
# REMOVING MISSINGS 
# NO IMPUTATION!!!

missings_c = PCA_variables_ages_country %>% 
  mutate(missings = map(value,
                        ~.x %>% 
                          complete.cases() %>% 
                          all(TRUE))) %>% 
  select(missings) %>% 
  deframe() %>% 
  unlist()


# Some missings: IMPUTATION!!!

PCA_variables_ages_country_FULL = PCA_variables_ages_country %>% 
  filter(missings_c)

age_country = PCA_variables_ages_country_FULL %>% pull(Age) %>% unique
variable_country = PCA_variables_ages_country_FULL %>% pull(Variable) %>% unique

# PCA_variables_ages_country_FULL %>% 
#   pluck("value", 1)

ages_variables_FULL_c = PCA_variables_ages_country_FULL %>% 
  pull(name)



# # Imputation function
# impute_function_country = function(age_variable){
#   
#   # ALternative: SINGLE Procedure
#   Imp = PCA_variables_ages_country %>% 
#     # filter(!(str_detect(name, "_male"))) %>% 
#     # filter(!(str_detect(name, "_m"))) %>% 
#     filter(name  == age_variable) %>%  #"hosp_dis_t17*TOTAL") %>%  #"Y65-74") %>% #data) %>%  #"Y16-24") %>% #  "Y16-24") %>% # data) %>% # data # "Y16-24"
#     select(value) %>% 
#     unnest() %>% 
#     select(-1) %>% 
#     # select(-O) %>% # not taken out???
#     as.data.frame() 
#   
#   # Coutries
#   rownames(Imp) = PCA_variables_ages_country %>% 
#     filter(!(str_detect(name, "_male"))) %>% 
#     filter(!(str_detect(name, "_m"))) %>%  
#     filter(name  == age_variable) %>%  #"hosp_dis_t17*TOTAL") %>%  #"Y65-74") %>% #data) %>%  #"Y16-24") %>% #  "Y16-24") %>% # data) %>% # data # "Y16-24"
#     select(value) %>% 
#     unnest() %>% 
#     select(1) %>%
#     pull() %>% 
#     as.character()
#   
#   
#   
#   comp <- imputePCA(Imp, ncp=2, scale=TRUE) ## Compl?te le tableau
#   
#   # # Adding Countries to imputed table
#   data_imp = comp$completeObs
#   #
#   data_imp
#   # %>%
#   #   tibble()
#   # 
#   # comp 
#   # # %>% 
#   # #   tibble()
#   
# }
# 
# 
# ages_variables %>% 
#   str_detect("_male" | "_m")
# 
# ages_variables_no_male = PCA_variables_ages_country %>% 
#   filter(!(str_detect(name, "_male"))) %>% 
#   filter(!(str_detect(name, "_m"))) %>% 
#   pull(name)
# 
# ### Lopping from 2 to 5: UNKNOWN/KNOWN problem with 1st element!
# imputed_data_country = map(ages_variables_no_male, impute_function_country) %>% 
#   set_names(ages_variables_no_male)





### ---- 2.2 Country PCA ------


# fun_rownames = function(age_variable_full){
# PCA_variables_ages_country_FULL %>% 
#   filter(name == "hosp_dis_t17*TOTAL") %>% 
#   pluck("value", 1) %>% 
#   pull(geo)
# }
# 
# row_names = map(PCA_variables_ages_country_FULL %>% 
#       select(value) %>% 
#       deframe(),
#     fun_rownames)


# map2(PCA_variables_ages_country_FULL %>% 
#            select(value) %>% 
#            deframe() %>% 
#        pluck(1),
#          row_names[1] %>% 
#        unlist(),
#          ~rownames(.x) = .y)



fun_rownames_c = function(age_variable_full){
  
  
  rownames = PCA_variables_ages_country_FULL %>% 
    filter(name == age_variable_full) %>%  #"hosp_dis_t17*TOTAL") %>% #  "hosp_dis_t17*TOTAL") %>% 
    pluck("value", 1) %>% 
    pull(geo)
  
  data = PCA_variables_ages_country_FULL %>% 
    filter(name == age_variable_full) %>%  #"hosp_dis_t17*TOTAL") %>% # age_variable_full) %>% # "hosp_dis_t17*TOTAL") %>% 
    pluck("value", 1) %>% 
    as.data.frame()
  
  rownames(data) <- rownames
  
  data
  
}

named_PCA_country = map(ages_variables_FULL_c, fun_rownames_c) %>% 
  set_names(ages_variables_FULL_c) %>% 
  enframe()
  
# # Testing
# named_PCA_country %>% 
#   pluck("value", 1)



res_PCA_country = named_PCA_country %>% 
  # mutate(named_PCA = map(value,
  #                      ~ rownames(.x) = .x %>% 
  #                             select(1) %>% 
  #                        pull))
# ,
#                             graph = FALSE)))
  mutate(res_PCA = map(value,
                       ~PCA(.x %>% 
                              select(-1),
                            graph = FALSE)))

res_PCA_country = res_PCA_country %>% 
  separate(name, into = c("Variable", "Age"), 
           sep = "[*]", 
           remove = FALSE)

### ---- 2.2.0 Flex: Select Input DYNAMICALLY ------

fun_variables_sel = function(age_group){
res_PCA_country %>% 
    filter(Age == age_group) %>% # "Y60-64") %>%  #
    pull(Variable)
}

variables_sel_input = map(ages, fun_variables_sel)



### ---- 2.2.1 Dims of Country PCA ------


### PLus interessant!!!
### PCA describe dim12 function
fun_pca_dim12_c = function(agegroup){
  
  dimdesc(res_PCA_country %>% 
            rownames_to_column() %>% 
            filter(rowname == agegroup) %>% # 10) %>%  # 
            pluck("res_PCA", 1), axes = 1:2)
  
}

# As enframed list!
res_PCA_dim12_c = map(c(1:length(ages_variables_FULL_c)), 
                    fun_pca_dim12_c) %>% # ages_variables_FULL_c
  set_names(ages_variables_FULL_c) %>%
  enframe() %>% 
  unnest(value) %>% 
  unnest(value) %>% 
  #  deframe()
  rename(Dataframe = name,
         Dimensions12 = value)


res_PCA_dim12_c = res_PCA_dim12_c %>% 
  separate(Dataframe, into = c("Variable", "Age"),
           sep = "[*]",
           remove = FALSE)

res_PCA_dim12_c = bind_cols(Dims = rep(c("dim1", "dim2"),length(ages_variables_FULL_c)),
                            res_PCA_dim12_c)


res_PCA_dim12_c  %>% 
  filter(Age == "TOTAL") %>% ## age_sel()) %>% #"Y25-34") %>% #   #agegroup) %>% 
  filter(Variable == "hosp_dis_t17") %>%
  filter(Dims == "dim1") %>% 
  pluck("Dimensions12") %>% 
  pluck(1) %>% 
  rownames()

# # As tibble
# res_PCA_dim12_c = tibble(Dims = rep(c("dim1", "dim2"),length(ages_variables_FULL_c)),
#                        Ages = res_PCA_dim12_c %>% 
#                          select(Dataframe),
#                        Axes = res_PCA_dim12_c %>% 
#                          select(Dimensions12)) 

# # ### Description dimensions as dataframes!
# dim12_desc = res_PCA_dim12[[3]] %>%
#   deframe() %>%
#   map(~as.data.frame(.x)) %>% 
#   enframe()


### PCA describe dim34 function
fun_pca_dim34_c = function(agegroup){
  
  dimdesc(res_PCA_country %>% 
            rownames_to_column() %>% 
            filter(rowname == agegroup) %>% # 10) %>%  # 
            pluck("res_PCA", 1), axes = 3:4)
  
}

# As enframed list!
res_PCA_dim34_c = map(c(1:length(ages_variables_FULL_c)), 
                    fun_pca_dim34_c) %>% # ages_variables_FULL_c
  set_names(ages_variables_FULL_c) %>%
  enframe() %>% 
  unnest(value) %>% 
  unnest(value) %>% 
  #  deframe()
  rename(Dataframe = name,
         Dimensions34 = value)


res_PCA_dim34_c = res_PCA_dim34_c %>% 
  separate(Dataframe, into = c("Variable", "Age"),
           sep = "[*]",
           remove = FALSE)

res_PCA_dim34_c = bind_cols(Dims = rep(c("dim3", "dim4"),length(ages_variables_FULL_c)),
                            res_PCA_dim34_c)



# res_PCA_dim34_c %>% 
#   pluck("Dimensions34", 1)
# 
# res_PCA_dim34_c = tibble(Dims = rep(c("dim3", "dim4"), length(ages_variables_FULL_c)),
#                        Ages = res_PCA_dim34_c %>% 
#                          select(Dataframe),
#                        Axes = res_PCA_dim34_c %>% 
#                          select(Dimensions34))

# res_PCA_dim34_c %>% 
#   filter(Dims == "dim3") %>% 
#   pluck(3, 1)


### ---- 2.2.2 RELEVANT VARIABLES from PCA dimensions of Country ------


### Return of raw data for DT
PCA_variables_ages_country_FULL %>% 
  filter(Age == "TOTAL") %>%  # age_sel()) %>% # "Y25-34") %>% #"Y16-24" - input$age_groupsX
  filter(Variable == "hosp_dis_t17") %>% 
  pluck("value") %>% 
  pluck(1) %>% 
  gather(Disease_Category, Proportion, -geo) %>% 
  filter(Disease_Category == "E") %>% # filter(combi %in% input$combi1) %>% # "6-9*ED0-2*M") %>%  #
  # bind_rows() %>% 
  # filter(combi %in% input$combi1) %>% # "6-9*ED0-2*M") %>%  #
  
  # deframe() %>% 
  # pluck(45) %>% # index of combi from adjusted input!!!
  rename(Country = geo) %>% 
  # select(Country, Selection, Proportion) %>% 
  arrange(-Proportion)






### ---- 2.3 Country HCPC ------


# res_HCPC_country = PCA_variables_ages_country_FULL %>% 
#   mutate(res_HCPC = map(res_PCA,
#                        ~PCA(.x %>% 
#                               select(-1),
#                             graph = FALSE)))

# x11()

fun_HCPC_c = function(agegroup){
  res.hcpc <- HCPC(res_PCA_country %>% 
                     filter(name == agegroup) %>% #"hosp_dis_t17*TOTAL") %>%  #
                     pluck("res_PCA", 1), 
                   nb.clust=-1, 
                   kk = Inf, 
                   graph=F, 
                   consol=F)
}


### List of cluster results - Cluster number optimally chosen (Huygens criteria or so)
HCPC_list_c = map(ages_variables_FULL_c, fun_HCPC_c) %>% 
  set_names(ages_variables_FULL_c) %>% 
  enframe()

HCPC_list_c = HCPC_list_c %>% 
  separate(name, into = c("Variable", "Age"), 
           sep = "[*]", 
           remove = FALSE)

### Testing
# x11()
cluster_viz12_c = function(age_variable){
  # x11()
  gg_fviz_12 = fviz_cluster(HCPC_list_c %>% 
                              filter(name == age_variable) %>% 
                              pluck("value", 1),
                            main = paste0("Cluster Plot for age group: ", age_variable),
                            axes = 1:2, 
                            repel = T, 
                            ggtheme = theme_bw() +
                              theme(text = element_text(family = "serif", size = 14),
                                    title = element_text(color = "#990000"))) 
  
}

gg_viz_list_c = map(ages_variables_FULL_c, cluster_viz12_c) %>% 
  set_names(ages_variables_FULL_c) %>% 
  enframe()



gg_viz_list_c = gg_viz_list_c %>% 
  separate(name, into = c("Variable", "Age"), 
           sep = "[*]", 
           remove = FALSE)

# # Checking!
# map(1:4, ~gg_viz_list_c %>% pluck("value", .x))





### ---- 3. Region then by age_group! ------


hosp_list_region = hosp_list_nest %>% 
  mutate(Region = map(AgeGroupPop,
                       ~.x %>% 
                         mutate(len = nchar(geo)) %>% 
                         filter(len == 4) %>% 
                         select(-len)))


# hosp_list_region %>% 
#   pluck("Region", 1) %>% 
#   pull(age) %>% 
#   unique

fun_hosp_region_age = function(arg){
  hosp_list_region %>% 
    mutate(AgeGroup = map(Region,
                          ~.x %>% filter(age == arg)))
}

hosp_region_age = map(ages, fun_hosp_region_age) %>% 
  set_names(ages) %>% 
  enframe()

# hosp_region_age %>% 
#   pluck("value", 1) %>% 
#   pluck("AgeGroup", 1) %>% 
#   pull(age) %>% 
#   unique






### PBM with WIDENING - Probably "O" and DIFFERENT dimensions!!!
# Testing
# map(ages, fun_hosp_country_age) %>% 
#   set_names(ages) %>% 
#   enframe() %>% 
#   pluck("value", 2) %>% 
#   pluck("AgeGroup", 1) %>% 
#   pull(icd10) %>% 
#   unique

# PROBLEM
# Testing - PBM with class 0!!!
# Widening each AgeGroup Dataframe!

# hosp_region_age %>% 
#   pluck("value", 2) %>% # 7 age groups
#   # pluck("AgeGroup", 1) %>% # 9 variables
#   mutate(Wide = map(AgeGroup,
#                     ~.x %>% 
#                       select(geo, icd10, values) %>% 
#                       spread(icd10, values))) %>% 
#   pluck("Wide",1)


# fun_hosp_region_age_wide = function(agegroup){
#   hosp_region_age %>% 
#     pluck("value", agegroup) %>% # 7 age groups
#     # pluck("AgeGroup", 1) %>% # 9 variables
#     mutate(Wide = map(AgeGroup,
#                       ~.x %>% 
#                         select(geo, icd10, values) %>% 
#                         spread(icd10, values))) 
# }
# 
# map(ages, fun_hosp_region_age_wide)

# SOLUTION: Alternative
# Alternative
# Widening each AgeGroup Dataframe!
# Testing - PBM with class 0!!!
# fun_hosp_region_age_wide = function(agegroup, variable){
#   hosp_region_age %>% 
#     pluck("value", agegroup) %>% # 7 num of age groups
#     filter(Dataframe == variable)  %>% # 9 variables "hosp_dis_t17"
#     pluck("AgeGroup", 1) %>% 
#     select(geo, icd10, values) %>% 
#     spread(icd10, values)
# } 

# Variables = modify(as.list(data_list), ~rep(.x, 7)) %>% 
#   unlist()
# 
# combi_age_variable = tibble(Ages = rep(1:7, 9),
#                             Variables = Variables,
#                             Groups = rep(ages, 9))
# 
# 
# ages 
# 
# combi_age_variable = combi_age_variable %>% 
#   unite(AgesVariables, Variables, Groups, 
#         sep = "*", 
#         remove = FALSE)
# 
# combi_ages = combi_age_variable %>% 
#   pull(Ages)
# combi_variables = combi_age_variable %>% 
#   pull(Variables)
# 
# ages_variables =  combi_age_variable %>% 
#   pull(AgesVariables)
# 
# map2(combi_ages[1:2], combi_variables[1:2], fun_hosp_country_age_wide) %>% 
#   set_names(ages_variables[1:2])


# SOLUTION: Alternative
# Widening each AgeGroup Dataframe!
# Testing - PBM with class 0!!!
fun_hosp_region_age_wide = function(agegroup, variable){
  hosp_region_age %>% 
    pluck("value", agegroup) %>% # 7 num of age groups
    filter(Dataframe == variable)  %>% # 9 variables "hosp_dis_t17"
    pluck("AgeGroup", 1) %>% 
    select(geo, icd10, values) %>% 
    spread(icd10, values)
} 

# Variables = modify(as.list(data_list), ~rep(.x, 7)) %>% 
#   unlist()
# 
# combi_age_variable = tibble(Ages = rep(1:7, 9),
#                             Variables = Variables,
#                             Groups = rep(ages, 9))
# 
# 
# ages 
# 
# combi_age_variable = combi_age_variable %>% 
#   unite(AgesVariables, Variables, Groups, 
#         sep = "*", 
#         remove = FALSE)
# 
# combi_ages = combi_age_variable %>% 
#   pull(Ages)
# combi_variables = combi_age_variable %>% 
#   pull(Variables)
# 
# ages_variables =  combi_age_variable %>% 
#   pull(AgesVariables)
# 
# map2(combi_ages[1:2], combi_variables[1:2], fun_hosp_region_age_wide) %>% 
#   set_names(ages_variables[1:2])
# 


PCA_variables_ages_region_list = map2(combi_ages, 
                                       combi_variables, 
                                       fun_hosp_region_age_wide) %>% 
  set_names(ages_variables)


PCA_variables_ages_region = PCA_variables_ages_region_list %>% 
  enframe() %>% 
  separate(name, into = c("Variable", "Age"), 
           sep = "[*]",
           remove = FALSE)



# PCA_variables_ages_region_list = map2(combi_ages, 
#                                        combi_variables, 
#                                        fun_hosp_region_age_wide) %>% 
#   set_names(ages_variables)
# 
# 
# PCA_variables_ages_region = PCA_variables_ages_region_list %>% 
#   enframe() %>% 
#   separate(name, into = c("Variable", "Age"), 
#            sep = "[*]",
#            remove = FALSE)



### ---- 2.1 Region Imputations AVOIDED this time again!!! ------

# # Testing
# all(c(T, T, T), T)
# 
# 
# PCA_variables_ages_region %>% 
#   pluck("value",44) %>% 
#   complete.cases() %>% 
#   all(TRUE)

# Checking for missings! MALE and OTHER situation causing PBM ...
# REMOVING MISSINGS 
# NO IMPUTATION!!!

missings_r = PCA_variables_ages_region %>% 
  mutate(missings = map(value,
                        ~.x %>% 
                          complete.cases() %>% 
                          all(TRUE))) %>% 
  select(missings) %>% 
  deframe() %>% 
  unlist()


# Some missings: IMPUTATION!!!

PCA_variables_ages_region_FULL = PCA_variables_ages_region %>% 
  filter(missings_r)

age_region = PCA_variables_ages_region_FULL %>% pull(Age) %>% unique
variable_region = PCA_variables_ages_region_FULL %>% pull(Variable) %>% unique


# PCA_variables_ages_region_FULL %>% 
#   pluck("value", 45)

ages_variables_FULL_r = PCA_variables_ages_region_FULL %>% 
  pull(name)


### ---- 3.2 Region PCA ------


# fun_rownames = function(age_variable_full){
#   PCA_variables_ages_country_FULL %>% 
#     filter(name == "hosp_dis_t17*TOTAL") %>% 
#     pluck("value", 1) %>% 
#     pull(geo)
# }
# 
# row_names = map(PCA_variables_ages_country_FULL %>% 
#                   select(value) %>% 
#                   deframe(),
#                 fun_rownames)


# map2(PCA_variables_ages_country_FULL %>% 
#            select(value) %>% 
#            deframe() %>% 
#        pluck(1),
#          row_names[1] %>% 
#        unlist(),
#          ~rownames(.x) = .y)



fun_rownames_r = function(age_variable_full){
  
  
  rownames = PCA_variables_ages_region_FULL %>% 
    filter(name == age_variable_full) %>%  #"hosp_dis_t17*TOTAL") %>% #  "hosp_dis_t17*TOTAL") %>% 
    pluck("value", 1) %>% 
    pull(geo)
  
  data = PCA_variables_ages_region_FULL %>% 
    filter(name == age_variable_full) %>%  #"hosp_dis_t17*TOTAL") %>% # age_variable_full) %>% # "hosp_dis_t17*TOTAL") %>% 
    pluck("value", 1) %>% 
    as.data.frame()
  
  rownames(data) <- rownames
  
  data
  
}

named_PCA_region = map(ages_variables_FULL_r, fun_rownames_r) %>% 
  set_names(ages_variables_FULL_r) %>% 
  enframe()

# # Testing
# named_PCA_region %>% 
#   pluck("value", 1)



res_PCA_region = named_PCA_region %>% 
  # mutate(named_PCA = map(value,
  #                      ~ rownames(.x) = .x %>% 
  #                             select(1) %>% 
  #                        pull))
  # ,
  #                             graph = FALSE)))
  mutate(res_PCA = map(value,
                       ~PCA(.x %>% 
                              select(-1),
                            graph = FALSE)))

res_PCA_region = res_PCA_region %>% 
  separate(name, into = c("Variable", "Age"),
           sep = "[*]",
           remove = FALSE)

### ---- 3.2.0 Flex: Select Input DYNAMICALLY ------

fun_variables_sel_r = function(age_group){
  res_PCA_region %>% 
    filter(Age == "Y60-64") %>% # age_group) %>% # "Y60-64") %>%  #
    pull(Variable)
}

variables_sel_input_r = map(ages, fun_variables_sel_r)

# variables_sel_input_r %>% 
#   enframe() %>% 
#   select(-name) %>% 
#   distinct()


### ---- 3.2.1 Dims of Region PCA ------


### PLus interessant!!!
### PCA describe dim12 function
fun_pca_dim12_r = function(agegroup){
  
  dimdesc(res_PCA_region %>% 
            rownames_to_column() %>% 
            filter(rowname == agegroup) %>% # 10) %>%  # 
            pluck("res_PCA", 1), axes = 1:2)
  
}

# As enframed list!
res_PCA_dim12_r = map(c(1:length(ages_variables_FULL_r)), 
                      fun_pca_dim12_r) %>% # ages_variables_FULL_r
  set_names(ages_variables_FULL_r) %>%
  enframe() %>% 
  unnest(value) %>% 
  unnest(value) %>% 
  #  deframe()
  rename(Dataframe = name,
         Dimensions12 = value)

res_PCA_dim12_r = res_PCA_dim12_r %>% 
  separate(Dataframe, into = c("Variable", "Age"),
           sep = "[*]",
           remove = FALSE)

res_PCA_dim12_r = bind_cols(Dims = rep(c("dim1", "dim2"),length(ages_variables_FULL_r)),
                     res_PCA_dim12_r)

# # As tibble
# res_PCA_dim12_r = tibble(Dims = rep(c("dim1", "dim2"),length(ages_variables_FULL_r)),
#                          Ages = res_PCA_dim12_r %>% 
#                            select(Dataframe),
#                          Axes = res_PCA_dim12_r %>% 
#                            select(Dimensions12)) 




# # ### Description dimensions as dataframes!
# dim12_desc = res_PCA_dim12[[3]] %>%
#   deframe() %>%
#   map(~as.data.frame(.x)) %>% 
#   enframe()


### PCA describe dim34 function
fun_pca_dim34_r = function(agegroup){
  
  dimdesc(res_PCA_region %>% 
            rownames_to_column() %>% 
            filter(rowname == agegroup) %>% # 10) %>%  # 
            pluck("res_PCA", 1), axes = 3:4)
  
}

# As enframed list!
res_PCA_dim34_r = map(c(1:length(ages_variables_FULL_r)), 
                      fun_pca_dim34_r) %>% # ages_variables_FULL_r
  set_names(ages_variables_FULL_r) %>%
  enframe() %>% 
  unnest(value) %>% 
  unnest(value) %>% 
  #  deframe()
  rename(Dataframe = name,
         Dimensions34 = value)


res_PCA_dim34_r = res_PCA_dim34_r %>% 
  separate(Dataframe, into = c("Variable", "Age"),
           sep = "[*]",
           remove = FALSE)

res_PCA_dim34_r = bind_cols(Dims = rep(c("dim3", "dim4"),length(ages_variables_FULL_r)),
                            res_PCA_dim34_r)
# 
# res_PCA_dim34_r %>% 
#   pluck("Dimensions34", 1)

# res_PCA_dim34_r = tibble(Dims = rep(c("dim3", "dim4"), length(ages_variables_FULL_r)),
#                          Ages = res_PCA_dim34_r %>% 
#                            select(Dataframe),
#                          Axes = res_PCA_dim34_r %>% 
#                            select(Dimensions34))


### ---- 3.3 Country HCPC ------


# res_HCPC_country = PCA_variables_ages_country_FULL %>% 
#   mutate(res_HCPC = map(res_PCA,
#                        ~PCA(.x %>% 
#                               select(-1),
#                             graph = FALSE)))

# x11()

fun_HCPC_r = function(agegroup){
  res.hcpc <- HCPC(res_PCA_region %>% 
                     filter(name == agegroup) %>% #"hosp_dis_t17*TOTAL") %>%  #
                     pluck("res_PCA", 1), 
                   nb.clust=-1, 
                   kk = Inf, 
                   graph=F, 
                   consol=F)
}


### List of cluster results - Cluster number optimally chosen (Huygens criteria or so)
HCPC_list_r = map(ages_variables_FULL_r, fun_HCPC_r) %>% 
  set_names(ages_variables_FULL_r) %>% 
  enframe()

HCPC_list_r = HCPC_list_r %>%  
separate(name, into = c("Variable", "Age"),
         sep = "[*]",
         remove = FALSE)

### Testing
# x11()
cluster_viz12_r = function(age_variable){
  # x11()
  gg_fviz_12 = fviz_cluster(HCPC_list_r %>% 
                              filter(name == age_variable) %>% 
                              pluck("value", 1),
                            main = paste0("Cluster Plot for age group: ", age_variable),
                            axes = 1:2, 
                            repel = T, 
                            ggtheme = theme_bw() +
                              theme(text = element_text(family = "serif", size = 14),
                                    title = element_text(color = "#990000"))) 
  
}

gg_viz_list_r = map(ages_variables_FULL_r, cluster_viz12_r) %>% 
  set_names(ages_variables_FULL_r) %>% 
  enframe()

gg_viz_list_r = gg_viz_list_r %>% 
  separate(name, into = c("Variable", "Age"),
           sep = "[*]",
           remove = FALSE)


# # Checking!
# map(1:5, ~gg_viz_list_r %>% pluck("value", .x))





# 
# 
# 
# 
# 
# 
# 
# ##### ------ 4. WIDE datasets for PCA - Batch operations on Lists------
# 
# ### Nested operations: Selecting TOTAL-AGE population and SPREADING
# ### Countries/Regions not accounted for!
# hosp_list_nest_totpop =  hosp_list_nest %>% 
#   mutate(TotalPopWide = map(AgeGroupPop, ~(.x %>% 
#                                              select(-indic_he, -unit, -time) %>% 
#                                              select(geo, age, icd10, values) %>% 
#                                              filter(age == "TOTAL") %>% 
#                                              select(-age) %>% 
#                                              spread(icd10, values))))
# 
# 
# ### Testing nested operations: Splitting countries and regions
# hosp_list_nest_totpop_geo =  hosp_list_nest_totpop %>% 
#   mutate(CountryTotalWide = map(TotalPopWide, ~(.x %>% 
#                                                   mutate(len = nchar(geo)) %>% 
#                                                   filter(len == 2) %>% 
#                                                   select(-len)))) 
# 
# 
# hosp_list_nest_totpop_geo =  hosp_list_nest_totpop_geo %>% 
#   mutate(RegionTotalWide = map(TotalPopWide, ~(.x %>% 
#                                                  mutate(len = nchar(geo)) %>% 
#                                                  filter(len > 3) %>% 
#                                                  select(-len)))) 
