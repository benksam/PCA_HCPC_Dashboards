### ----- 2. Visits Frequencies - Education and Agegroups PCA4:   ------
visit12_doct_1 = read_csv("1_RawData/visit12_doct_1.csv")

# Selected edu
edu = visit12_doct_1 %>% pull(isced11) %>% unique
edu = edu[1:3]

# Selected age groups!
ages = visit12_doct_1 %>% pull(age) %>% unique
ages = ages[c(2:3,5:7)]

# Uniting combinations excl. age groups as main cateory!
visit12_doct_1_sel = visit12_doct_1 %>% 
  mutate_if(is.character, as.factor) %>% 
  filter(med_spec == "GEN") %>%
  filter(isced11 %in% edu) %>% 
  filter(age %in% ages) %>% 
  select(-med_spec, -time, -unit) %>% 
  select(geo, everything()) %>% 
  unite(combi, frequenc, isced11, sex, sep = "*", remove = FALSE)

# Vectorised combi!
combinaison = visit12_doct_1_sel %>% 
  unite(combi, frequenc, isced11, sex, sep = "*", remove = FALSE) %>% 
  distinct(combi) %>% 
  pull()

length(combinaison)

# dataframe of combi   
df_combi = visit12_doct_1_sel %>% 
  unite(combi, frequenc, isced11, sex, sep = "*", remove = FALSE) %>% 
  distinct(combi, frequenc, isced11, sex) 

# age df as list column 
df_age = visit12_doct_1_sel %>%
  group_by(age) %>% 
  nest

# Vectorised combi of age groups and created combi!!! (Prep of age-group PCA)
age_combinaison = visit12_doct_1_sel %>% 
  unite(age_combi, age, combi, sep = "&", remove = FALSE) %>% 
  distinct(age_combi) %>% 
  pull()

# df of new combi!
age_combi = visit12_doct_1_sel %>% 
  unite(age_combi, age, combi, sep = "&", remove = FALSE) %>% 
  distinct(age_combi, age, combi)

### Extracting age groups for mapping
edad = age_combi %>% pull(age) %>% as.character()
edad_num = rep(1:5, 45) 

### Extracting first combis for mapping!
combis = age_combi %>% pull(combi) %>% as.character()

fun_ages_combi = function(edades, combis){
  df_age %>% 
    pluck("data", edades) %>% # in number!!!
    filter(combi == combis)
}


### Full list - 225 age x combi
age_combinaison_list = map2(edad_num, combis, fun_ages_combi) %>% 
  set_names(age_combinaison)

# Function to extract age specifis combis: 5 groups will all combis!
plus5 = function(arg){
  map_dbl(0:44, ~5*.x + arg)
}

plus5_list = map(1:5, plus5)

# # Function: extracting out 225 for Y16_24: 45 df in total!
# fun_AgeGroup_combinaison_list = function(arg){
#   age_combinaison_list %>% 
#     pluck(arg)
# }


# # Testing on 1 element! - Y16_24
# Y16_24_combinaison_list = map(as.list(unlist(plus5_list[1])), 
#                               fun_AgeGroup_combinaison_list) %>% 
#   set_names(map_chr(as.list(unlist(plus5_list[1])),
#                     ~age_combinaison[.x]))
# 

### Testing generally
fun_AgeGroup_combinaison_list = function(arg){
  age_combinaison_list %>% 
    pluck(arg)
}

# Function: extracting out 225 for Y16_24: 45 df in total!
fun_by_age_list = function(arg){
  
  map(as.list(unlist(plus5_list[arg])), 
      fun_AgeGroup_combinaison_list) %>% 
    set_names(map_chr(as.list(unlist(plus5_list[arg])),
                      ~age_combinaison[.x]))
  
}

by_age_list = map(1:5, fun_by_age_list) 

by_age_list_nested = map(1:5, fun_by_age_list) %>% 
  set_names(ages) %>% 
  enframe()

# by_age_list %>% 
#   enframe()

fun_by_age_df = function(arg){
  
  by_age_list[[arg]] %>% 
    bind_rows() %>% 
    filter(sex != "T") %>% 
    select(geo, combi, values) %>% 
    spread(combi, values)
  
}

# List for PCA!
by_age_wide_list = lapply(1:5, fun_by_age_df) %>% 
  set_names(ages)


# Check countries - fct ordered alphabetically!!!
Countries = by_age_wide_list[[1]] %>% 
  pull(geo) %>% 
  as.character()

fun_has_NA = function(arg){
  
  
  by_age_wide_list[arg] %>% 
    pluck(1) %>% # since still a list!!!
    complete.cases() %>% 
    any(FALSE)
  
}

# # All have NAs: need to impute
# map(1:5, fun_has_NA)

# Imputation function
impute_function = function(data){
  
  # ALternative: SINGLE Procedure
  Imp = by_age_wide_list %>% # data %>%  # 
    # pluck(1) %>% 
    enframe() %>% 
    filter(name  == data) %>%  #"Y65-74") %>% #data) %>%  #"Y16-24") %>% #  "Y16-24") %>% # data) %>% # data # "Y16-24"
    select(value) %>% 
    unnest() %>% 
    select(-1) %>% 
    # select(-O) %>% # not taken out???
    as.data.frame() 
  
  # Coutries
  rownames(Imp) = by_age_wide_list %>% # data %>%  # "Y16-24"
    # pluck(1) %>% 
    enframe() %>% 
    filter(name  == data) %>%  #"Y65-74") %>% #data) %>%  #"Y16-24") %>% # data) %>%  ## data) %>% # data # "Y16-24"
    select(value) %>% 
    unnest() %>% 
    select(1) %>%
    pull() %>% 
    as.character()
  
  comp <- imputePCA(Imp, ncp=2, scale=TRUE) ## Compl?te le tableau
  
  # # Adding Countries to imputed table
  # data_imp = comp$completeObs
  # 
  # data_imp
  
  comp 
  # %>% 
  #   tibble()
  
}

### Lopping from 2 to 5: UNKNOWN/KNOWN problem with 1st element!
imputed_data = lapply(as.list(ages[2:5]), impute_function) %>% 
  set_names(ages[2:5])


# PCA
res_PCA = imputed_data %>% 
  enframe() %>% 
  # pluck("value") %>% 
  mutate(res.PCA = map(value,
                       ~PCA(.x,
                            graph = FALSE)))

### Structure resultat PCA (create dataframe!)

PCA_Descriptions = read_csv("1_RawData/PCA_Description.csv")   


### Alternative - Eigenvalues
obj = res_PCA %>% 
  pluck("res.PCA", 1) 

obj$eig %>% as.data.frame() %>% glimpse()


# names - 5 main categories
infos = res_PCA %>% 
  pluck("res.PCA", 1) %>% 
  names()

# ### Function extraction additional PCA-infos
# fun_pca_info = function(arg, info){
#   
#   ### Direct
#   res_PCA %>% 
#     pluck("res.PCA", arg) %>% 
#     pluck(info)
#   
# }

### PLus interessant!!!
### PCA describe dim12 function
fun_pca_dim12 = function(agegroup){
  
  dimdesc(res_PCA %>% 
            pluck("res.PCA", agegroup), axes = 1:2)
  
}

# As enframed list!
res_PCA_dim12 = map(c(1:4), fun_pca_dim12) %>% 
  set_names(ages[2:5]) %>%
  enframe() %>% 
  unnest(value) %>% 
  unnest(value) %>% 
  #  deframe()
  rename(Dataframe = name,
         Dimensions12 = value)

# As tibble
res_PCA_dim12 = tibble(Dims = rep(c("dim1", "dim2"),4),
                       Ages = res_PCA_dim12 %>% 
                         select(Dataframe),
                       Axes = res_PCA_dim12 %>% 
                         select(Dimensions12)) 

# # ### Description dimensions as dataframes!
# dim12_desc = res_PCA_dim12[[3]] %>%
#   deframe() %>%
#   map(~as.data.frame(.x)) %>% 
#   enframe()


### PCA describe dim34 function
fun_pca_dim34 = function(agegroup){
  
  dimdesc(res_PCA %>% 
            pluck("res.PCA", agegroup), axes = 3:4)
  
}

res_PCA_dim34 = map(c(1:4), fun_pca_dim34) %>% 
  set_names(ages[2:5]) %>%
  enframe() %>% 
  unnest(value) %>% 
  unnest(value) %>% 
  #  deframe()
  rename(Dataframe = name,
         Dimensions34 = value)

res_PCA_dim34 = tibble(Dims = rep(c("dim3", "dim4"),4),
                       Ages = res_PCA_dim34 %>% 
                         select(Dataframe),
                       Axes = res_PCA_dim34 %>% 
                         select(Dimensions34)) 

# ### Description dimensions as dataframes!
# dim34_desc = res_PCA_dim34[[3]] %>% 
#   deframe() %>% 
#   map(~as.data.frame(.x))







fun_HCPC = function(agegroup){
  
  res.hcpc <- HCPC(res_PCA %>% 
                     pluck("res.PCA", agegroup), 
                   nb.clust=-1, 
                   kk = Inf, 
                   graph=F, 
                   consol=F)
}


### List of cluster results - Cluster number optimally chosen (Huygens criteria or so)
HCPC_list = map(1:4, fun_HCPC) %>% 
  set_names(ages[2:5]) %>% 
  enframe()

cluster_viz12 = function(agegroup, names){
  
  gg_fviz_12 = fviz_cluster(HCPC_list %>% 
                              pluck("value", agegroup),
                            main = paste0("Cluster Plot for age group: ", names),
                            axes = 1:2, 
                            repel = T, 
                            ggtheme = theme_bw() +
                              theme(text = element_text(family = "serif", size = 14),
                                    title = element_text(color = "#990000"))) 
  
}

gg_viz_list = map2(1:4, ages[2:5], cluster_viz12) %>% 
  set_names(ages[2:5]) %>% 
  enframe()





#### Selectionner les variables importantes pour 
### 1. chaque age group 
### 2. dimension!!!


### Selection of PCA-relevant variables for dim12
fun_age_dim12_names = function(dims, agegroup){
  ### For getting raw Variables after PCA dimension interpretation
  dim_input_var = res_PCA_dim12 %>% 
    filter(.[[2]] == agegroup) %>% # agegroup) %>% # age group
    filter(.[[1]] == dims) %>% # dimension!
    .[[3]] %>% 
    deframe() %>% 
    pluck(1) %>% 
    row.names()
}

### For name identifier of the list!
df_combi_dim12_age = tibble(Dims = res_PCA_dim12 %>% pull(1),
                            Ages = res_PCA_dim12 %>% pull(2) %>% deframe())

### Unique ombi as vector
combi_dim12_age = df_combi_dim12_age %>% 
  unite(DimAge, Dims, Ages, remove = TRUE) %>% 
  pull(DimAge)

### Named list of ALL Relevant PCA-variables from dim12
dim12_age_var_names = map2(res_PCA_dim12 %>% pull(1) %>% as.list(), 
                           res_PCA_dim12 %>% pull(2) %>% deframe() %>%  as.list(),
                           fun_age_dim12_names) %>% 
  set_names(combi_dim12_age)


dim1_age_var_names = dim12_age_var_names %>% 
  enframe() %>% 
  slice(c(1,3,5,7))

dim2_age_var_names = dim12_age_var_names %>% 
  enframe() %>% 
  slice(c(2,4,6,8))


# ### 
# dim12_age_var_names %>% 
#   enframe() %>% 
#   separate(name, into = c("Dims", "Ages"), 
#            sep = "_",
#            remove = FALSE) %>% 
#   pluck("value", 1)


### Selection of PCA-relevant variables for dim34
fun_age_dim34_names = function(dims, agegroup){
  ### For getting raw Variables after PCA dimension interpretation
  dim_input_var = res_PCA_dim34 %>% 
    filter(.[[2]] == agegroup) %>% # agegroup) %>% # age group
    filter(.[[1]] == dims) %>% # dimension!
    .[[3]] %>% 
    deframe() %>% 
    pluck(1) %>% 
    row.names()
}

### For name identifier of the list!
df_combi_dim34_age = tibble(Dims = res_PCA_dim34 %>% pull(1),
                            Ages = res_PCA_dim34 %>% pull(2) %>% deframe())

### Unique ombi as vector
combi_dim34_age = df_combi_dim34_age %>% 
  unite(DimAge, Dims, Ages, remove = TRUE) %>% 
  pull(DimAge)

### Named list of ALL Relevant PCA-variables from dim12
dim34_age_var_names = map2(res_PCA_dim34 %>% pull(1) %>% as.list(), 
                           res_PCA_dim34 %>% pull(2) %>% deframe() %>%  as.list(),
                           fun_age_dim34_names) %>% 
  set_names(combi_dim34_age)


dim3_age_var_names = dim34_age_var_names %>% 
  enframe() %>% 
  slice(c(1,3,5,7))

dim4_age_var_names = dim34_age_var_names %>% 
  enframe() %>% 
  slice(c(2,4,6,8))

# ### 
# dim34_age_var_names %>% 
#   enframe() %>% 
#   separate(name, into = c("Dims", "Ages"), 
#            sep = "_",
#            remove = FALSE) %>% 
#   pluck("value", 1)




dims_age_var_names = bind_rows(dim1_age_var_names,
                               dim2_age_var_names,
                               dim3_age_var_names,
                               dim4_age_var_names) %>% 
  separate(name, into = c("Dims", "Ages"), 
           sep = "_",
           remove = FALSE)

