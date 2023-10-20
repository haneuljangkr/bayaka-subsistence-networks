######################################################################### Emprical
library(igraph)
V = 1                    # One blocking variable
G = 4                    # Four categories in this variable
N_id = length(Age$Age)       # Number of people

B_post = res_nomask$samples$srm_model_samples$block_parameters[[2]]

for(i in 1:4){
      for(j in 1:4){
  B_post[,i,j] = B_post[,i,j] + res_nomask$samples$srm_model_samples$block_parameters[[1]]          
      }
}

B =  apply(B_post, 2:3, median)

r2 = array(NA,c(N_id, N_id, 4))
r2[,,1] = r
r2[,,2] = both_female
r2[,,3] = different_sex
r2[,,4] = coresidence

dyadic_ests = as.numeric(res_nomask$summary$Median[4:7])

A = simulate_sbm_network(N_id = N_id, B=list(B=B), V=V, groups = data.frame(Age=as.numeric(as.factor(Age$Age))),
                         dyadic_predictors = r2,  
                         dyadic_effects = dyadic_ests,
                         mode="bernoulli"
                               )

Net = graph_from_adjacency_matrix(A$network, mode = c("directed"))
V(Net)$color = c("turquoise4","gray13", "goldenrod3","darkred")[A$group_ids$Age]

plot(Net, edge.arrow.size =0.1, edge.curved = 0.3, vertex.label=NA, vertex.size = 5)



######################################################################### Age-graded
B2 = matrix(NA, nrow=G, ncol=G)

B2[1,1] = -1
B2[1,2] = -1.5
B2[1,3] = -3
B2[1,4] = -4

B2[2,1] = B2[1,2]
B2[2,2] = -1
B2[2,3] = -1.5
B2[2,4] = -3

B2[3,1] = B2[1,3]
B2[3,2] = B2[2,3]
B2[3,3] = -1
B2[3,4] = -1.5

B2[4,1] = B2[1,4]
B2[4,2] = B2[2,4]
B2[4,3] = B2[3,4]
B2[4,4] = -1

B2 = B2 - 1.5

A = simulate_sbm_network(N_id = N_id, B=list(B=B2), V=V, groups = data.frame(Age=as.numeric(as.factor(Age$Age))),
                         dyadic_predictors = r2,  
                         dyadic_effects = dyadic_ests,
                         mode="bernoulli"
                               )

Net = graph_from_adjacency_matrix(A$network, mode = c("directed"))
V(Net)$color = c("turquoise4","gray13", "goldenrod3","darkred")[A$group_ids$Age]

plot(Net, edge.arrow.size =0.1, edge.curved = 0.3, vertex.label=NA, vertex.size = 5)

# Libraries
library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer)
set.seed(1)

####################################
# MAKE RADIAL BAR CHART
####################################

# individual information data
d <- read.table("./Data/all_ind_info.txt", header=T, sep="\t")
str(d)  

# daily data
data <- read.table("./Data/data_for_daily_matrices.txt", header = TRUE, sep="\t")

# daily foraging group
df = data[data$observed_together==1,]

dayID <- unique(df$dayid)

df_total = data.frame()

for(i in 1:length(dayID)){
  
  sub <- df[df$dayid==dayID[i],]
  
  focal <- unique(sub$focalid)
  
  sub <- sub[order(sub$ind1, sub$ind2),]

  inds <- c(sub$ind1, sub$ind2)
  
  # add vector to a dataframe
  df_inds <- data.frame(unique(inds))
  df_total <- rbind(df_total,df_inds)
}

df_total$inds = df_total$unique.inds.

df_total = df_total %>% select (-"unique.inds.")

freq_f <- df_total %>% group_by(inds) %>% 
  count(inds) %>% # this creates a new column in the data labeled 'n'
  arrange(desc(n)) # this sorts the output by 'n' in descending order.
freq_f 

new <- data.frame(inds = "boke", n =0)

freq_f = rbind(freq_f, new)


# daily camp composition
dc = data[data$camp_together==1,]

dayID <- unique(dc$dayid)

dc_total = data.frame()

for(i in 1:length(dayID)){
  
  sub <- dc[dc$dayid==dayID[i],]
  
  focal <- unique(sub$focalid)
  
  sub <- sub[order(sub$ind1, sub$ind2),]
  
  inds <- c(sub$ind1, sub$ind2)
  
  # add vector to a dataframe
  dc_inds <- data.frame(unique(inds))
  dc_total <- rbind(dc_total,dc_inds)
}

dc_total$inds = dc_total$unique.inds.

dc_total = dc_total %>% select (-"unique.inds.")

freq_c <- dc_total %>% group_by(inds) %>% 
  count(inds) %>% # this creates a new column in the data labeled 'n'
  arrange(desc(n)) # this sorts the output by 'n' in descending order.
freq_c 


# merge two datasets
freq_c$obs_n = freq_f$n [match(freq_c$inds, freq_f$inds)]

freq_c$age_class = d$age_class [match(freq_c$inds, d$ind_name)]
freq_c$sex = d$sex [match(freq_c$inds, d$ind_name)]

freq_c$age_class = factor(freq_c$age_class, levels = c("early childhood", "middle childhood", "adolescence", "adulthood"))
freq_c <- freq_c[!is.na(freq_c$age_class), ]

freq_c <- freq_c[order(freq_c$age_class),]
freq_c$sort_vec <- 1:nrow(freq_c)

angle <- 90 - 360 * (freq_c$sort_vec-0.5) /nrow(freq_c)     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
freq_c$hjust <- ifelse( angle < -90, 1, 0)
freq_c$angle <- ifelse(angle < -90, angle+180, angle)
 
pdf("./radial.pdf", height = 10, width = 10)
ggplot(freq_c, aes(x=sort_vec,  fill=age_class)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", aes(y=n), alpha = 0.5) +
  geom_bar(stat="identity", aes(y=obs_n)) +
  scale_fill_manual(values = hcl.colors(4, "Earth")) + 
  ylim(-200,300) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  guides(fill=FALSE, color=FALSE) +
  coord_polar(start = 0) 
  #geom_text(data=freq_c, aes(x=sort_vec, y=n+10, label=inds, hjust=hjust), alpha=0.6, size=2.5, angle= freq_c$angle, inherit.aes = FALSE ) 
dev.off()
 
####################################
# MAKE HIERARCHICAL EDGE BUNDLING
####################################

network = read.table("Data/foraging_weighted_network.txt", sep = "\t", header = TRUE)
people = read.table("Data/people.txt", sep = "\t", header = TRUE)


# get in-sample names
insample_names = people$indID[which(people$coresidence==1 & people$householdID2!=13 & people$age_class!=0)]

# prune dataset to only insample individuals
people = people[which(people$indID %in% insample_names),] 

people$age_class_text = NA
people$age_class_text[people$age_class == 1] = "early childhood"
people$age_class_text[people$age_class == 2] = "middle childhood"
people$age_class_text[people$age_class == 3] = "adolescence"
people$age_class_text[people$age_class == 4] = "adulthood"


forage_array = array(NA,c(length(daily_foraging),60,60))
daily_array = array(NA,c(length(daily_camp),60,60))

for(i in 1:length(daily_foraging)){
 forage_array[i,,] = daily_foraging[[i]]
 daily_array[i,,] = daily_camp[[i]]
}

f_a = apply(forage_array, 2:3,sum)
f_c = apply(daily_array, 2:3,sum)

f_rat = f_a / f_c

colnames(f_rat) = rownames(f_rat) = colnames(r)
f_rat = as.matrix(f_rat)
f_rat[lower.tri(f_rat)] = NA 
diag(f_rat) = NA 
f_rat = melt(f_rat)
network = f_rat[complete.cases(f_rat),]

network$from_sort = freq_c$sort_vec[match(as.character(network$Var1), freq_c$inds)]
network$to_sort = freq_c$sort_vec[match(as.character(network$Var2), freq_c$inds)]
 
# create a data frame giving the hierarchical structure of your individuals
d1 = data.frame(from="origin", to=c("early childhood", "middle childhood", "adolescence", "adulthood"))
d2 = data.frame(from=people$age_class_text, to=people$indID)
# Play with sorting here!
d2=d2[order(d2$from),]

edges = rbind(d1, d2)

# create a dataframe with connection between leaves (individuals)
#connect = data.frame( from=as.character(network$Var1), to=as.character(network$Var2))
connect = data.frame( from=network$from_sort, to=network$to_sort)
connect$value = network$value
 
# create a vertices data.frame. One line per object of our hierarchy
vertices  =  data.frame(
  name = unique(c(edges$from, edges$to)) , 
  value = runif(65)
) 

# Let's add a column with the group of each name. It will be useful later to color points
vertices$group  =  edges$from[ match( vertices$name, edges$to ) ]

vertices$name_sort = freq_c$sort_vec[match(vertices$name, freq_c$inds)]
vertices <- vertices[order(vertices$name_sort),]
#vertices$name_sort[is.na(vertices$name_sort)] <- vertices$name[is.na(vertices$name_sort)]
#Let's add information concerning the label we are going to add: angle, horizontal adjustement and potential flip
#calculate the ANGLE of the labels
vertices$id = NA
myleaves = which(is.na( match(vertices$name, edges$from) ))
nleaves = length(myleaves)
vertices$id[ myleaves ] = seq(1:nleaves)
vertices$angle = 90 - 360 * vertices$name_sort / nleaves
vertices <- vertices[order(vertices$name_sort),]
# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust = ifelse( vertices$angle < -90, 1, 0)
 
# flip angle BY to make them readable
vertices$angle = ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)

deg = aggregate(connect$value, by=list(name=connect$from), FUN=sum)
deg$degree = deg$x
deg=deg[,colnames(deg) !="x"]
deg$degree = (deg$degree/max(deg$degree))^2

#vertices = merge(vertices,deg, by="name",all=TRUE)

#vertices=vertices[order(vertices$group),]

# Create a graph object
mygraph = igraph::graph_from_data_frame( edges, vertices=vertices )
 
# The connection object must refer to the ids of the leaves:
connect2 = connect[which(connect$value > 0.075),]
connect2[order(connect2$from),]
from  =  match( connect2$from, vertices$name_sort)
to  = match( connect2$to, vertices$name_sort)
 
# Basic usual argument
pdf("./network.pdf", height = 10, width = 10)
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05), color="white") +
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="#2F4F4F", width=0.9,tension = 0.9) +
  geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=3.5, alpha=1) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-1.2, 1.2), y = c(-1.2, 1.2)) +
  theme(legend.position = "bottom", legend.title=element_blank(), legend.text=element_text(size=14)) +
    guides(alpha = "none", colour = guide_legend(override.aes = list(size=6, alpha=0.6)))
dev.off()

