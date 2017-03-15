library(dplyr)

## single-entity join

# data = read.csv("data.csv")
# 
# joined_data = left_join(x = data,
#                         y = data,
#                         by = c("A_ID"="B_ID")) %>% 
#   select(A_ID = A_ID,
#          A_time = B_Time.y,
#          B_ID = B_ID,
#          B_time = B_Time.x
#          ) %>% 
#   #filter(!is.na(A_time))
#   mutate(A_time = ifelse(is.na(A_time),0,A_time))  
  

## multi-entities join

data = read.csv("multi_entity_data.csv")

### joined_data: The base table
joined_data = left_join(x = data,
                        y = data,
                        by = c("A_ID"="B_ID","entity"="entity")) %>% 
  select(entity = entity,
         A_ID = A_ID,
         A_time = B_Time.y,
         B_ID = B_ID,
         B_time = B_Time.x) %>% 
  #filter(!is.na(A_time))
  mutate(A_time = ifelse(is.na(A_time),0,A_time),
         Interval_time = B_time - A_time,
         n = 1)  


nodes_share_times = joined_data %>% 
  distinct(entity,A_ID) %>% 
  group_by(A_ID) %>% 
  count()

nodes_influence_times = joined_data %>% 
  #distinct(entity,A_ID) %>% 
  group_by(A_ID) %>% 
  count()

link_influence_times = joined_data %>% 
  #distinct(entity,A_ID,B_ID) %>% 
  group_by(A_ID, B_ID) %>% 
  count()

### test the influence times consistency here:
if(!sum(nodes_influence_times$nn) == sum(link_influence_times$nn)){
  print("NOTE:the total nodes influence times and link influence times are not equal, please check!")
} else{
  print("Good job!, the influence times maintain the consistency ")
}

link_average_interval_time = joined_data %>% 
  group_by(A_ID, B_ID) %>% 
  summarise(mean(Interval_time))

### test the links consistency here:
if(!nrow(link_influence_times) == nrow(link_average_interval_time)){
  print("NOTE:the total links among 'link_influence_times' and 
        'link_average_interval_time' are not equal, please check!")
} else{
  print("Good job!, the links maintain the consistency ")
}



## main link table:

main_link_table = link_influence_times %>% 
  rename(link_influence_times = nn) %>% 
  left_join(link_average_interval_time, 
            by = c("A_ID" = "A_ID", "B_ID" = "B_ID")) %>% 
  rename(mean_interval_time = `mean(Interval_time)`) %>% 
  left_join(nodes_share_times, 
            by = c("A_ID" = "A_ID")) %>% 
  rename(A_share_times = n) %>% 
  left_join(nodes_influence_times, 
            by = c("A_ID" = "A_ID")) %>% 
  rename(A_influence_times = nn) %>% 
  mutate(share_P = link_influence_times/A_share_times)


