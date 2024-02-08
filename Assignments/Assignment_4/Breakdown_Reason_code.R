library(tidyverse)

data <- read.csv("applications.csv")
str(data)
data_filter <- data %>% 
  filter(review_reason_code %in% c("1", "2", "3", "4", "5", "6", "7"))

         
  
 x <- data_filter %>% 
   group_by(review_reason_code) %>% 
   summarise(Count = n()) %>% 
   mutate(Percentage = Count / sum(Count) * 100)
 
 ggplot(x, aes(x = review_reason_code, y = Percentage, fill = review_reason_code)) +
   geom_bar(stat = "identity") +
   theme_minimal() +
   geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5)+
               labs(title = "Breakdown of Review Reason Codes Used",
                    x = "Review Reason Code",
                    y = "Percentage") +
   scale_fill_discrete(name = "Review Reason", labels = c("Sexual connotation/Depravity", "Vulgar/Hostile", "Profanity", "Negative towards a specific group", 
                                                          "Misrepresents  law enforcement", "Deleted from regular series", "Foreign/Slang"))
         
             