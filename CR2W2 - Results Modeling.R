library(readr);library(dplyr);library(Metrics)

CR2W2_Main <- read.csv("CR2 Wave 2 Results Model Friendly Draft - Results by Ad Set (Grouped).csv")
CR2W2_Filtered <- CR2W2_Main[!is.na(CR2W2_Main$Lead_Rate),] %>% filter(Lead_Rate != 0, Impressions >= 1000)
CR2W2_Filtered$Leads_Per_1k_Impressions <- CR2W2_Filtered$Leads / CR2W2_Filtered$Impressions * 1000
CR2W2_Filtered$K_Impressions_Per_Click <- (CR2W2_Filtered$Impressions / CR2W2_Filtered$Clicks) / 1000
pred_Leads_Per_1k_Impressions <- CR2W2_Filtered %>% glm(Leads_Per_1k_Impressions ~ Brand_Position + 
                                                          Audience + Creative, family = "gaussian",.)
pred_1k_Impressions_Per_Click <- CR2W2_Filtered %>% glm(K_Impressions_Per_Click ~ Brand_Position + 
                                                          Audience + Creative, family = "gaussian",.)
CR2W2_Filtered$pred_Leads_Per_1k_Impressions <- predict(pred_Leads_Per_1k_Impressions)
CR2W2_Filtered$pred_1k_Impressions_Per_Click <- predict(pred_1k_Impressions_Per_Click)
CR2W2_Filtered$pred_Lead_Rate <- CR2W2_Filtered$pred_1k_Impressions_Per_Click * CR2W2_Filtered$pred_Leads_Per_1k_Impressions



CR2W2_Analysis <- CR2W2_Filtered %>% group_by(Brand_Position, Audience) %>% summarise(lead_rate_ovr = sum(Leads)/sum(Clicks), 
                                     pred_lead_rate_ovr = mean(pred_Lead_Rate),
                                     tot_clicks = sum(Clicks), tot_leads = sum(Leads))

View(CR2W2_Filtered)
write.csv(CR2W2_Filtered, "CR2 Wave 2 Results Modeled.csv")
write.csv(CR2W2_Analysis, "CR2 Wave 2 Analysis.csv")
