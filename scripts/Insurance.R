library(tidyverse)
library(ggridges)
library(GGally)

travel_insurance <- read.csv('data/travel_insurance.csv')

str(travel_insurance)

travel_insurance_prep <- travel_insurance %>% 
  mutate(GraduateOrNot = as.logical(ifelse(GraduateOrNot=="Yes",1,0)),
         ChronicDiseases = as.logical(ChronicDiseases),
         FrequentFlyer = as.logical(ifelse(FrequentFlyer =="Yes",1,0)),
         EverTravelledAbroad = as.logical(ifelse(EverTravelledAbroad =="Yes",1,0)),
         TravelInsurance = as.logical(TravelInsurance),
         GovernmentEmployee = as.logical(ifelse(Employment.Type=="Government Sector",1,0)),
         Age = Age,
         FamilyMembers = FamilyMembers)

str(travel_insurance_prep)
summary(travel_insurance_prep)

#ggpairs(travel_insurance_prep)

age_freq <- travel_insurance_prep %>%
  group_by(Age, TravelInsurance) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

travel_insurance_prep %>%
  ggplot(aes(x= as.factor(Age))) +
  geom_histogram(stat = "count") +
  theme_classic() +
  ggtitle("Age distribution") +
  labs(x = "Age")

travel_insurance_prep %>% 
  ggplot(aes(x= as.factor(Age), fill = TravelInsurance)) +
  geom_histogram(stat = "count", position = "fill") +
  theme_classic() +
  ggtitle("Traveller insurance vs Age") +
  labs(x = "Age", y ="", fill = "Traveller insurance") +
  scale_y_continuous(labels = scales::percent)

family_freq <- travel_insurance_prep %>%
  group_by(FamilyMembers, TravelInsurance) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

travel_insurance_prep %>%
  ggplot(aes(x= as.factor(FamilyMembers))) +
  geom_histogram(stat = "count") +
  theme_classic() +
  ggtitle("Family size distribution") +
  labs(x = "Family size")

travel_insurance_prep %>% 
  ggplot(aes(x= as.factor(FamilyMembers), fill = TravelInsurance)) +
  geom_histogram(stat = "count", position = "fill") +
  theme_classic() +
  ggtitle("Traveller insurance vs family size") + 
  labs(x = "Family size", y ="", fill = "Traveller insurance") +
  scale_y_continuous(labels = scales::percent)

travel_insurance_prep %>%
  ggplot(aes(x= AnnualIncome)) +
  geom_histogram(stat = "count") +
  theme_classic() +
  ggtitle("Annual income distribution") +
  labs(x = "Annual income")

travel_insurance_prep %>% 
  ggplot(aes(x= AnnualIncome, fill = TravelInsurance)) +
  geom_histogram(position = "fill", bins = 10) +
  theme_classic() +
  ggtitle("Traveller insurance vs annual income") +
  labs(x = "Annual income", y ="", fill = "Traveller insurance") +
  scale_y_continuous(labels = scales::percent)

travel_insurance_prep %>%
  group_by(Employment.Type, TravelInsurance) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

travel_insurance_prep %>%
  ggplot(aes(x = Employment.Type, fill = TravelInsurance)) +
  geom_bar(position = "fill") +
  theme_classic() +
  ggtitle("Traveller insurance vs type of employment") +
  labs(x = "Employment type", y ="", fill = "Traveller insurance") +
  scale_y_continuous(labels = scales::percent)

travel_insurance_prep %>%
  group_by(FrequentFlyer, TravelInsurance) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

travel_insurance_prep %>%
  ggplot(aes(x = FrequentFlyer, fill = TravelInsurance)) +
  geom_bar(position = "fill") +
  theme_classic() +
  ggtitle("Traveller insurance vs frequent flyer status") +
  labs(x = "Books  frequent  tickets", y ="", fill = "Traveller insurance") +
  scale_y_continuous(labels = scales::percent)

travel_insurance_prep %>%
  group_by(EverTravelledAbroad, TravelInsurance) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

travel_insurance_prep %>%
  ggplot(aes(x = EverTravelledAbroad, fill = TravelInsurance)) +
  geom_bar(position = "fill") +
  theme_classic() +
  ggtitle("Traveller insurance vs experience travelling abroad") +
  labs(x = "Has travelled abroad", y ="", fill = "Traveller insurance") +
  scale_y_continuous(labels = scales::percent)

travel_insurance_prep %>%
  group_by(ChronicDiseases, TravelInsurance) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

travel_insurance_prep %>%
  ggplot(aes(x = ChronicDiseases, fill = TravelInsurance)) +
  geom_bar(position = "fill") +
  theme_classic() +
  ggtitle("Traveller insurance vs chronic condition") +
  labs(x = "Has a chronic  condition", y ="", fill = "Traveller insurance") +
  scale_y_continuous(labels = scales::percent)

travel_insurance_prep %>%
  group_by(GraduateOrNot, TravelInsurance) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

travel_insurance_prep %>%
  ggplot(aes(x = GraduateOrNot, fill = TravelInsurance)) +
  geom_bar(position = "fill") +
  theme_classic() +
  ggtitle("Traveller insurance vs higher education") +
  labs(x = "Is  a  college  graduate", y ="", fill = "Traveller insurance") +
  scale_y_continuous(labels = scales::percent)

travel_insurance_prep %>%
  group_by(Age, Employment.Type) %>%
  summarize(MeanAnnualIncome = mean(AnnualIncome)) %>%
  ggplot(aes(x = as.factor(Age), y = MeanAnnualIncome, group = Employment.Type, color = Employment.Type)) +
  geom_line(size = 1) +
  scale_color_manual(values=c("blue", "red"))+
  theme_classic() +
  ggtitle("Salary vs Age, by type of employment") +
  labs(x = "Age", y ="Mean annual income", color = "Type of employment")

travel_insurance_prep %>%
  group_by(EverTravelledAbroad) %>%
  summarize(MeanAnnualIncome = mean(AnnualIncome))

travel_insurance_prep %>%
  group_by(FrequentFlyer) %>%
  summarize(MeanAnnualIncome = mean(AnnualIncome))

travel_insurance_prep %>%
  group_by(Age, ChronicDiseases) %>%
  summarize(MeanAnnualIncome = mean(AnnualIncome)) %>%
  ggplot(aes(x = Age, y = MeanAnnualIncome, group = ChronicDiseases, color = ChronicDiseases)) +
  geom_line() +
  theme_classic() +
  ggtitle("Salary vs Age, with chronic disease effect") +
  labs(x = "Age", y ="Mean Annual Income", color = "Has a Chronic disease")

travel_insurance_prep %>%
  group_by(FamilyMembers) %>%
  summarize(MeanAnnualIncome = mean(AnnualIncome)) %>%
  ggplot(aes(x = as.numeric(FamilyMembers), y = MeanAnnualIncome)) +
  geom_line() +
  theme_classic() +
  ggtitle("Salary vs Family size") +
  labs(x = "Family size", y ="Mean Annual Income")

cor(as.numeric(travel_insurance_prep$Age), travel_insurance_prep$TravelInsurance)
cor(as.numeric(travel_insurance_prep$FamilyMembers), travel_insurance_prep$TravelInsurance)
cor(travel_insurance_prep$AnnualIncome, travel_insurance_prep$TravelInsurance)
cor(travel_insurance_prep$FrequentFlyer, travel_insurance_prep$TravelInsurance)
cor(travel_insurance_prep$EverTravelledAbroad, travel_insurance_prep$TravelInsurance)
cor(travel_insurance_prep$ChronicDiseases, travel_insurance_prep$TravelInsurance)
cor(travel_insurance_prep$GraduateOrNot, travel_insurance_prep$TravelInsurance)
cor(travel_insurance_prep$GovernmentEmployee, travel_insurance_prep$TravelInsurance)
cor(travel_insurance_prep$GovernmentEmployee, travel_insurance_prep$AnnualIncome)
cor(as.numeric(travel_insurance_prep$Age), travel_insurance_prep$AnnualIncome)
  