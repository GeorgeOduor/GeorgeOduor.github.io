# /",month
rm(list = ls())
# golem::detach_all_attached()
# rstudioapi::restartSession()
library(openxlsx);library(dplyr);library(car);library(tidyverse);library(gender);library(AzureAuth);library(AzureGraph);library(Microsoft365R)
# library(future);library(promises);plan(multisession);
library(mbanalytics)
library(gender)

exceptions = read_csv("E:/CRB/Deletion/cumlist.csv",col_names = F) %>% 
  separate(X7,into = c('AccountID',NA),sep = "_")  %>% 
  select(AccountID) %>% mutate(AccountID = paste0("00",as.numeric(AccountID))) %>%
  pull(AccountID) 
contact_list = read.xlsx("E:/external_debt_collectors/contact/contactlist.xlsx",2)
path_to_cred = "E:/Automation/resources/lawrence"
path_to_cred = "E:/Automation/resources/george"
pathtoupdates = paste0("E:/CRB/Daily_Update/",month(Sys.Date()-3,T))#change here
last_update_date = dir(pathtoupdates,r=T,pattern = ".xlsx") %>% 
  str_extract("[0-9]{2}[.][0-9]{2}[.][0-9]{4}") %>% 
  max()

# last_update_date = "20.12.2023"

prev_reporting_date <- as.Date(glue("{str_sub(last_update_date,-4,-1)}-{str_sub(last_update_date,-7,-6)}-{str_sub(last_update_date,1,2)}"))

last_update_date <- ifelse(is.na(last_update_date),format(prev_reporting_date,"%d.%m.%Y"),as.character(last_update_date))

cbk_exception <- read.xlsx("E:/Reports/improptu/cbk_waiver/2022/listing.xlsx",1) %>% 
  select(Account_Number,Current_Balance) %>% mutate(Excempted = 1)


# impact = read.xlsx("C:/Users/George Oduor/Downloads/web/skype/CRB_Impact2.xlsx") %>% 
#   mutate(LoanAccount = paste0(AccountID,"_",LoanSeries),
#          Status = ifelse(CRB_Status == "Already_in_NPA_sep_2020",
#                          "Not Affected","Affected")) %>% 
#   dplyr::filter(
#     # Status == 'Not Affected',
#     grepl('MFC|MLS904',ProductID)) %>% 
#   select(LoanAccount,CRB_Status,Status,ProductID)
# Done on a daily basis
# rm(list = ls())
print("Creating Directories")
# creating directories ====
timechecker  = strsplit(strsplit(as.character(Sys.time()),split = " ")[[1]][2],":")[[1]][1] # for customised email sending
PastDays = 1# for auto report creation and file upload
# \\\\\\\\\\\\\\\\\\CONTROL POINT\\\\\\\\\\\\\\\\\
reporting_date = Sys.Date()-PastDays#<====\\\\\\\\\\\\\
# \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\737804999

year <- substr(reporting_date,start = 1,stop = 4)# gives the year of report
day <-  substr(reporting_date,start = 9,stop = 10) # gives the numeric representation of the day of reporting
month.d <- substr(reporting_date,start = 6,stop = 7) # numeric representation of the month
month <- month.abb[as.numeric(month.d)]#  month of reporting abreviation 

# This aides dynamic upload of previous reporting active loan book

prev_reporting_date <- as.Date(glue("{str_sub(last_update_date,-4,-1)}-{str_sub(last_update_date,-7,-6)}-{str_sub(last_update_date,1,2)}"))

# create a directory to contain daily update reports
dir.create(paste0("E:/CRB/Daily_Update/",month),recursive = T,showWarnings = F)
MainDir <- paste0("E:/CRB/Daily_Update/",month)
SubDir = paste0(day,".",month.d,".",year)
SubDir.1 = paste0(substr(prev_reporting_date,start = 9,stop = 10),".",
                  substr(prev_reporting_date,start = 6,stop = 7),".",
                  substr(prev_reporting_date,start = 1,stop = 4))
month.1 = month.abb[as.numeric(substr(prev_reporting_date,6,7))]
dir.create(file.path(MainDir,SubDir),r=T,sh=F)
# loadind data =====
print("Downloading and Uploading Data")

# pathtoactive = readxl::read_excel("E:/Reports/Dashboard/Apr/06.04.2020/M-Fanisi_Dashboard_06.04.2020.xlsx")
# pathtoprevious = readxl::read_excel("E:/Reports/Dashboard/Apr/04.04.2020/M-Fanisi_Saf_Dashboard_04.04.2020.xlsx")
# 
ActiveLoanBK <- read.xlsx(paste0("E:/Reports/Dashboard/",month,"/",SubDir,"/LoanList_",SubDir,".xlsx")) %>% 
  mutate(Loanac = paste0(AccountID,"_",LoanSeries),
         ClearBalance = ifelse(OSPrinciple == 0,0,ClearBalance)) %>% 
  maisha_current_Portfolio("all") %>% 
  filter(!AccountID %in% c('0019040000005','0019040029628'),
         MobileNo != '254711894704',
         grepl("MFC|MLS",ProductID)) 

# ActiveLoanBK <- rbind(read.xlsx(paste0("E:/Reports/Dashboard/",month,"/",SubDir,"/M-Fanisi_Dashboard_",SubDir,".xlsx"),sheet = 3)#%>% 
#                       # dplyr::filter(!CreditOfficerName == "ONFON")
#                       ,
#                       read.xlsx(paste0("E:/Reports/Dashboard/",month,"/",SubDir,"/M-Fanisi_Saf_Dashboard_",SubDir,".xlsx"),sheet = 3)
#                       # %>% 
#                       # dplyr::filter(CreditOfficerName == "ONFON")
# ) 
# ActiveLoanBK <- ActiveLoanBK %>% 
#   mutate(Loanac = paste0(AccountID,"_",LoanSeries),
#          ClearBalance = ifelse(OSPrinciple == 0,0,ClearBalance))     
# left_join(impact %>% select(-c(ProductID,CRB_Status)) %>%
#             distinct(LoanAccount,.keep_all=T) ,
#           by = c("Loanac" = "LoanAccount"))
# 
library(magrittr)
ActiveLoanBK %<>% dplyr::filter(!AccountID %in% exceptions,
                                as.numeric(ClearBalance) >= 1000)%>% 
  left_join(cbk_exception,by=c('Loanac' = 'Account_Number')) %>% 
  mutate(ClearBalance = ifelse(!is.na(Excempted),
                               ifelse(as.numeric(ClearBalance) > Current_Balance * .5,
                                      Current_Balance * .5,as.numeric(ClearBalance))
                               ,as.numeric(ClearBalance))) #%>% 
# filter(is.na(ClearBalance))
# check last listing date
# Prev_ActiveLoanBK %>% 
#   left_join(cbk_exception,by=c('Loanac' = 'Account_Number')) %>% 
#   mutate(ClearBalance = ifelse(as.numeric(ClearBalance) > Current_Balance * .5,
#                                 Current_Balance * .5,as.numeric(ClearBalance))) #%>%
# select(ClearBalance,ClearBalance2,Excempted) %>% summarise_all(sum,na.rm=T) 

# ActiveLoanBK =ActiveLoanBK %>%  mutate(ClearBalance = ifelse(AccountID == "018020031807",0,ClearBalance))
# if (overidep08.) {
# SubDir.1 = "09.06.2022"
# month.1 = "Jun"
# }
ActiveLoanBK = ActiveLoanBK %>% dplyr::filter(AccountID != "0019040009798")
# 

# Prev_ActiveLoanBK <- rbind(
#   read.xlsx(paste0("E:/Reports/Dashboard/",month.1,"/",SubDir.1,"/M-Fanisi_Dashboard_",SubDir.1,".xlsx"),sheet = 3),
#   # dplyr::filter(!CreditOfficerName == "ONFON"),
#   read.xlsx(paste0("E:/Reports/Dashboard/",month.1,"/",SubDir.1,"/M-Fanisi_Saf_Dashboard_",SubDir.1,".xlsx"),sheet = 3)
#   # dplyr::filter(CreditOfficerName == "ONFON")
# ) 

Prev_ActiveLoanBK <- read.xlsx(paste0("E:/Reports/Dashboard/",month.1,"/",SubDir.1,"/LoanList_",SubDir.1,".xlsx")) %>% 
  mutate(Loanac = paste0(AccountID,"_",LoanSeries),
         ClearBalance = ifelse(OSPrinciple == 0,0,ClearBalance)) %>% 
  maisha_current_Portfolio("all") %>% 
  filter(!AccountID %in% c('0019040000005','0019040029628'),
         grepl("MFC|MLS",ProductID)
  )

Prev_ActiveLoanBK  %<>% dplyr::filter(!AccountID %in% exceptions) %>% 
  left_join(cbk_exception,by=c('Loanac' = 'Account_Number')) %>% 
  mutate(ClearBalance = ifelse(!is.na(Excempted),
                               ifelse(as.numeric(ClearBalance) > Current_Balance * .5,
                                      Current_Balance * .5,as.numeric(ClearBalance))
                               ,as.numeric(ClearBalance))) 
ActiveLoanBK %<>% select(-Current_Balance)
Prev_ActiveLoanBK %<>% select(-Current_Balance)
Daily_Payment <- merge(x = Prev_ActiveLoanBK,
                       y= ActiveLoanBK [,c("Loanac","ClearBalance")],by = "Loanac",all.x = T) %>%
  mutate(LastRepaymentAmount = ifelse(is.na((as.numeric(ClearBalance.y) - as.numeric(ClearBalance.x))),
                                      as.numeric(ClearBalance.x),
                                      ifelse((as.numeric(ClearBalance.y) - as.numeric(ClearBalance.x)) > 0,
                                             as.numeric(ClearBalance.y)- as.numeric(ClearBalance.x),0))) %>%
  dplyr::filter(LastRepaymentAmount >= 0)  %>%
  mutate(LastRepaymentDate =gsub("-","",Sys.Date()-PastDays)) %>%
  mutate(ClearBalance.x = NULL) 


colnames(Daily_Payment)[colnames(Daily_Payment)=="ClearBalance.y"] = "ClearBalance"

Daily_Payment = Daily_Payment %>% select(
  names(ActiveLoanBK)[names(ActiveLoanBK) %in% names(Daily_Payment)]
) %>% mutate(
  ClearBalance = ifelse(is.na(ClearBalance) ,0,ClearBalance)
)


new = merge(
  x = ActiveLoanBK ,
  y = Daily_Payment %>%  dplyr::filter(ClearBalance>0),# %>% mutate(Loanac = paste0(AccountID,"_",LoanSeries)) %>% select(Loanac),
  by = "Loanac") %>% select(Loanac)

ActiveLoanBK <- ActiveLoanBK[-match(new$Loanac,ActiveLoanBK$Loanac),]
# names(Daily) %>% View()
ActiveLoanBK <- bind_rows(ActiveLoanBK%>% mutate_all(as.character),Daily_Payment %>% mutate_all(as.character))

# ActiveLoanBK %>% dplyr::filter(ClearBalance >0&ClearBalance<1,
# LastRepaymentAmount > 0) %>% dplyr::filter(ClearBalance <= 0.0099) %>% View()
ActiveLoanBK<-ActiveLoanBK %>%
  mutate(Installment1=InstallmentDueDate,DisbDate = DisbursedOn,
         ClearBalance = ifelse(as.numeric(ClearBalance) <= 0.0099,0,as.numeric(ClearBalance)),LastRepaymentAmount = as.numeric(LastRepaymentAmount) )

ActiveLoanBK<-separate(ActiveLoanBK,Installment1,into=c("Installement1Date","Time"),sep=" ")
ActiveLoanBK<-separate(ActiveLoanBK,DisbDate,into=c("DisbDate","Time2"),sep=" ")


ActiveLoanBK<-mutate(ActiveLoanBK,
                     Excempted = as.numeric(Excempted),
                     ArrearsDays=ifelse((as.numeric((Sys.Date()-PastDays)-as.Date(Installement1Date,origin = "1899-12-31"))) <= 0 | ClearBalance <= 0,0,
                                        (as.numeric((Sys.Date()-PastDays)-as.Date(Installement1Date,origin = "1899-12-31")))),
                     ArrearsDays = ifelse(!is.na(Excempted),0,ArrearsDays),
                     ArrearsDays = ifelse(as.numeric(LoanAmount) < 1000 & ArrearsDays > 90,90,ArrearsDays ),
                     RiskClass=ifelse(ArrearsDays<=0,"Normal",ifelse(ArrearsDays<=30,"Watch",
                                                                     ifelse(ArrearsDays<=60,"SubStandard",
                                                                            ifelse(ArrearsDays<=90,"Doubtful","Loss")))))

ActiveLoanBK <- ActiveLoanBK %>% separate(IDNo,into =c("MFanisi","Ref","IDNo"),sep = "_") %>% 
  mutate(IDNo = ifelse((is.na(IDNo) & is.na(Ref)),MFanisi,
                       ifelse(is.na(IDNo),Ref,IDNo)))

ActiveLoanBK <- ActiveLoanBK %>%
  mutate(IDNo = ifelse(IDNo == "032432473","32432473",ifelse(IDNo == "224476611","9258750",ifelse(IDNo == "108438875","10843875",ifelse(IDNo == "278773559","27877359",IDNo)))))


Gender1 <- read.xlsx("E:/CRB/Static_Data/Gender/Gender.xlsx",sheet = 1)
Gender3 <- read.xlsx("E:/CRB/Static_Data/Gender/Gender3.xlsx",sheet = 1)
Gender_KYC <- read.xlsx("E:/CRB/Static_Data/Gender/Gender_KYC.xlsx",sheet = 1)
# 1. loading KYC data
KYC <- read.xlsx("E:/CRB/Static_Data/KYC_Data/KYCDATA.xlsx",sheet = 1)

# 2. loading eronously listed data
ErronousListed<-read.xlsx("E:/CRB/Static_Data/Erroneous_Listing_File/ErroneousListedIDs.xlsx",sheet = 1)

#Previous_Listing<-read.xlsx("E:/Backup/Credit and Operations/2019/M-Fanisi/CRB_Listing/Static_Data/Previous_Listing/Previous_Listing.xlsx",sheet=1)
#Previous_UpdatedCleared<-read.xlsx("E:/Backup/Credit and Operations/2019/M-Fanisi/CRB_Listing/Static_Data/Updated_Cleared_Loans/Previous_UpdatedCleared.xlsx",sheet=1)
#Previous_UpdatedCleared<-mutate(Previous_UpdatedCleared,Status="Cleared")
#ReconciledAccounts<-read.xlsx("E:/Backup/Credit and Operations/2019/M-Fanisi/CRB_Listing/Static_Data/Reconciled_Accounts/Reconciled_Accounts.xlsx",sheet = 1)


KYC<-KYC[!duplicated(KYC$NationalID),]
Gender1<-Gender1[!duplicated(Gender1$NationalID),]
Gender_KYC<-Gender_KYC[!duplicated(Gender_KYC$NationalID),]
ActiveLoanBK<-merge(ActiveLoanBK,ErronousListed,by= "IDNo",all.x = T)
ActiveLoanBK<-dplyr::filter(ActiveLoanBK,is.na(Right_Name))

ActiveLoanBK <-merge(ActiveLoanBK,KYC,by.x ="IDNo",by.y= "NationalID",all.x = T)

ActiveLoanBK <- ActiveLoanBK %>%
  select(-c(Gender))

ActiveLoanBK_Check <- ActiveLoanBK%>%
  mutate(FirstName = sapply(strsplit(AccountName, " "), `[`, 1))
Gender2 <- gender(ActiveLoanBK_Check$FirstName)[c("name","gender")]
Gender2 <- Gender2 %>%
  mutate(Gender = ifelse(gender == "male","M","F"))

Gender3 <- Gender3[!duplicated(Gender3$FirstName),]

Gender2 <- Gender2[!duplicated(Gender2$name),]

Gender2 <- Gender2 %>%
  mutate(FirstName = as.character(name))
# 
# ActiveLoanBK2 <- ActiveLoanBK2%>%
#   mutate(FirstName = as.character(FirstName))

ActiveLoanBK_Check <- merge(ActiveLoanBK_Check,Gender2[,c("FirstName","Gender")],by.x = "FirstName",by.y = "FirstName", all.x = T)


ActiveLoanBK1 <- ActiveLoanBK_Check%>%
  dplyr::filter(!is.na(Gender)) %>%
  select(IDNo,OurBranchID,BranchName,ClientID,AccountID,Loanac,ProductID,ProductName,GroupID,GroupName,AccountName,LoanSeries,LoanAmount,Interest,DisbursedOn,OSPrinciple,OSInterest,SalesOfficer,CreditOfficerName,LastInstallmentDate,ClearBalance,InterestRate,LoanInterest,RepaymentTerm,RepaymentFrequency,InstallmentAmount,InstallmentDueDate,AccountCloseDate,GenderID,SectorID,SectorName,ArrearsDays,RiskClass,ArrearsAmount,MFanisi,Ref,IDType,MobileNo,RepaymentAccountID,RepaymentAccountBalance,ActiveLoanSeries,LastRepaymentDate,LastRepaymentAmount,LastProvisionDate,GeneralProvision,SpecificProvision,CollateralID,CollateralType,SecMarketValue,ForcedSaleValue,InterestReceivable,SuspendedInterest,LoanStatusID,CBK_Class,Installement1Date,Time,DisbDate,Time2,Right_Name,Gender,Date_Birth,Postal_Address,Postal_Code,Town,Location)


ActiveLoanBK_Check1<- ActiveLoanBK_Check %>%
  dplyr::filter(is.na(Gender)) %>%
  select(-c(Gender))


ActiveLoanBK_Check1 <- merge(ActiveLoanBK_Check1,Gender1[,c("NationalID","Gender")],by.x = "IDNo",by.y = "NationalID", all.x = T)


ActiveLoanBK2 <- ActiveLoanBK_Check1 %>%
  dplyr::filter(!is.na(Gender)) %>%
  select(IDNo,OurBranchID,BranchName,ClientID,AccountID,Loanac,ProductID,ProductName,GroupID,GroupName,AccountName,LoanSeries,LoanAmount,Interest,DisbursedOn,OSPrinciple,OSInterest,SalesOfficer,CreditOfficerName,LastInstallmentDate,ClearBalance,InterestRate,LoanInterest,RepaymentTerm,RepaymentFrequency,InstallmentAmount,InstallmentDueDate,AccountCloseDate,GenderID,SectorID,SectorName,ArrearsDays,RiskClass,ArrearsAmount,MFanisi,Ref,IDType,MobileNo,RepaymentAccountID,RepaymentAccountBalance,ActiveLoanSeries,LastRepaymentDate,LastRepaymentAmount,LastProvisionDate,GeneralProvision,SpecificProvision,CollateralID,CollateralType,SecMarketValue,ForcedSaleValue,InterestReceivable,SuspendedInterest,LoanStatusID,CBK_Class,Installement1Date,Time,DisbDate,Time2,Right_Name,Gender,Date_Birth,Postal_Address,Postal_Code,Town,Location)

ActiveLoanBK_Check2 <- ActiveLoanBK_Check1 %>%
  dplyr::filter(is.na(Gender)) %>%
  select(-c(Gender))

ActiveLoanBK_Check2 <- merge(ActiveLoanBK_Check2,Gender_KYC[,c("NationalID","Gender")],by.x = "IDNo",by.y = "NationalID", all.x = T)

ActiveLoanBK3 <- ActiveLoanBK_Check2 %>%
  dplyr::filter(!is.na(Gender)) %>%
  select(IDNo,OurBranchID,BranchName,ClientID,AccountID,Loanac,ProductID,ProductName,GroupID,GroupName,AccountName,LoanSeries,LoanAmount,Interest,DisbursedOn,OSPrinciple,OSInterest,SalesOfficer,CreditOfficerName,LastInstallmentDate,ClearBalance,InterestRate,LoanInterest,RepaymentTerm,RepaymentFrequency,InstallmentAmount,InstallmentDueDate,AccountCloseDate,GenderID,SectorID,SectorName,ArrearsDays,RiskClass,ArrearsAmount,MFanisi,Ref,IDType,MobileNo,RepaymentAccountID,RepaymentAccountBalance,ActiveLoanSeries,LastRepaymentDate,LastRepaymentAmount,LastProvisionDate,GeneralProvision,SpecificProvision,CollateralID,CollateralType,SecMarketValue,ForcedSaleValue,InterestReceivable,SuspendedInterest,LoanStatusID,CBK_Class,Installement1Date,Time,DisbDate,Time2,Right_Name,Gender,Date_Birth,Postal_Address,Postal_Code,Town,Location)

ActiveLoanBK_Check3 <- ActiveLoanBK_Check2 %>%
  dplyr::filter(is.na(Gender)) %>%
  select(-c(Gender))

ActiveLoanBK_Check3 <- merge(ActiveLoanBK_Check3,Gender3[,c("FirstName","Gender")],by.x = "FirstName",by.y = "FirstName", all.x = T)

ActiveLoanBK4 <- ActiveLoanBK_Check3 %>%
  mutate(Gender = ifelse(is.na(Gender),"M",Gender)) %>%
  select(IDNo,OurBranchID,BranchName,ClientID,AccountID,Loanac,ProductID,ProductName,GroupID,GroupName,AccountName,LoanSeries,LoanAmount,Interest,DisbursedOn,OSPrinciple,OSInterest,SalesOfficer,CreditOfficerName,LastInstallmentDate,ClearBalance,InterestRate,LoanInterest,RepaymentTerm,RepaymentFrequency,InstallmentAmount,InstallmentDueDate,AccountCloseDate,GenderID,SectorID,SectorName,ArrearsDays,RiskClass,ArrearsAmount,MFanisi,Ref,IDType,MobileNo,RepaymentAccountID,RepaymentAccountBalance,ActiveLoanSeries,LastRepaymentDate,LastRepaymentAmount,LastProvisionDate,GeneralProvision,SpecificProvision,CollateralID,CollateralType,SecMarketValue,ForcedSaleValue,InterestReceivable,SuspendedInterest,LoanStatusID,CBK_Class,Installement1Date,Time,DisbDate,Time2,Right_Name,Gender,Date_Birth,Postal_Address,Postal_Code,Town,Location)

ActiveLoanBK <- rbind(ActiveLoanBK1,ActiveLoanBK2,ActiveLoanBK3,ActiveLoanBK4)


ActiveLoanBK[ActiveLoanBK=="NULL"]<-""
ActiveLoanBK<-separate(ActiveLoanBK,DisbursedOn,into="DisbursedOn",sep = " ")
ActiveLoanBK<-separate(ActiveLoanBK,LastInstallmentDate,into="LastInstallmentDate",sep = " ")
ActiveLoanBK<-separate(ActiveLoanBK,InstallmentDueDate,into="InstallmentDueDate",sep = " ")
ActiveLoanBK<-separate(ActiveLoanBK,LastRepaymentDate,into="LastRepaymentDate",sep = " ")
ActiveLoanBK<-mutate(ActiveLoanBK,
                     DisbursedOn=gsub("-","",DisbursedOn),
                     LastInstallmentDate=gsub("-","",LastInstallmentDate),
                     InstallmentDueDate=gsub("-","",InstallmentDueDate),
                     LastRepaymentDate=gsub("-","",LastRepaymentDate),
                     LoanAmount=round(100*as.numeric(LoanAmount),0),
                     ClearBalance=round(100*ClearBalance,0),
                     InstallmentAmount=round(100*as.numeric(InstallmentAmount),0),
                     ArrearsAmount=round(100*as.numeric(ArrearsAmount),0),
                     LastRepaymentAmount=round(100*as.numeric(LastRepaymentAmount),0))

#as.Date(strptime('20180617', "%Y%m%d")) + 1




PaymentFrequency<-function(F){
  
  ifelse(F=="Weekly","W",ifelse(F=="Monthly","M","O"))
}

ActiveLoanBK<-mutate(ActiveLoanBK,RepaymentFrequency=PaymentFrequency(RepaymentFrequency))

PerformingIndicator<-function(P){
  
  ifelse(P=="Normal","A",ifelse(P=="Watch","A","B")) 
}
ActiveLoanBK<-mutate(ActiveLoanBK,Performing_NPL_Indicator=PerformingIndicator(RiskClass))


InstallmentArrears<-function(I){
  ifelse(I>0,1,0)
}
ActiveLoanBK<-mutate(ActiveLoanBK,Installement_Arrears=InstallmentArrears(ArrearsDays))

ActiveLoanBK<-mutate(ActiveLoanBK,PDate=as.Date(Date_Birth,format="%Y%m%d"))

DOBFill<-function(D,B){
  ifelse(is.na(D)|D == "","19990101",ifelse(B>="2000-01-01","19990101",ifelse(B<="1938-01-01","19990101",D)))
}
Locate<-function(L){
  ifelse(is.na(L),"N/A",L)
}
Address<-function(A){
  ifelse(is.na(A),0,A)
}

ActiveLoanBK<-mutate(ActiveLoanBK,Date_Birth=DOBFill(Date_Birth,PDate),Location=Locate(Location),Town=Locate(Town),Postal_Address=Address(Postal_Address),Postal_Code=Address(Postal_Code))

ActiveLoanBK<-ActiveLoanBK[,c("AccountName","IDNo","MobileNo","ClientID","RepaymentAccountID","AccountID","Loanac","LoanSeries","LoanAmount","ClearBalance","ArrearsAmount","DisbursedOn","LastRepaymentDate","RepaymentFrequency","InstallmentDueDate","LastRepaymentAmount","ArrearsDays","Installement_Arrears","RiskClass","CreditOfficerName","Performing_NPL_Indicator","Gender","Date_Birth","Location","Postal_Address","Postal_Code","Town")]



ActiveLoanBK <- ActiveLoanBK %>%
  mutate(Prudential_Risk_Classfication = ifelse(ArrearsDays <= 30,"A",ifelse(ArrearsDays <= 90,"B",ifelse(ArrearsDays <= 180, "C",ifelse(ArrearsDays <= 360,"D","E")))),
         Overdue_Balance = ifelse(ArrearsDays <= 0 ,0,ClearBalance),
         Account_Status = ifelse(ClearBalance <= 0|is.na(ClearBalance)|ClearBalance =="","A","F"),
         Overdue_Date = ifelse(ArrearsDays <= 0, "",gsub("-","",(as.Date(strptime(InstallmentDueDate, "%Y%m%d")) + 1))))

print("Creating Reports")

# ====
CRBListing <- ActiveLoanBK #%>% dplyr::filter(!AccountID %in% exceptions)

# ==========================================

CRBListing<-separate(CRBListing,AccountName,into=c("Forename 1","Surname","Forename 2","Forename 3"),sep = " ")
CRBListing<-mutate(CRBListing,Salutation="",Nationality="KE",Marital_Status="U",Primary_Identification_Document_Type="001",Secondary_Identification_Document_Type="",
                   Secondary_Identification_Document_Number="",Other_Identification_Document_Type="",Other_Identification_Document_Number="",Home_Telephone_Number="",Work_Telephone_Number="",
                   Postal_Address_1=Postal_Address,Postal_Address_2="",Postal_Location_Town=Town,Postal_Location_Country="KE",Post_Code=Postal_Code,Physical_Address1=Location,Physical_Address2="",Plot_Number="",Location_Town=Location,Location_Country="KE",
                   Date_at_Physical_Address="",PIN_Number="",Consumer_work_EMail="",Employer_Name="",Employer_Industry_Type="012",Employment_Date="",Employment_type="",Salary_Band="",Lenders_Registered_Name="MAISHA MICROFINANCE LIMITED",Lenders_Trading_Name="MAISHA MICROFINANCE BANK",Lenders_Branch_Name="CHESTER HOUSE",Lenders_Branch_Code="N343001",Account_Joint_Single_Indicator="S",Account_Product_Type="I",
                   Currency_of_Facility="KES",Performing_NPL_Indicator=Performing_NPL_Indicator,Account_Status= Account_Status,Account_Status_Date= gsub("-","",(Sys.Date()-PastDays)),Account_Closure_Reason="",Repayment_Period=1,Deferred_Payment_Date="",Deferred_Payment_Amount="",Payment_Frequency=RepaymentFrequency,Disbursement_Date=DisbursedOn,Instalment_Amount=LoanAmount,
                   Date_of_Latest_Payment=LastRepaymentDate,
                   Last_Payment_Amount=LastRepaymentAmount,
                   
                   Type_of_Security="U",Date_Of_Birth=Date_Birth,
                   Client_Number=ClientID,Account_Number=Loanac,Gender=Gender,Mobile_Telephone_Number=MobileNo,Nr_of_Days_In_Arrears=ArrearsDays,Nr_of_Instalments_In_Arrears=Installement_Arrears,Instalment_Due_Date=InstallmentDueDate,Original_Amount=LoanAmount,Amount_in_Kenya_shillings=LoanAmount,Current_Balance=ClearBalance,Overdue_Balance = Overdue_Balance,Overdue_Date = Overdue_Date,Primary_Identification_Doc_Number=IDNo,Date_Account_Opened=DisbursedOn,Instalment_Due_Date=InstallmentDueDate,
                   Original_Amount=LoanAmount,Amount_in_Kenya_shillings=ClearBalance,Current_Balance=ClearBalance,Nr_of_Days_In_Arrears=ArrearsDays,Trading_As = "",
                   Prudential_Risk_Classfication = Prudential_Risk_Classfication)

# CRBListing1<-CRBListing[,c("Surname","Forename 1","Forename 2","Forename 3","Trading_As","Salutation","Date_Of_Birth","Client_Number","Account_Number","Gender","Nationality","Marital_Status","Primary_Identification_Document_Type","Primary_Identification_Doc_Number","Secondary_Identification_Document_Type","Secondary_Identification_Document_Number","Other_Identification_Document_Type","Other_Identification_Document_Number","Mobile_Telephone_Number","Home_Telephone_Number","Work_Telephone_Number","Postal_Address_1","Postal_Address_2","Postal_Location_Town","Postal_Location_Country","Post_Code","Physical_Address1","Physical_Address2","Plot_Number","Location_Town","Location_Country","Date_at_Physical_Address","PIN_Number","Consumer_work_EMail","Employer_Name","Employer_Industry_Type","Employment_Date","Employment_type","Salary_Band","Lenders_Registered_Name","Lenders_Trading_Name","Lenders_Branch_Name","Lenders_Branch_Code","Account_Joint_Single_Indicator","Account_Product_Type","Date_Account_Opened","Instalment_Due_Date","Original_Amount","Currency_of_Facility","Amount_in_Kenya_shillings","Current_Balance","Overdue_Balance","Overdue_Date","Nr_of_Days_In_Arrears","Nr_of_Instalments_In_Arrears","Performing_NPL_Indicator","Account_Status","Account_Status_Date","Account_Closure_Reason","Repayment_Period","Deferred_Payment_Date","Deferred_Payment_Amount","Payment_Frequency","Disbursement_Date","Instalment_Amount","Date_of_Latest_Payment","Last_Payment_Amount","Type_of_Security")]

CRBListing<-CRBListing[,c("Surname","Forename 1","Forename 2","Forename 3","Trading_As","Date_Of_Birth","Client_Number","Account_Number","Gender","Nationality","Primary_Identification_Document_Type","Primary_Identification_Doc_Number","Mobile_Telephone_Number","Account_Product_Type","Instalment_Due_Date","Original_Amount","Currency_of_Facility","Amount_in_Kenya_shillings","Current_Balance","Overdue_Balance","Overdue_Date","Nr_of_Days_In_Arrears","Nr_of_Instalments_In_Arrears",
                          "Prudential_Risk_Classfication","Account_Status","Account_Status_Date","Repayment_Period","Disbursement_Date","Instalment_Amount","Date_of_Latest_Payment","Last_Payment_Amount")]
#CRBListing <- CRBListing %>%
#mutate(Physical_Address1 = ifelse(Physical_Address1 %in% c("N/A"," ","  ","  ","    ","      "),"Nairobi",Physical_Address1))

# CRBListing %>% dplyr::filter(Current_Balance <= 0) %>% View()


# attach(CRBListing)
# SAL<-function(K){
#   ifelse(K=="F","MRS.",ifelse(K=="M","MR.",NA))
# }
# CRBListing<-mutate(CRBListing,Salutation=SAL(Gender))

OverDBal<-function(D,B){
  ifelse(D<=0,0,B)
}
OverDate<-function(X,Y){
  ifelse(X<=0,"",Y)
}

LastPDate<-function(L,P){
  ifelse(L==""|is.na(L),P,L)
}

LastPAmount<-function(A){
  ifelse(is.na(A),100,A)
}

GenderFill<-function(G){
  ifelse(is.na(G),"U",G)
}
SurnameFill<-function(S,N){
  ifelse(S==""|is.na(S),N,S)
}
DateofBirth<-function(B){
  ifelse(is.na(B),"19990101",B)
}

CRBListing<-mutate(CRBListing,Date_Of_Birth=DateofBirth(Date_Of_Birth),Overdue_Balance=OverDBal(Nr_of_Days_In_Arrears,Current_Balance),
                   Overdue_Date=OverDate(Nr_of_Days_In_Arrears,Instalment_Due_Date),Date_of_Latest_Payment=LastPDate(Date_of_Latest_Payment,Disbursement_Date),Last_Payment_Amount=LastPAmount(Last_Payment_Amount),Gender=GenderFill(Gender),Surname=SurnameFill(Surname,`Forename 1`),
                   Primary_Identification_Document_Type = if_else(substr(Primary_Identification_Doc_Number,1,1) %in% LETTERS | substr(Primary_Identification_Doc_Number,1,1) %in% letters,"002","001")       ) 



#CRB_Jul<-mutate(CRBListingF,Overdue_Balance=OverDBal(Nr_of_Days_In_Arrears,Current_Balance),Overdue_Date=OverDate(Nr_of_Days_In_Arrears,Instalment_Due_Date))
#CRBListing <- rbind(CRBListing,ClearedLoans) 
CRBListing <- CRBListing %>%
  mutate(Last_Payment_Amount = ifelse(Last_Payment_Amount <= 0,1,Last_Payment_Amount))

#CRBListing <- CRBListing %>%
#mutate(Physical_Address1 = ifelse(Physical_Address1 %in% c("N/A"," ","  ","  ","   ","    ","      "),"Nairobi",Physical_Address1))



# ====
# CRBListing <- CRBListing %>%
#   mutate(Date_Account_Opened = as.Date(as.numeric(Date_Account_Opened)-1,origin = "1899-12-31"),
#          Instalment_Due_Date = as.Date(as.numeric(Instalment_Due_Date)-1,origin = "1899-12-31"),
#          Overdue_Date = as.Date(as.numeric(Overdue_Date)-1,origin = "1899-12-31"),
#          Account_Status_Date = as.Date(as.numeric(Account_Status_Date)-1,origin = "1899-12-31"),
#          Disbursement_Date = as.Date(as.numeric(Disbursement_Date)-1,origin = "1899-12-31"),
#          Date_of_Latest_Payment = as.Date(as.numeric(Date_of_Latest_Payment)-1,origin = "1899-12-31"))
# 
# CRBListing <- CRBListing %>%
#   mutate(Date_Account_Opened = gsub("-","",Date_Account_Opened ),
#          Instalment_Due_Date = gsub("-","",Instalment_Due_Date ),
#          Overdue_Date = gsub("-","",Overdue_Date ),
#          Account_Status_Date = gsub("-","",Account_Status_Date),
#          Disbursement_Date = gsub("-","",Disbursement_Date ),
#          Date_of_Latest_Payment = gsub("-","",Date_of_Latest_Payment))
# 
# CRBListing <- CRBListing %>%
#   mutate_all(as.character)

#Daily_CRB_Update<-rbind(ClearedNewLoan,ClearedLoans)
print("Final Cleaning")
# ====

CRBListing = CRBListing %>%mutate(
  Surname = ifelse(Surname == "" | nchar(Surname) < 3 & `Forename 1` != "",`Forename 1`,Surname),
  `Forename 1` = ifelse(`Forename 1` == "" | nchar(`Forename 1`) < 3 & `Forename 2` != "",`Forename 2`,`Forename 1`)
  
) 


#%>% select(Surname,`Forename 1`,`Forename 2`,`Forename 3`) %>% View()
# CRBListing[CRBListing$Primary_Identification_Doc_Number == "5655487",][1] = "Mango"
# CRBListing[CRBListing$Primary_Identification_Doc_Number == "5655487",][2] = "Matete"
# CRBListing[CRBListing$Primary_Identification_Doc_Number == "5655487",][3] = "Salim"

# CRBListing[CRBListing$Primary_Identification_Doc_Number == "1287550",][2] = "Robert"
# CRBListing[CRBListing$Primary_Identification_Doc_Number == "1287550",][3] = "Munoko"

CRBListing = CRBListing %>% mutate(
  Primary_Identification_Doc_Number = 
    ifelse( Primary_Identification_Document_Type  == "001", stringr::str_sub(Primary_Identification_Doc_Number,-8),
            Primary_Identification_Doc_Number)
) 
CRBListing[substr(CRBListing$Account_Number,1,13) == "0018020010703",][2] = "John"

# CRBListing <- CRBListing %>% filter(Account_Number != '0019040000005_20')
print("Writing Final Report Output")


# cleaning for credit info report
library(tidyverse)
options(scipen = 999)
CRBListing = CRBListing %>% 
  mutate(Instalment_Due_Date = ifelse(Current_Balance == 0,format(Sys.Date()-PastDays,"%Y%m%d"),Instalment_Due_Date)) %>% 
  # dplyr::filter(Nr_of_Days_In_Arrears <= 90) %>%
  distinct(Account_Number,.keep_all = T) %>% 
  mutate_all(.funs = function(x){format(x,scientific = FALSE);ifelse(x == "NA"," ",x)})
library(mbanalytics)
# updatedb
library(magrittr)




# write normal excel file
write.xlsx(CRBListing,paste0("E:/CRB/Daily_Update/",month,"/",SubDir,"/MobileFacilitiesfile_",SubDir,".xlsx"))
# write_csv(mutate_all(CRBListing,.funs = function(x)ifelse(is.na(x),"",x)),paste0("E:/CRB/Daily_Update/",month,"/",SubDir,"/MobileFacilitiesfile_",SubDir,".csv"))
# file = read_lines(paste0("E:/CRB/Daily_Update/",month,"/",SubDir,"/MobileFacilitiesfile_",SubDir,".csv"))
# file2 = gsub(',',"|",file)
# write_delim(x = mutate_all(CRBListing,.funs = function(x)ifelse(is.na(x),"",x)),delim = "|",
#             paste0("E:/CRB/Daily_Update/",month,"/",SubDir,"/CRBBDMF",anytime::yyyymmdd(dmy(SubDir)),"001.N343"))

# write.xlsx(CRBListing,paste0("E:/CRB/Daily_Update/",month,"/",SubDir,"/MobileFacilitiesfile_creditInfo",SubDir,".xlsx"))
# write.table(x = CRBListing,paste0("E:/CRB/Daily_Update/",month,"/",SubDir,"/MobileFacilitiesfile_",SubDir,".N343"),sep = "|",col.names = F,quote = F,row.names = F)
data.table::fwrite(CRBListing,file = paste0("E:/CRB/Daily_Update/",month,"/",SubDir,"/CRBBDMF",anytime::yyyymmdd(dmy(SubDir)),"001.N343"),scipen = 999,quote = F,sep = "|", row.names = F,col.names = F,na = "")
data.table::fwrite(CRBListing %>% filter(as.numeric(Original_Amount) >= 1000),file = paste0("E:/CRB/Daily_Update/",month,"/",SubDir,"/CRBBDMF",anytime::yyyymmdd(dmy(SubDir)),"002.N343"),scipen = 999,quote = F,sep = "|", row.names = F,col.names = F,na = "")
shell.exec(paste0("E:/CRB/Daily_Update/",month,"/",SubDir,"/CRBBDMF",anytime::yyyymmdd(dmy(SubDir)),"001.N343"))
# shell.exec(paste0("E:/CRB/Daily_Update","/",SubDir))


# end  highlight ----------------------------------------------------------




# metro = crb_helper$MetropolCRBUpload(user='lawrence.kipkemoi@maishabank.com',pwd='Muhura.123$',
#                                      browser=browser,crb_file=file_path,is_final="YES")
# trans = crb_helper$TransunionCRBUpload(user='6n8Hz%Sj',pwd='Chiromo*2018',browser=browser,
#                                        crb_file=file_path,test=F)
# credit_info = crb_helper$CreditInfoCRBUpload("CI.KE068","km35c0DH",browser=browser,
#                                   crb_file=file_path)