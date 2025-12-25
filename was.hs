import Data.Ratio

main :: IO()
validateAdmissionDate :: Int -> Bool
validateicuDate :: Int -> Int -> Bool
typeOfSurgery :: Int -> String
typeOfInsurance :: Int -> String
baseSurgeryBill :: Int -> Int
discount :: Int -> Int
hospitalBill :: Int -> Int -> Int -> Int -> Int -> Int

validateAdmissionDate x | y == 0 = True
                        | otherwise = False
                        where y = x `mod` 2

validateicuDate admissionDays icuDays | y >= 0 = True
                                      | otherwise = False
                                      where y = admissionDays - icuDays

typeOfSurgery x | x == 0 = "No Surgery"
                | x == 1 = "Minor Surgery"
                | x == 2 = "Major Surgery"
                | otherwise = "Invalid"

baseSurgeryBill x | z == "No Surgery" = 0
                  | z == "Minor Surgery" = 15000
                  | z == "Major Surgery" = 40000
                  | otherwise = -1
                  where z = typeOfSurgery x

typeOfInsurance x | x == 0 = "No Insurance"
                  | x == 1 = "Insurance avaiable"
                  | otherwise = "Invalid"

discount x | y < 20000 = y
        --    | otherwise = 20000
           where y = (x * 30) `div` 100

hospitalBill pendingDues admissionDays icuDays surgeryType insurance 
    | typeOfInsurance insurance == "Insurance avaiable" && pendingDues <= 0 = bill - (discount bill)
    | otherwise = bill + pendingDues
    where 
        admissionBill = (admissionBill - icuDays) * 1500
        icuBill = icuDays * 4000
        medicalService = if icuDays + admissionDays > 3
                            then 1000
                        else 2500
        surgeryBill = baseSurgeryBill surgeryType
        penality = if pendingDues > 0
                        then 300
                   else 0

        bill = admissionBill + icuBill + medicalService + surgeryBill + penality

main = do   
    putStrLn "Hiii This is THE HOSPITAL\nEnter the details required"
    
--  Write a Haskell program to compute a patient’s hospital bill.
-- The program should
-- accept  as  input  the  number  of  days  the  patient  is  admitted,  the  number  of  icu 
-- days, the type of surgery performed(use pattern matching : 0 for no surgery, 1 for 
-- minor
-- surgery,  and  2  for  major  surgery),  the  insurance  type  (0  for  no  insurance 
-- and  1  for
-- insurance),  and  any  pending  dues  from  a  previous  visit.  The  hospital 
-- charges  a  room
-- rent  of  Rs.  1,500  per  day  and  an  icu  charge  of  Rs.  4,000  per 
-- day. Surgery charges are
-- Rs
-- . 0 for no surgery, Rs. 15,000 for minor surgery, and 
-- Rs.  40,000 for  major  surgery.
-- A  medical  service  charge  of  Rs. 1,000  is  added  if 
-- the  stay  is  up  to  three  days,  and  Rs.2,500  is  added  if  the  stay  exceeds  three 
-- days.  If  the  patient  has  insurance  and  no
-- pe
-- nding  dues,  a  discount  of  30%  is 
-- applied  on  the  gross  bill  amount,  subject  to  a  maximum  discount  of  Rs. 
-- 20,000(use guards). Patients with pending dues are not eligible for the discount, 
-- and  if  the  pending  dues  are  greater  than  zero,  a  penalty  of  Rs.300  is 
-- added  to 
-- the bill. The program should finally display the total hospital bill amount