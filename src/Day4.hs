module Day4
    (  day4
      ,day4b
    ) where

import Aoc (countTrue)
import Data.List (isSuffixOf)
import Data.Char (isNumber,isHexDigit)
import Data.List.Split (splitOn)

data Passport = Passport {ecl::String, pid::String, eyr::String, hcl::String, byr::String, cid::Int, hgt::String} 
  deriving (Eq,Ord,Show)

day4 :: String -> Int
day4 input = countTrue . map validPassport $ parseInput input

day4b :: String -> Int
day4b input = countTrue . map validPassportb $ parseInput input

validPassport :: [(String,String)] -> Bool
validPassport fields 
  | length fields==8 = True 
  | length fields==7 = not $ any (\(key,_)->key=="cid") fields
  | otherwise = False

validHairColor :: String -> Bool
validHairColor hcl@(h:hs) = length hcl == 7 && all isHexDigit hs

validEyeColor :: String -> Bool
validEyeColor col = col `elem` ["amb", "blu","brn","gry","grn","hzl","oth"]

validPid :: String -> Bool
validPid hcl@(h:hs) = length hcl == 9 && all isNumber hs

validHeight :: String -> Bool
validHeight hgt  
  | "cm" `isSuffixOf` hgt = num >= 150 && num <= 193
  | "in" `isSuffixOf` hgt = num >= 59 && num <= 76 
  | otherwise = False
 where num = (read $ reverse $ drop 2 $ reverse hgt)::Int

validByr :: String -> Bool
validByr byr = value >=1920 && value <=2002
  where value = read byr

validIyr :: String -> Bool
validIyr iyr = value >=2010 && value <=2020
  where value = read iyr

validEyr :: String -> Bool
validEyr iyr = value >=2020 && value <=2030
  where value = read iyr

validPassportb :: [(String,String)] -> Bool
validPassportb fields = 
      any (\(key,value)-> key=="byr" && validByr value) fields 
   && any (\(key,value)-> key=="iyr" && validIyr value) fields 
   && any (\(key,value)-> key=="eyr" && validEyr value) fields 
   && any (\(key,value)-> key=="hgt" && validHeight value) fields 
   && any (\(key,value)-> key=="hcl" && validHairColor value) fields 
   && any (\(key,value)-> key=="ecl" && validEyeColor value) fields 
   && any (\(key,value)-> key=="pid" && validPid value) fields 

toPassport = map ((\[x,y]->(x,y)).splitOn ":") 

parseInput input = map (toPassport .concatMap words . lines) $ splitOn "\n\n" input

_input = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in\n"
