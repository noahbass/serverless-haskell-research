-- Data Transform Example
-- A Use Case:
--     The ACM chapter at UC needs to find members that will be graduating in 2018,
--     then return a compressed csv of their names and email addresses
--
-- How we'll do it:
--     Take an export from a database (or other data store),
--     then filter by graduation date (keeping only those graduating in 2018),
--     then map the id to a full email address (ex: bassnh -> bassnh@mail.uc.edu)
-- 
-- This example demonstrates:
--     - Monads
--     - map and filter
--     - Thinking about saftey in transforming the data
--     - Returning transformed data in a compressed format

import Control.Monad
import Data.List.Split
import Data.Maybe

-- TODO: return data in an compressed format for piping out
-- TODO: optimize

main :: IO ()
main = do
    Prelude.putStrLn (show compute)


compute :: Maybe [Member]
compute = do
    let (csvHeader:csvBody) = csvToList (getCsvData "MyFakeConnectionConfig") -- getCsvData pretends to retrieve a csv data from a database
    members <- linesToMembers csvBody
    filteredMembers <- filteredMembers members 2018
    membersWithEmails <- expandEmailAddress filteredMembers
    return membersWithEmails

-- A simple data structure to signify a 'member'
-- Data for each member is wrapped in a Maybe monad
data Member = Member {
    timestamp :: Maybe String,
    name :: Maybe String,
    graduationYear :: Maybe Int,
    sixPlusTwo :: Maybe String,
    emailAddress :: Maybe String
} deriving (Show)

-- csvToList converts a raw csv data dump to a list of strings by breaking on each `\n`
csvToList :: String -> [String]
csvToList rawCsvData = Prelude.lines rawCsvData

-- mapEmailAddress maps over each member and appends `@mail.uc.edu` to their email address if it exists
expandEmailAddress :: [Member] -> Maybe [Member]
expandEmailAddress members = do
    let membersWithEmails = Prelude.map
                  (\member -> member { emailAddress=Just ((Data.Maybe.fromMaybe "" (sixPlusTwo member)) ++ "@mail.uc.edu") })
                  members
    return membersWithEmails

-- filterMembers filters a list and returns only the members that are class of 'year'
filteredMembers :: [Member] -> Int -> Maybe [Member]
filteredMembers members year = do
    let filtered = Prelude.filter (\member -> (graduationYear member) == (Just year)) members
    return filtered

-- linesToMembers converts a list of strings to a list of members
linesToMembers :: [String] -> Maybe [Member]
linesToMembers lines = do
    let members = Prelude.map (\line -> (lineToMember line)) lines
    return members

-- lineToMember converts a single line to a single member
lineToMember :: String -> Member
lineToMember line = Member {
        timestamp = (Just ts),
        name = (Just nm),
        graduationYear = (Just (read gy :: Int)),
        sixPlusTwo = (Just st),
        emailAddress = Nothing
    }
    where (ts:nm:gy:spt) = Data.List.Split.splitOn "," line
          st = Prelude.head spt


-- A function to retrieve data to be transformed
-- This is used  to simulate retrieving data from a production database or other data store with some connection config
getCsvData :: String -> String
getCsvData connection = "Timestamp,Name,Graduation Year,SixPlusTwo\n\
\8/31/2017 17:57:08,Korey Huskonen,2021,huskonkw\n\
\8/31/2017 17:57:54,Alex Vennemeyer,2022,vennemar\n\
\8/31/2017 17:58:15,Kristian Snyder,2020,snyderks\n\
\8/31/2017 18:01:43,Kyle Arens,2018,arenskp\n"



-- f1 :: (String a, Num b) => a -> b
-- f1 x = if (x == "hello") then Nothing else (Just 10)
-- f2 x = if (x == 'a') then Nothing else (Just 9)

-- add :: Num b => [Char] -> Char -> Maybe b
-- add x y = do
--     x' <- f1 x
--     y' <- f2 y
--     return (x' + y')

-- v1 = filterM (\x -> Just (x > 0)) [2, 0, 1, -1]

-- coolMembers = [Member {timestamp=(Just "now"), name=(Just "Kyle"), graduationYear=(Just 2018), sixPlusTwo=(Just "kyle")}, Member {timestamp=(Just "now"), name=(Just "Korey"), graduationYear=(Just 2021), sixPlusTwo=(Just "korey")}]
-- filtered = Control.Monad.filterM (\member -> Just ((graduationYear member) == Just 2018)) coolMembers

-- v3 = fmap (*2) (Just 200)
-- v4 = fmap (*2) Nothing
