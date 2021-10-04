module Data.Lens.Barlow.GenericSimpleSpec where

-- import Data.Eq (class Eq)
-- import Data.Generic.Rep (class Generic)
-- import Data.Lens (over, preview, set)
-- import Data.Lens.Barlow (barlow, key)
-- import Data.Maybe (Maybe(..))
-- import Data.Show (class Show)
-- import Data.Show.Generic (genericShow)
-- import Data.String (toLower, toUpper)
-- import Data.Unit (unit)
-- import Prelude (Unit, discard, (+))
-- import Test.Spec (Spec, describe, it)
-- import Test.Spec.Assertions (shouldEqual)

-- data TrafficLight = Red | Green | Yellow 
-- derive instance genericTL :: Generic TrafficLight _ 
-- instance showTL :: Show TrafficLight where 
--   show = genericShow
-- derive instance eqTL :: Eq TrafficLight 

-- data ADT1 = A1 
-- data ADT2 = A2 String 
-- data ADT3 = A3 TrafficLight
-- data ADT11 = A11 { name :: String }
-- data ADT4 = A4 String Int 
-- data ADT9 = A9 String Int Boolean 
-- data ADT5 = A5 | B5
-- data ADT6 = A6 String | B6 Int 
-- data ADT7 = A7 | B7 TrafficLight
-- data ADT8 = A8 String Int | B8 Int | C8 
-- data ADT10 = A10 Int | B10 TrafficLight String Int | C10 String String String 

-- derive instance generic1 :: Generic ADT1 _
-- derive instance generic2 :: Generic ADT2 _
-- derive instance generic3 :: Generic ADT3 _
-- derive instance generic4 :: Generic ADT4 _
-- derive instance generic9 :: Generic ADT9 _
-- derive instance generic5 :: Generic ADT5 _
-- derive instance generic6 :: Generic ADT6 _
-- derive instance generic7 :: Generic ADT7 _
-- derive instance generic8 :: Generic ADT8 _
-- derive instance generic10 :: Generic ADT10 _
-- derive instance generic11 :: Generic ADT11 _

-- derive instance eqGeneric1 :: Eq ADT1
-- derive instance eqGeneric2 :: Eq ADT2
-- derive instance eqGeneric3 :: Eq ADT3
-- derive instance eqGeneric4 :: Eq ADT4
-- derive instance eqGeneric9 :: Eq ADT9
-- derive instance eqGeneric5 :: Eq ADT5
-- derive instance eqGeneric6 :: Eq ADT6
-- derive instance eqGeneric7 :: Eq ADT7
-- derive instance eqGeneric8 :: Eq ADT8
-- derive instance eqGeneric10 :: Eq ADT10
-- derive instance eqGeneric11 :: Eq ADT11

-- instance show1 :: Show ADT1 where 
--   show = genericShow 
-- instance show2 :: Show ADT2 where 
--   show = genericShow 

-- instance show3 :: Show ADT3 where 
--   show = genericShow 

-- instance show4 :: Show ADT4 where 
--   show = genericShow 

-- instance show5 :: Show ADT5 where 
--   show = genericShow 

-- instance show6 :: Show ADT6 where 
--   show = genericShow 

-- instance show7 :: Show ADT7 where 
--   show = genericShow 

-- instance show8 :: Show ADT8 where 
--   show = genericShow 

-- instance show9 :: Show ADT9 where 
--   show = genericShow 

-- instance show10 :: Show ADT10 where 
--   show = genericShow 

-- instance show11 :: Show ADT11 where 
--   show = genericShow 

-- spec :: Spec Unit
-- spec =
--   describe "Data.Lens.Barlow" do
--     describe "barlow - generic" do
--       it "should view into a single constructor with no arguments" do
--         let
--           sky =
--             { zodiac: A1
--             }

--           actual = preview (barlow (key :: _ "zodiac.%A1")) sky
--         actual `shouldEqual` (Just unit)
--       it "should view into a single constructor with one final arguments" do
--         let
--           sky =
--             { zodiac: A2 "World"
--             }

--           actual = preview (barlow (key :: _ "zodiac.%A2")) sky
--           actual2 = over (barlow (key :: _ "zodiac.%A2")) toUpper sky
--         actual `shouldEqual` (Just "World")
--         actual2 `shouldEqual` { zodiac: A2 "WORLD"}
--       it "should view into a single constructor with one adt argument" do
--         let
--           sky =
--             { zodiac: A3 Red
--             }

--           actual = preview (barlow (key :: _ "zodiac.%A3")) sky
--           actual2 = set (barlow (key :: _ "zodiac.%A3")) Yellow sky 
--         actual `shouldEqual` (Just Red) 
--         actual2 `shouldEqual` { zodiac: A3 Yellow }
--       it "should view into a single constructor with one non-adt argument" do
--         let
--           sky =
--             { zodiac: A11 { name : "John Doe" } 
--             }

--           actual = preview (barlow (key :: _ "zodiac.%A11.name")) sky
--           actual2 = over (barlow (key :: _ "zodiac.%A11.name")) toLower sky
--         actual `shouldEqual` (Just "John Doe")
--         actual2 `shouldEqual` { zodiac: A11 { name : "john doe"}}
--       it "should view into a single constructor with a tuple product argument (left && right)" do
--         let
--           sky =
--             { zodiac: A4 "Caramba" 10
--             }

--           actualLeft = preview (barlow (key :: _ "zodiac.%A4.%1")) sky
--           actualRight = preview (barlow (key :: _ "zodiac.%A4.%2")) sky
--           actualLeft2 = over (barlow (key :: _ "zodiac.%A4.%1")) toUpper sky
--           actualRight2 = over (barlow (key :: _ "zodiac.%A4.%2")) (_ + 1) sky
--         actualLeft `shouldEqual` (Just "Caramba")
--         actualRight `shouldEqual` (Just 10)
--         actualLeft2 `shouldEqual` { zodiac: A4 "CARAMBA" 10}
--         actualRight2 `shouldEqual` { zodiac: A4 "Caramba" 11}

--       it "should view into a sum type with no argument constructors (left)" do
--         let
--           sky =
--             { zodiac: A5
--             }

--           actual = preview (barlow (key :: _ "zodiac.%A5")) sky
--         actual `shouldEqual` (Just unit)

--       it "should view into a sum type with no argument constructors (right)" do
--         let
--           sky =
--             { zodiac: B5
--             }

--           actual = preview (barlow (key :: _ "zodiac.%B5")) sky
--         actual `shouldEqual` (Just unit)

--       it "should view into a sum type with one argument constructors (left)" do
--         let
--           sky =
--             { zodiac: A6 "Football"
--             }

--           actual = preview (barlow (key :: _ "zodiac.%A6")) sky
--           actual2 = over (barlow (key :: _ "zodiac.%A6")) toUpper sky
--         actual `shouldEqual` (Just "Football")
--         actual2 `shouldEqual` { zodiac : A6 "FOOTBALL"}

--       it "should view into a sum type with one argument constructors (right)" do
--         let
--           sky =
--             { zodiac: B6 12
--             }

--           actualLeft = preview (barlow (key :: _ "zodiac.%B6")) sky
--         actualLeft `shouldEqual` (Just 12)

--       it "should view into a multi sum type with multi argument constructors (right)" do
--         let
--           sky =
--             { zodiac: A8 "hello" 13
--             }

--           actualLeft = preview (barlow (key :: _ "zodiac.%A8.%1")) sky
--           actualRight = preview (barlow (key :: _ "zodiac.%A8.%2")) sky
--         actualLeft `shouldEqual` (Just "hello")
--         actualRight `shouldEqual` (Just 13)

--       it "should view into a multi sum type with multi argument constructors (left)" do
--         let
--           sky =
--             { zodiac: A10 14
--             }

--           actualA10 = preview (barlow (key :: _ "zodiac.%A10")) sky
--         actualA10 `shouldEqual` (Just 14)

--       it "should view into a multi sum type with multi argument constructors (right)" do
--         let
--           sky =
--             { zodiac: B10 Yellow "hello" 15
--             }

--           actual1 = preview (barlow (key :: _ "zodiac.%B10.%1")) sky
--           actual2 = preview (barlow (key :: _ "zodiac.%B10.%1.%Yellow")) sky
--           actual3 = preview (barlow (key :: _ "zodiac.%B10.%1.%Red")) sky
--           actual4 = preview (barlow (key :: _ "zodiac.%B10.%2")) sky
--           actual5 = preview (barlow (key :: _ "zodiac.%B10.%3")) sky
--         actual1 `shouldEqual` (Just Yellow)
--         actual2 `shouldEqual` (Just unit)
--         actual3 `shouldEqual` Nothing
--         actual4 `shouldEqual` Just "hello"
--         actual5 `shouldEqual` Just 15

--       it "should view into a multi sum type with multi argument constructors (2x right)" do
--         let
--           sky =
--             { zodiac: C10 "first" "second" "third"
--             }

--           actual1 = preview (barlow (key :: _ "zodiac.%C10.%1")) sky
--           actual2 = preview (barlow (key :: _ "zodiac.%C10.%2")) sky
--           actual3 = preview (barlow (key :: _ "zodiac.%C10.%3")) sky
--           actual4 = over (barlow (key :: _ "zodiac.%C10.%1")) toUpper sky
--           actual5 = over (barlow (key :: _ "zodiac.%C10.%2")) toUpper sky
--           actual6 = over (barlow (key :: _ "zodiac.%C10.%3")) toUpper sky
--         actual1 `shouldEqual` (Just "first")
--         actual2 `shouldEqual` (Just "second")
--         actual3 `shouldEqual` (Just "third")
--         actual4 `shouldEqual` { zodiac: C10 "FIRST" "second" "third" } 
--         actual5 `shouldEqual` { zodiac: C10 "first" "SECOND" "third" } 
--         actual6 `shouldEqual` { zodiac: C10 "first" "second" "THIRD" } 

--       it "should view into a sum type with one argument constructors (left)" do
--         let
--           sky =
--             { zodiac: A6 "Football"
--             }

--           actual = preview (barlow (key :: _ "zodiac.%A6")) sky
--           actual2 = over (barlow (key :: _ "zodiac.%A6")) toUpper sky
--         actual `shouldEqual` (Just "Football")
--         actual2 `shouldEqual` { zodiac : A6 "FOOTBALL"}

--       it "should view into a sum type with no argument constructors (left)" do
--         let
--           sky =
--             { zodiac: A5
--             }

--           actual = preview (barlow (key :: _ "zodiac.%A5")) sky
--         actual `shouldEqual` (Just unit)

--       it "should view into a multi sum type with multi argument constructors (right)" do
--         let
--           sky =
--             { zodiac: B8 13
--             }

--           actualLeft = preview (barlow (key :: _ "zodiac.%B8")) sky
--           actualRight = preview (barlow (key :: _ "zodiac.%C8")) sky
--         actualLeft `shouldEqual` (Just 13)
--         actualRight `shouldEqual` Nothing
