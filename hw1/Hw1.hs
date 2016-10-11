-- ---
-- title: Homework #1, Due Monday 1/26/15
-- ---


-- Haskell Formalities
-- -------------------

-- We declare that this is the Hw1 module and import some libraries:

module Hw1 where
import SOE
import Play
import XMLTypes

-- Part 0: All About You
-- ---------------------

-- Tell us your name, email and student ID, by replacing the respective
-- strings below

myName  = "Kaiwen Sun"
myEmail = "kas003@ucsd.edu"
mySID   = "A53091621"

-- Part 1: Defining and Manipulating Shapes
-- ----------------------------------------

-- You will write all of your code in the `Hw1.hs` file, in the spaces
-- indicated. Do not alter the type annotations --- your code must
-- typecheck with these types to be accepted.

-- The following are the definitions of shapes:

data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
           deriving Show
-- >
type Radius = Float
type Side   = Float
type Vertex = (Float, Float)

-- 1. Below, define functions `rectangle` and `rtTriangle` as suggested
--    at the end of Section 2.1 (Exercise 2.1). Each should return a Shape
--    built with the Polygon constructor.

rectangle :: Side -> Side -> Shape
rectangle w h = Polygon [(0,0),(0,h),(w,h),(w,0)]  --w:width, h:height, from origin, draw clockwise.

rtTriangle :: Side -> Side -> Shape
rtTriangle w h = Polygon [(0,0),(0,h),(0,w)]  --w:width, h:height, the right angle is at origin on bottom left, draw clockwise.

-- 2. Define a function

sides :: Shape -> Int
sides (Rectangle _ _) = 4
sides (Ellipse _ _) = 42
sides (RtTriangle _ _) = 3
sides (Polygon ver) = if l<3
                      then 0
                      else l
                      where l = length ver

--   which returns the number of sides a given shape has.
--   For the purposes of this exercise, an ellipse has 42 sides,
--   and empty polygons, single points, and lines have zero sides.

-- 3. Define a function

bigger :: Shape -> Float -> Shape
bigger (Rectangle w h) e = Rectangle (w*sqrt(e)) (h*sqrt(e))
bigger (Ellipse r1 r2) e = Ellipse (r1*sqrt(e)) (r2*sqrt(e))
bigger (RtTriangle w h) e = RtTriangle (w*sqrt(e)) (h*sqrt(e))
bigger (Polygon ver) e = Polygon (map (\(x,y)->(x*sqrt(e),y*sqrt(e))) ver)
 

--   that takes a shape `s` and expansion factor `e` and returns
--   a shape which is the same as (i.e., similar to in the geometric sense)
--   `s` but whose area is `e` times the area of `s`.

-- 4. The Towers of Hanoi is a puzzle where you are given three pegs,
--    on one of which are stacked $n$ discs in increasing order of size.
--    To solve the puzzle, you must move all the discs from the starting peg
--    to another by moving only one disc at a time and never stacking
--    a larger disc on top of a smaller one.

--    To move $n$ discs from peg $a$ to peg $b$ using peg $c$ as temporary storage:

--    1. Move $n - 1$ discs from peg $a$ to peg $c$.
--    2. Move the remaining disc from peg $a$ to peg $b$.
--    3. Move $n - 1$ discs from peg $c$ to peg $b$.

--    Write a function

hanoi :: Int -> String -> String -> String -> IO ()
hanoi 0 _ _ _ = putStr ""
hanoi 1 a b _ = putStrLn ("move disc from " ++ a ++" to " ++ b)
hanoi n a b c = do
                hanoi (n-1) a c b
                hanoi 1 a b c
                hanoi (n-1) c b a

--   that, given the number of discs $n$ and peg names $a$, $b$, and $c$,
--   where a is the starting peg,
--   emits the series of moves required to solve the puzzle.
--   For example, running `hanoi 2 "a" "b" "c"`

--   should emit the text

-- ~~~
-- move disc from a to c
-- move disc from a to b
-- move disc from c to b
-- ~~~

-- Part 2: Drawing Fractals
-- ------------------------

-- 1. The Sierpinski Carpet is a recursive figure with a structure similar to
--    the Sierpinski Triangle discussed in Chapter 3:

-- ![Sierpinski Carpet](/static/scarpet.png)

-- Write a function `sierpinskiCarpet` that displays this figure on the
-- screen:

minSize :: Int
minSize = 5

fillSqr :: Window -> Int -> Int -> Int -> IO ()
fillSqr w x y size
  = drawInWindow w (withColor Blue
      (polygon [(x,y),(x+size,y),(x+size,y+size),(x,y+size)]))

sierpinskiCarpetInner :: Window -> Int -> Int -> Int -> IO ()
sierpinskiCarpetInner w x y size
  = if size <= minSize
    then fillSqr w x y size
    else let size3 = (size-1) `div` 3
      in do sierpinskiCarpetInner w x y size3
            sierpinskiCarpetInner w (x+size3)     y         size3
            sierpinskiCarpetInner w (x+size3*2)   y         size3
            sierpinskiCarpetInner w x             (y+size3) size3
            sierpinskiCarpetInner w (x+size3*2)   (y+size3) size3
            sierpinskiCarpetInner w x             (y+size3*2) size3
            sierpinskiCarpetInner w (x+size3)     (y+size3*2) size3
            sierpinskiCarpetInner w (x+size3*2)   (y+size3*2) size3
  

sierpinskiCarpet :: IO ()
sierpinskiCarpet
  = runGraphics (
    do w <- openWindow "Sierpinski's Carpet" (700,700)
       sierpinskiCarpetInner w 1 1 (648)
       spaceClose w
    )

{-
sierpinskiCarpet
    = runGraphics (
    do w <- openWindow "TEST" (400,400)
       drawInWindow w (withColor Blue (polygon [(50,50),(250,50),(250,250),(50,250),(50,50)]))
       spaceClose w
    )
-}

spaceClose :: Window -> IO ()
spaceClose w
  = do k <- getKey w
       if k==' ' || k == '\x0'
          then closeWindow w
          else spaceClose w

-- Note that you either need to run your program in `SOE/src` or add this
-- path to GHC's search path via `-i/path/to/SOE/src/`.
-- Also, the organization of SOE has changed a bit, so that now you use
-- `import SOE` instead of `import SOEGraphics`.

-- 2. Write a function `myFractal` which draws a fractal pattern of your
--    own design.  Be creative!  The only constraint is that it shows some
--    pattern of recursive self-similarity.


fillParr :: Window -> Int -> Int -> Int -> IO ()
fillParr w x y size
  = drawInWindow w (withColor Blue
      (polygon [(x,y),(x+size,y),(x+size*2,y+size),(x+size,y+size)]))

myFractalInner :: Window -> Int -> Int -> Int -> IO ()
myFractalInner w x y size
  = if size <= minSize
    then fillParr w x y size
    else let size3 = (size-1) `div` 3
      in do myFractalInner w x y size3
            myFractalInner w (x+size3)     (y+(size3 `div` 4))    size3
            myFractalInner w (x+size3*2)   y         size3
            myFractalInner w (x+(size3 `div` 4))     (y+size3) size3
            myFractalInner w (x+size3*3)   (y+size3) size3
            myFractalInner w (x+size3*2)   (y+size3*2) size3
            myFractalInner w (x+size3*3)     (y+size3*2) size3
            myFractalInner w (x+size3*4)   (y+size3*2) size3
  

myFractal :: IO ()
myFractal
  = runGraphics (
    do w <- openWindow "Sierpinski's Carpet" (1300,700)
       myFractalInner w 1 1 (648)
       spaceClose w
    )
  

-- Part 3: Recursion Etc.
-- ----------------------

-- First, a warmup. Fill in the implementations for the following functions.

-- (Your `maxList` and `minList` functions may assume that the lists
-- they are passed contain at least one element.)

-- Write a *non-recursive* function to compute the length of a list

lengthNonRecursive :: [a] -> Int
lengthNonRecursive l = length l

-- `doubleEach [1,20,300,4000]` should return `[2,40,600,8000]`

doubleEach :: [Int] -> [Int]
doubleEach [] = []
doubleEach lst = ((head lst) * 2):(doubleEach (tail lst))

-- Now write a *non-recursive* version of the above.

doubleEachNonRecursive :: [Int] -> [Int]
doubleEachNonRecursive lst = map (\e->e*2) lst

-- `pairAndOne [1,20,300]` should return `[(1,2), (20,21), (300,301)]`

pairAndOne :: [Int] -> [(Int, Int)]
pairAndOne [] = []
pairAndOne lst = ((first,first+1):(pairAndOne (tail lst)))
                 where first = head lst


-- Now write a *non-recursive* version of the above.

pairAndOneNonRecursive :: [Int] -> [(Int, Int)]
pairAndOneNonRecursive lst = map (\e->(e,e+1)) lst

-- `addEachPair [(1,2), (20,21), (300,301)]` should return `[3,41,601]`

addEachPair :: [(Int, Int)] -> [Int]
addEachPair [] = []
addEachPair lst = ((fst first)+(snd first)):(addEachPair (tail lst))
                  where first = head lst

-- Now write a *non-recursive* version of the above.

addEachPairNonRecursive :: [(Int, Int)] -> [Int]
addEachPairNonRecursive lst = map (\(a,b)->(a+b)) lst

-- `minList` should return the *smallest* value in the list. You may assume the
-- input list is *non-empty*.

minList :: [Int] -> Int
minList [a] = a
minList (x:t) = if x<=r
                then x
                else r
                where r = (minList t)

-- Now write a *non-recursive* version of the above.

minListNonRecursive :: [Int] -> Int
minListNonRecursive lst = minimum lst

-- `maxList` should return the *largest* value in the list. You may assume the
-- input list is *non-empty*.

maxList :: [Int] -> Int
maxList [a] = a
maxList (x:t) = if x>=r
                then x
                else r
                where r = (maxList t)

-- Now write a *non-recursive* version of the above.

maxListNonRecursive :: [Int] -> Int
maxListNonRecursive lst = maximum lst

-- Now, a few functions for this `Tree` type.

data Tree a = Leaf a | Branch (Tree a) (Tree a)
              deriving (Show, Eq)

-- `fringe t` should return a list of all the values occurring as a `Leaf`.
-- So: `fringe (Branch (Leaf 1) (Leaf 2))` should return `[1,2]`

fringe :: Tree a -> [a]
fringe (Leaf a) = [a]
fringe (Branch a b) = (fringe a) ++ (fringe b)

-- `treeSize` should return the number of leaves in the tree.
-- So: `treeSize (Branch (Leaf 1) (Leaf 2))` should return `2`.

treeSize :: Tree a -> Int
treeSize (Leaf a) = 1
treeSize (Branch a b) = (treeSize a) + (treeSize b)

-- `treeSize` should return the height of the tree.
-- So: `height (Branch (Leaf 1) (Leaf 2))` should return `1`.

treeHeight :: Tree a -> Int
treeHeight (Leaf a) = 0
treeHeight (Branch a b) = 1 + (max (treeHeight a) (treeHeight b))

-- Now, a tree where the values live at the nodes not the leaf.

data InternalTree a = ILeaf | IBranch a (InternalTree a) (InternalTree a)
                      deriving (Show, Eq)

-- `takeTree n t` should cut off the tree at depth `n`.
-- So `takeTree 1 (IBranch 1 (IBranch 2 ILeaf ILeaf) (IBranch 3 ILeaf ILeaf)))`
-- should return `IBranch 1 ILeaf ILeaf`.

takeTree :: Int -> InternalTree a -> InternalTree a
takeTree _ ILeaf = ILeaf
takeTree 0 _ = ILeaf
takeTree n (IBranch value b1 b2) = IBranch value (takeTree (n-1) b1) (takeTree (n-1) b2)

-- `takeTreeWhile p t` should cut of the tree at the nodes that don't satisfy `p`.
-- So: `takeTreeWhile (< 3) (IBranch 1 (IBranch 2 ILeaf ILeaf) (IBranch 3 ILeaf ILeaf)))`
-- should return `(IBranch 1 (IBranch 2 ILeaf ILeaf) ILeaf)`.

takeTreeWhile :: (a -> Bool) -> InternalTree a -> InternalTree a
takeTreeWhile _ ILeaf = ILeaf
takeTreeWhile p (IBranch v b1 b2) = if (p v) then (IBranch v (takeTreeWhile p b1) (takeTreeWhile p b2)) else ILeaf

-- Write the function map in terms of foldr:

myMap :: (a -> b) -> [a] -> [b]
myMap f lst = foldr (\x y->((f x):y)) ([]) lst

-- Part 4: Transforming XML Documents
-- ----------------------------------

-- The rest of this assignment involves transforming XML documents.
-- To keep things simple, we will not deal with the full generality of XML,
-- or with issues of parsing. Instead, we will represent XML documents as
-- instances of the following simpliﬁed type:

-- ~~~~
-- data SimpleXML =
--    PCDATA String
--  | Element ElementName [SimpleXML]
--  deriving Show

-- type ElementName = String
-- ~~~~

-- That is, a `SimpleXML` value is either a `PCDATA` ("parsed character
-- data") node containing a string or else an `Element` node containing a
-- tag and a list of sub-nodes.

-- The file `Play.hs` contains a sample XML value. To avoid getting into
-- details of parsing actual XML concrete syntax, we'll work with just
-- this one value for purposes of this assignment. The XML value in
-- `Play.hs` has the following structure (in standard XML syntax):

-- ~~~
-- <PLAY>
--   <TITLE>TITLE OF THE PLAY</TITLE>
--   <PERSONAE>
--     <PERSONA> PERSON1 </PERSONA>
--     <PERSONA> PERSON2 </PERSONA>
--     ... -- MORE PERSONAE
--     </PERSONAE>
--   <ACT>
--     <TITLE>TITLE OF FIRST ACT</TITLE>
--     <SCENE>
--       <TITLE>TITLE OF FIRST SCENE</TITLE>
--       <SPEECH>
--         <SPEAKER> PERSON1 </SPEAKER>
--         <LINE>LINE1</LINE>
--         <LINE>LINE2</LINE>
--         ... -- MORE LINES
--       </SPEECH>
--       ... -- MORE SPEECHES
--     </SCENE>
--     ... -- MORE SCENES
--   </ACT>
--   ... -- MORE ACTS
-- </PLAY>
-- ~~~

-- * `sample.html` contains a (very basic) HTML rendition of the same
--   information as `Play.hs`. You may want to have a look at it in your
--   favorite browser.  The HTML in `sample.html` has the following structure
--   (with whitespace added for readability):

-- ~~~
-- <html>
--   <body>
--     <h1>TITLE OF THE PLAY</h1>
--     <h2>Dramatis Personae</h2>
--     PERSON1<br/>
--     PERSON2<br/>
--     ...
--     <h2>TITLE OF THE FIRST ACT</h2>
--     <h3>TITLE OF THE FIRST SCENE</h3>
--     <b>PERSON1</b><br/>
--     LINE1<br/>
--     LINE2<br/>
--     ...
--     <b>PERSON2</b><br/>
--     LINE1<br/>
--     LINE2<br/>
--     ...
--     <h3>TITLE OF THE SECOND SCENE</h3>
--     <b>PERSON3</b><br/>
--     LINE1<br/>
--     LINE2<br/>
--     ...
--   </body>
-- </html>
-- ~~~

-- You will write a function `formatPlay` that converts an XML structure
-- representing a play to another XML structure that, when printed,
-- yields the HTML speciﬁed above (but with no whitespace except what's
-- in the textual data in the original XML).

accArray :: ElementName -> [SimpleXML] -> SimpleXML
accArray tag xmlArray = Element tag (foldr (\ele arr -> ele:arr) [] xmlArray)

formatPlay :: SimpleXML -> SimpleXML
formatPlay xml = removeEmptyTag (formatXml 1 xml)
{-
formatPlay (Element "TITLE" xmlArray) = accArray "h1" (map formatPlay xmlArray)

formatPlay (Element "PERSONAE" xmlArray) = Element "h2" [(PCDATA "Dramatis Personae")]):[(accArray "" (xmlArray))])
formatPlay (PCDATA str) = (PCDATA str)


formatPlay (Element tag xmlArray) = Element tag []
formatPlay _ = Element "na" []
-}

emptyTag = ""
noTag = "notag"

getXmlTag :: SimpleXML -> ElementName
getXmlTag (Element tag _) = tag
getXmlTag (PCDATA _) = noTag

getXmlArray :: SimpleXML -> [SimpleXML]
getXmlArray (Element _ array) = array
getXmlArray _ = error "getXmlArray"

getData :: SimpleXML -> String
getData (PCDATA str) = str
getData _ = error "Can't get data from Element"

reduceEptEle :: SimpleXML -> [SimpleXML] -> [SimpleXML]
reduceEptEle h right =
  if ((getXmlTag h)==emptyTag)
  then ((foldr reduceEptEle [] (getXmlArray h))++(right))
  else if ((getXmlTag h)==noTag)
  then (h : right)
  else ((removeEmptyTag h):(right))

removeEmptyTag :: SimpleXML -> SimpleXML
removeEmptyTag (Element tag array) = (Element tag (foldr reduceEptEle [] array))
removeEmptyTag (PCDATA str) = PCDATA str


getHeadNum :: Int -> ElementName
getHeadNum n = case n of
  1 -> "h1"
  2 -> "h2"
  3 -> "h3"
  4 -> "h4"
  5 -> "h5"
  6 -> "h6"

formatXml :: Int -> SimpleXML  -> SimpleXML
formatXml _ (PCDATA str) = PCDATA str
formatXml flag (Element tag array) = case tag of
  "PLAY" -> Element "html" [Element "body" (map (formatXml flag) array)]
  "TITLE" -> Element (getHeadNum flag) (map (formatXml (flag+1)) array)
  "PERSONAE" -> Element emptyTag ((Element (getHeadNum (flag+1)) [(PCDATA "Dramatis Personae")]):(map (formatXml (flag+1)) array))
  "PERSONA" -> PCDATA ((getData (head array))++"<br/>")
  "ACT" -> Element emptyTag (map (formatXml (flag+1)) array) 
  "SCENE" -> Element emptyTag (map (formatXml (flag+1)) array)
  "SPEECH" -> Element emptyTag (map (formatXml flag) array)
  "SPEAKER" -> PCDATA ("<b>"++(getData (head array))++"</b><br/>")
  "LINE" -> PCDATA ((getData (head array))++"<br/>")
  _ -> error ("No such tag "++tag)

-- The main action that we've provided below will use your function to
-- generate a ﬁle `dream.html` from the sample play. The contents of this
-- ﬁle after your program runs must be character-for-character identical
-- to `sample.html`.

mainXML = do writeFile "dream.html" $ xml2string $ formatPlay play
             testResults "dream.html" "sample.html"
-- >
firstDiff :: Eq a => [a] -> [a] -> Maybe ([a],[a])
firstDiff [] [] = Nothing
firstDiff (c:cs) (d:ds)
     | c==d = firstDiff cs ds
     | otherwise = Just (c:cs, d:ds)
firstDiff cs ds = Just (cs,ds)
-- >
testResults :: String -> String -> IO ()
testResults file1 file2 = do
  f1 <- readFile file1
  f2 <- readFile file2
  case firstDiff f1 f2 of
    Nothing -> do
      putStr "Success!\n"
    Just (cs,ds) -> do
      putStr "Results differ: '"
      putStr (take 20 cs)
      putStr "' vs '"
      putStr (take 20 ds)
      putStr "'\n"

-- Important: The purpose of this assignment is not just to "get the job
-- done" --- i.e., to produce the right HTML. A more important goal is to
-- think about what is a good way to do this job, and jobs like it. To
-- this end, your solution should be organized into two parts:

-- 1. a collection of generic functions for transforming XML structures
--    that have nothing to do with plays, plus

-- 2. a short piece of code (a single deﬁnition or a collection of short
--    deﬁnitions) that uses the generic functions to do the particular
--    job of transforming a play into HTML.

-- Obviously, there are many ways to do the ﬁrst part. The main challenge
-- of the assignment is to ﬁnd a clean design that matches the needs of
-- the second part.

-- You will be graded not only on correctness (producing the required
-- output), but also on the elegance of your solution and the clarity and
-- readability of your code and documentation.  Style counts.  It is
-- strongly recommended that you rewrite this part of the assignment a
-- couple of times: get something working, then step back and see if
-- there is anything you can abstract out or generalize, rewrite it, then
-- leave it alone for a few hours or overnight and rewrite it again. Try
-- to use some of the higher-order programming techniques we've been
-- discussing in class.

-- Submission Instructions
-- -----------------------

-- * If working with a partner, you should both submit your assignments
--   individually.

-- * Make sure your `Hw1.hs` is accepted by GHCi without errors or warnings.

-- * Attach your `hw1.hs` file in an email to `cse230@goto.ucsd.edu` with the
--   subject "HW1" (minus the quotes). *This address is unmonitored!*

-- Credits
-- -------

-- This homework is essentially Homeworks 1 & 2 from
-- <a href="http://www.cis.upenn.edu/~bcpierce/courses/552-2008/index.html">UPenn's CIS 552</a>.
