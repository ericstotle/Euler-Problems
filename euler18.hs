import Data.List
import Data.List.Split
import Data.Char




o =               [75]
n =              [95,64]
m =             [17,47,82]
l =            [18,35,87,10]
k =           [20,04,82,47,65]
j =          [19,01,23,75,03,34]
i =         [88,02,77,73,07,63,67]
h =        [99,65,04,28,06,16,70,92]
g =       [41,41,26,56,83,40,80,70,33]
f =      [41,48,72,33,47,32,37,16,94,29]
e =     [53,71,44,65,25,43,91,52,97,51,14]
d =    [70,11,33,28,77,73,17,78,39,68,17,57]
c =   [91,71,52,38,17,14,91,43,58,50,27,29,48]
b =  [63,66,04,68,89,53,67,30,73,16,69,87,40,31]
a = [04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]

rows = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o] --bottom up

nodes = divvy 2 1 -- divvies row into the two sum possibilities for elements in above row

levelb = zip b $ nodes a
levelc = zip c $ nodes maxb
leveld = zip d $ nodes maxc
levele = zip e $ nodes maxd
levelf = zip f $ nodes maxe
levelg = zip g $ nodes maxf
levelh = zip h $ nodes maxg
leveli = zip i $ nodes maxh
levelj = zip j $ nodes maxi
levelk = zip k $ nodes maxj
levell = zip l $ nodes maxk
levelm = zip m $ nodes maxl
leveln = zip n $ nodes maxm

maxb = map maximum $ map (\(x,y) -> map (+x) y) levelb 
maxc = map maximum $ map (\(x,y) -> map (+x) y) levelc
maxd = map maximum $ map (\(x,y) -> map (+x) y) leveld
maxe = map maximum $ map (\(x,y) -> map (+x) y) levele
maxf = map maximum $ map (\(x,y) -> map (+x) y) levelf
maxg = map maximum $ map (\(x,y) -> map (+x) y) levelg
maxh = map maximum $ map (\(x,y) -> map (+x) y) levelh
maxi = map maximum $ map (\(x,y) -> map (+x) y) leveli
maxj = map maximum $ map (\(x,y) -> map (+x) y) levelj
maxk = map maximum $ map (\(x,y) -> map (+x) y) levelk
maxl = map maximum $ map (\(x,y) -> map (+x) y) levell
maxm = map maximum $ map (\(x,y) -> map (+x) y) levelm
maxn = map maximum $ map (\(x,y) -> map (+x) y) leveln
--maxo = map maximum $ map (\(x,y) -> map (+x) y) levelo

euler18 = 75 + maximum maxn



problem_18 = head $ foldr1 g tri 
  where
    f x y z = x + max y z
    g xs ys = zipWith3 f xs ys $ tail ys
    tri = [
        [75],
        [95,64],
        [17,47,82],
        [18,35,87,10],
        [20,04,82,47,65],
        [19,01,23,75,03,34],
        [88,02,77,73,07,63,67],
        [99,65,04,28,06,16,70,92],
        [41,41,26,56,83,40,80,70,33],
        [41,48,72,33,47,32,37,16,94,29],
        [53,71,44,65,25,43,91,52,97,51,14],
        [70,11,33,28,77,73,17,78,39,68,17,57],
        [91,71,52,38,17,14,91,43,58,50,27,29,48],
        [63,66,04,68,89,53,67,30,73,16,69,87,40,31],
        [04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]]