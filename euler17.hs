a = replicate 90 "onetwothreefourfivesixseveneightnine" -- one-nine comes up 90 times
b = replicate 10 "ten" -- 10, 110, 210...
c = replicate 10 "eleventwelvethirteenfourteenfifteensixteenseventeeneighteennineteen" -- sequence elever-ninteen occurs 10 times
d = replicate 100 "twentythirtyfortyfiftysixtyseventyeightyninety" -- 100x twenty, 100x thirty.. 
e = replicate 900 "hundred" -- hundred comes up 900 times from 1 to 1000
f = replicate 100 "onetwothreefourfivesixseveneightnine" -- one hundred, two hundred...
g = replicate 891 "and"
                                                                

euler17 = length (concat (a ++ b ++ c ++ d ++ e ++ f ++ g) ++ "onethousand") 