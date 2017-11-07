data Animal = Lion | Tiger | Gazelle | Ant deriving (Eq, Show)

--Question 1
type NumTerritories = Int
type MaxNumAns = Int
type PlotShallow = (NumTerritories, MaxNumAns)

territory :: [Animal] -> (Int, Int)
territory as = (1,length as)

quadrant :: PlotShallow -> PlotShallow -> PlotShallow -> PlotShallow -> (Int, Int)
quadrant (x1, y1) (x2, y2) (x3, y3) (x4, y4)= (x1+x2+x3+x4, maximum[y1,y2,y3,y4])

--Question 2

data Plot = Territory [Animal] | NewPlot Plot Plot Plot Plot

territories :: Plot -> Int
territories (Territory as) = 1
territories (NewPlot p1 p2 p3 p4)  = (territories p1) + (territories p2) + (territories p3) + (territories p4)

maxAnimals :: Plot -> Int
maxAnimals (Territory as) = length as
maxAnimals (NewPlot p1 p2 p3 p4)  = maximum[maxAnimals p1, maxAnimals p2, maxAnimals p3, maxAnimals p4]
                                    -- maximum (map maxAnimals [p1,p2,p3,p4])

--Question 3

--no change to shallow embedding apart from removing a parameter
--deep becomes

data Plot' = Territory' [Animal] | NewPlot' Plot' Plot' Plot'

territories' :: Plot' -> Int
territories' (Territory' as) = 1
territories' (NewPlot' p1 p2 p3)= (territories' p1) + (territories' p2) + (territories' p3)

maxAnimals' :: Plot' -> Int
maxAnimals' (Territory' as) = length as
maxAnimals' (NewPlot' p1 p2 p3) = maximum[maxAnimals' p1, maxAnimals' p2, maxAnimals' p3]

--Question 4

--have to change shallow


type NewPlotShallow = (NumTerritories, MaxNumAns, [Animal])

territory' :: [Animal] -> (Int, Int, [Animal])
territory' as = (1,length as, as)


quadrant' :: NewPlotShallow -> NewPlotShallow -> NewPlotShallow -> NewPlotShallow -> (Int, Int, [Animal])
quadrant' (x1, y1, z1) (x2, y2, z2) (x3, y3, z3) (x4, y4, z4)= (x1+x2+x3+x4, maximum[y1,y2,y3,y4], z1++z2++z3++z4)

--no change to deep apart from adding the following

listAnimals :: Plot -> [Animal]
listAnimals (Territory as) = as
listAnimals (NewPlot p1 p2 p3 p4) = listAnimals p1 ++  listAnimals p2 ++ listAnimals p3 ++ listAnimals p4

--Question 5

--mistakes
--pattern matching

--Question 6/7

f :: NewPlot -> Int -> Int
f (AnyTerritory as) k = 1 + k
f (AnyNum ps) k = k + (foldr (f) 0 ps)

data NewPlot = AnyTerritory [Animal] | AnyNum [NewPlot]

territoriesNew :: NewPlot -> Int
territoriesNew (AnyTerritory as) = 1
territoriesNew (AnyNum ps) = foldr (f) 0 (ps)

g :: NewPlot -> Int -> Int
g (AnyTerritory as) k = max (length as) k
g (AnyNum ps) k = max (foldr (g) 0 ps) k

maxAn :: NewPlot -> Int
maxAn (AnyTerritory as) = length as
maxAn (AnyNum ps) = foldr (g) 0 ps

h :: NewPlot -> [Animal] -> [Animal]
h (AnyTerritory as) k = as ++ k
h (AnyNum ps) k =  (foldr (h) [] ps) ++ k

listAns :: NewPlot -> [Animal]
listAns (AnyTerritory as) = as
listAns (AnyNum ps) = foldr (h) [] ps

data NewPlot' = AnyTerritory' [Animal] | AnyNum' [(NewPlot', Float)]

quad :: NewPlot' -> NewPlot' -> NewPlot' -> NewPlot' -> NewPlot'
quad p1 p2 p3 p4 = AnyNum'[(p1, 0.25), (p2, 0.25),(p3, 0.25), (p4, 0.25)]

split :: NewPlot' -> NewPlot' -> NewPlot' -> NewPlot'
split plot1 plot2 plot3 = AnyNum'[(plot1, 0.33), (plot2, 0.33), (plot3, 0.33)]




--shallow-> you would pass in a list of NewPlotShallow



--1.3:

--shallow advantageous when changing constructors
--deep advantageous when multiple semantics available
