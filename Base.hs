type Coordinate = (Int, Int)
type Sun = Int

data Plant = Peashooter Int | Sunflower Int | Walnut Int | CherryBomb Int
    deriving (Eq,Show)
data Zombie = Basic Int Int | Conehead Int Int | Buckethead Int Int | Vaulting Int Int
    deriving (Eq,Show)
data GameModel = GameModel Sun [(Coordinate,Plant)] [(Coordinate,Zombie)]
    deriving (Eq,Show)

defaultPeashooter :: Plant
defaultPeashooter = Peashooter 3

defaultSunflower :: Plant
defaultSunflower = Sunflower 2

defaultWalnut :: Plant
defaultWalnut = Walnut 15

defaultCherryBomb :: Plant
defaultCherryBomb = CherryBomb 2

basic :: Zombie
basic = Basic 5 1

coneHead :: Zombie
coneHead = Conehead 10 1

bucketHead :: Zombie
bucketHead = Buckethead 20 1

vaulting :: Zombie
vaulting = Vaulting 7 2

plantPrice :: Plant -> Int
plantPrice (Peashooter _) = 100
plantPrice (Sunflower _) = 50
plantPrice (Walnut _) = 50
plantPrice (CherryBomb _) = 150

isThereEnoughSunForPlant :: Sun -> Plant -> Bool
isThereEnoughSunForPlant sun plant
    |plantPrice(plant) > sun= False
    |otherwise=True

tryPurchase :: GameModel -> Coordinate -> Plant -> Maybe GameModel
tryPurchase (GameModel sun plant zombie) (x,y) (noveny)
    |lookup (x,y) plant == Nothing && isThereEnoughSunForPlant sun noveny && x<5 && -1<x && y<12 && -1<y =Just(GameModel(sun-plantPrice(noveny))(((x,y),noveny):plant)zombie)
    |otherwise=Nothing

placeZombieInLane :: GameModel -> Zombie -> Int -> Maybe GameModel
placeZombieInLane (GameModel sun plant zombie) zombi x
    |lookup (x,11) zombie == Nothing && x<5 && -1<x = Just(GameModel sun plant (((x,11),zombi):zombie))
    |otherwise=Nothing

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

moveif2 :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
moveif2 _ []= []
moveif2 (y:ys) (x:xs)
    |canGoForwardOneDb (y:ys) x==True=(move2 (y:ys) x : moveif2 (y:ys) xs)
    |canGoForwardOneDb (y:ys) x==False=(x : moveif2 (y:ys) xs)

move2 :: [(Coordinate, Plant)] -> (Coordinate, Zombie) -> (Coordinate, Zombie)
move2 [] ((z,u),(Vaulting lf m))=((z,u-m),(Vaulting lf m))
move2 (((x,y),plant):xs) ((z,u),(Vaulting lf m))
    |x==z && u==y && m==2 = ((z,u-1),(Vaulting lf (1)))
    |x==z && y+1==u && m==2 = ((z,u-2),(Vaulting lf (1)))
    |otherwise= move2 xs ((z,u),(Vaulting lf m))
move2 (((x,y),plant):xs) ((z,u),(Basic i o))=((z,u-1),(Basic i o))
move2 (((x,y),plant):xs) ((z,u),(Conehead i o))=((z,u-1),(Conehead i o))
move2 (((x,y),plant):xs) ((z,u),(Buckethead i o))=((z,u-1),(Buckethead i o))

move :: (Coordinate, Zombie) -> (Coordinate, Zombie)
move ((z,u),(Vaulting lf m))
    |m==2=((z,u-2),(Vaulting lf (m-1)))
    |m==1=((z,u-1),(Vaulting lf m))
move ((z,u),zombie)=((z,u-1),zombie)

movelist :: [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
movelist [] = []
movelist (x:xs) = (move x : movelist xs)

vaulthitcounter :: Coordinate -> [(Coordinate, Zombie)] -> Int
vaulthitcounter _ [] = 0
vaulthitcounter (x,y) (((z,u),Vaulting lf m):ys)
    |x==z && u==y && m==2 = 1 + vaulthitcounter (x,y) ys
vaulthitcounter (x,y) (((z,u),zombie):ys) = vaulthitcounter (x,y) ys

hitcounter :: Coordinate -> [(Coordinate, Zombie)] -> Int
hitcounter _ []=0
hitcounter (x,y) (((z,u),zombie):ys)
    |x==z && u==y = (1 + hitcounter (x,y) ys)
    |otherwise=(hitcounter (x,y) ys)

counter :: Coordinate -> [(Coordinate, Zombie)] -> Int
counter (x,y) (((z,u),zombie):ys) = (hitcounter(x,y) (((z,u),zombie):ys))-(vaulthitcounter( x,y) (((z,u),zombie):ys))    

hit :: (Coordinate, Plant) -> [(Coordinate, Zombie)] -> (Coordinate, Plant)
hit x [] = x
hit ((x,y),Peashooter lf) (((z,u),zombie):ys)
    |x==z && u==y = ((x,y),Peashooter (lf-(counter (x,y) (((z,u),zombie):ys))))
    |otherwise=hit ((x,y),Peashooter lf) ys
hit ((x,y),Sunflower lf) (((z,u),zombie):ys)
    |x==z && u==y = ((x,y),Sunflower (lf-(counter (x,y) (((z,u),zombie):ys))))
    |otherwise=hit ((x,y),Sunflower lf) ys
hit ((x,y),Walnut lf) (((z,u),zombie):ys)
    |x==z && u==y = ((x,y),Walnut (lf-(counter (x,y) (((z,u),zombie):ys))))
    |otherwise=hit ((x,y),Walnut lf) ys
hit ((x,y),CherryBomb lf) (((z,u),zombie):ys)
    |x==z && u==y = ((x,y),CherryBomb (lf-(counter (x,y) (((z,u),zombie):ys))))
    |otherwise=hit ((x,y),CherryBomb lf) ys

hitlist :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [(Coordinate, Plant)]
hitlist [] _ = []
hitlist (x:xs) (y:ys)= (hit x (y:ys) : hitlist xs (y:ys))

canGoForwardOneDb :: [(Coordinate, Plant)] -> (Coordinate, Zombie) -> Bool
canGoForwardOneDb [] ((z,u),zombie) = True
canGoForwardOneDb (x:xs) ((z,u),Vaulting i o)= canGoForwardVaulting (x:xs) ((z,u),Vaulting i o)
canGoForwardOneDb (((x,y),plant):xs) ((z,u),zombie)
    |x==z && y==u=False
    |otherwise=canGoForwardOneDb xs ((z,u),zombie)

canGoForwardVaulting :: [(Coordinate, Plant)] -> (Coordinate, Zombie) -> Bool
canGoForwardVaulting [] ((z,u),(Vaulting lf m))=True
canGoForwardVaulting (((x,y),plant):xs) ((z,u),(Vaulting lf m))
    |x==z && y==u && m==1 = False
    |x==z && (y==u || y+1==u) &&  m==2= True
    |otherwise=canGoForwardVaulting xs ((z,u),(Vaulting lf m))   

isIn :: (Coordinate,Zombie) -> Bool
isIn ((x,y),zombie)
    |y<0=True
    |otherwise=False

checkisIn :: [(Coordinate, Zombie)] -> Bool
checkisIn []= False
checkisIn (x:xs)
    |isIn x==True=True
    |otherwise= checkisIn xs
         
performZombieActions :: GameModel -> Maybe GameModel
performZombieActions (GameModel s (p:ps) []) = Just(GameModel s (p:ps) [])
performZombieActions (GameModel s [] (z:zs))
    |checkisIn (movelist (z:zs))==True = Nothing
    |otherwise=Just(GameModel s [] (movelist (z:zs)))
performZombieActions (GameModel s [] []) = Just(GameModel s [] [])
performZombieActions (GameModel s (p:ps) (z:zs))
    |checkisIn (moveif2 (p:ps) (z:zs))==True = Nothing
    |otherwise=Just(GameModel s (hitlist (p:ps) (z:zs)) (moveif2 (p:ps) (z:zs)))

----------------------------------------------------------------------------------------------------------------------------------------------------------------------

zombieLife :: Zombie -> Int
zombieLife (Basic x _) = x
zombieLife (Conehead x _) = x
zombieLife (Buckethead x _) = x
zombieLife (Vaulting x _) = x

plantLife :: Plant -> Int
plantLife (Peashooter x) = x
plantLife (Sunflower x) = x
plantLife (Walnut x) = x
plantLife (CherryBomb x) = x

cleanBoardZombies :: [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
cleanBoardZombies [] = []
cleanBoardZombies (((x,y),zombie):xs)
    |zombieLife zombie < 1 = cleanBoardZombies xs
    |otherwise= (((x,y),zombie): cleanBoardZombies xs)

cleanBoardPlants :: [(Coordinate, Plant)] -> [(Coordinate, Plant)]
cleanBoardPlants [] = []
cleanBoardPlants (((x,y),plant):xs)
    |plantLife plant < 1 = cleanBoardPlants xs
    |otherwise= (((x,y),plant):cleanBoardPlants xs)

cleanBoard :: GameModel -> GameModel
cleanBoard (GameModel u [] []) =(GameModel u [] [])
cleanBoard (GameModel u x y) = (GameModel u (cleanBoardPlants x) (cleanBoardZombies y))

--------------------------------------------------------------------------------------------------------------------------

--canGoForwardlist :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [Bool]
--canGoForwardlist _ []= []
--canGoForwardlist (x:xs) (y:ys) = (canGoForwardOneDb (x:xs) y : canGoForwardlist (x:xs) (ys))

--moveif :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
--moveif _ []= []
--moveif (y:ys) (x:xs)
--    |canGoForwardOneDb (y:ys) x==True=(move x : moveif (y:ys) xs)
--    |canGoForwardOneDb (y:ys) x==False=(x : moveif (y:ys) xs)

--canGoForward :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [(Bool,Int)]
--canGoForward _ []= []
--canGoForward (x:xs) (y:ys) = (canGoForwardOneDb (x:xs) y : canGoForward (x:xs) (ys))

--canGoForwardOneDb :: [(Coordinate, Plant)] -> (Coordinate, Zombie) -> (Bool,Int)
--canGoForwardOneDb [] ((z,u),(Vaulting lf m)) = (True,m)
--canGoForwardOneDb [] ((z,u),zombie) = (True,1)
--canGoForwardOneDb (x:xs) ((z,u),Vaulting i o)= canGoForwardVaulting (x:xs) ((z,u),Vaulting i o)
--canGoForwardOneDb (((x,y),plant):xs) ((z,u),zombie)
--    |x==z && y==u=(False,1)
--    |otherwise=canGoForwardOneDb xs ((z,u),zombie)

--canGoForwardVaulting :: [(Coordinate, Plant)] -> (Coordinate, Zombie) -> (Bool,Int)
--canGoForwardVaulting [] ((z,u),(Vaulting lf m))=(True,m)
--canGoForwardVaulting (((x,y),plant):xs) ((z,u),(Vaulting lf m))
--    |x==z && y==u && m==1 = (False,1)
--    |x==z && (y==u || y+1==u) &&  m==2= (True,1)
--    |otherwise=canGoForwardVaulting xs ((z,u),(Vaulting lf m))

--canGoForward :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [Bool]
--canGoForward _ []= []
--canGoForward (x:xs) (y:ys) = (canGoForwardOneDb (x:xs) y : canGoForward (x:xs) (ys))

--action :: GameModel -> GameModel
--action (GameModel s (((x,y),plant):xs) (((z,u),zombie):ys) )
--    |canGoForwardOneDb (((x,y),plant):xs) ((z,u),zombie)==True=(GameModel s (((x,y),plant):xs) (((move((z,u),zombie))):(move ys)))
--    |canGoForwardOneDb (((x,y),plant):xs) ((z,u),zombie)==False = (GameModel s ((hit((x,y),plant (((z,u),zombie):ys))):(hit xs (((z,u),zombie):ys))) (((z,u),zombie):ys) )

--performZombieActions (GameModel s (((x,y),plant):xs) (((z,u),zombie):ys))
--    |canGoForwardOneDb (((x,y),plant):xs) ((z,u),zombie)==True=Just(GameModel s (((x,y),plant):xs) (((move((z,u),zombie))):(move ys)))
--    |canGoForwardOneDb (((x,y),plant):xs) ((z,u),zombie)==False=Just(GameModel s ((hit((x,y),plant (((z,u),zombie):ys))):(hit xs (((z,u),zombie):ys))) (((z,u),zombie):ys) )

--action :: GameModel -> GameModel
--action (GameModel s (z:zs) (p:ps) )
--    |canGoForwardOneDb (x:xs) (y)==True=(GameModel s (movelist (x:xs)) y:ys)
--    |canGoForwardOneDb (x:xs) (y)==False = (GameModel s (x:xs) (hitlist((x:xs) (y:ys))))

--action :: Sun -> [(Coordinate, Plant)] -> (Coordinate, Zombie) -> [(Coordinate, Zombie)] -> GameModel
--action s (p:ps) y (z:zs)
--    |canGoForwardOneDb (p:ps) y==True=(GameModel s (p:ps) ((move y): zs))
--    |otherwise=(GameModel s (hitlist (p:ps) (z:zs)) (z:zs))

--hulyevault :: [(Coordinate, Plant)] -> (Coordinate, Zombie)->(Coordinate, Zombie)