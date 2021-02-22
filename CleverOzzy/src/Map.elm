module Map exposing (..)


type alias Block =
    { blockType : String
    , movable : Bool
    , pushable : Bool
    , keyDialogueNum: Maybe Int
    , dialog: Maybe String
    , pos : ( Int , Int )  --xPos from 0 to 23, yPos From 0 to 15, starts from the upper right corner
    }


genPos: Int -> List Int -> List (Int, Int)
genPos x ys =
    List.map (Tuple.pair x) ys

genPosy : Int -> List Int -> List (Int,Int)
genPosy y xs =
    let
        reverse aTuple =
            (Tuple.second aTuple,Tuple.first aTuple)
    in
    List.map reverse (List.map (Tuple.pair y) xs)


line : Int -> Int -> Int -> List (Int,Int)
line x a b =
    List.map (Tuple.pair x) ( List.range a b )

liney : Int -> Int -> Int -> List (Int,Int)
liney y a b =
    genPosy y (List.range a b)

mapDorm : List Block
mapDorm =
    let

        pos_grass2 = genPos 0 ((List.range 19 29) ++ List.range 34 43)
          ++ genPos 1 (List.range 19 29++ [43,34])
          ++ genPos 2 ((List.range 0 7) ++  (List.range 19 29) ++ [43,34])
          ++ genPos 3 (List.range 19 29 ++ [43,34] )
          ++ genPosy 36 ( List.range 7 14)
          ++ genPosy 40 ( List.range 7 14)
          ++ genPos 18 ( List.range 17 21)
          ++ line 14 37 39
          ++ [(3,8),(5,8),(7,9),(8,9),(7,37),(7,38),(7,39),(8,38),(10,38),(10,39),(11,38),(13,38),
              (19,21),(20,21),(22,21),(40,7),(41,7),(40,8),(41,8)]
        pos_lake5 = genPosy 12 (List.range 1 7) ++ genPosy 13 (List.range 1 7)
          ++genPosy 14 (List.range 1 7)
          ++ line 8 20 25 ++ line 9 20 27 ++ line 10 20 27 ++ line 11 20 27 ++ line 12 19 27
          ++ line 13 19 27 ++ line 14 19 27 ++ line 15 19 27 ++ line 16 24 27 ++ line 17 25 27
          ++ line 18 24 27++ line 19 24 27++ line 20 24 27++ line 21 24 27
          ++ [(1,11),(2,11),(3,11),(4,11),(9,27),(36,6),(36,7),(37,5),(37,6),(37,7),(38,5)
          ,(38,6),(8,27)]
        pos_wall1 = [(4,5),(5,5),(7,5),(8,5),(14,2),(14,5),(15,8),(17,11),(22,2),(22,5),(22,8),(22,11)
          , (19,20),(20,20),(22,20),(8,34),(9,34),(11,34),(12,34),(8,43),(9,43),(11,43),(12,43)
          , (17,34),(18,34),(20,34),(21,34),(17,38),(18,38),(20,38),(21,38),(17,43),(18,43),(20,43)
          , (21,43)] ++ genPosy 36 [31,33,41,43,44] ++ genPosy 40 [31,33,34,42] ++ genPosy 44 [29,30,
          31,33,39,40,41,43,44] ++ liney 25 31 35++ liney 25 37 41 ++ liney 42 1 3
        pos_door1 = [(6,5),(15,2),(15,5),(16,8),(18,11),(21,2),(21,5),(21,8),(21,11),(21,20),(10,34)
          , (10,43),(19, 34),(19,38),(19,43),(36,25),(32,36),(32,40),(32,44),(42,36),(41,40),(42,44)]
        pos_dom_roof = genPosy 33 (List.range 8 12) ++ genPosy 33 (List.range 17 21)
          ++ genPosy 42 (List.range 8 12) ++ genPosy 37 (List.range 17 21)
          ++ genPosy 42 (List.range 17 21)
          ++[(14,1),(15,1),(14,4),(15,4),(15,7),(16,7),(17,10),(18,10),(21,1),(22,1),(21,4)
            ,(22,4),(21,7),(22,7),(21,10),(22,10)]
        pos_tree2 = genPos 0 (List.range 0 8)
          ++ [(2,8),(6,9),(11,4),(11,10),(14,13),(20,13),(2,18),(4,19),(4,22),(4,25),(4,28),(4,34)
          ,(4,43),(4,45),(4,47),(6,32),(6,34),(6,36),(6,38),(6,40),(6,43),(6,45),(10,45),(13,45),
          (16,45),(20,45), (23,45),(24,34),(24,38),(24,43),(26,2),(26,5),(26,11),(26,8),(26,18)
          ,(26,21),(26,24),(26,27),(3,30),(0,30)] ++ genPosy 15 [11,14,17,20,23]
          ++ genPosy 16 (List.range 26 45) ++ genPosy 13 [27,30,33,36,39,43,45]
          ++ genPosy 15 [27,30,33,36,39,43,45] ++ genPosy 29 [27,30,33,36,39,43,45]
          ++ genPosy 32 [27,30,33,36,39,43,45] ++ genPosy 47 [27,30,33,36,39,43,45]
          ++ genPos 43 [17,19,21,23,25,27]++ genPos 45 [17,19,21,23,25,27]
          ++ genPosy 47 [6,9,12,15,18,21,24]
        pos_tree1 = genPos 1 (List.range 0 8) ++ [(4,8),(9,1),(9,7),(9,9),(11,1),(11,7),(11,13)
          ,(17,13),(23,13)]
        pos_stone_road = [(0,9),(1,9),(2,9),(3,9),(4,9),(5,9),(5,10),(6,10),(7,10),(8,10),(7,26),(21,21)]
          ++ genPos 9 (List.range 10 15) ++ line 6 18 29 ++ line 17 17 23 ++ line 23 22 29
          ++ liney 18 7 11 ++ liney 17 11 16 ++ liney 22 18 22 ++ liney 29 7 22
          ++ line 4 35 42 ++ line 15 32 43 ++ liney 35 6 14 ++ liney 35 8 24++ liney 32 15 24
          ++ liney 39 15 24++ liney 44 6 24 ++ liney 6 26 32 ++ line 30 7 9 ++ line 31 9 10
          ++ liney 10 31 40 ++ liney 9 39 43 ++ line 39 7 8 ++ line 43 4 9 ++ liney 41 6 14
          ++ liney 1 35 42 ++ liney 2 32 35 ++ line 32 3 5 ++ line 42 2 4
          ++ line 28 35 45 ++ liney 35 26 27 ++ liney 39 26 27 ++ liney 44 26 27
          ++ liney 37 31 36 ++ liney 41 31 36 ++ line 35 37 44 ++ liney 45 29 36
          ++ liney 33 38 45 ++ liney 45 38 45  ++ liney 37 41 45++ liney 41 41 45
          ++ line 43 38 40 ++ liney 26 31 42 ++ liney 27 31 38 ++ line 34 28 29++ line 38 28 29
          ++ liney 16 0 9

        pos_road_ud = genPos 10 (List.range 1 13) ++ line 5 18 29 ++ line 5 31 45
          ++ line 25 1 13 ++ line 25 15 29 ++ line 25 31 45
          ++ line 46 2 13 ++ line 46 15 29 ++ line 46 31 45 ++ line 37 31 45
          ++ [(10,15),(10,16),(25,47)]
        pos_lane_lr = genPosy 3 (List.range 11 15) ++ genPosy 3 (List.range 21 23)
          ++genPosy 3 (List.range 17 19)
          ++genPosy 6 (List.range 11 15) ++ genPosy 6 (List.range 21 23)
          ++genPosy 9 (List.range 11 15) ++ genPosy 9 (List.range 21 23)
          ++ genPosy 12 (List.range 21 23) ++ genPosy 12 (List.range 17 19)
          ++ liney 16 18 20 ++  liney 19 26 28 ++ liney 18 30 41
          ++ [(22,17),(24,19),(30,27)] ++ genPos 24 [3,6,9,12]
        pos_lane_ud = [(16,4),(16,5),(17,7),(17,8),(16,10),(16,11),(20,4),(20,5),(20,7),(20,8)
          ,(20,10),(20,11),(23,21),(23,18),(23,20)]
          ++ line 29 19 26 ++ line 42 19 25
        pos_road_lr = liney 14 11 45 ++ liney 30 6 45 ++ liney 46 6 45 ++ liney 17 6 10
        pos_lake2 =[(1,10),(2,10),(3,10),(5,11),(6,11),(7,11), (39,5),(17,24)]
          ++ liney 19 8 11 ++ liney 18 13 15 ++ liney 23 19 21
        pos_lake4 = genPos 0 [11,12,13,14] ++ line 7 20 24
          ++[(8,26),(35,7)]
        pos_lake6 = genPos 8 [12,14,13] ++ line 16 19 23 ++ line 22 24 27 ++ [(38,7)]
        pos_lake8 = [(4,15),(1,15),(2,15),(3,15),(5,15),(6,15),(7,15),(36,8),(37,8),(39,6)]
          ++ liney 28 8 21
        pos_roof2 = [(5,2),(6,2),(7,2),(32,19),(36,19),(40,19),(32,19),(36,19),(40,19),(2,35)]
          ++ liney 22 32 40 ++ genPosy 34 [31,32,41,42,43] ++ genPosy 38 [31,32,33,41]
          ++ genPosy 42 [31,32,41,42,43]
        pos_roof4 = [(4,3), (31,20),(35,20),(39,20),(31,23),(19,18)] ++ line 1 36 40 ++ line 29 35 42
          ++ line 39 35 42
        pos_roof5 = [(5,3),(6,3),(7,3),(32,20),(36,20),(40,20)]++ liney 23 32 40 ++ line 2 36 40
        pos_roof6 = [(8,3),(33,20),(37,20),(41,20),(41,23),(20,18)] ++ line 3 36 40 ++ line 30 35 42
          ++ line 40 35 42
        pos_roof8 = liney 24 32 40 ++ genPosy 35 [31,32,41,42,43] ++ genPosy 39 [31,32,33,41]
          ++ genPosy 43 [31,32,41,42,43] ++ [(5,4),(6,4),(7,4),(2,41)]++ genPosy 21 [32,36,40]
        pos_river_lr = liney 31 1 4 ++ liney 31 6 24 ++ liney 31 26 36 ++ liney 31 38 45
        pos_river_ud = line 47 0 30 ++ line 47 32 48
        pos_grass4 = genPos 7 [33,34,43,42] ++ genPos 13 [33,34,43,42] ++ genPos 16 [33,34,43,42,37,38]
         ++genPos 22 [33,34,43,42,37,38] ++ line 34 7 9 ++ liney 32 7 13 ++ liney 41 16 22
        pos_grass9 = line 33 4 9 ++ line 43 2 3++ liney 10 41 43
        pos_tree5 = liney 1 28 34++ line 28 2 5 ++ line 28 7 11 ++ liney 11 29 44 ++ line 44 1 10
          ++ [(43,1)]
        pos_tree8 = liney 2 36 41 ++ line 31 4 5 ++ [(30,5)]
        pos_tree9 = line 29 4 5 ++ line 29 7 8 ++ genPosy 3 [30,31,33,34,35,40,41]
        pos_tree10 = genPos 29 [2,3,9,10]++ genPos 30 [2,10]
        pos_flower = liney 37 8 13 ++ genPosy 39 [8,9,11,12,13] ++[(9,38),(12,38)]
          ++ line 18 5 8 ++ line 19 5 8
        pos_car1 = line 27 21 25 ++ genPosy 17 [31,33,35,37,39,41]++ line 27 37 38 ++ line 27 41 43
          ++ line 34 34 36 ++ line 35 34 36 ++ liney 27 39 42 ++ liney 28 39 42 ++ line 44 38 40
        blocks = List.map (Block "lake1" False False Nothing Nothing) [(0,10),(7,19),(7,27),(12,18),(18,23),(37,4)
          ,(36,5),(35,6)]
          ++  List.map (Block "lake2" False False Nothing Nothing) pos_lake2
          ++  List.map (Block "lake3" False False Nothing Nothing) [(4,10),(8,11), (16,18),(40,5),(22,23),(38,4)]
          ++  List.map (Block "lake4" False False Nothing Nothing) pos_lake4
          ++  List.map (Block "lake5" False False Nothing Nothing) pos_lake5
          ++  List.map (Block "lake6" False False Nothing Nothing) pos_lake6
          ++  List.map (Block "lake7" False False Nothing Nothing) [(0,15), (7,25),(7,28),(35,8)]
          ++  List.map (Block "lake8" False False Nothing Nothing) pos_lake8
          ++  List.map (Block "lake9" False False Nothing Nothing) [(8,15),(22,28),(38,8),(40,6)]
          ++  List.map (Block "grass2" True False Nothing Nothing)  pos_grass2
          ++  List.map (Block "grass1" True False Nothing Nothing) [(17,4),(17,5),(18,4),(18,9),(19,4),(19,9),(19,10),(19,11)]
          ++  List.map (Block "wall1" False False Nothing Nothing) pos_wall1
          ++  List.map (Block "door1" False False Nothing Nothing) pos_door1
          ++  List.map (Block "dom_roof" False False Nothing Nothing) pos_dom_roof
          ++  List.map (Block "tree1" False False Nothing Nothing) pos_tree1
          ++  List.map (Block "tree2" False False Nothing Nothing) pos_tree2
          ++  List.map (Block "stone_road" True False Nothing Nothing) pos_stone_road
          ++  List.map (Block "road_ud" True False Nothing Nothing) pos_road_ud
          ++  List.map (Block "road_lr" True False Nothing Nothing) pos_road_lr
          ++  List.map (Block "road_udr" True False Nothing Nothing) [(10,14),(5,30)]
          ++  List.map (Block "road_ul" True False Nothing Nothing) [(46,46)]
          ++  List.map (Block "road_udl" True False Nothing Nothing) [(46,14),(46,30)]
          ++  List.map (Block "road_dr" True False Nothing Nothing) [(5,17)]
          ++  List.map (Block "road_d" True False Nothing Nothing) [(10,0),(25,0)]
          ++  List.map (Block "road_udlr" True False Nothing Nothing) [(25,14),(25,30),(25,46)]
          ++  List.map (Block "road_dlr" True False Nothing Nothing) [(37,30)]
          ++  List.map (Block "road_ur" True False Nothing Nothing) [(5,46)]
          ++  List.map (Block "road_ulr" True False Nothing Nothing) [(37,46)]
          ++  List.map (Block "road_ul" True False Nothing Nothing) [(10,17)]
          ++  List.map (Block "lane_lr" True False Nothing Nothing) pos_lane_lr
          ++  List.map (Block "lane_ud" True False Nothing Nothing) pos_lane_ud
          ++  List.map (Block "lane_dr" True False Nothing Nothing) [(17,16),(29,18)]
          ++  List.map (Block "lane_dlr" True False Nothing Nothing) [(16,3),(20,3),(16,9)]
          ++  List.map (Block "lane_ulr" True False Nothing Nothing) [(16,6),(20,12)]
          ++  List.map (Block "lane_udr" True False Nothing Nothing) [(20,6),(20,9),(23,19)]
          ++  List.map (Block "lane_udl" True False Nothing Nothing) [(29,19)]
          ++  List.map (Block "lane_ur" True False Nothing Nothing) [(16,12),(29,27),(21,17)]
          ++  List.map (Block "lane_dl" True False Nothing Nothing) [(17,6),(42,18),(21,16),(23,17)]
          ++  List.map (Block "lane_ul" True False Nothing Nothing) [(17,9)]
          ++  List.map (Block "roof1" False False Nothing Nothing) [(4,2),(31,19),(35,19),(39,19),(31,22),(29,34)
            ,(39,34),(1,35), (19,17),(21,18)]
          ++  List.map (Block "roof2" False False Nothing Nothing) pos_roof2
          ++  List.map (Block "roof3" False False Nothing Nothing) [(8,2),(33,19),(37,19),(41,19),(33,19),(37,19),(41,19),(22,18)
            , (33,34),(34,38),(33,42),(44,34),(42,38),(44,42),(3,35),(30,34),(40,34),(41,22),(20,17)]
          ++  List.map (Block "roof4" False False Nothing Nothing) pos_roof4
          ++  List.map (Block "roof5" False False Nothing Nothing) pos_roof5
          ++  List.map (Block "roof6" False False Nothing Nothing) pos_roof6
          ++  List.map (Block "roof7" False False Nothing Nothing) [(4,4),(19,19),(1,41),(29,43),(39,43),(21,19)
            ,(31,21),(35,21),(39,21),(31,24)]
          ++  List.map (Block "roof8" False False Nothing Nothing) pos_roof8
          ++  List.map (Block "roof9" False False Nothing Nothing) [(8,4),(20,19),(3,41),(33,21),(37,21),(41,21)
            ,(33,35),(44,35),(33,43),(44,43),(34,39),(42,39),(30,43),(40,43),(41,24),(22,19)]
          ++  List.map (Block "home" True False Nothing Nothing) [(13,12)]
          ++  List.map (Block "river_ud" False False Nothing Nothing) pos_river_ud
          ++  List.map (Block "river_lr" False False Nothing Nothing) pos_river_lr
          ++  List.map (Block "river_r" False False Nothing Nothing) [(0,31)]
          ++  List.map (Block "river_d" False False Nothing Nothing) [(47,0)]
          ++  List.map (Block "river_udl" False False Nothing Nothing) [(47,31)]
          ++  List.map (Block "grass1" True False Nothing Nothing) ([(35,5)] ++ line 34 4 6)
          ++  List.map (Block "grass3" True False Nothing Nothing) (liney 9 35 38)
          ++  List.map (Block "grass4" True False Nothing Nothing) pos_grass4
          ++  List.map (Block "grass7" True False Nothing Nothing) (line 41 4 6 ++ liney 4 39 40)
          ++  List.map (Block "grass8" True False Nothing Nothing) (liney 4 35 36 ++ liney 3 36 39 ++ line 31 7 8)
          ++  List.map (Block "grass9" True False Nothing Nothing) pos_grass9
          ++  List.map (Block "tree5" False False Nothing Nothing) pos_tree5
          ++  List.map (Block "tree8" False False Nothing Nothing) pos_tree8
          ++  List.map (Block "tree9" False False Nothing Nothing) pos_tree9
          ++  List.map (Block "tree10" False False Nothing Nothing) pos_tree10
          ++  List.map (Block "tree12" False False Nothing Nothing) (line 42 5 8 ++ [(31,2),(30,4)]++ line 32 7 9)
          ++  List.map (Block "flower" True False Nothing Nothing)  pos_flower
          ++  List.map (Block "yard1" False False Nothing Nothing) [(27,0),(-1,-1)]
          ++  List.map (Block "yard2" False False Nothing Nothing) (liney 0 28 44++liney -1 0 47)
          ++  List.map (Block "yard3" False False Nothing Nothing) [(45,0) ,(48,-1)]
          ++  List.map (Block "yard4" False False Nothing Nothing) (line 27 1 5 ++ line 27 7 11++ line -1 0 47)
          ++  List.map (Block "yard6" False False Nothing Nothing) (line 45 1 11++ line 48 0 47)
          ++  List.map (Block "yard7" False False Nothing Nothing) [(27,12),(-1,48)]
          ++  List.map (Block "yard8" False False Nothing Nothing) (liney 12 28 44 ++ liney 48 0 24++liney 48 26 46)
          ++  List.map (Block "yard9" False False Nothing Nothing) [(45,12),(48,48)]
          ++ List.map (Block "car1" False False Nothing Nothing) pos_car1
          ++ List.map (Block "boat1" True False Nothing Nothing) [(17,24),(39,6)]
          ++ List.map (Block "gate" False False Nothing Nothing) [(25,48)]

    in
    blocks

mapDream1 : List Block
mapDream1 =
    let
        pos_back = genPos 0 ((List.range 0 6) ++ (List.range 8 14))
            ++ genPos 1 ([0,1] ++ (List.range 3 11) ++ [13,14])
            ++ genPos 2 (List.range 0 14)
            ++ genPos 3 ((List.range 0 4) ++ (List.range 9 14))
            ++ genPos 4 ((List.range 0 3) ++ [9] ++ (List.range 11 14))
            ++ genPos 5 ((List.range 1 3) ++ [9] ++ (List.range 11 14))
            ++ genPos 6 ((List.range 0 3) ++ (List.range 9 14))
            ++ genPos 7 ((List.range 0 4) ++ (List.range 9 14))
            ++ genPos 8 ([0] ++ (List.range 2 4) ++ [6,7] ++ [9,10] ++ (List.range 12 14))
            ++ genPos 9 ((List.range 0 4) ++ [7] ++ (List.range 9 14))
            ++ genPos 10 (List.range 0 14)
            ++ genPos 11 ((List.range 0 5) ++ (List.range 8 14))
            ++ genPos 12 ([0,1] ++ (List.range 3 9) ++ (List.range 11 14))
            ++ genPos 13 ((List.range 0 4) ++ [6,7] ++ (List.range 9 12) ++ [14])
            ++ genPos 14 ((List.range 0 4) ++ (List.range 9 12) ++ [14])
            ++ genPos 15 ((List.range 0 12) ++ [14])
            ++ genPos 16 ((List.range 0 7) ++ (List.range 9 14))
            ++ genPos 17 ([0] ++ [2,3] ++ (List.range 5 6) ++ (List.range 8 14))
            ++ genPos 18 ((List.range 0 3) ++ (List.range 5 7) ++[9,10,11]++[13,14])
            ++ genPos 19 (List.range 0 14)
        pos_grass =  [(0,7),(1,12),(5,0),(8,1),(8,11),(12,10),(17,1),(18,12)]
        pos_fence1 = [(8,5),(8,8),(9,5),(9,8),(13,5),(13,8)]
        pos_fence2 = [(14,5),(14,8)]
        pos_fence3 = [(14,6),(14,7)]
        pos_barrel = [(1,2),(9,6),(13,13),(14,13),(15,13)]
        pos_wood1 = [(4,10),(17,4)]
        pos_wood2 = [(5,10),(18,4)]
        pos_tree = [(12,2)]
        pos_chop = [(16,8),(17,7),(18,8)]
        pos_snowman1 = [(11,6)]
        pos_snowman2 = [(11,7)]
        pos_house1 = [(5,4)]
        pos_house2 = [(5,5)]
        pos_yancong1 = [(4,4)]
        pos_yancong2 = [(4,5)]
        pos_house4 = [(6,4)]
        pos_house5 = [(3,5)]
        pos_house7 = [(6,5)]
        pos_house8 = [(7,5)]
        pos_house9 = [(5,6)]
        pos_house10 = [(3,6)]
        pos_house11 = [(4,6)]
        pos_house12 = [(6,6)]
        pos_house13 = [(7,6)]
        pos_house14 = [(5,7)]
        pos_house15 = [(3,7)]
        pos_house16 = [(4,7)]
        pos_house17 = [(6,7)]
        pos_house18 = [(7,7)]
        pos_house19 = [(5,8)]
        pos_house20 = [(3,8)]
        pos_house21 = [(4,8),(6,8)]
        pos_house22 = [(7,8)]
        blocks = List.map (Block "backsnow" True False Nothing Nothing) pos_back
          ++  List.map (Block "grass" True False Nothing Nothing) pos_grass
          ++  List.map (Block "fence1" False False Nothing Nothing) pos_fence1
          ++  List.map (Block "fence2" False False Nothing Nothing) pos_fence2
          ++  List.map (Block "fence3" False False Nothing Nothing) pos_fence3
          ++  List.map (Block "barrel" False False Nothing Nothing) pos_barrel
          ++  List.map (Block "wood1" False False Nothing Nothing) pos_wood1
          ++  List.map (Block "wood2" False False Nothing Nothing) pos_wood2
          ++  List.map (Block "tree" False False Nothing Nothing) pos_tree
          ++  List.map (Block "chop" False False Nothing Nothing) pos_chop
          ++  List.map (Block "snowman1" False False Nothing Nothing) pos_snowman1
          ++  List.map (Block "snowman2" False False Nothing Nothing) pos_snowman2
          ++  List.map (Block "house1" False False Nothing Nothing) pos_house1
          ++  List.map (Block "house2" False False Nothing Nothing) pos_house2
          ++  List.map (Block "yancong1" False False Nothing Nothing) pos_yancong1
          ++  List.map (Block "yancong2" False False Nothing Nothing) pos_yancong2
          ++  List.map (Block "house4" False False Nothing Nothing) pos_house4
          ++  List.map (Block "house5" False False Nothing Nothing) pos_house5
          ++  List.map (Block "house7" False False Nothing Nothing) pos_house7
          ++  List.map (Block "house8" False False Nothing Nothing) pos_house8
          ++  List.map (Block "house9" False False Nothing Nothing) pos_house9
          ++  List.map (Block "house10" False False Nothing Nothing) pos_house10
          ++  List.map (Block "house11" False False Nothing Nothing) pos_house11
          ++  List.map (Block "house12" False False Nothing Nothing) pos_house12
          ++  List.map (Block "house13" False False Nothing Nothing) pos_house13
          ++  List.map (Block "house14" False False Nothing Nothing) pos_house14
          ++  List.map (Block "house15" False False Nothing Nothing) pos_house15
          ++  List.map (Block "house16" False False Nothing Nothing) pos_house16
          ++  List.map (Block "house17" False False Nothing Nothing) pos_house17
          ++  List.map (Block "house18" False False Nothing Nothing) pos_house18
          ++  List.map (Block "house19" False False Nothing Nothing) pos_house19
          ++  List.map (Block "house20" False False Nothing Nothing) pos_house20
          ++  List.map (Block "house21" False False Nothing Nothing) pos_house21
          ++  List.map (Block "house22" False False Nothing Nothing) pos_house22

    in
    blocks

genBlo : String -> Bool -> Bool -> ((Int,Int) -> Block)
genBlo name movable pushable =
    Block name movable pushable Nothing Nothing

mapLisa1 : List Block
mapLisa1 =
    let
        blocks =List.map (genBlo "wall1" False False) (line 0 0 11 ++ line 17 0 11 ++ liney 0 1 16
            ++ liney 11 1 16)
          ++ List.map (genBlo "V" False False) [(5,5)]
          ++ List.map (genBlo "E" True True) [(6,4)]
          ++ List.map (genBlo "C" True True) [(7,3)]
          ++ List.map (genBlo "T" True True) [(8,9)]
          ++ List.map (genBlo "O" True True) [(9,7)]
          ++ List.map (genBlo "R" False False) [(10,5)]
    in
    blocks

mapLisa2 : List Block
mapLisa2 =
    let
        blocks =List.map (genBlo "wall1" False False) (line 0 0 11 ++ line 17 0 11 ++ liney 0 1 16
            ++ liney 11 1 16)
          ++ List.map (genBlo "rope" True False) (genPosy 1 [3,11]++ genPosy 2 [2,4,6,13]
            ++genPosy 3 [5,9,10,15])
          ++ List.map (genBlo "lever1" True False) [(2,2),(6,2),(5,3),(10,3)]
          ++ List.map (genBlo "lever2" True False) ([(3,2)]
            ++ liney 2 7 12 ++ liney 3 6 8 ++ liney 3 11 14)
          ++ List.map (genBlo "lever3" True False) [(4,2),(9,3),(13,2),(15,3)]
          ++ List.map (genBlo "weigh" True True) ([(2,3),(4,3)] ++ line 5 4 6 ++ line 9 4 6
            ++ line 10 4 6)
          ++ List.map (genBlo "weigh" False False) (line 15 4 6)
    in
    blocks

mapLisa3 =
    let
        blocks =List.map (genBlo "wall1" False False) (line 0 1 11 ++ line 15 0 11 ++ liney 1 1 14
            ++ liney 11 1 14)
          ++ List.map (genBlo "O" True True) [(1,0),(2,7),(8,0)]
          ++ List.map (genBlo "U" True True) [(2,0),(6,2),(8,5),(6,14)]
          ++ List.map (genBlo "T" True True) [(3,0),(5,4),(8,2)]
          ++ List.map (genBlo "E" True True) [(4,0),(1,2),(11,2)]
          ++ List.map (genBlo "R" True True) [(5,0),(5,2),(12,3),(9,0)]
          ++ List.map (genBlo "W" True True) [(7,0)]
          ++ List.map (genBlo "D" True True) [(11,0),(5,14)]
          ++ List.map (genBlo "L" True True) [(10,0),(2,2)]
          ++ List.map (genBlo "M" True True) [(3,2),(10,2)]
          ++ List.map (genBlo "N" True True) [(7,2),(10,4),(5,15)]
          ++ List.map (genBlo "I" True True) [(9,2),(13,8),(4,14),(4,8)]
          ++ List.map (genBlo "S" True True) [(2,4),(1,10),(8,8),(5,13)]
          ++ List.map (genBlo "B" True True) [(3,5),(2,14)]
          ++ List.map (genBlo "P" True True) [(5,6)]
          ++ List.map (genBlo "C" True True) [(12,6),(5,12)]
          ++ List.map (genBlo "B" True True) [(3,5),(2,14)]
          ++ List.map (genBlo "A" True True) [(3,14)]
          ++ List.map (genBlo "bomb" False False) [(9,13)]
    in
    blocks

mapLisa4 =
    let
        blocks =List.map (genBlo "wall1" False False) (line 0 0 12 ++ line 18 0 12 ++ liney 0 0 17
            ++ liney 12 0 17 ++ line 9 1 7 ++ line 9 9 11 ++ line 3 6 9 ++ line 11 6 9
            ++ [(2,9),(4,6),(12,6),(12,9),(13,7),(13,8)])
          ++ List.map (genBlo "E" True True) [(5,3)]
          ++ List.map (genBlo "H" True True) [(2,3)]
          ++ List.map (genBlo "W" True True) [(3,3)]
          ++ List.map (genBlo "one" True True) [(4,3),(7,3)]
          ++ List.map (genBlo "X" True True) [(6,3)]
          ++ List.map (genBlo "equal" True True) [(6,8)]
          ++ List.map (genBlo "answer" True False) (genPosy 9 [6,7,15,16])
    in
    blocks

