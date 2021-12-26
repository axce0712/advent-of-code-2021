open System
open System.IO

type Range = Range of Int64 * Int64

type Cuboid = Cuboid of Range * Range * Range

type OptionBuilder () =
    member _.Bind(x, f) = Option.bind f x
    member _.Map(x, f) = Option.map f x
    member _.Return(x) = Some x
    member _.ReturnFrom(x) = x

type Instruction =
    | On of Cuboid
    | Off of Cuboid

let parseRange (input : string) =
    let [| fromPart; toPart |] = input.Split ("..", StringSplitOptions.RemoveEmptyEntries)
    Range (int fromPart, int toPart)

let parseCuboid (input : string) =
    let [| xPart; yPart; zPart |] = input.Split ','
    let rx = parseRange xPart[2..]
    let ry = parseRange yPart[2..]
    let rz = parseRange zPart[2..]
    Cuboid (rx, ry, rz)

let parseInstruction (input : string) =
    match input.Split ' ' with
    | [| "on"; cuboidPart |] -> On (parseCuboid cuboidPart)
    | [| "off"; cuboidPart |] -> Off (parseCuboid cuboidPart)
    | _ -> invalidArg (nameof input) input

let option = OptionBuilder()

let range (f, t) =
    if f > t then Range (t, f) else Range (f, t)

let cuboid (xf, xt) (yf, yt) (zf, zt) =
    Cuboid (range (xf, xt), range (yf, yt), range (zf, zt))

let toListR (Range (f, t)) = [f..t]

let toListC (Cuboid (rx, ry, rz)) =
    [
        for x in toListR rx do
            for y in toListR ry do
                for z in toListR rz do
                    x, y, z
    ]

let isBetween p (Range (f, t)) =
    f <= p && p <= t

let isBetweenR (Range (f1, t1) as range1) (Range (f2, t2) as range2) =
    (isBetween f1 range2 && isBetween t1 range2) || (isBetween f2 range1 && isBetween t2 range1)

let isBetweenC (Cuboid (rx1, ry1, rz1)) (Cuboid (rx2, ry2, rz2)) =
    isBetweenR rx1 rx2 && isBetweenR ry1 ry2 && isBetweenR rz1 rz2

let intersectR (Range (f1, t1) as range1) (Range (f2, t2)) =
    if isBetween f2 range1 then
        let tMin = min t1 t2
        Some (range (f2, tMin))
    elif isBetween t2 range1 then
        Some (range (f1, t2))
    else
        None

let intersectC (Cuboid (rx1, ry1, rz1)) (Cuboid (rx2, ry2, rz2)) = 
    option {
        let! rx = intersectR rx1 rx2
        let! ry = intersectR ry1 ry2
        let! rz = intersectR rz1 rz2
        return Cuboid (rx, ry, rz)
    }

let turnOn (added, subtracted) c =
    let newAdded = c :: added @ (List.choose (intersectC c) subtracted)
    let newSubtracted = subtracted @ (List.choose (intersectC c) added)
    newAdded, newSubtracted

let turnOff (added, subtracted) c =
    let newAdded = added @ (List.choose (intersectC c) subtracted)
    let newSubtracted = subtracted @ (List.choose (intersectC c) added)
    newAdded, newSubtracted

let countR (Range (f, t)) = t - f + 1L

let countC (Cuboid (rx, ry, rz)) =
    countR rx * countR ry * countR rz

let count (added, subtracted) =
    List.sumBy countC added - List.sumBy countC subtracted

let flip f x y = f y x

let solve instructions =
    instructions
    |> Seq.fold (fun acc ->
        function
        | On c -> turnOn acc c
        | Off c -> turnOff acc c) ([], [])
    |> count

([ cuboid (10, 12) (10, 12) (10, 12) ], [])
|> flip turnOn (cuboid (11, 13) (11, 13) (11, 13))
|> flip turnOff (cuboid (9, 11) (9, 11) (9, 11))
|> flip turnOn (cuboid (10, 10) (10, 10) (10, 10))

([ cuboid (10, 10) (10, 10) (10, 10) ], [])
|> flip turnOn (cuboid (10, 10) (10, 10) (10, 11))

let content =
    "on x=-5..47,y=-31..22,z=-19..33
on x=-44..5,y=-27..21,z=-14..35
on x=-49..-1,y=-11..42,z=-10..38
on x=-20..34,y=-40..6,z=-44..1
off x=26..39,y=40..50,z=-2..11
on x=-41..5,y=-41..6,z=-36..8
off x=-43..-33,y=-45..-28,z=7..25
on x=-33..15,y=-32..19,z=-34..11
off x=35..47,y=-46..-34,z=-11..5
on x=-14..36,y=-6..44,z=-16..29
on x=-57795..-6158,y=29564..72030,z=20435..90618
on x=36731..105352,y=-21140..28532,z=16094..90401
on x=30999..107136,y=-53464..15513,z=8553..71215
on x=13528..83982,y=-99403..-27377,z=-24141..23996
on x=-72682..-12347,y=18159..111354,z=7391..80950
on x=-1060..80757,y=-65301..-20884,z=-103788..-16709
on x=-83015..-9461,y=-72160..-8347,z=-81239..-26856
on x=-52752..22273,y=-49450..9096,z=54442..119054
on x=-29982..40483,y=-108474..-28371,z=-24328..38471
on x=-4958..62750,y=40422..118853,z=-7672..65583
on x=55694..108686,y=-43367..46958,z=-26781..48729
on x=-98497..-18186,y=-63569..3412,z=1232..88485
on x=-726..56291,y=-62629..13224,z=18033..85226
on x=-110886..-34664,y=-81338..-8658,z=8914..63723
on x=-55829..24974,y=-16897..54165,z=-121762..-28058
on x=-65152..-11147,y=22489..91432,z=-58782..1780
on x=-120100..-32970,y=-46592..27473,z=-11695..61039
on x=-18631..37533,y=-124565..-50804,z=-35667..28308
on x=-57817..18248,y=49321..117703,z=5745..55881
on x=14781..98692,y=-1341..70827,z=15753..70151
on x=-34419..55919,y=-19626..40991,z=39015..114138
on x=-60785..11593,y=-56135..2999,z=-95368..-26915
on x=-32178..58085,y=17647..101866,z=-91405..-8878
on x=-53655..12091,y=50097..105568,z=-75335..-4862
on x=-111166..-40997,y=-71714..2688,z=5609..50954
on x=-16602..70118,y=-98693..-44401,z=5197..76897
on x=16383..101554,y=4615..83635,z=-44907..18747
off x=-95822..-15171,y=-19987..48940,z=10804..104439
on x=-89813..-14614,y=16069..88491,z=-3297..45228
on x=41075..99376,y=-20427..49978,z=-52012..13762
on x=-21330..50085,y=-17944..62733,z=-112280..-30197
on x=-16478..35915,y=36008..118594,z=-7885..47086
off x=-98156..-27851,y=-49952..43171,z=-99005..-8456
off x=2032..69770,y=-71013..4824,z=7471..94418
on x=43670..120875,y=-42068..12382,z=-24787..38892
off x=37514..111226,y=-45862..25743,z=-16714..54663
off x=25699..97951,y=-30668..59918,z=-15349..69697
off x=-44271..17935,y=-9516..60759,z=49131..112598
on x=-61695..-5813,y=40978..94975,z=8655..80240
off x=-101086..-9439,y=-7088..67543,z=33935..83858
off x=18020..114017,y=-48931..32606,z=21474..89843
off x=-77139..10506,y=-89994..-18797,z=-80..59318
off x=8476..79288,y=-75520..11602,z=-96624..-24783
on x=-47488..-1262,y=24338..100707,z=16292..72967
off x=-84341..13987,y=2429..92914,z=-90671..-1318
off x=-37810..49457,y=-71013..-7894,z=-105357..-13188
off x=-27365..46395,y=31009..98017,z=15428..76570
off x=-70369..-16548,y=22648..78696,z=-1892..86821
on x=-53470..21291,y=-120233..-33476,z=-44150..38147
off x=-93533..-4276,y=-16170..68771,z=-104985..-24507"

#time
File.ReadLines (Path.Combine (__SOURCE_DIRECTORY__, "input.txt"))
|> Seq.map parseInstruction
|> solve
#time

// 4411366377266339L