module Program

open System
open System.Drawing
open System.Drawing.Imaging
open System.Text
open System.Threading
open System.Threading.Tasks
open System.Windows.Forms

open Microsoft.FSharp.NativeInterop

let random = Random ()

/// Identifies the state of a single cell in a world.
type Cell = 
    | Dead = 0
    | Young = 1
    | Old = 75

/// The number of ages possible for a cell.
let ageRange = int Cell.Old - int Cell.Young + 1

/// Determines the age of a live cell.
let age (cell : Cell) = int cell - 1

/// A two-dimensional grid of cells.
type World = Cell[,]

/// Creates a random world populated entirely of young cells with the given fill ratio.
let randomWorld width height fill =
    Array2D.init width height (fun _ _ -> if random.NextDouble () < fill then Cell.Young else Cell.Dead)

/// Defines a rule for a cellular automata simulation.
type Rule (weights : int[], lifeMin : int, birthMin : int, lifeMax : int) =

    /// The influence of each of a cell's neighbors on its life value.
    member this.Weights = weights

    /// The minimum life value needed for a live cell to remain alive.
    member this.LifeMin = lifeMin

    /// The minimum life value needed for a dead cell to become alive.
    member this.BirthMin = birthMin

    /// The maximum life value possible for a live cell to remain alive (or a dead cell to
    /// become alive).
    member this.LifeMax = lifeMax

/// Creates a random rule (untested) for a simulation.
let randomRule () =
    let weights = Array.zeroCreate 8
    let baseWeight = random.Next 30 - 10
    let randomWeight = random.Next 20

    // Apply base and random weights.
    for i = 0 to 7 do
        weights.[i] <- baseWeight + random.Next randomWeight

    // Apply individual weights.
    while random.NextDouble () < 0.4 do
        let i = random.Next 8
        weights.[i] <- weights.[i] + random.Next 40 - 20

    // Create rule parameters.
    let lifeMin = random.Next 30
    let birthMin = max lifeMin 1 + random.Next 30
    let lifeMax = birthMin + random.Next 30
    Rule (weights, lifeMin, birthMin, lifeMax)

/// The rule for Conway's game of life.
let gol = Rule (Array.create 8 1, 2, 3, 3)

/// Evaluates the next iteration of the given world with the given rule.
let stepEx (births : int byref) (count : int byref) (rule : Rule) (world : World) =
    let width = world.GetLength 0
    let height = world.GetLength 1
    let result = Array2D.zeroCreate width height
    let weights = rule.Weights
    let lifeValue (x, y) =
        let inline isAlive (x, y) = world.[(x + width) % width, (y + height) % height] <> Cell.Dead
        let mutable count = 0
        if isAlive (x + 1, y) then count <- count + weights.[0]
        if isAlive (x - 1, y) then count <- count + weights.[1]
        if isAlive (x, y + 1) then count <- count + weights.[2]
        if isAlive (x, y - 1) then count <- count + weights.[3]
        if isAlive (x + 1, y + 1) then count <- count + weights.[4]
        if isAlive (x - 1, y + 1) then count <- count + weights.[5]
        if isAlive (x + 1, y - 1) then count <- count + weights.[6]
        if isAlive (x - 1, y - 1) then count <- count + weights.[7]
        count
    for x = 0 to width - 1 do
        for y = 0 to height - 1 do
            let lifeValue = lifeValue (x, y)
            if world.[x, y] = Cell.Dead then
                if lifeValue >= rule.BirthMin && lifeValue <= rule.LifeMax then
                    result.[x, y] <- Cell.Young
                    births <- births + 1
                    count <- count + 1
            else
                if lifeValue >= rule.LifeMin && lifeValue <= rule.LifeMax then
                    result.[x, y] <- enum (int world.[x, y] + 1)
                    count <- count + 1
    result

/// Evaluates the next iteration of the given world with the given rule.
let step rule world =
    let mutable dummy = 0
    stepEx &dummy &dummy rule world

/// Determines how closely a value between 0.0 and 1.0 fits with the given target value, returning 1.0
/// for a perfect fit and 0.0 for a bad fit.
let goodness target value =
    let offset =
        if value < target then -(target - value) / target
        else (value - target) / (1.0 - target)
    1.0 - offset * offset

/// Tests a rule to determine whether it has the aesthetic qualities needed to be shown to a human. The
/// smaller the returned value, the uglier the rule is.
let test (rule : Rule) =
    let size = 20
    let mutable world = randomWorld size size 0.3

    let mutable births = 0
    let mutable dummy = 0
    for i = 0 to 170 do world <- stepEx &births &dummy rule world

    let worldBase = world
    for i = 0 to 20 do world <- stepEx &births &dummy rule world

    let mutable population = 0
    let mutable totalAge = 0
    let mutable changed = 0
    for x = 0 to size - 1 do
        for y = 0 to size - 1 do
            if worldBase.[x, y] = Cell.Dead && world.[x, y] <> Cell.Dead then
                changed <- changed + 1
            if worldBase.[x, y] <> Cell.Dead && world.[x, y] = Cell.Dead then
                changed <- changed + 1
            if world.[x, y] <> Cell.Dead then
                population <- population + 1
                totalAge <- totalAge + age world.[x, y]
    let changedDensity = float changed / float (size * size)
    let normalizedChangedDensity = min 1.0 (changedDensity * 2.0)
    if population = 0 then -infinity else
        let density = float population / float (size * size)
        let averageAge = float totalAge / float population
        let mutable ageDeviation = 0.0
        for x = 0 to size - 1 do
            for y = 0 to size - 1 do
                if world.[x, y] <> Cell.Dead then
                    let deviation = float (age world.[x, y]) - averageAge
                    ageDeviation <- ageDeviation + deviation * deviation
        ageDeviation <- ageDeviation / float population
        (-0.3 * log (float births) +
            0.5 * log ageDeviation +
            0.7 * log (goodness 0.3 (averageAge / float ageRange)) +
            0.8 * log (goodness 0.2 density) +
            1.3 * log (goodness 0.1 normalizedChangedDensity))

/// Creates a random rule with the given minimum test threshold.
let rec randomGoodRule threshold =
    let rule = randomRule ()
    if test rule >= threshold then rule else randomGoodRule threshold

/// Defines a color scheme for a simulation.
type Scheme = (uint8 * uint8 * uint8)[]

/// Creates a random color scheme.
let randomScheme () : Scheme =
    let mults = Array.zeroCreate 6
    for i = 0 to 5 do
        mults.[i] <- random.NextDouble ()
    Array.init ageRange (fun i ->
        let x = float i / float ageRange
        let p = x * 2.0 * Math.PI
        let r = cos (2.0 * p * mults.[0] + 2.0 * Math.PI * mults.[3]) * 0.5 + 0.5
        let g = cos (2.0 * p * mults.[1] + 2.0 * Math.PI * mults.[4]) * 0.5 + 0.5
        let b = cos (2.0 * p * mults.[2] + 2.0 * Math.PI * mults.[5]) * 0.5 + 0.5
        let s = 1.0 - x * 0.5
        let r = uint8 (r * s * 255.0)
        let g = uint8 (g * s * 255.0)
        let b = uint8 (b * s * 255.0)
        (r, g, b))

type Window () as this =
    inherit Form ()
    let width = 650
    let height = 250
    let scale = 2
    let bitmap = new Bitmap (width, height)

    do
        this.Text <- "Herp of Derp"
        this.ClientSize <- new Size (width * scale, height * scale)
        this.FormBorderStyle <- FormBorderStyle.Fixed3D
        this.StartPosition <- FormStartPosition.CenterScreen
        this.SetStyle (ControlStyles.AllPaintingInWmPaint, true)
        this.SetStyle (ControlStyles.UserPaint, true)
        this.SetStyle (ControlStyles.OptimizedDoubleBuffer, true)

    let mutable generation = 0
    let mutable rule = gol
    let mutable colors = randomScheme ()
    let mutable world = randomWorld width height 0.1
    let mutable nextRule = Unchecked.defaultof<Rule>

    member this.Update () =
        let getNextRule = new Thread (fun () -> nextRule <- randomGoodRule 0.0)
        if generation = 0 then getNextRule.Start ()
        generation <- generation + 1
        if generation > 200 then
            rule <- nextRule
            colors <- randomScheme ()
            generation <- 0
        else
            let mutable dummy = 0
            let mutable count = 0
            world <- stepEx &dummy &count rule world
            if count = 0 then
                world <- randomWorld width height 0.1
                rule <- randomGoodRule 0.0
                colors <- randomScheme ()
                generation <- 0
        this.Refresh ()

    override this.OnPaint (e : PaintEventArgs) =
        let bitmapData = bitmap.LockBits (Drawing.Rectangle (0, 0, width, height), ImageLockMode.WriteOnly, PixelFormat.Format24bppRgb)
        let ptr = NativePtr.ofNativeInt bitmapData.Scan0

        for j = 0 to height - 1 do
            for i = 0 to width - 1 do
                let ptr = NativePtr.add ptr (i * 3 + j * bitmapData.Stride)
                if world.[i, j] <> Cell.Dead then
                    let r, g, b = colors.[(age world.[i, j]) % int Cell.Old]
                    NativePtr.set ptr 0 b
                    NativePtr.set ptr 1 g
                    NativePtr.set ptr 2 r
                else ()

        bitmap.UnlockBits bitmapData
        e.Graphics.CompositingQuality <- Drawing2D.CompositingQuality.HighSpeed
        e.Graphics.CompositingMode <- Drawing2D.CompositingMode.SourceCopy
        e.Graphics.InterpolationMode <- Drawing2D.InterpolationMode.NearestNeighbor
        e.Graphics.DrawImage (bitmap, Drawing.Rectangle (0, 0, width * scale, height * scale))

[<STAThread; EntryPoint>]
let main args = 
    Application.EnableVisualStyles ()
    let window = new Window ()
    window.Show ()
    while window.Visible do
        window.Update ()
        Application.DoEvents ()
    0