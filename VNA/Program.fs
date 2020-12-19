// Learn more about F# at http://fsharp.org

open XPlot.Plotly   
open OpenTapVisa


type vna (address)=
    let mutable sesn=0
    let mutable vi=1
    let mutable retCount=0
    let mutable Readbuffer=""
    let mutable status=Visa.viOpenDefaultRM &sesn
    do
        status <- Visa.viOpen(sesn,address,0,0,&vi) 
    member this.Status with get() =status 
    member this.write (command:string)= status <-Visa.IOWrite(vi,command,&retCount)
    member this.query (command)=status <- Visa.IOQuery(vi,"*opc?",&Readbuffer)

[<EntryPoint>]

let main argv =
    printfn "Hello World from F#!"
    // Learn more about F# at http://fsharp.org
    let mutable sesn1=0
    let mutable status= Visa.viOpenDefaultRM &sesn1
    let mutable vi=1
    let mutable retCount=0
    let mutable Readbuffer=""
    printfn "Hello World from F#!"
    let VNA="TCPIP0::localhost::hislip0::INSTR"
    
    let GetIden address=
        status <- Visa.viOpen(sesn1,address,0,0,&vi) 
        status <- Visa.IOQuery(vi,"*IDN?",&Readbuffer)
        if status<>0 then  printfn  "Error writing Command"
        else    status <- Visa.viClose(vi)
                printfn "%s" Readbuffer
    GetIden VNA
    
    let EvenIdx values= let End=(Array.length values)-1
                        [|0..2..End|] |>Array.map (Array.get values) 
    
    let OddIdx values = let End=(Array.length values)-1
                        [|1..2..End|] |>Array.map (Array.get values) 
    let GetTrace snn address=
        let vna=vna(address)
        status <- Visa.IOWrite(vi,"SENSe1:SWEep:POINts 2000",&retCount)
        status <- Visa.viOpen(sesn1,address,0,0,&vi) 
        status <- Visa.IOWrite(vi,"CALC1:PARameter1:DEF "+snn,&retCount)
        vna.write("CALC1:PARameter1:DEF "+snn)
        vna.write("TRIG:SOUR BUS\n")
        vna.write("TRIG:SING\n")
        vna.query("*OPC?\n")
        //status <- Visa.IOWrite(vi,"SENSe1:SWEep:POINts 2000",&retCount)
        status <- Visa.IOQuery(vi,"*opc?",&Readbuffer)
        //Thread.Sleep 0
        status <- Visa.IOQuery(vi,"SENSe1:FREQuency:Data?",&Readbuffer)
        if status<>0 then  printfn  "Error writing Command"
        let F=Readbuffer.Split ','
        status <- Visa.IOQuery(vi,"CALC1:trac1:DATA:FDATa?",&Readbuffer)
        if status<>0 then  printfn  "Error writing Command"
        let Y=Readbuffer.Split ',' 
        let Y1=EvenIdx Y
        let Y2=OddIdx Y
        [F;Y1;Y2] |>List.map (Array.map float) 
    
    let layout =  Layout( showlegend = false ,smith=true, plot_bgcolor = "rgb(255,255,255)")
    let S21=(GetTrace "S21" VNA) 
    let S11=(GetTrace "S11" VNA) 
    
    [Seq.zip S21.[0] S21.[1] ; Seq.zip S11.[0] S11.[1]]
    |> Chart.Line
    |> Chart.WithTitle "S21"
    |> Chart.WithLayout layout
    |> Chart.Show
    1