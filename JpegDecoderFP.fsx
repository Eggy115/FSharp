// From https://github.com/JimBobSquarePants/ImageProcessor/blob/Core/src/ImageProcessorCore/Formats/Jpg/JpegDecoder.cs

open System
open System.IO
open System.Threading.Tasks

//open BitMiracle.LibJpeg

#load "Guard.fsx"
open Guard

#load "ErrorHandling.fsx"
open ErrorHandling

/// =======================================
/// Dummy classes to make code compile
/// =======================================


[<AllowNullLiteral>]
type Image() =
    member this.Height = 1
    member this.Width = 1
    member this.SetPixels(pixelWidth, pixelHeight, pixels) = ()
    static member FromPixels(pixelWidth, pixelHeight, pixels) = Image() 

type Sample = int16[]

type SampleRow() = 
    member this.GetAt(x) = [||]

type Colorspace = RGB  | Grayscale

type JpegImage(stream) = 
    member this.Height = 1
    member this.Width = 1
    member this.Colorspace = Colorspace.RGB
    member this.BitsPerComponent = 1
    member this.GetRow(x) = SampleRow()
    member self.Dispose() = ()    
    interface IDisposable with
        member self.Dispose() = ()

/// =======================================
/// Main code 
/// =======================================

type FileExtension =
    | PNG
    | GIF
    | JPG
    | JPEG
    | JFIF
    | UNKNOWN

/// Image decoder for generating an image out of a jpg stream.
module JpegDecoder1 =

    let (|EqualI|_|) compare (str:string) =
        if str.Equals(compare, StringComparison.OrdinalIgnoreCase) then 
            Some ()
        else 
            None

    let toFileExtension extensionStr = 
        match extensionStr with
        | null -> UNKNOWN
        | EqualI "png" -> PNG
        | EqualI "gif" -> GIF
        | EqualI "jpg" -> JPG
        | EqualI "jpeg" -> JPEG
        | EqualI "jfif" -> JFIF
        | _  -> UNKNOWN
    
    (*
    toFileExtension "png"
    toFileExtension null
    toFileExtension ".doc"
    *)


    let isSupportedFileExtension extension = 
        match extension with
        | JPG 
        | JPEG 
        | JFIF -> true
        | PNG
        | GIF
        | UNKNOWN -> false






    let headerHasTag (header : byte[]) (tag:byte[]) start = 
        let mutable allEqual = true

        for i=0 to tag.Length-1 do
            let elementEqual = 
                header.[start+i] = tag.[i]
            allEqual <- allEqual && elementEqual

        allEqual 
    
    (*
    let exifTag = "Exif"B
    let header = "abcdefExifxxx"B
    headerHasTag header exifTag 6
    *)


    let headerHasTagV2 header tag start = 
        let len = Array.length tag
        let segment = Array.sub header start len 
        segment = tag

    (*
    headerHasTagV2 header exifTag 6
    headerHasTagV2 header exifTag 3
    
    // this is generic and should be renamed -- Array.hasSegment or similar
    *)

    
    let isJfif header =
        let exifTag = "JFIF"B
        headerHasTagV2 header exifTag 6

    let isExif header =
        let exifTag = "Exif"B
        headerHasTagV2 header exifTag 6

    let isJpeg header =
        let jpegTag = [| 0xFFuy; 0xD8uy |]
        headerHasTagV2 header jpegTag 0

    let isSupportedFileFormat header =
    
        Guard.NotNull(header, "header")

        (Array.length header >= 11) &&
        (isJfif header || isExif header || isJpeg header)
        








module JpegDecoder2 =

    // all the stuff from above, plus..
    open JpegDecoder1 

    type DecoderInput = {
        Image:Image
        Stream:Stream
    }

    type DecodingError =
        | DecodingUnsupported
        | DecodingFailed

    let parallelFor f fromInclusive toExclusive =
        Parallel.For(fromInclusive=fromInclusive,toExclusive=toExclusive,body=Action<int>(f) ) |> ignore

    let updateFromSample getPixels (jpg:JpegImage) (pixels:float[]) y =
        let row : SampleRow = jpg.GetRow(y)
        let pixelWidth = jpg.Width

        for x = 0 to pixelWidth-1 do
            let sample : Sample = row.GetAt(x)
            let offset : int = ((y * pixelWidth) + x) * 4

            let sample0,sample1,sample2,sample3 = getPixels sample

            pixels.[offset + 0] <- sample0 
            pixels.[offset + 1] <- sample1 
            pixels.[offset + 2] <- sample2
            pixels.[offset + 3] <- sample3


    let (|RGB8bit|Grayscale8bit|Unsupported|) (jpg:JpegImage) = 
        if (jpg.Colorspace = Colorspace.RGB && jpg.BitsPerComponent = 8) then
            RGB8bit
        elif (jpg.Colorspace = Colorspace.Grayscale && jpg.BitsPerComponent = 8) then
            Grayscale8bit
        else
            Unsupported

    /// Decodes the image from the specified stream and sets
    /// the data to image.
    let decode (input:DecoderInput) = 
    
        use jpg = new JpegImage(input.Stream)

        let pixelWidth = jpg.Width
        let pixelHeight = jpg.Height

        let pixels = Array.zeroCreate(pixelWidth * pixelHeight * 4)

        match jpg with
        | RGB8bit ->
            let getPixels (sample:Sample) = 
                float sample.[0] / 255.0,
                float sample.[1] / 255.0,
                float sample.[2] / 255.0,
                1.0
            let update = updateFromSample getPixels jpg pixels
            parallelFor update 0 pixelHeight 
            
            Image.FromPixels(pixelWidth, pixelHeight, pixels)
            |> Result.success

        | Grayscale8bit ->        
            let getPixels (sample:Sample) = 
                float sample.[0] / 255.0,
                float sample.[0] / 255.0,
                float sample.[0] / 255.0,
                1.0
            let update = updateFromSample getPixels jpg pixels
            parallelFor update 0 pixelHeight 
            
            Image.FromPixels(pixelWidth, pixelHeight, pixels)
            |> Result.success
        
        | Unsupported ->
            Result.failure DecodingUnsupported

        

    

    
    


