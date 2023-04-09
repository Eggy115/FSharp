// From https://github.com/JimBobSquarePants/ImageProcessor/blob/Core/src/ImageProcessorCore/Formats/Jpg/JpegDecoder.cs

open System
open System.IO
open System.Threading.Tasks

//open BitMiracle.LibJpeg

#load "Guard.fsx"
open Guard

/// =======================================
/// Dummy classes to make code compile
/// =======================================


[<AllowNullLiteral>]
type Image() =
    member this.Height = 1
    member this.Width = 1
    member this.SetPixels(pixelWidth, pixelHeight, pixels) = ()

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

/// Image decoder for generating an image out of a jpg stream.
type JpegDecoder() =

    /// Gets the size of the header for this image type.
    member this.HeaderSize :int = 11

    /// <summary>
    /// Indicates if the image decoder supports the specified
    /// file extension.
    /// </summary>
    /// <exception cref="System.ArgumentNullException"><paramref name="extension"/>
    /// is null (Nothing in Visual Basic).</exception>
    /// <exception cref="System.ArgumentException"><paramref name="extension"/> is a string
    /// of length zero or contains only blanks.</exception>
    member this.IsSupportedFileExtension (extension:string) : bool = 
    
        Guard.NotNullOrEmpty(extension, "extension")

        let mutable extension = extension
        
        if (extension.StartsWith(".")) then
            extension <- extension.Substring(1)

        extension.Equals("JPG", StringComparison.OrdinalIgnoreCase) ||
                extension.Equals("JPEG", StringComparison.OrdinalIgnoreCase) ||
                extension.Equals("JFIF", StringComparison.OrdinalIgnoreCase)

    /// <summary>
    /// Indicates if the image decoder supports the specified
    /// file header.
    /// </summary>
    /// <exception cref="System.ArgumentNullException"><paramref name="header"/>
    /// is null (Nothing in Visual Basic).</exception>
    member this.IsSupportedFileFormat(header : byte[]) : bool =
    
        Guard.NotNull(header, "header")

        let mutable isSupported = false

        if (header.Length >= 11) then
            let isJfif = JpegDecoder.IsJfif(header)
            let isExif = JpegDecoder.IsExif(header)
            let isJpeg = JpegDecoder.IsJpeg(header)
            isSupported <- isJfif || isExif || isJpeg

        isSupported
    

    /// Decodes the image from the specified stream and sets
    /// the data to image.
    member this.Decode(image:Image, stream:Stream) :unit = 
    
        Guard.NotNull(image, "image")
        Guard.NotNull(stream, "stream")

        let jpg = new JpegImage(stream)

        let pixelWidth = jpg.Width
        let pixelHeight = jpg.Height

        let pixels :float[] = Array.zeroCreate(pixelWidth * pixelHeight * 4)

        if (jpg.Colorspace = Colorspace.RGB && jpg.BitsPerComponent = 8) then
        
            Parallel.For(
                0,
                pixelHeight,
                fun y ->
                
                    let row : SampleRow = jpg.GetRow(y)

                    for x = 0 to pixelWidth-1 do
                    
                        let sample : Sample = row.GetAt(x)

                        let offset : int = ((y * pixelWidth) + x) * 4

                        pixels.[offset + 0] <- float sample.[0] / 255.0
                        pixels.[offset + 1] <- float sample.[1] / 255.0
                        pixels.[offset + 2] <- float sample.[2] / 255.0
                        pixels.[offset + 3] <- 1.0
                    
                ) |> ignore
        
        else if (jpg.Colorspace = Colorspace.Grayscale && jpg.BitsPerComponent = 8) then
        
            Parallel.For(
                0,
                pixelHeight,
                fun y ->
                
                    let row : SampleRow = jpg.GetRow(y)

                    for x = 0 to pixelWidth-1 do
                    
                        let sample : Sample = row.GetAt(x)

                        let offset : int = ((y * pixelWidth) + x) * 4

                        pixels.[offset + 0] <- float sample.[0] / 255.0
                        pixels.[offset + 1] <- float sample.[0] / 255.0
                        pixels.[offset + 2] <- float sample.[0] / 255.0
                        pixels.[offset + 3] <- 1.0
                    
                ) |> ignore
        else
        
            raise <| NotSupportedException("JpegDecoder only supports RGB and Grayscale color spaces.")
        

        image.SetPixels(pixelWidth, pixelHeight, pixels)

        jpg.Dispose()
    

    /// <summary>
    /// Returns a value indicating whether the given bytes identify Jfif data.
    /// </summary>
    /// <param name="header">The bytes representing the file header.</param>
    /// <returns>The <see cref="bool"/></returns>
    static member IsJfif(header : byte[] ) : bool =
    
        let isJfif =
            header.[6] = 0x4Auy && // J
            header.[7] = 0x46uy && // F
            header.[8] = 0x49uy && // I
            header.[9] = 0x46uy && // F
            header.[10] = 0x00uy

        isJfif
    

    /// <summary>
    /// Returns a value indicating whether the given bytes identify EXIF data.
    /// </summary>
    /// <param name="header">The bytes representing the file header.</param>
    /// <returns>The <see cref="bool"/></returns>
    static member IsExif(header : byte[] ) : bool = 
    
        let isExif =
            header.[6] = 0x45uy && // E
            header.[7] = 0x78uy && // x
            header.[8] = 0x69uy && // i
            header.[9] = 0x66uy && // f
            header.[10] = 0x00uy

        isExif
    

    /// <summary>
    /// Returns a value indicating whether the given bytes identify Jpeg data.
    /// This is a last chance resort for jpegs that contain ICC information.
    /// </summary>
    /// <param name="header">The bytes representing the file header.</param>
    /// <returns>The <see cref="bool"/></returns>
    static member IsJpeg(header : byte[] ) : bool =
    
        let isJpg =
            header.[0] = 0xFFuy && // 255
            header.[1] = 0xD8uy // 216

        isJpg
    


