module WAVLibrary

open System.Diagnostics
open System.IO
open System.Runtime.InteropServices

(* This Library was created with a sample function and posted to F# Snippets by user Phillip Trelford. The
original function can be found at this link: http://www.fssnip.net/iz/title/Generate-WAVE-file *)

/// Write WAVE PCM soundfile (8KHz Mono 8-bit)
let write stream (data:byte[]) =
    use writer = new BinaryWriter(stream)
    // RIFF
    writer.Write("RIFF"B)
    let size = 36 + data.Length in writer.Write(size)
    writer.Write("WAVE"B)
    // fmt
    writer.Write("fmt "B)
    let headerSize = 16 in writer.Write(headerSize)
    let pcmFormat = 1s in writer.Write(pcmFormat)
    let mono = 1s in writer.Write(mono)
    let sampleRate = 16000 in writer.Write(sampleRate)
    let byteRate = sampleRate in writer.Write(byteRate)
    let blockAlign = 1s in writer.Write(blockAlign)
    let bitsPerSample = 8s in writer.Write(bitsPerSample)
    // data
    writer.Write("data"B)
    writer.Write(data.Length)
    writer.Write(data)

 (*Sound playing methods by Dan Barowy*)
 (* PLAYING SOUND FILES *)
let initWAVProcessMac(wavpath: string) : Process =
    // "afplay" only works on the Mac
    let info = new ProcessStartInfo (
                 FileName = "/bin/bash",
                 Arguments = "-c \"afplay " + wavpath + "\"",
                 RedirectStandardOutput = true,
                 UseShellExecute = false,
                 CreateNoWindow = true
               )
    let p = new Process()
    p.StartInfo <- info
    p

let initWAVProcessWindows(wavpath: string) : Process =
    // Makes a thing
    let info = new ProcessStartInfo (
                 FileName = "powershell",
                 Arguments = "-c (New-Object Media.SoundPlayer \"" + wavpath + "\").PlaySync();",
                 RedirectStandardOutput = true,
                 UseShellExecute = false,
                 CreateNoWindow = true
               )
    let p = new Process()
    p.StartInfo <- info
    p

let playWAV(wavpath: string) : unit =
    let p = 
        if RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then (initWAVProcessMac wavpath)
        else (initWAVProcessWindows wavpath)
    if p.Start() then
        let result = p.StandardOutput.ReadToEnd()
        p.WaitForExit()
        File.Delete(wavpath)
    else
        printfn "Could not start afplay."
        exit 1

      