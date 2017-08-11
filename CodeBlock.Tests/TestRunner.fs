module TestRunner


if System.Diagnostics.Debugger.IsAttached then
    printf "press any key to continue..."
    System.Console.ReadKey() |> ignore
