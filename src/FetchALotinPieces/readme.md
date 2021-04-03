Här tänkte jag försöka hämta från en server en stor mängd data 
i chunk för att få bort problem med prestanda när det blir många träffar i en sökning.

- Kan man använda en sekvens av Task för det? T ex att hämta alla bilder för profiler i en lista?
- Eller är det enklare att bara köra sekvens av Cmd?
- Hur avbryter man om användaren startar en ny sökning innan man är klar?

Troligen inte svårt att lösa.

Inspiration: https://korban.net/posts/elm/2019-02-15-combining-http-requests-with-task-in-elm/

Korban använder Http.task och läser in alla användare, alla inlägg från en användare och alla kommentarer på alla inläggen.

Klurigt med `Http.task ... |> Task.map List.head |> Task.andThen`
