### JĘZYK
Język imperatywny wzorowany na języku Latte z pewnymi uproszczeniami zapożyczonymi z Tiny i pewnymi rozszerzeniami.
Dokładny opis znajduje się w pliku karolina_drabik.txt i różni się nieznacznie od pierwotnej deklaracji języka.

### KOMPILACJA
W przypadku budowania interpretera poza maszyną students należy przed wywołaniem `make` ustawić pod zmienną środowiskową BNFC ścieżkę do bnfc obsługującego poprawnie flagę `--functor`.

### URUCHAMIANIE
- `./interpreter PLIK` - uruchamia interpreter na programie wczytanym spod ścieżki PLIK
- `./interpreter     ` - uruchamia interpreter na programie wczytanym ze standardowego wejścia (wyjście: CTRL+D)

### STRUKTURA KATALOGU
- **bad**                - katalog zawierający przykłady niepoprawnych programów: zawierających błędy składniowe, błędy wykrywane
                           statycznie oraz błędy wykonania, wraz z odpowiadającym im plikom .out i .err zawierającym oczekiwane wyjście
- **good**               - katalog zawierający poprawne programy ilustrujące wszystkie punktowane konstrukcje języka, wraz z
                           odpowiadającym im plikom .out i .err zawierającym oczekiwane wyjście
- **src/Typechecker.hs** - moduł wykrywający statycznie wykrywalne błędy programu (błędy typów, nieznany identyfikator, zła
                           liczba argumentów itp.)
- **src/Interpreter.hs** - moduł dynamicznie wykonujący program
- **src/Main.hs**        - moduł obsługujący argumenty wiersza poleceń
- **test.sh**            - skrypt uruchamiający interpreter na przykładowych programach i porównujący wyjście oraz wyjście błędów
                           z odpowiednim plikiem .out lub .err
- **Makefile**

### TESTOWANIE
```
./test.sh interpreter good
./test.sh interpreter bad
```
