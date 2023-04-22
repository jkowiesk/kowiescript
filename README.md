# TKOM - Projekt wstępny

**Autor**: Jakub Kowieski

**Przydział**: typowanie dynamiczne silne, zmienne domyślnie mutowalne przekazywanie przez kopię

# Funkcjonalności

- Obsługa zmiennych, stałych, pętli, instrukcji warunkowych, funkcji, operacji arytmetycznych i logicznych
- Posiada vectory oraz proste operacje na nich
- Operacje na ciągach znaków
- Sam język będzie typowany dynamicznie silnie, a jego zmienne będą domyślnie mutowalne i przekazywane przez kopię
- Posiada range_expression “x to y”, by wygenerować sekwencję liczb całkowitych
- zmienne przekazywane do funkcji są przekazywane przez kopię oraz są stałe wewnątrz funkcji

# Dopuszczalne konstrukcje językowe

```jsx
fn test() {
  print("test")
  ret 2;
}

const b = 3

const files = ["test1", "test2"]

let a = test()
a = a + 1

if a != 5  {
  print("'a' is not a five")
}

for file in files {
	// do sth with file
  if (file == "end") {
  	end
  }

  if (file == "skip") {
  	next
  }
}

for i in 1 to 10 {
  print(i)

}

let x = 5
match x {
  when 1 then {
    print("x is 1")
  }
  when 2 either 3 then {
    print("x is 2 or 3")
  }
  default {
    print("x is something  else")
  }
}

```

# Przykłady

Ciąg fibonachiego

```jsx
fn fibonacci(n) {
  if n <= 1 {
    ret n
  } else {
    ret fibonacci(n - 1) + fibonacci(n - 2)
  }
}

let fib_sequence = [fibonacci(0), fibonacci(1)]
let n = 10
for i in 2 to n {
  fib_sequence[i] = fib_sequence[i-1] + fib_sequence[i-2]
}
```

Pattern Matching

```jsx
let how = "well"
match how {
  when "bad" either "awfull" then {
  	print("Not good at all :(")
  }
  when "well" then {
  	print("Great success !!!")
  }
  default {
  	print("What ???")
  }
}
```

**Przetwarzanie vectorów i stringów**

```jsx
fn starts_with_vowel(word) {
  const first_leteter = word[0]
  for vowel in ["a", "e", "i", "o", "u"] {
  		if first_letter == vowel {
  			ret true
  		}
  }
  ret false
}

let words = ["apple", "cat", "dog", "egg", "fish", "sheep"]
let vowel_words = []

for word in words {
  if starts_with_vowel(word) {
    push(vowel_words, word)
  }
}

print(vowel_words)
```

# Semantyka

- **let** - deklaracja zmiennej
- **ret** - zwracana wartość z funkcji
- **const** - deklaracja wartości stałej
- **if** - instrukcja warunkowa wykonuje instrukcje zależnie od warunku logicznego,
- **for** - pętla wykonuje iteracje po danym obiekcie
- **fn** - funkcja wykonuje określony blok kodu
- **match** - patter matching
- [**+**, **-**, **/**, __*__, **%**] **-** operacje arytmetyczne
- [**and**, **or**, **not**, **<**, **≤**, **≥**, **>**, **==**, **≠**] - operacje logiczne
- **end** - natychmiastowe zakończenie pętli
- **next** - przekazanie sterowanie do następnej iteracji

# Funkcje wbudowane

W język będzie kilka funkcji wbudowanych:

**funkcje I/O**

- input - do wczytywania danych z **stdin**
- print - do wypisania danych do **stdout** (na koniec dodany będzie znak nowej linii)

**funkcje vectorów**:

- push - dodaje element do vectora
- remove - usuwa element o zadanej pozycji

# Rzutowanie

W języku jest możliwe rzutowanie prostych typów i stringa za pomocą operatora "as":

```jsx
let a = 5
let b = a as string
let c = a as float
let d = a as bool
```

# Komentarze

Jednolinijkowe zaczynające się od znaku ‘#’

# Składnia (EBNF)

```ebnf
program               = { statement };

statement             = variable_declaration
                      | constant_declaration
                      | assignment_statement
                      | loop_statement
                      | conditional_statement
                      | function_declaration
                      | pattern_match_stmt;

variable_declaration  = "let" identifier "=" expression;
constant_declaration  = "const" identifier "=" expression;
assignment_statement  = identifier "=" expression;
loop_statement        = "for" identifier "in" iterator_expression "{" { statement | loop_substatment } "}";
conditional_statement = "if" expression "{" { statement } "}" [ "else" "{" { statement } "}" ];
function_declaration  = "fn" identifier "(" [ parameter_list ] ")" "{" { statement | return_statement } "}";
pattern_match_stmt    = "match" identifier "{" { when_substatement } default_substatement "}";

when_substatement     = "when" match_expression "then" "{" { statement } "}";
default_substatement  = "default" "{" { statement } "}";

return_statement      = "ret" expression;
loop_substatment      = "end"
                      | "next"


iterator_expression   = vector | identifier | range_expression | function_call;
range_expression      = range_factor "to" range_factor;
match_expression      = expression { "either" expression };

expression            = conjuction { "or" conjuction };
conjuction            = relation_expression { "and" relation_expression };
relation_expression   = simple_expression [relational_operator, simple_expression];
simple_expression     = term { lower_arithmetic term };
term                  = type_conversion { higher_arithmetic type_conversion };
type_conversion       = inversion [ cast_operator cast_type ];
inversion             = ["!"] factor;


factor                = literal
                      | identifier
                      | vector
                      | function_call
                      | vector_access
                      | "(" expression ")";

function_call         = identifier "(" [ parameter_list ] ")";
parameter_list        = identifier { "," identifier };

range_factor          = natural_number | identifier;

vector_access         = identifier "[" expression "]";

cast_operator         = "as";

literal               = numeric_literal
                      | string_literal
                      | logic_literal;

logic_literal         = "true" | "false";

cast_type             = "int"
                      | "float"
                      | "bool"
                      | "string";

lower_arithmetic      = "+"
                      | "-";

higher_arithmetic     = "*"
                      | "/"
                      | "%";

relational_operator   = "<"
                      | "<="
                      | ">"
                      | ">="
                      | "=="
                      | "!=";

vector                = "[" [ expression { "," expression } ] "]";

numeric_literal       =  intager_number
                      |  real_number;

string_literal        = '"' character { character } '"';

identifier            = letter { letter | digit | "_" };

real_number           = intager_literal "." natural_number;

intager_number        = ["-"] natural_number;

natural_number        = (digit_without_zero { digit })
                      | "0";

character             = letter | digit | " " | " " | "." | "," | "!" | "?" | ":" | "-" | "_" | "/" | "+" | "=";

digit                 = "0" | digit_without_zero;

letter                = [a-zA-Z];

digit_without_zero    = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
```

# Format plików

- \*.ks - format plików

# Analiza wymagań niefunkcjonalnych

- Łatwość nauki - powinien być wystarczająco łatwy do zrozumienia by osoby przychodzące z innych popularnych języków mogliby szybko się zaadoptować. Mając podstawy programowania w dwa dni powinien być w stanie napisać program w języku ks.
- Wydajność - język powinien być wystarczająco szybki, aby umożliwić wydajne przetwarzanie dużej ilości danych

# Typy danych

Proste:

- int
- bool
- float

Złożone:

- **String**
- **Vector**

# Obsługa błędów

Błedy wyświetlane będą użytkownikowi w formacie**:**

_<Typ błędu> \<w jakiej lini> <wyjaśnienie>_

## **Błedy:**

1. **Syntax error**

```jsx
if x > 0 {
  print("Bigger than 0")
```

_Komunikat_:

```bash
Syntax error at line 2: missing closing brace '{'
```

2. **Division by zero**

```jsx
let x = 2 / 0;
```

_Komunikat_:

```bash
Division by zero at line 1: division by zero is not allowed
```

3. **Unknown identifier**

```jsx
const a = b + 9;
```

_Komunikat_:

```bash
Unknown identifier error at line 1: identifier 'b' not defined
```

4. **Variable redeclaration**

```jsx
let a = 5;
let a = 1;
```

_Komunikat_:

```bash
Variable redeclaration at line 2: variable ‘a’ is already declerated
```

5. **Type mismatch**

```jsx
let x = "hello";
let z = x + 5;
```

_Komunikat:_

```bash
Type mismatch at line 2: the types aren't the same
```

6. **Impossible casting**

```jsx
let x = "hello"
let z = x as int
```

_Komunikat:_

```bash
Impossible casting at line 2: type 'string' can't be casted to 'int'
```

# Sposób uruchomienia

1. Napisać kod programu w pliku o rozszerzeniu \*_.ks_
2. Uruchumienie skompilowanego interpretera za pomocą komendy:

```bash
./ks plik.ks
```

3. Wejście podczas wykonwyania obsługiwany przez **stdin,** wynik zostanie przesłany do **stdout** (wyświetli się na terminale)

**Przykładowe uruchomienie pliku** _plik.ks_

```jsx
fn sum(x, y) {
    return x + y
}

let a = input()
let b = input()
print(sum(a, b))
```

**Wejście**

```bash
1
2
```

**Wyjście**

```bash
3
```

# Podział na komponenty

- **Lexer** - pobiera ciąg znaków wejściowych i przetwarza go na listę tokenów zgodnie z zasadami gramatyki języka
- **Parser** - przetwarza listę tokenów na abstrakcyjne drzewo składniowe (AST) na podstawie zasad gramatyki
- **AST** - abstrakcyjne drzewo składniowe, reprezentujące strukturę składniową programu w sposób hierarchiczny. Każdy węzeł drzewa reprezentuje konkretny element składniowy języka, a jego child nodes reprezentują elementy składniowe zależne od niego.
- **Interpreter** - główny komponent odpowiedzalny za nadzorowanie całego procesu. Jego głównym celem będzie wykonanie kodu źródłowego oraz zwrócenie wyniku. W razie błedu podczas wykonywania programu, zgłosi go użytkownikowi.

# Opisy sposobu testowania

- **Lexer** - do przetestowania poprawnośći działania analizatora leksykalnego, użyję testów jednostkowych, które pozwalają sprawdzić, czy na podstawie określonego pliku wejściowego lekser zwraca oczekiwaną listę tokenów, albo zgłosi błąd.
- **Parser** - w przypadku analizatora składniowego, też użyję testów jednostkowych, które służą do weryfikacji, czy parser tworzy oczekiwane drzewo rozbioru składniowego na podstawie strumienia tokenów, lub tak jak lexer, zgłosi błąd.

Poza tym użyję także testów integracyjnych, do sprawdzenia połączenia między lekserem a parserem, czy parser poprawnie buduje drzewo rozbioru na podstawie pliku wejściowego.
