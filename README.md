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
  print("test");
  ret 2;
}

const b = 3;

const files = ["test1", "test2"];

let a = test();
a = a + 1;

if a != 5  {
  print("'a' is not a five");
}

for file in files {
	// do sth with file
  if file == "end" {
  	end;
  }

  if file == "skip" {
  	next;
  }
}

for i in 1 to 10 {
  print(i);

}

let x = 5;
match x {
  when 1 then {
    print("x is 1");
  }
  when 2 either 3 then {
    print("x is 2 or 3");
  }
  default {
    print("x is something  else");
  }
}

```

# Przykłady

Ciąg fibonachiego

```jsx
fn fibonacci(n) {
  if n <= 1 {
    ret n;
  } else {
    ret fibonacci(n - 1) + fibonacci(n - 2);
  }
}

let n = 10;  // Replace with the desired value of n
let fib_number = fibonacci(n);
print(fib_number);
```

Pattern Matching

```jsx
let how = "awful";

match how {
  when "bad" either "awful" then {
  	print("Not good at all :(");
  }
  when "well" then {
  	print("Great success !!!");
  }
  default {
  	print("What ???");
  }
}
```

**Przetwarzanie vectorów i stringów**

```jsx
fn starts_with_vowel(word) {
  const first_letter = word[0];
  for vowel in ["a", "e", "i", "o", "u"] {
  		if first_letter == vowel {
  			ret true;
  		}
  }
  ret false;
}

let words = ["apple", "cat", "dog", "egg", "fish", "sheep"];
let vowel_words = [];

for word in words {
  if starts_with_vowel(word) {
    push("vowel_words", word);
  }
}
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
- [**and**, **or**, **!**, **<**, **≤**, **≥**, **>**, **==**, **≠**] - operacje logiczne
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
let b = a as int
let c = a as float
let d = a as bool
```

# Komentarze

Jednolinijkowe zaczynające się od znaku ‘//’

# Składnia (EBNF)

```ebnf
program               = { statement };

statement             = variable_declaration
                      | constant_declaration
                      | assign_declaration
                      | loop_statement
                      | loop_substatement
                      | conditional_statement
                      | function_statement
                      | pattern_match_stmt
                      | return_statement
                      | comment
                      | expression_stmt;

variable_declaration  = "let" identifier "=" expression ";";
constant_declaration  = "const" identifier "=" expression ";";
assign_declaration    = identifier "=" expression ";";
loop_statement        = "for" identifier "in" iterator_expression "{" { statement } "}";
conditional_statement = "if" expression "{" { statement } "}" [ "else" "{" { statement } "}" ];
function_statement    = "fn" identifier "(" [ parameter_list ] ")" "{" { statement } "}";
pattern_match_stmt    = "match" expression "{" { when_branch } default_branch "}";
expression_stmt	      = expression ";" ;

comment               = "//" { character } "\n";

when_branch           = "when" when_expression "then" "{" { statement } "}";
default_branch        = "default" "{" { statement } "}";

return_statement      = "ret" [expression] ";";
loop_substatement      = "end;"
                      | "next;"


iterator_expression   = vector | identifier | range_expression;
range_expression      = range_factor "to" range_factor;
when_expression       = simple_expression { "either" simple_expression };
expression            = conjuction { "or" conjuction };
conjuction            = relation { "and" relation };
relation              = simple_expression [relational_operator, simple_expression];
simple_expression     = term { lower_arithmetic term };
term                  = conversion { higher_arithmetic conversion };
conversion            = inversion [ cast_operator cast_type ];
inversion             = ["!"] factor;


factor                = literal
                      | identifier
                      | vector
                      | fun_call_or_vec_acc
                      | vector_access_ident
                      | "(" expression ")";

fun_call_or_vec_acc   = identifier "(" [ argument_list ] ")" [ "[" expression "]" ];
parameter_list        = parameter { "," parameter };
parameter             = ["const"] identifier;
argument_list         = expression { "," expression };

range_factor          = natural_number | identifier;

vector_access_ident   = identifier "[" expression "]";

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

numeric_literal       =  integer_number
                      |  real_number;

string_literal        = '"' character { character } '"';

identifier            = letter { letter | digit | "_" };

real_number           = integer_literal "." digit { digit };

integer_number        = ["-"] natural_number;

natural_number        = (digit_without_zero { digit })
                      | "0";

character             = escape_character | letter | digit | " " | " " | "." | "," | "!" | "?" | ":" | "-" | "_" | "/" | "+" | "=";

escape_character      = "#", ( "\t" | "\r" | "\n" | "\"" | "#") ;

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
- string

Złożone:

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
Syntax error at line 1: missing closing brace '{'
```

2. **Division by zero**

```jsx
let x = 2 / 0;
```

_Komunikat_:

```bash
Runtime error at line 1: division by zero is not allowed
```

3. **Unknown identifier**

```jsx
const a = b + 9;
```

_Komunikat_:

```bash
Runtime error at line 1: identifier 'b' not defined
```

5. **Type mismatch**

```jsx
let x = "hello";
let z = x + 5;
```

_Komunikat:_

```bash
Runtime error at line 2: Cannot add 'hello' and '5'
```

6. **Impossible casting**

```jsx
let x = "hello"
let z = x as int
```

_Komunikat:_

```bash
Runtime error at line 2: Cannot convert string to int
```

# Sposoby uruchomienia

**1.** Interprertacja programu zapisanego w pliku o rozszerzeniu \*_.ks_

```bash
./ks plik.ks
```

**2.** Uruchomienie interpretera w CLI

```bash
./ks_interpreter
```

3. Wejście podczas wykonwyania obsługiwany przez **stdin,** wynik zostanie przesłany do **stdout** (wyświetli się na terminale)

**Przykładowe uruchomienie pliku** _plik.ks_

```jsx
fn sum(x, y) {
    ret x + y;
}

let a = input();
let b = input();
print(sum(a, b));
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

- **io** - odpowiedzialny za obsługę źródła (czy w postaci pliku czy stringa)
- **Lexer** - jest odpowiedzialny za przetworzenie źródła (io::Input) na listę tokenów zgodną z zasadami gramatyki języka
- **Parser** - przetwarza listę tokenów na abstrakcyjne drzewo składniowe (AST) na podstawie zasad gramatyki
- **AST** - abstrakcyjne drzewo składniowe, reprezentujące strukturę składniową programu w sposób hierarchiczny. Każdy węzeł drzewa reprezentuje konkretny element składniowy języka.
- **Interpreter** - główny komponent odpowiedzalny za nadzorowanie całego procesu. Jego głównym celem będzie wykonanie kodu źródłowego oraz zwrócenie wyniku. W razie błedu podczas wykonywania programu, zgłosi go użytkownikowi.

# Opisy sposobu testowania

- **Lexer** - do przetestowania poprawnośći działania analizatora leksykalnego, użyję testów jednostkowych, które pozwalają sprawdzić, czy na podstawie określonego pliku wejściowego lekser zwraca oczekiwaną listę tokenów, albo zgłosi błąd.
- **Parser** - w przypadku analizatora składniowego, też użyję testów jednostkowych, które służą do weryfikacji, czy parser tworzy oczekiwane drzewo rozbioru składniowego na podstawie strumienia tokenów, lub tak jak lexer, zgłosi błąd.

Poza tym użyję także testów integracyjnych, do sprawdzenia połączenia między lekserem a parserem, czy parser poprawnie buduje drzewo rozbioru na podstawie pliku wejściowego.
