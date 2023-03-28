# TKOM - Projekt wstępny

**Autor**: Jakub Kowieski

**Przydział**: typowanie dynamiczne silne, zmienne domyślnie mutowalne przekazywanie przez kopię

# Funkcjonalności

- Obsługa zmiennych, stałych, pętli, instrukcji warunkowych, funkcji, operacji arytmetycznych i logicznych
- Operacje na ciągach znaków
- Sam język będzie typowany dynamicznie silnie, a jego zmienne będą domyślnie mutowalne i przekazywane przez kopię
- posiada niestandardowy typ danych **Range**

**Range** - reprezentuje zakres wartości, może służyć jako iterator.

# Dopuszczalne konstrukcje językowe

```jsx
const b = 3

const files = ["test1", "test2"]

let a = 5
a = a + 1

if a != 5  {
	print("A is not a five")
}

for file in files {
	file
}

for i in Range(1, 10) {
	print(i)
}

let how = "well"

match how {
	case "bad" => print("Not good at all :(")
	case "well" => print("Great success !!!"),
	default => print("What ???")
}
```

# Semantyka

- **let** - deklaracja zmiennej
- **const** - deklaracja wartości stałej
- **if** - instrukcja warunkowa wykonuje instrukcje zależnie od warunku logicznego,
- **for** - pętla wykonuje iteracje po danym obiekcie
- **fn** - funkcja wykonuje określony blok kodu
- **match** - patter matching
- [**+**, -, **/**, __*__, **%**] - operacje arytmetyczne
- [**and**, **or**, **not**, **<**, **≤**, **≥**, **>**, **==**, **≠**] - operacje logiczne

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

loop_statement        = "for" identifier "in" iterator_expression "{" { statement } "}";

conditional_statement = "if" expression "{" { statement } "}";

function_declaration  = "fn" identifier "(" [ parameter_list ] ")" "{" { statement } "}";

pattern_match_stmt    = "match" identifier "{" { case_statement } "}";
case_statement        = "case" expression "=>" statement;
default_statement     = "default" "=>" statement;

parameter_list        = identifier { "," identifier };

logical_operator      = "and"
                      | "or"
                      | "<"
                      | "<="
                      | ">"
                      | ">="
                      | "=="
                      | "!=";

pre_logical_operator  = "!";

iterator_expression   = range | vector | identifier;

expression            = simple_expression [ logical_operator simple_expression ];

simple_expression     = term { arithmetic_operator term };

term                  = factor { arithmetic_operator factor };

factor                = numeric_literal
                      | string_literal
                      | identifier
                      | vector
                      | "(" expression ")"
                      | pre_logical_operator expression;

vector                = "[" [ expression { "," expression } ] "]";

numeric_literal       = natural_number;

string_literal        = '"' character { character } '"';

identifier            = letter { letter | digit | "_" };

arithmetic_operator   = "+"
                      | "-"
                      | "*"
                      | "/"
                      | "%";

natural_number        = digit_without_zero { digit };
character             = letter | digit | " ";

letter                = [a-zA-Z];

digit                 = "0" | digit_without_zero;

digit_without_zero    = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
```

# Format plików

- \*.ks - format plików

Wymagania funkcjonalne

# Analiza wymagań niefunkcjonalnych

- Łatwość nauki - powinien być wystarczająco łatwy do zrozumienia by osoby przychodzące z innych popularnych języków mogliby szybko się zaadoptować
- •Wydajność - język powinien być wystarczająco szybki, aby umożliwić wydajne przetwarzanie dużej ilości danych

# Typy danych

Proste:

- int
- bool
- float

Złożone:

- **String**
- **Range**

# Obsługa błędów

**Błedy**

1. **Syntax error**

```jsx
if x > 0 {
	print("Bigger than 0")
```

_Komunikat_

```bash
Syntax error at line 2: missing closing brace '{'
```

1. **Division by zero**

```jsx
let x = 2 / 0;
```

_Komunikat_

```bash
Division by zero at line 1: division by zero is not allowed
```

2. **Unknown identifier**

```jsx
const a = b + 9;
```

_Komunikat_

```bash
Unknown identifier error at line 1: identifier 'b' not defined
```

3. **Variable redeclaration**

```jsx
let a = 5;
let a = 1;
```

_Komunikat_

```bash
Variable redeclaration ****at line 2: variable ‘a’ is already declerated
```

4. **Type mismatch**

```jsx
let x = "hello";
let z = x + 5;
```

_Komunikat_

```bash
Type mismatch at line 2: the types aren't the same
```

# Sposób uruchomienia

1. Napisać kod programu w pliku o rozszerzeniu \*_.ks_
2. Uruchumienie skompilowanego interpretera za pomocą komendy:

```bash
./ks plik.ks
```

3. Wejście podczas wykonwyania obsługiwany przez **stdin,** wynik zostanie przesłany do **stdout** (wyświetli się na terminalu)

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

# Opis sposobu testowania

Język będzie testowany używając testów jednostkowych. w każdym pliku np. pliku zawierający Lexer będzie się znajdywał blok testów jednostkowych dla modułu.
