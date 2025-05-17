# FLP Projekt 2 --- Babylonská Věž

Autor: Roman Janota
Login: xjanot04
Datum: 26. 04. 2025

## Popis řešení

Implementace řešení problému Babylonské věže je založena na prohledávání stavového prostoru s využitím algoritmu A*. Celkem byly implementovány tři varianty - klasické A\*, A\* využívající dynamickou databázi klauzulí a *Iterative Deepening* A\* (IDA\*). Zásadní rozdíl mezi prvními dvěma algoritmy spočívá v tom, že varianta s dynamickou databází klauzulí nevyžaduje ukládání seznamu *Closed* na zásobník, jelikož k tomuto účelu využívá dynamické predikáty, a tak je jeho náročnost na paměť nižší. Ve výchozím stavu je aktivní varianta "dynamické A\*". Pro aktivaci jiné varianty algoritmu je nutné odkomentovat příslušný řádek v predikátu `main`.

Nedílnou součástí algoritmu A\* je heuristická funkce. Jako heuristika byla zvolena **suma Manhattanských vzdáleností jednotlivých kuliček od jejich cílových pozic**, tedy minimální počet rotací prstenců a přesunů kuliček pomocí volných míst mezi prstenci, potřebných k dosažení správného umístění každé kuličky. Tato heuristika nikdy nepřecení skutečnou cenu optimálního řešení, jelikož každý povolený tah zahrnuje manipulaci pouze s jednou kuličkou, což může potenciálně zhoršit pozici jiné kuličky, a proto tvoří dolní odhad skutečné ceny řešení. Nejprve však byla součástí řešení heuristika, která počítá počet špatně umístěných kuliček. Nicméně testování ukázalo, že tato heuristika neposkytuje dostatečně informativní odhad, jelikož docházelo k uváznutí díry na špatném místě.

## Překlad a spuštění

Součástí projektu je *Makefile*, který přeloží zdrojový kód do binárního souboru s názvem *flp-log24*. Velikost zásobníku je při překladu nastavena na 16 GB.

## Omezení

Hlavním nedostatkem implementace je rozhodně problém práce s pamětí. Ačkoliv by řešení mělo být vždy nalezeno, pokud tedy existuje, vlastní experimenty ukazují, že snadno může dojít k vyčerpání paměti zásobníku (a to i při použití IDA*). Příčin je několik - obrosvký stavový prostor (např. jen pro 4x5 věž je to 20 * 19!), který není dostatečně efektivně filtrován, což vede ke generování a prozkoumávání zbytečných větví. Další příčinou je nekompaktní reprezentace stavu, kdy je každý stav ukládán jako tabulka termů (tj. seznam seznamů). K neefektivitě přispívá také použitá heuristika, která by mohla být optimálnější, a absence podpory pro současné rotace více prstenců.

## Vlastní testovací sada

V adresáři *sample_inputs* se nachází několik vstupních souborů, které byly použity k testování projektu. Tato sada obsahuje konfigurace věží různých velikostí, od 1x1 až po 26x10.

Následující tabulka porovnává rychlost běhu algoritmů A*, A* s dynamickou databází klauzulí a IDA* na konkrétním příkladu věže o rozměrech 4x3:

```
A1 B1 C1 D1
A2 C3 C2 D3
B3 ** D2 B2
```

|         | Klasické A* | Dynamické A* | IDA* |
|:-------:|:-----------:|:------------:|:----:|
| čas (s) |    27.36    |     23.77    |   -  |

Z výsledků tohoto testu vyplývá, že algoritmus "dynamické A*" je nejrychlejší a ke složení věže bylo potřeba 17 kroků. Algoritmu IDA* se v tomto konkrétním případě nepodařilo nalézt řešení, přestože s jednoduššími konfiguracemi věží nemá problémy. S největší pravděpodobností je to způsobeno tím, že oproti ostatním algoritmům vždy vygeneruje všechny možné stavy dané hloubky (tj. hodnoty heuristiky), kterých je ovšem tolik, že dojde k přeplnění zásobníku.
