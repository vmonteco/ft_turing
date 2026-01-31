# Machine de Turing Universelle - Encodage et Génération

## Encodage des machines

### Format de la bande UTM

La bande de la UTM a cette structure:

```encoding
<S[F](t1)(t2)...(tn)#<left_tape>|<head_symbol><right_tape>
```

**Décomposition:**

| Élément | Description | Exemple |
|---------|-------------|---------|
| `<` | Marqueur de début de bande (permet à la UTM de retrouver le début) | `<` |
| `S` | État courant (lettre a-j) | `a` |
| `[F]` | Liste des états finaux entre crochets | `[cd]` = c et d sont finaux |
| `(ti)` | Transitions encodées (voir format ci-dessous) | `(a,0,b,1,R)` |
| `#` | Séparateur entre description machine et bande simulée | `#` |
| `<left_tape>` | Partie gauche de la bande simulée | `~~~` (blancs) |
| `\|` | Position de la tête de lecture simulée | `\|` |
| `<head_symbol>` | Symbole sous la tête (juste après `\|`) | `0` |
| `<right_tape>` | Reste de la bande à droite | `01` |

**Exemple complet pour la machine `02n` avec input `"00"`:**

```encoding
<a[c](a,0,b,0,R)(a,~,c,y,R)(b,0,a,0,R)(b,~,c,n,R)#~~|00
```

### Mapping des états

Les états de la machine originale sont mappés sur des lettres simples (a-j, max 10 états):

- **L'état initial est toujours `'a'`**
- Les autres états sont mappés dans l'ordre: `b`, `c`, `d`, etc.

### Encodage des symboles

- **Symboles normaux**: gardés tels quels (`0`, `1`, `+`, `-`, `=`, etc.)
- **Blank (`.`)**: encodé comme `~` (tilde) pour éviter confusion avec le blank UTM

### Format des transitions

Chaque transition est encodée comme:

```transition encoding
(from,read,to,write,dir)
```

**Où:**

- `from` : état source (lettre a-j)
- `read` : symbole lu (encodé)
- `to` : état destination (lettre a-j)
- `write` : symbole à écrire (encodé)
- `dir` : direction (`L` ou `R`)

**Exemple:**

```transition encoding
(a,0,b,1,R)  →  "En état a, si je lis 0, j'écris 1, je vais en état b et je me déplace à droite"
```

### États finaux

Liste des états finaux entre crochets, triée alphabétiquement:

```encoding
[cd]   →  les états c et d sont finaux
[a]    →  seul l'état a est final
[]     →  aucun état final
```

### Padding de sécurité

L'encodage ajoute des blancs **avant** le marqueur de tête `|`:
si l'input fait une taille de `n`, il y aura `n` occurences de `~` ajoutés avant.

**Pourquoi?** La UTM ne peut pas insérer dynamiquement des symboles (c'est compliqué et ajouterait des milliers de states pour gérer le décalage à droite de chaque symbol possible de l'alphabet). Si la machine simulée doit écrire à gauche de l'input initial, elle a besoin d'espace pré-alloué.

Cette façon de faire permet de gérer un bon nombre de machines de Turing dont celles du sujet.

---

## Génération de la UTM

### Architecture en 10 phases

La UTM simule une machine M en exécutant un cycle en **10 phases** pour chaque étape de M:

| Phase | Nom | Objectif |
| ----- | --- | -------- |
| **P1** | READ STATE | Lire l'état courant (après `<`) |
| **P2** | FIND HEAD | Trouver le marqueur `\|` |
| **P3** | READ SYMBOL | Lire le symbole sous la tête (après `\|`) |
| **P4** | BACKTRACK | Revenir au début pour chercher transition |
| **P5** | FIND TRANS | Trouver la transition qui matche `(état, symbole)` |
| **P6** | EXTRACT ACTION | Extraire `(to_state, write, dir)` |
| **P7** | UPDATE STATE | Mettre à jour l'état au début de la bande |
| **P8** | WRITE SYMBOL | Écrire le nouveau symbole à la position `\|` |
| **P9** | MOVE HEAD | Déplacer `\|` à gauche ou droite |
| **P10** | CHECK HALT | Vérifier si nouvel état est final → halt ou continuer |

### Convention de nommage des états

#### Structure des noms d'états

Format: `P<phase>_<ACTION>_<contexte>`

**Composants:**

- **Préfixe de phase** : `P1_`, `P2_`, ..., `P10_`
- **Verbe d'action** : `SCAN_`, `READ_`, `MATCH_`, `WRITE_`, `MOVE_`, `CHECK_`
- **Contexte** : variables mémorisées (état, symbole, direction)

#### Exemples concrets

```typescript
// Phase 1 - Lecture de l'état
'P1_START'                    // Point d'entrée
'P1_READ_STATE_LETTER'        // En train de lire l'état
'P1_FOUND_STATE_a'            // État 'a' trouvé et mémorisé

// Phase 3 - Lecture du symbole
'P3_READ_SYM_AT_HEAD_state_a' // Lire symbole, on se souvient que state=a

// Phase 5 - Recherche de transition
'P5_FIND_TRANS_state_a_sym_0'       // Chercher transition pour (a, 0)
'P5_MATCH_FROM_state_a_sym_0'       // Vérifier champ "from"
'P5_MATCH_READ_state_a_sym_0'       // Vérifier champ "read"
'P5_FOUND_TRANS_state_a_sym_0'      // Transition trouvée!

// Phase 9 - Déplacement
'P9_MOVE_R_step1_newState_b'        // Déplacement droit, étape 1, nouvel état = b
'P9_MOVE_L_step2_newState_c'        // Déplacement gauche, étape 2, nouvel état = c
```

#### Pattern de mémorisation

Les noms d'états **encodent les variables** que la UTM doit "se rappeler":

### Alphabet de la UTM

L'alphabet UTM doit contenir **tous** les symboles possibles:

```typescript
const UTM_ALPHABET = [
  '.',                                // Blank UTM
  '<',                                // marqueur début de bande
  ...STATE_LETTERS.split(''),         // a-j
  '@',                                // marqueur état actuel
  '0', '1', '~', '+', '-', '=', 'y', 'n',  // Symboles simulés
  '#', '|', '(', ')', ',', '[', ']', 'L', 'R'  // Structure
];
```

**Note:** Le tilde `~` représente le blank de la machine **simulée**, tandis que `.` est le blank de la UTM elle-même.
