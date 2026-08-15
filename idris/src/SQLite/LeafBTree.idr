module SQLite.LeafBTree

%default covering

public export
record Cell payload where
  constructor MkCell
  rowId : Integer
  value : payload

public export
record LeafPage payload where
  constructor MkLeafPage
  pageNumber : Nat
  cells : List (Cell payload)

public export
record LeafStore payload where
  constructor MkLeafStore
  pageCapacity : Nat
  pages : List (LeafPage payload)

capacity : Nat → Nat
capacity Z = 1
capacity value = value

public export
emptyStore : Nat → LeafStore payload
emptyStore requested = MkLeafStore (capacity requested) []

insertCell : Cell payload → List (Cell payload) → List (Cell payload)
insertCell new [] = [new]
insertCell new (old :: rest) =
  case compare new.rowId old.rowId of
    LT ⇒ new :: old :: rest
    EQ ⇒ new :: rest
    GT ⇒ old :: insertCell new rest

splitAt : Nat → List element → (List element, List element)
splitAt Z values = ([], values)
splitAt _ [] = ([], [])
splitAt (S amount) (value :: rest) =
  let (front, back) = splitAt amount rest
   in (value :: front, back)

paginate : Nat → Nat → List (Cell payload) → List (LeafPage payload)
paginate _ _ [] = []
paginate amount pageNumber values =
  let (here, later) = splitAt amount values
   in MkLeafPage pageNumber here :: paginate amount (S pageNumber) later

public export
storeScan : LeafStore payload → List (Cell payload)
storeScan store = concatMap cells store.pages

public export
storeInsert : Integer → payload → LeafStore payload → LeafStore payload
storeInsert rowId value store =
  let amount = capacity store.pageCapacity
      ordered = insertCell (MkCell rowId value) (storeScan store)
   in MkLeafStore amount (paginate amount 1 ordered)

public export
storeLookup : Integer → LeafStore payload → Maybe payload
storeLookup wanted store = findIn (storeScan store)
  where
    findIn : List (Cell payload) → Maybe payload
    findIn [] = Nothing
    findIn (cell :: rest) =
      case compare wanted cell.rowId of
        LT ⇒ Nothing
        EQ ⇒ Just cell.value
        GT ⇒ findIn rest

strictlyIncreasing : List (Cell payload) → Bool
strictlyIncreasing [] = True
strictlyIncreasing [_] = True
strictlyIncreasing (left :: right :: rest) =
  left.rowId < right.rowId && strictlyIncreasing (right :: rest)

pagesFit : Nat → List (LeafPage payload) → Bool
pagesFit amount = all (\page ⇒ length page.cells <= amount)

numberedFrom : Nat → List (LeafPage payload) → Bool
numberedFrom _ [] = True
numberedFrom expected (page :: rest) =
  page.pageNumber == expected && numberedFrom (S expected) rest

public export
validStore : LeafStore payload → Bool
validStore store =
  store.pageCapacity > 0
    && pagesFit store.pageCapacity store.pages
    && numberedFrom 1 store.pages
    && strictlyIncreasing (storeScan store)
