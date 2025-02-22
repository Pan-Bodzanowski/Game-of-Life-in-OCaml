type coord = { x: int; y: int }

type state = { c: bool; n: bool option }

let crd (x, y) = { x; y }

module CoordMap = Map.Make (struct
    type t = coord
    let compare = compare
  end)

type board = state CoordMap.t

type rules = { born: int list; live: int list }

type life = rules * board