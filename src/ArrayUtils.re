let _createEmpty = () => [||];

let flatten = (arr: array('item)) =>
  arr |> Js.Array.reduce((a, b) => Js.Array.concat(b, a), _createEmpty());