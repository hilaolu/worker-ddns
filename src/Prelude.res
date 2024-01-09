module AnyDict = {
  type t
  let init = () => {%raw("{}")}
  let get = %raw("(d,k)=>{return d[k]}")
  let set = %raw("(d,k,v)=>{d[k]=v}")
  let fromArray = arr => {
    let d = init()
    arr->Array.forEach(((k, v)) => d->set(k, v))
    d
  }
  let parseString = %raw("(s)=>{return JSON.parse(s)}")
}

let flatMap = async (x, f) => {
  switch x {
  | Ok(x) => await f(x)
  | Error(s) => Error(s)
  }
}

type ip =
  | IPv6(string)
  | IPv4(string)

let ip2string = ip =>
  switch ip {
  | IPv4(s) => s
  | IPv6(s) => s
  }

let ip2kind = ip =>
  switch ip {
  | IPv4(_) => "A"
  | IPv6(_) => "AAAA"
  }
