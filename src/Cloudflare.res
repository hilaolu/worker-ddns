open Prelude

type response = {headers: Js.Dict.t<string>}
module Response = {
  @send external text: response => promise<string> = "text"
}

external fetch: (string, AnyDict.t) => promise<response> = "fetch"

type account = {
  zone: string,
  key: string,
}

let domain2id = async (account, domain, kind) => {
  let api = `https://api.cloudflare.com/client/v4/zones/${account.zone}/dns_records`

  let headers = AnyDict.fromArray([
    ("Content-Type", "application/json"),
    ("Authorization", `Bearer ${account.key}`),
  ])

  let init = AnyDict.fromArray([("headers", headers)])

  let response = await fetch(api, init)
  let text = await response->Response.text

  let records = text->AnyDict.parseString

  let record = records->Option.flatMap(r => {
    r
    ->AnyDict.get("result")
    ->Array.filter(json => {
      json->AnyDict.get("name") == Some(domain) && json->AnyDict.get("type") == Some(kind)
    })
    ->Array.get(0)
  })

  let id = record->Option.flatMap(r => {
    r->Js.Dict.get("id")
  })

  switch id {
  | Some(id) => Ok(id)
  | None => Error("Record not found")
  }
}

let update = async (account, record, name, ip) => {
  let api = `https://api.cloudflare.com/client/v4/zones/${account.zone}/dns_records/${record}`

  let headers = AnyDict.fromArray([
    ("Content-Type", "application/json"),
    ("Authorization", `Bearer ${account.key}`),
  ])

  let init = AnyDict.fromArray([
    ("method", "PUT"),
    ("body", `{"type":"${ip->ip2kind}","name":"${name}","content":"${ip->ip2string}"}`),
    ("headers", headers),
  ])

  let response = await fetch(api, init)
  let text = await response->Response.text
  switch text->AnyDict.parseString->AnyDict.get("success") {
  | Some(true) => Ok("Update success")
  | _ => Error(text)
  }
}
