open Belt.Result
open Prelude

type headers
type request = {
  url: string,
  headers: headers,
}

let token2record = Env.domains->Js.Dict.get(_)

@new external makeURL: string => NodeJs.Url.t = "URL"

module Request = {
  @send external get: (headers, string) => string = "get"
}

let parseIP = s => {
  switch s->String.includes(":") {
  | true => IPv6(s)
  | false => IPv4(s)
  }
}

let fetch = async (req: request) => {
  let {url, headers} = req
  let ip = headers->Request.get("CF-Connecting-IP")->parseIP
  let path = (url->makeURL).pathname

  let result = switch path->String.split("/") {
  | [_, "update", token] =>
    switch token2record(token) {
    | Some(domain) =>
      let kind = ip->ip2kind
      let id = await Cloudflare.domain2id(Env.account, domain, kind)
      await id->flatMap(Cloudflare.update(Env.account, _, domain, ip))
    | None => Error("Invalid Token")
    }
  | _ => Error("Invalid Parameter")
  }

  let (resp, code) = switch result {
  | Error(s) => (s, 403)
  | Ok(s) => (s, 200)
  }
  Worker.createResponse(resp)
}
