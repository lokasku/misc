open Types

type t =
  { severity: severity
  ; source: source
  ; msg: string option
  ; code: int option
  ; info: string option
  ; hint: string option
  ; labels: Label.t list }

let create_report severity source =
  {severity; source; msg= None; code= None; info= None; hint= None; labels= []}

let with_msg msg report = {report with msg= Some msg}

let with_code code report = {report with code= Some code}

let with_info info report = {report with info= Some info}

let with_hint hint report = {report with hint= Some hint}

let add_label label report = {report with labels= label :: report.labels}
