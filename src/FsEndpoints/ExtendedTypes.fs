module internal FsEndpoints.ExtendedTypes

module String =
  let remove (pattern: string) (str: string) =
    str.Replace(pattern, "")