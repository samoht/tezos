(* The MIT License

Copyright (c) 2019 Craig Ferguson <craig@tarides.com>
                   Thomas Gazagnaire <thomas@tarides.com>
                   Ioana Cristescu <ioana@tarides.com>
                   Clément Pascutto <clement@tarides.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software. *)

include Io_intf

module Extend (S : S) = struct
  include S

  let iter ~page_size ?min:(min_off = 0L) ?max:max_off f io =
    let max_off = match max_off with None -> offset io | Some m -> m in
    let page = String.make page_size '0' in
    let rec aux offset =
      let remaining = Int64.sub max_off offset in
      if remaining <= 0L then Fmt.epr "XXX iter finito\n%!"
      else (
        Fmt.epr "XXX iter offset=%Ld\n%!" offset;
        let n = f ~off:offset ~buf:page ~buf_off:0 in
        aux Int64.(add (of_int n) offset))
    in
    aux min_off
end
