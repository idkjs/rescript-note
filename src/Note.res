/* ---------------------------------------------------------------------------
   Copyright (c) 2018 The note programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*/
module rec Src: {
  type rec t = V(typed<_>): t
  and typed<'a>
  let compare: (t, t) => int
  let id: t => int
  let cell: typed<'a> => C.t<'a>
  let untyped: typed<'a> => t
  let logrs: t => list<Logr.t>
  let add_logr: (Logr.t, t) => unit
  let rem_logr: (Logr.t, t) => unit
  let reset_stamp: t => unit
  let find_active_step: (Step.t, Srcs.t) => Step.t
  let create: (~eq: ('a, 'a) => bool=?, 'a) => typed<'a>
} = {
  type rec t = V(typed<_>): t
  and typed<'a> = {
    id: int,
    cell: C.t<'a>,
    mutable logrs: list<Logr.t>,
    self: t,
  }
  let id = (V(s)) => s.id
  let cell = s => s.cell
  let untyped = s => s.self
  let compare = (V(s), V(t)) => (Pervasives.compare: (int, int) => int)(s.id, t.id)
  let logrs = (V(s)) => s.logrs
  let add_logr = (logr, V(s)) => s.logrs = list{logr, ...s.logrs}
  let rem_logr = (logr, V(s)) => {
    let rec rem = (logr, acc, x) =>
      switch x {
      | list{} => acc
      | list{v, ...vs} if v === logr => List.rev_append(vs, acc)
      | list{v, ...vs} => rem(logr, list{v, ...acc}, vs)
      }
    s.logrs = rem(logr, list{}, s.logrs)
  }
  let reset_stamp = (V(s)) => C.set_stamp(s.cell, Step.nil)
  exception Step(Step.t)
  let find_active_step = (step, ss) =>
    if step !== Step.nil {
      step
    } else {
      let find_not_nil = (V(s)) => {
        let step = C.stamp(s.cell)
        if step !== Step.nil {
          raise_notrace(Step(step))
        }
      }
      try {
        Srcs.iter(find_not_nil, ss)
        Step.nil
      } catch {
      | Step(s) => s
      }
    }
  let uid = {
    let id = ref(0)
    () => {
      incr(id)
      id.contents
    }
  }
  let create = (~eq=?, v) => {
    let update = (_, _) => ()
    let cell = C.create(~eq?, ~step=Step.nil, ~srcs=Srcs.empty, v, ~update)
    let rec src = {id: uid(), cell: cell, logrs: list{}, self: V(src)}
    C.set_srcs(cell, Srcs.singleton(src.self))
    C.set_srcs_changed(cell, false)
    src
  }
}
/* Sources where data is pushed */

/* unique id for the source */
/* Cell holding the source's value */
/* loggers that depend on the source */
/* self, untyped. */

and Srcs: Set.S with type elt = Src.t = Set.Make(Src)
and C: {
  /* Cells */
  type t<'a>
  type rec untyped = C(t<'a>): untyped
  let create: (
    ~eq: ('a, 'a) => bool=?,
    ~step: Step.t,
    ~srcs: Srcs.t,
    'a,
    ~update: (Step.t, t<'a>) => unit,
  ) => t<'a>
  let const: (~eq: ('a, 'a) => bool=?, 'a) => t<'a>
  let eq: (t<'a>, 'a, 'a) => bool
  let set_eq: (t<'a>, ('a, 'a) => bool) => unit
  let with_eq: (('a, 'a) => bool, t<'a>) => t<'a>
  let stamp: t<'a> => Step.t
  let set_stamp: (t<'a>, Step.t) => unit
  let srcs: t<'a> => Srcs.t
  let srcs_changed: t<'a> => bool
  let set_srcs: (t<'a>, Srcs.t) => unit
  let set_srcs_changed: (t<'a>, bool) => unit
  let value: t<'a> => 'a
  let value_changed: t<'a> => bool
  let set_value: (t<'a>, 'a) => unit
  let update: (Step.t, t<'a>) => unit
  let set_update: (t<'a>, (Step.t, t<'a>) => unit) => unit
  let src_update: (Step.t, t<'a>, 'a) => bool
  let up_to_date_value: t<'a> => 'a
  let create_instant: (
    ~step: Step.t,
    ~srcs: Srcs.t,
    option<'a>,
    ~update: (Step.t, t<option<'a>>) => unit,
  ) => t<option<'a>>
  // let reset_instant: t<option<'a>> => unit
  let set_instant: (Step.t, t<option<'a>>, option<'a>) => unit
  let delay: ('a, Lazy.t<t<'a>>) => t<'a>
  let fix: (~eq: ('a, 'a) => bool=?, 'a, t<'a> => (t<'a>, 'b)) => 'b
  let defer: ('a, t<'a>) => t<'a>
  let dump_src_ids: (unit, t<'a>) => unit
} = {
  type rec t<'a> = {
    mutable eq: ('a, 'a) => bool /* testing for cell value equality */,
    mutable stamp: Step.t /* last step in which the cell updated */,
    mutable srcs: Srcs.t /* sources the cell depends on */,
    mutable srcs_changed: bool /* [true] if [srcs] changed */,
    mutable value: 'a /* cell value */,
    mutable value_changed: bool /* [true] if [value] changed */,
    mutable update: (Step.t, t<'a>) => unit,
  } /* updates [value] and [srcs] */
  type rec untyped = C(t<'a>): untyped
  let create = (~eq=\"=", ~step, ~srcs, value, ~update) => {
    eq: eq,
    stamp: step,
    srcs: srcs,
    srcs_changed: true,
    value: value,
    value_changed: true,
    update: update,
  }
  let const = (~eq=\"=", v) => {
    eq: eq,
    stamp: Step.nil,
    srcs: Srcs.empty,
    srcs_changed: false,
    value: v,
    value_changed: false,
    update: (_, _) => (),
  }
  let eq = c => c.eq
  let set_eq = (c, eq) => c.eq = eq
  let with_eq = (eq, c) => {...c, eq: eq}
  let stamp = c => c.stamp
  let set_stamp = (c, stamp) => c.stamp = stamp
  let srcs_changed = c => c.srcs_changed
  let set_srcs_changed = (c, bool) => c.srcs_changed = bool
  let srcs = c => c.srcs
  let set_srcs = (c, srcs) => {
    c.srcs_changed = true
    c.srcs = srcs
  }
  let value = c => c.value
  let value_changed = c => c.value_changed
  let set_value = (c, v) =>
    if c.eq(v, c.value) {
      ()
    } else {
      c.value_changed = true
      c.value = v
    }
  let update = (step, c) =>
    if step !== Step.nil && c.stamp !== step {
      c.stamp = step
      c.srcs_changed = false
      c.value_changed = false
      /* XXX would be nice to avoid constructing the set */
      if {
        open Srcs
        is_empty(inter(c.srcs, Step.srcs(step)))
      } {
        ()
      } else {
        c.update(step, c)
      }
    }
  let set_update = (c, u) => c.update = u
  let src_update = (step, c, v) => {
    c.value_changed = false
    if c.eq(v, c.value) {
      false
    } else {
      c.stamp = step
      c.value_changed = true
      c.value = v
      true
    }
  }
  let up_to_date_value = c => {
    let step = Src.find_active_step(Step.nil, c.srcs)
    update(step, c)
    c.value
  }
  let reset_instant = c => {
    c.value_changed = false
    c.value = None
  }
  let set_instant = (step, c, x) =>
    switch x {
    | None => ()
    | Some(_) as v =>
      c.value_changed = true
      c.value = v
      Step.add_cleanup(step, () => reset_instant(c))
    }
  let create_instant = (~step, ~srcs, value, ~update) => {
    let value_changed = switch value {
    | None => false
    | Some(_) => true
    }
    let c = {
      eq: \"=",
      stamp: step,
      srcs: srcs,
      srcs_changed: true,
      value: value,
      value_changed: value_changed,
      update: update,
    }
    if value_changed && step != Step.nil {
      Step.add_cleanup(step, () => reset_instant(c))
    }
    c
  }
  let delay = (i, z) => failwith("TOOD")
  let fix = (~eq=?, i, cf) => {
    let src = Src.create(~eq?, i)
    let src = Src.V(src)
    and d = Src.cell(src)
    let (c, r) = cf(d)
    let c_update = c.update
    let c_update = (step, self) => {
      c_update(step, self)
      if c.value_changed {
        Step.add_delayed(step, src)
      }
    }
    let d_update = (step, self) =>
      if step === Step.delayed {
        set_value(self, value(c))
      } else {
        ()
      }
    c.update = c_update
    d.update = d_update
    let step = Src.find_active_step(Step.nil, C.srcs(c))
    let () = update(step, c)
    if step === Step.nil {
      Step.execute_delayed(Srcs.singleton(src))
    }
    r
  }
  let defer = (init, c) => {
    /* ** XXX do we really need a source for that. */
    let src = Src.create(~eq=c.eq, init)
    let src = Src.V(src)
    and d = Src.cell(src)
    let update = (step, self) =>
      if step === Step.delayed {
        set_value(self, value(c))
      } else {
        {
          open C
          update(step, c)
        }
        if C.srcs_changed(c) {
          C.set_srcs(d, C.srcs(c))
        }
        if C.value_changed(c) {
          Step.add_delayed(step, src)
        }
      }
    d.update = update
    let step = Src.find_active_step(Step.nil, srcs(c))
    let () = update(step, c)
    let () = update(step, d)
    if step === Step.nil {
      Step.execute_delayed(Srcs.singleton(src))
    }
    d
  }
  let dump_src_ids = (ppf, c) =>
    Js.log3(
      ppf,
      "@[{%a}@]",
      // {
      //   open Format
      //   pp_print_list(~pp_sep=pp_print_space, pp_print_int)
      // },
      List.map(s => Src.id(s), Srcs.elements(c.srcs)),
    )
}
and Logr: {
  type obs<'a>
  let const: 'a => obs<'a>
  let obs_cell: C.t<'a> => obs<'a>
  let app: (obs<'a => 'b>, obs<'a>) => obs<'b>
  let dollar: (obs<'a => 'b>, obs<'a>) => obs<'b>
  type t
  let create: (~now: bool=?, obs<unit>) => t
  let for_cell: (~now: bool=?, C.t<'a>, 'a => unit) => t
  let force: t => unit
  let destroy: t => unit
  let update: (Step.t, t) => unit
  let hold: t => unit
  let may_hold: option<t> => unit
  let unhold_all: unit => unit
} = {
  type obs<'a> = (list<C.untyped>, unit => 'a)
  let const = v => (list{}, () => v)
  let obs_cell = c => (list{C.C(c)}, () => C.value(c))
  let app = ((fcs, f), (vcs, v)) => (List.rev_append(fcs, vcs), () => f()(v()))
  let dollar = app
  type t = {
    mutable stamp: Step.t,
    mutable srcs: Srcs.t /* sources we are registered with */,
    cells: list<C.untyped> /* cells we are observing */,
    log: unit => unit /* logger action */,
  }
  let update_srcs = l => {
    let cells_srcs = l => {
      let add_cell = (acc, C.C(c)) => Srcs.union(acc, C.srcs(c))
      List.fold_left(add_cell, Srcs.empty, l.cells)
    }
    let new_srcs = cells_srcs(l)
    let rems = Srcs.diff(l.srcs, new_srcs)
    let adds = Srcs.diff(new_srcs, l.srcs)
    Srcs.iter(Src.rem_logr(l), rems)
    Srcs.iter(Src.add_logr(l), adds)
    l.srcs = new_srcs
  }
  let update = (step, l) =>
    if step !== Step.nil && step !== l.stamp {
      l.stamp = step
      let rec loop = (step, srcs_changed, value_changed, x) =>
        switch x {
        | list{} =>
          if srcs_changed {
            update_srcs(l)
          }
          if value_changed {
            l.log()
          }
        | list{C.C(c), ...cs} =>
          C.update(step, c)
          loop(step, srcs_changed || C.srcs_changed(c), value_changed || C.value_changed(c), cs)
        }
      loop(step, false, false, l.cells)
    }
  let force = l => {
    let step = Src.find_active_step(Step.nil, l.srcs)
    update(step, l)
    l.log()
  }
  let create = (~now=true, (cells, log)) => {
    let l = {stamp: Step.nil, srcs: Srcs.empty, cells: cells, log: log}
    update_srcs(l)
    if now {
      force(l)
    }
    l
  }
  let for_cell = (~now=?, c, log) => create(~now?, (list{C.C(c)}, () => log(C.value(c))))
  let destroy = l => Srcs.iter(Src.rem_logr(l), l.srcs)
  let held: ref<list<t>> = ref(list{})
  let hold = l => held := list{l, ...held.contents}
  let may_hold = x =>
    switch x {
    | None => ()
    | Some(l) => hold(l)
    }
  let unhold_all = () => {
    List.iter(destroy, held.contents)
    held := list{}
  }
}
and Step: {
  type t
  let create: unit => t
  let nil: t
  let delayed: t
  let srcs: t => Srcs.t
  let add_src: (t, Src.t) => unit
  let add_delayed: (t, Src.t) => unit
  let add_cleanup: (t, unit => unit) => unit
  let execute: t => unit
  let execute_delayed: Srcs.t => unit
} = {
  type t = {
    mutable srcs: Srcs.t /* sources part of the update step */,
    mutable delayed: Srcs.t /* sources for delayed cells */,
    mutable cleanup: list<unit => unit> /* for reseting events to None */,
  }
  let _create = srcs => {srcs: srcs, delayed: Srcs.empty, cleanup: list{}}
  let create = () => _create(Srcs.empty)
  let nil = create()
  let delayed = create()
  let srcs = step => step.srcs
  let add_src = (step, src) => step.srcs = Srcs.add(src, step.srcs)
  let add_delayed = (step, src) => step.delayed = Srcs.add(src, step.delayed)
  let add_cleanup = (step, clean) => step.cleanup = list{clean, ...step.cleanup}
  let cleanup = step => {
    List.iter(f => f(), step.cleanup)
    step.cleanup = list{}
  }
  let already_executed = () => invalid_arg("step already executed")
  let rec execute_delayed = srcs => {
    let update_delayed_src = (ds, Src.V(s)) => {
      let c = Src.cell(s)
      C.update(delayed, c)
      C.set_stamp(c, ds)
    }
    let ds = _create(srcs)
    delayed.srcs = srcs
    Srcs.iter(update_delayed_src(ds), srcs)
    execute(ds)
  }
  and execute = step => {
    let update_src_logs = src => List.iter(Logr.update(step), Src.logrs(src))
    Srcs.iter(update_src_logs, step.srcs)
    Srcs.iter(Src.reset_stamp, step.srcs)
    cleanup(step)
    add_cleanup(step, already_executed) /* error further executes */
    Srcs.is_empty(step.delayed) ? () : execute_delayed(step.delayed)
  }
}

/* High-level interface */
type signal<'a> = C.t<'a>

type event<'a> = C.t<option<'a>>

/* Signal and event definition always have the same structure.

   let combinator ... =
     let update step self =
       C.update step ...
       if C.srcs_changed ... then C.set_srcs self ...
       if C.value_changed ... then C.set_{instant,value} self ...
     in
     let srcs = ...
     let step = Src.find_active_step Step.nil srcs in
     let () = C.update step ... in
     let srcs = ...
     let init =
     C.create ....

   In [update], update dependencies. If dependencies sources changed
   update the cell's sources, if dependencies values changed update
   the cell's value.

   To create the cell. Get the dependency sources. Find the update
   step going on (will be Step.nil if there is none). Update the
   dependencies with the step. Get the sources again (they may have changed)
   and the needed values to create the cell.

   XXX it would be nice to see if we can simply invoke [update] for
   init, possibly with a special Step.init step on Step.nil. */
module E = {
  type t<'a> = event<'a>
  type send<'a> = (~step: Step.t=?, 'a) => unit
  let obs = Logr.obs_cell
  let log = (~now=?, e, f) => {
    let wrap = x =>
      switch x {
      | None => ()
      | Some(v) => f(v)
      }
    Some(Logr.for_cell(~now?, e, wrap))
  }
  let create = () => {
    let src = Src.create(None)
    let send = (~step=?, v) => {
      let (step, exec) = switch step {
      | None => (Step.create(), true)
      | Some(step) => (step, false)
      }
      C.set_stamp(Src.cell(src), step)
      C.set_instant(step, Src.cell(src), Some(v))
      Step.add_src(step, Src.untyped(src))
      if exec {
        Step.execute(step)
      }
    }
    (Src.cell(src), send)
  }
  let value = C.up_to_date_value
  let never = /* XXX */ \"@@"(Obj.magic, C.const(None))
  let bind = (e, f) => {
    let step = Src.find_active_step(Step.nil, C.srcs(e))
    let () = C.update(step, e)
    let current = switch C.value(e) {
    | None => never
    | Some(curr) => f(curr)
    }
    let current = ref(current)
    let update = (step, self) => {
      C.update(step, e)
      switch C.value(e) {
      | None =>
        C.update(step, current.contents)
        if {
          open C
          srcs_changed(e) || srcs_changed(current.contents)
        } {
          C.set_srcs(self, Srcs.union(C.srcs(e), C.srcs(current.contents)))
        }
        C.set_instant(step, self, C.value(current.contents))
      | Some(curr) =>
        current := f(curr)
        C.update(step, current.contents)
        C.set_srcs(self, Srcs.union(C.srcs(e), C.srcs(current.contents)))
        C.set_instant(step, self, C.value(current.contents))
      }
    }
    let step = Src.find_active_step(step, C.srcs(current.contents))
    let () = C.update(step, current.contents)
    let srcs = Srcs.union(C.srcs(e), C.srcs(current.contents))
    let init = C.value(current.contents)
    C.create_instant(~step, ~srcs, init, ~update)
  }
  let join = ee => bind(ee, e => e)
  let swap = es => {
    let step = Src.find_active_step(Step.nil, C.srcs(es))
    let () = C.update(step, es)
    let current = ref(C.value(es))
    let update = (step, self) => {
      C.update(step, es)
      switch C.value_changed(es) {
      | false =>
        C.update(step, current.contents)
        if {
          open C
          srcs_changed(es) || srcs_changed(current.contents)
        } {
          C.set_srcs(self, Srcs.union(C.srcs(es), C.srcs(current.contents)))
        }
      | true =>
        current := C.value(es)
        C.update(step, current.contents)
        C.set_srcs(self, Srcs.union(C.srcs(es), C.srcs(current.contents)))
      }
      C.set_instant(step, self, C.value(current.contents))
    }
    let step = Src.find_active_step(step, C.srcs(current.contents))
    let () = C.update(step, current.contents)
    let srcs = Srcs.union(C.srcs(es), C.srcs(current.contents))
    let init = C.value(current.contents)
    C.create_instant(~step, ~srcs, init, ~update)
  }
  let map = (f, e) => {
    let map = (f, x) =>
      switch x {
      | None => None
      | Some(v) => Some(f(v))
      }
    let update = (step, self) => {
      C.update(step, e)
      if C.srcs_changed(e) {
        C.set_srcs(self, C.srcs(e))
      }
      C.set_instant(step, self, map(f, C.value(e)))
    }
    let step = Src.find_active_step(Step.nil, C.srcs(e))
    let () = C.update(step, e)
    C.create_instant(~step, ~srcs=C.srcs(e), map(f, C.value(e)), ~update)
  }
  let stamp = (e, v) => {
    let stamp = x =>
      switch x {
      | None => None
      | Some(_) => Some(v)
      }
    let update = (step, self) => {
      C.update(step, e)
      if C.srcs_changed(e) {
        C.set_srcs(self, C.srcs(e))
      }
      C.set_instant(step, self, stamp(C.value(e)))
    }
    let step = Src.find_active_step(Step.nil, C.srcs(e))
    let () = C.update(step, e)
    let init = stamp(C.value(e))
    C.create_instant(~step, ~srcs=C.srcs(e), init, ~update)
  }
  let filter = (f, e) => {
    let filter = (f, x) =>
      switch x {
      | None => None
      | Some(v) as occ if f(v) => occ
      | Some(_) => None
      }
    let update = (step, self) => {
      C.update(step, e)
      if C.srcs_changed(e) {
        C.set_srcs(self, C.srcs(e))
      }
      C.set_instant(step, self, filter(f, C.value(e)))
    }
    let step = Src.find_active_step(Step.nil, C.srcs(e))
    let () = C.update(step, e)
    let init = filter(f, C.value(e))
    C.create_instant(~step, ~srcs=C.srcs(e), init, ~update)
  }
  let filter_map = (f, e) => {
    let filter_map = (f, x) =>
      switch x {
      | None => None
      | Some(v) => f(v)
      }
    let update = (step, self) => {
      C.update(step, e)
      if C.srcs_changed(e) {
        C.set_srcs(self, C.srcs(e))
      }
      C.set_instant(step, self, filter_map(f, C.value(e)))
    }
    let step = Src.find_active_step(Step.nil, C.srcs(e))
    let () = C.update(step, e)
    let init = filter_map(f, C.value(e))
    C.create_instant(~step, ~srcs=C.srcs(e), init, ~update)
  }
  let select = es => {
    let add_srcs = (acc, e) => Srcs.union(acc, C.srcs(e))
    let or_srcs_changed = (acc, e) => acc || C.srcs_changed(e)
    let update = (step, self) => {
      List.iter(C.update(step), es)
      let srcs_changed = List.fold_left(or_srcs_changed, false, es)
      if srcs_changed {
        C.set_srcs(self, List.fold_left(add_srcs, Srcs.empty, es))
      }
      let v = switch List.find(e => C.value(e) != None, es) {
      | exception Not_found => None
      | e => C.value(e)
      }
      C.set_instant(step, self, v)
    }
    let find_step = (step, e) => Src.find_active_step(step, C.srcs(e))
    let step = List.fold_left(find_step, Step.nil, es)
    let () = List.iter(C.update(step), es)
    let init = switch List.find(e => C.value(e) != None, es) {
    | exception Not_found => None
    | e => C.value(e)
    }
    let srcs = List.fold_left(add_srcs, Srcs.empty, es)
    C.create_instant(~step, ~srcs, init, ~update)
  }
  let accum = (acc, e) => {
    let acc = ref(acc)
    let accum = x =>
      switch x {
      | None => None
      | Some(f) =>
        acc := f(acc.contents)
        Some(acc.contents)
      }
    let update = (step, self) => {
      C.update(step, e)
      if C.srcs_changed(e) {
        C.set_srcs(self, C.srcs(e))
      }
      C.set_instant(step, self, accum(C.value(e)))
    }
    let step = Src.find_active_step(Step.nil, C.srcs(e))
    let () = C.update(step, e)
    let init = accum(C.value(e))
    C.create_instant(~step, ~srcs=C.srcs(e), init, ~update)
  }
  let until = (~limit=false, ~next, e) => {
    let nop = (step, self) => ()
    let update = (step, self) => {
      {
        open C
        update(step, next)
        update(step, e)
      }
      switch C.value(next) {
      | None =>
        if {
          open C
          srcs_changed(next) || srcs_changed(e)
        } {
          C.set_srcs(self, Srcs.union(C.srcs(next), C.srcs(e)))
        }
        C.set_instant(step, self, C.value(e))
      | Some(_) =>
        C.set_srcs(self, Srcs.empty)
        C.set_update(self, nop)
        C.set_instant(
          step,
          self,
          if limit {
            C.value(e)
          } else {
            None
          },
        )
      }
    }
    let step = Src.find_active_step(Step.nil, C.srcs(next))
    let step = Src.find_active_step(step, C.srcs(e))
    let () = {
      open C
      update(step, next)
      update(step, e)
    }
    switch C.value(next) {
    | None =>
      let srcs = Srcs.union(C.srcs(next), C.srcs(e))
      C.create_instant(~step, ~srcs, C.value(e), ~update)
    | Some(_) =>
      let init = if limit {
        C.value(e)
      } else {
        None
      }
      C.create_instant(~step, ~srcs=Srcs.empty, init, ~update=nop)
    }
  }
  let follow = (e, ~on) => {
    /* FIXME rewrite combinators with this style.
     FIXME determine why we don't simply call update for init in general */
    let deps_srcs = (e, on) => Srcs.union(C.srcs(e), C.srcs(on))
    let deps_srcs_changed = (e, on) => {
      open C
      srcs_changed(e) || srcs_changed(on)
    }
    let update_deps = (step, e, on) => {
      open C
      update(step, e)
      update(step, on)
    }
    let follow = (e, on) =>
      switch e {
      | Some(_) as o if on => o
      | _ => None
      }
    let update = (step, self) => {
      update_deps(step, e, on)
      if deps_srcs_changed(e, on) {
        C.set_srcs(self, deps_srcs(e, on))
      }
      C.set_instant(step, self, follow(C.value(e), C.value(on)))
    }
    let step = Src.find_active_step(Step.nil, C.srcs(e))
    let step = Src.find_active_step(step, C.srcs(on))
    let () = update_deps(step, e, on)
    let init = follow(C.value(e), C.value(on))
    C.create_instant(~step, ~srcs=deps_srcs(e, on), init, ~update)
  }
  let defer = e => C.defer(None, e)
  let fix = ef => C.fix(None, ef)
  module Option = {
    let on_some = e => filter_map(x => x, e)
    let some = e => map(v => Some(v), e)
    let value = (e, ~default) => {
      let update = (step, self) => {
        C.update(step, e)
        if C.srcs_changed(e) {
          C.set_srcs(self, C.srcs(e))
        }
        let occ = switch C.value(e) {
        | None => None
        | Some(Some(_) as v) => v
        | Some(None) =>
          C.update(step, default)
          Some(C.value(default))
        }
        C.set_instant(step, self, occ)
      }
      let step = Src.find_active_step(Step.nil, C.srcs(e))
      let () = {
        C.update(step, e)
        C.update(step, default)
      }
      let init = switch C.value(e) {
      | None => None
      | Some(Some(_) as v) => v
      | Some(None) => Some(C.value(default))
      }
      C.create_instant(~step, ~srcs=C.srcs(e), init, ~update)
    }
    let get = e => map(x =>
        switch x {
        | Some(v) => v
        | None => invalid_arg("option is None")
        }
      , e)
    let bind = (e, f) => map(x =>
        switch x {
        | None => None
        | Some(v) => f(v)
        }
      , e)
    let join = e => map(x =>
        switch x {
        | Some(Some(_) as o) => o
        | _ => None
        }
      , e)
    let is_none = e => map(x =>
        switch x {
        | None => true
        | Some(_) => false
        }
      , e)
    let is_some = e => map(x =>
        switch x {
        | None => false
        | Some(_) => true
        }
      , e)
    let map = (f, e) => map(x =>
        switch x {
        | None => None
        | Some(v) => Some(f(v))
        }
      , e)
  }
  module Pair = {
    let fst = e => map(fst, e)
    let snd = e => map(snd, e)
    let v = (e0, e1) => {
      let update = (step, self) => {
        {
          open C
          update(step, e0)
          update(step, e1)
        }
        if {
          open C
          srcs_changed(e0) || srcs_changed(e1)
        } {
          C.set_srcs(self, Srcs.union(C.srcs(e0), C.srcs(e1)))
        }
        let occ = switch (C.value(e0), C.value(e1)) {
        | (Some(v0), Some(v1)) => Some((v0, v1))
        | _ => None
        }
        C.set_instant(step, self, occ)
      }
      let step = Src.find_active_step(Step.nil, C.srcs(e0))
      let step = Src.find_active_step(step, C.srcs(e1))
      let srcs = Srcs.union(C.srcs(e0), C.srcs(e1))
      let init = switch (C.value(e0), C.value(e1)) {
      | (Some(v0), Some(v1)) => Some((v0, v1))
      | _ => None
      }
      C.create_instant(~step, ~srcs, init, ~update)
    }
  }
  let dump_src_ids = C.dump_src_ids
}

module S = {
  type t<'a> = signal<'a>
  type set<'a> = (~step: Step.t=?, 'a) => unit
  let log = Logr.for_cell
  let obs = Logr.obs_cell
  let eq = C.eq
  let with_eq = C.with_eq
  let create = (~eq=?, v) => {
    let src = Src.create(~eq?, v)
    let set = (~step=?, v) => {
      let (step, exec) = switch step {
      | None => (Step.create(), true)
      | Some(step) => (step, false)
      }
      let cell = Src.cell(src)
      if C.src_update(step, cell, v) {
        Step.add_src(step, Src.untyped(src))
      }
      if exec {
        Step.execute(step)
      }
    }
    (Src.cell(src), set)
  }
  let value = C.up_to_date_value
  let rough_value = C.value
  let const = C.const
  let bind = (v, f) => {
    let step = Src.find_active_step(Step.nil, C.srcs(v))
    let () = C.update(step, v)
    let current = ref(f(C.value(v)))
    let update = (step, self) => {
      C.update(step, v)
      switch C.value_changed(v) {
      | false =>
        C.update(step, current.contents)
        if {
          open C
          srcs_changed(v) || srcs_changed(current.contents)
        } {
          C.set_srcs(self, Srcs.union(C.srcs(v), C.srcs(current.contents)))
        }
        if C.value_changed(current.contents) {
          C.set_value(self, C.value(current.contents))
        }
      | true =>
        current := f(C.value(v))
        C.update(step, current.contents)
        C.set_eq(self, C.eq(current.contents))
        C.set_srcs(self, Srcs.union(C.srcs(v), C.srcs(current.contents)))
        C.set_value(self, C.value(current.contents))
      }
    }
    let step = Src.find_active_step(step, C.srcs(current.contents))
    let () = C.update(step, current.contents)
    let srcs = Srcs.union(C.srcs(v), C.srcs(current.contents))
    let init = C.value(current.contents)
    C.create(~eq=C.eq(current.contents), ~step, ~srcs, init, ~update)
  }
  let hold = (~eq=?, i, e) => {
    let update = (step, self) => {
      C.update(step, e)
      if {
        open C
        srcs_changed(e)
      } {
        C.set_srcs(self, C.srcs(e))
      }
      switch C.value(e) {
      | None => ()
      | Some(v) => C.set_value(self, v)
      }
    }
    let step = Src.find_active_step(Step.nil, C.srcs(e))
    let () = C.update(step, e)
    let init = switch C.value(e) {
    | None => i
    | Some(v) => v
    }
    C.create(~eq?, ~step, ~srcs=C.srcs(e), init, ~update)
  }
  let join = ss => bind(ss, s => s)
  let swap = (s, se) => join(hold(~eq=\"==", s, se))
  let changes = s => {
    let update = (step, self) => {
      C.update(step, s)
      if C.srcs_changed(s) {
        C.set_srcs(self, C.srcs(s))
      }
      if C.value_changed(s) {
        C.set_instant(step, self, Some(C.value(s)))
      }
    }
    let step = Src.find_active_step(Step.nil, C.srcs(s))
    let () = C.update(step, s)
    /* NB: 0 - dt doesn't exist so this is always None */
    C.create_instant(~step, ~srcs=C.srcs(s), None, ~update)
  }
  let sample = (s, ~on, f) => {
    let update = (step, self) => {
      {
        open C
        update(step, on)
        update(step, s)
      }
      if {
        open C
        srcs_changed(on) || srcs_changed(s)
      } {
        C.set_srcs(self, Srcs.union(C.srcs(s), C.srcs(on)))
      }
      switch C.value(on) {
      | None => ()
      | Some(v) => C.set_instant(step, self, Some(f(C.value(s), v)))
      }
    }
    let step = Src.find_active_step(Step.nil, C.srcs(s))
    let step = Src.find_active_step(step, C.srcs(on))
    let () = {
      open C
      update(step, on)
      update(step, s)
    }
    let srcs = Srcs.union(C.srcs(s), C.srcs(on))
    let init = switch C.value(on) {
    | None => None
    | Some(v) => Some(f(C.value(s), v))
    }
    C.create_instant(~step, ~srcs, init, ~update)
  }
  let sample_filter = (s, ~on, f) => E.Option.on_some(sample(s, ~on, f))
  let snapshot = (s, ~on) => sample(s, ~on, (v, _) => v)
  let map = (~eq=?, f, v) => {
    let update = (step, self) => {
      C.update(step, v)
      if C.srcs_changed(v) {
        C.set_srcs(self, C.srcs(v))
      }
      if C.value_changed(v) {
        C.set_value(self, f(C.value(v)))
      }
    }
    let step = Src.find_active_step(Step.nil, C.srcs(v))
    let () = C.update(step, v)
    C.create(~eq?, ~step, ~srcs=C.srcs(v), f(C.value(v)), ~update)
  }
  let app = (~eq=?, f, v) => {
    let update = (step, self) => {
      {
        open C
        update(step, f)
        update(step, v)
      }
      if {
        open C
        srcs_changed(f) || srcs_changed(v)
      } {
        C.set_srcs(self, Srcs.union(C.srcs(f), C.srcs(v)))
      }
      if {
        open C
        value_changed(f) || value_changed(v)
      } {
        C.set_value(self, C.value(f)(C.value(v)))
      }
    }
    let step = Src.find_active_step(Step.nil, C.srcs(f))
    let step = Src.find_active_step(step, C.srcs(v))
    let () = {
      C.update(step, f)
      C.update(step, v)
    }
    let srcs = Srcs.union(C.srcs(f), C.srcs(v))
    let init = C.value(f)(C.value(v))
    C.create(~eq?, ~step, ~srcs, init, ~update)
  }
  let accum = (~eq=?, i, e) => hold(~eq?, i, E.accum(i, e))
  let until = (~limit=false, ~init=?, ~next, s) => {
    let nop = (step, self) => ()
    let update = (step, self) => {
      {
        open C
        update(step, next)
        update(step, s)
      }
      switch C.value(next) {
      | None =>
        if {
          open C
          srcs_changed(next) || srcs_changed(s)
        } {
          C.set_srcs(self, Srcs.union(C.srcs(next), C.srcs(s)))
        }
        C.set_value(self, C.value(s))
      | Some(_) =>
        C.set_srcs(self, Srcs.empty)
        C.set_update(self, nop)
        if limit {
          C.set_value(self, C.value(s))
        } else {
          ()
        }
      }
    }
    let step = Src.find_active_step(Step.nil, C.srcs(next))
    let step = Src.find_active_step(step, C.srcs(s))
    let () = {
      open C
      update(step, next)
      update(step, s)
    }
    switch C.value(next) {
    | None =>
      let srcs = Srcs.union(C.srcs(next), C.srcs(s))
      C.create(~eq=eq(s), ~step, ~srcs, C.value(s), ~update)
    | Some(_) =>
      let init = switch init {
      | None => C.value(s)
      | Some(i) => i
      }
      C.create(~eq=eq(s), ~step, ~srcs=Srcs.empty, init, ~update=nop)
    }
  }
  let follow = (~init=?, s, ~on) => {
    let deps_srcs = (s, on) => Srcs.union(C.srcs(s), C.srcs(on))
    let deps_srcs_changed = (s, on) => {
      open C
      srcs_changed(s) || srcs_changed(on)
    }
    let update_deps = (step, s, on) => {
      open C
      update(step, s)
      update(step, on)
    }
    let update = (step, self) => {
      update_deps(step, s, on)
      if deps_srcs_changed(s, on) {
        C.set_srcs(self, deps_srcs(s, on))
      }
      if C.value(on) {
        C.set_value(self, C.value(s))
      }
    }
    let step = Src.find_active_step(Step.nil, C.srcs(s))
    let step = Src.find_active_step(step, C.srcs(on))
    let () = update_deps(step, s, on)
    let init = switch init {
    | None => C.value(s)
    | Some(i) => i
    }
    C.create(~eq=eq(s), ~step, ~srcs=deps_srcs(s, on), init, ~update)
  }
  let delay = C.delay
  let defer = (~init=?, s) => {
    let init = switch init {
    | Some(init) => init
    | None =>
      let step = Src.find_active_step(Step.nil, C.srcs(s))
      let () = C.update(step, s)
      C.value(s)
    }
    C.defer(init, s)
  }
  let fix = C.fix
  let l1 = (~eq=?, f, x) => map(~eq?, f, x)
  let l2 = (~eq=?, f, x, y) => {
    let update = (step, self) => {
      {
        open C
        update(step, x)
        update(step, y)
      }
      if {
        open C
        srcs_changed(x) || srcs_changed(y)
      } {
        C.set_srcs(self, Srcs.union(C.srcs(x), C.srcs(y)))
      }
      if {
        open C
        value_changed(x) || value_changed(y)
      } {
        C.set_value(self, f(C.value(x), C.value(y)))
      }
    }
    let step = Src.find_active_step(Step.nil, C.srcs(x))
    let step = Src.find_active_step(step, C.srcs(y))
    let () = {
      open C
      update(step, x)
      update(step, y)
    }
    let srcs = Srcs.union(C.srcs(x), C.srcs(y))
    let init = f(C.value(x), C.value(y))
    C.create(~eq?, ~step, ~srcs, init, ~update)
  }
  let l3 = (~eq=?, f, x, y, z) => {
    let srcs_union = (x, y, z) => Srcs.union(C.srcs(x), Srcs.union(C.srcs(y), C.srcs(z)))
    let update = (step, self) => {
      {
        open C
        update(step, x)
        update(step, y)
        update(step, z)
      }
      if {
        open C
        srcs_changed(x) || (srcs_changed(y) || srcs_changed(z))
      } {
        C.set_srcs(self, srcs_union(x, y, z))
      }
      if {
        open C
        value_changed(x) || (value_changed(y) || value_changed(z))
      } {
        C.set_value(self, f(C.value(x), C.value(y), C.value(z)))
      }
    }
    let step = Src.find_active_step(Step.nil, C.srcs(x))
    let step = Src.find_active_step(step, C.srcs(y))
    let step = Src.find_active_step(step, C.srcs(z))
    let () = {
      open C
      update(step, x)
      update(step, y)
      update(step, z)
    }
    let srcs = srcs_union(x, y, z)
    let init = f(C.value(x), C.value(y), C.value(z))
    C.create(~eq?, ~step, ~srcs, init, ~update)
  }
  module Bool = {
    let eq: (bool, bool) => bool = \"="
    let false' = const(false)
    let true' = const(true)
    let not = s => map(~eq, not, s)
    let \"&&" = l2(\"&&")
    let \"||" = l2(\"||")
    let edge = s => changes(s)
    let edge_detect = (edge, s) => {
      let update = (step, self) => {
        C.update(step, s)
        if C.srcs_changed(s) {
          C.set_srcs(self, C.srcs(s))
        }
        if Pervasives.\"&&"(C.value_changed(s), C.value(s) == edge) {
          C.set_instant(step, self, Some())
        }
      }
      let step = Src.find_active_step(Step.nil, C.srcs(s))
      let () = C.update(step, s)
      C.create_instant(~step, ~srcs=C.srcs(s), None, ~update)
    }
    let rise = s => edge_detect(true, s)
    let fall = s => edge_detect(false, s)
    let flip = (~init, e) => {
      let update = (step, self) => {
        C.update(step, e)
        if C.srcs_changed(e) {
          C.set_srcs(self, C.srcs(e))
        }
        switch C.value(e) {
        | None => ()
        | Some(_) => C.set_value(self, Pervasives.not(C.value(self)))
        }
      }
      let step = Src.find_active_step(Step.nil, C.srcs(e))
      let () = C.update(step, e)
      let init = switch C.value(e) {
      | Some(_) => Pervasives.not(init)
      | None => init
      }
      C.create(~eq, ~step, ~srcs=C.srcs(e), init, ~update)
    }
  }
  module Option = {
    let _eq = (eq, v0, v1) =>
      switch (v0, v1) {
      | (Some(v0), Some(v1)) => eq(v0, v1)
      | (None, None) => true
      | (_, _) => false
      }
    let none = /* XXX */ \"@@"(Obj.magic, const(None))
    let some = s => map(~eq=_eq(eq(s)), v => Some(v), s)
    let hold_value = (i, s) => {
      let update = (step, self) => {
        C.update(step, s)
        if C.srcs_changed(s) {
          C.set_srcs(self, C.srcs(s))
        }
        switch C.value(s) {
        | None => ()
        | Some(v) => C.set_value(self, v)
        }
      }
      let eq = (v, v') => C.eq(s, Some(v), Some(v'))
      let step = Src.find_active_step(Step.nil, C.srcs(s))
      let () = C.update(step, s)
      let init = switch C.value(s) {
      | None => i
      | Some(v) => v
      }
      C.create(~eq, ~step, ~srcs=C.srcs(s), init, ~update)
    }
    let value = (s, ~default) => {
      let update = (step, self) => {
        C.update(step, default)
        C.update(step, s)
        if {
          open C
          srcs_changed(default) || C.srcs_changed(s)
        } {
          C.set_srcs(self, Srcs.union(C.srcs(default), C.srcs(s)))
        }
        if C.value_changed(default) || C.value_changed(s) {
          switch C.value(s) {
          | None => C.set_value(self, C.value(default))
          | Some(v) => C.set_value(self, v)
          }
        }
      }
      let step = Src.find_active_step(Step.nil, C.srcs(default))
      let step = Src.find_active_step(step, C.srcs(s))
      let () = {
        open C
        update(step, default)
        update(step, s)
      }
      let init = switch C.value(s) {
      | None => C.value(default)
      | Some(v) => v
      }
      let srcs = Srcs.union(C.srcs(default), C.srcs(s))
      C.create(~eq=eq(default), ~step, ~srcs, init, ~update)
    }
    let get = (~eq=?, s) =>
      map(
        ~eq?,
        x =>
          switch x {
          | Some(v) => v
          | None => invalid_arg("option is None")
          },
        s,
      )
    let bind = (~eq=?, s, f) =>
      map(
        ~eq?,
        x =>
          switch x {
          | None => None
          | Some(v) => f(v)
          },
        s,
      )
    let join = (~eq=?, s) =>
      map(
        ~eq?,
        x =>
          switch x {
          | Some(Some(_) as o) => o
          | _ => None
          },
        s,
      )
    let is_none = s =>
      map(
        ~eq=Bool.eq,
        x =>
          switch x {
          | None => true
          | Some(_) => false
          },
        s,
      )
    let is_some = s =>
      map(
        ~eq=Bool.eq,
        x =>
          switch x {
          | None => false
          | Some(_) => true
          },
        s,
      )
    let map = (~eq=?, f, s) =>
      map(
        ~eq?,
        x =>
          switch x {
          | None => None
          | Some(v) => Some(f(v))
          },
        s,
      )
    let eq = _eq
  }
  module Pair = {
    let fst = (~eq=?, s) => map(~eq?, fst, s)
    let snd = (~eq=?, s) => map(~eq?, snd, s)
    let v = (s0, s1) => l2((x, y) => (x, y), s0, s1)
  }
  let dump_src_ids = C.dump_src_ids
}
/* ---------------------------------------------------------------------------
   Copyright (c) 2018 The note programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*/
