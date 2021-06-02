/* ---------------------------------------------------------------------------
   Copyright (c) 2018 The note programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*/
@ocaml.doc(" Declarative events and signals for OCaml.

    {b XXX.} The denotational semantics and notations are not
    documented yet. But basically they are the same
    as {!React}'s ones. ")
@ocaml.doc(" {1 Note} ")
@ocaml.doc(" Update steps.

    Update steps allow to schedule {e simultaneous} primitive event occurence
    and signal changes. ")
module Step: {
  @ocaml.doc(" {1:steps Steps} ") @ocaml.doc(" The type for update steps. ")
  type t
  @ocaml.doc(" [create ()] is a new update step. ")
  let create: unit => t
  @ocaml.doc(" [execute step] executes the update step [step].

      @raise Invalid_argument if [step] was already executed. ")
  let execute: t => unit
}

@ocaml.doc(" Event and signal changes loggers.

    Loggers are the output interface of the reactive system. They
    allow external entities to observe event occurences and signal
    changes. ")
module Logr: {
  @ocaml.doc(" {1:obs Observations} ") @ocaml.doc(" The type for observing changes of type ['a]. ")
  type obs<'a>
  @ocaml.doc(" [const v] never changes and always observes [v]. ")
  let const: 'a => obs<'a>
  @ocaml.doc(" [app f v] is the observation that result from applying the
      changes of [f] to the ones of [v]. ")
  let app: (obs<'a => 'b>, obs<'a>) => obs<'b>
  @ocaml.doc(" [f $ v] is [app f v]. ")
  let dollar: (obs<'a => 'b>, obs<'a>) => obs<'b>
  @ocaml.doc(" {1:loggers Loggers} ") @ocaml.doc(" The type for loggers. ")
  type t
  @ocaml.doc(" [create ~now o] is a logger that observes changes to [o]. If
      [now] is [true] (default) logs at least one observation before
      the call returns. ")
  let create: (~now: bool=?, obs<unit>) => t
  @ocaml.doc(" [force l] observes [l] even if nothing changed. ")
  let force: t => unit
  @ocaml.doc(" [destroy l] destroys log [l] this ensure that [l]'s does not
      observe any change again. The underlying observed events or
      signals also stop updating, unless they are observed by another
      logger. ")
  let destroy: t => unit
  @ocaml.doc(" [hold l] holds logger [l] to ensure it does not get garbage
      collected. ")
  let hold: t => unit
  @ocaml.doc(" [may_hold l] holds logger [Some l] to ensure it does not get
      garbage collected. Does nothing on [None]. ")
  let may_hold: option<t> => unit
  @ocaml.doc(" [unhold_all ()] {{!destroy}destroys} and unholds all loggers held
      via {!hold}. ")
  let unhold_all: unit => unit
}

@ocaml.doc(" The type for signals of type 'a ")
type signal<'a>

@ocaml.doc(" The type for events of type 'a ")
type event<'a>

@ocaml.doc(" Events.

    An event is a value with discrete occurences over time. ")
module E: {
  @ocaml.doc(" {1:ev Events} ") @ocaml.doc(" The type for events with occurences of type ['a]. ")
  type t<'a> = event<'a>
  @ocaml.doc(" The type for functions sending event occurences of type ['a].
      See {!create}. ")
  type send<'a> = (~step: Step.t=?, 'a) => unit
  @ocaml.doc(" [obs e] is an observation for [e]'s occurences. ")
  let obs: t<'a> => Logr.obs<option<'a>>
  @ocaml.doc(" [log ?now e f] is [Some (Logr.(create ?now (const f $ obs e)))]
      if [e] is not {!never} and [None] otherwise. ")
  let log: (~now: bool=?, event<'a>, 'a => unit) => option<Logr.t>
  @ocaml.doc(" [create ()] is a primitive event [e] and a [send] function.
      The function [send] is such that:
      {ul
      {- [send v] generates an occurence [v] of [e] at the time it is
         called .}
      {- [send ~step v] generates an occurence [v] of [e] at the time
         [step]. The function should not be called again before [step]
         is {{!Step.execute}executed}.}}
      {b Warning.} [send] must not be used in the definition of signals
      or events. ")
  let create: unit => (event<'a>, send<'a>)
  @ocaml.doc(" [value e] is the value of event [e] at call time. If this is [None]
      the event has no occurence, if this is [Some v], the event occurs
      with [v]. ")
  let value: event<'a> => option<'a>
  @ocaml.doc(" [never] is a never occuring event, \[[never]\]{_t} [= None]. ")
  let never: event<'a>
  @ocaml.doc(" [bind e f] is the event that results from applying [f] to
      the last event of [e]:
      {ul
      {- \[[bind e f]\]{_ t} [=] \[[f v]\]{_t} if \[[e]\]{_<=t} [= Some v].}
      {- \[[bind e f]\]{_ t} [=] [never] if \[[e]\]{_<=t} [= None].}} ")
  let bind: (event<'a>, 'a => event<'b>) => event<'b>
  @ocaml.doc(" [join ee] is [bind ee (fun e -> e)]. ")
  let join: event<event<'a>> => event<'a>
  @ocaml.doc(" [swap es] is the current event of [es],
      \[[swap es]\]{_t} [=] \[\[[es]\]{_t}\]{_t}. ")
  let swap: signal<event<'a>> => event<'a>
  @ocaml.doc(" [map f e] applies [f] to [e]'s occurrences.
      {ul
      {- \[[map f e]\]{_t} [= Some (f v)] if \[[e]\]{_t} [= Some v].}
      {- \[[map f e]\]{_t} [= None] otherwise.}} ")
  let map: ('a => 'b, event<'a>) => event<'b>
  @ocaml.doc(" [stamp e v] is [map e (fun _ -> v)] ")
  let stamp: (event<'b>, 'a) => event<'a>
  @ocaml.doc(" [filter p e] are the occurences of [e] that satisfy [p].
       {ul
       {- \[[filter p e]\]{_t} [= Some v] if \[[e]\]{_t} [= Some v] and
       [p v = true]}
       {- \[[filter p e]\]{_t} [= None] otherwise.}} ")
  let filter: ('a => bool, event<'a>) => event<'a>
  @ocaml.doc(" [filter_map fm e] are [e]'s occurrences filtered and mapped by [fm].
      {ul
      {- \[[filter_map fm e]\]{_t} [= Some v] if [fm] \[[e]\]{_t} [= Some v]}
      {- \[[filter_map fm e]\]{_t} [= None] otherwise.}} ")
  let filter_map: ('a => option<'b>, event<'a>) => event<'b>
  @ocaml.doc(" [select el] is the occurences of every event in [el]. If more
      than one event occurs simlutanously, the leftmost is taken
      and the other are lost:
      {ul
      {- \[[select el]\]{_ t} [=] \[[List.find (fun e -> ]\[[e]\]{_t}
      [<> None) el]\]{_t}.}
      {- \[[select el]\]{_ t} [= None] otherwise.}} ")
  let select: list<event<'a>> => event<'a>
  @ocaml.doc(" [accum i e] accumulates a value, starting with [i], using [e]'s
      functional occurrences.
      {ul
      {- \[[accum i e]\]{_t} [= Some (f i)] if \[[e]\]{_t} [= Some f]
      and \[[e]\]{_<t} [= None].
      }
      {- \[[accum i e]\]{_t} [= Some (f acc)] if \[[e]\]{_t} [= Some f]
         and \[[accum i e]\]{_<t} [= Some acc].}
      {- \[[accum i e]\] [= None] otherwise.}} ")
  let accum: ('a, event<'a => 'a>) => event<'a>
  @ocaml.doc(" [until ~limit ~next e] is [e]'s occurences until [next]
      occurs. At that point if [e] occurs simultaneously the occurence is
      discarded ([limit] is [false], default) or kept ([limit] is [true])
      and after this the event never occurs again.
      {ul
      {- \[[until ~limit ~next e]\]{_t} [=] \[[e]\]{_t}
         if \[[next]\]{_<=t} [= None]}
      {- \[[until ~limit:false ~next e]\]{_t} [= None]
         if \[[next]\]{_t} [= Some _] and \[[next]\]{_<t} [= None].}
      {- \[[until ~limit:true ~next e]\]{_t} [=] \[[e]\]{_t}
         if \[[next]\]{_t} [= Some _] and \[[next]\]{_<t} [= None].}
      {- \[[until ~limit ~next e]\]{_t} [= None] otherwise.}} ")
  let until: (~limit: bool=?, ~next: event<'a>, event<'b>) => event<'b>
  @ocaml.doc(" [follow e ~on] is [e]'s occurences whenever [on] is [true].
      {ul
      {- \[[follow e ~on]\]{_t} [=] \[[e]\]{_t} if \[[on]\]{_t} [= true]}
      {- \[[follow e ~on]\]{_t} [= None]  if \[[on]\]{_t} [= false]}} ")
  let follow: (event<'a>, ~on: signal<bool>) => event<'a>
  @ocaml.doc(" [defer s] is [s] delayed by an infinitesimal amount of time.
      At creation time [init] is used (defaults to [S.value s]).
      {ul
      {- \[[defer e]\]{_ t} [=] [None] for t = 0. }
      {- \[[defer e]\]{_ t} [=] \[[e]\]{_t-dt} otherwise.}} ")
  let defer: event<'a> => event<'a>
  @ocaml.doc(" [fix ef] allows to refer to the value an event had an
      infinitesimal amount of time before.

      In [fix ef], [ef] is called with an event [e] that represents
      the event returned by [ef] delayed by an infinitesimal amount of
      time.  If [e', r = ef e] then [r] is returned by [fix] and [e]
      is such that :
      {ul
      {- \[[e]\]{_ t} [=] [None] if t = 0 }
      {- \[[e]\]{_ t} [=] \[[e']\]{_t-dt} otherwise}}

      {b Raises.} [Invalid_argument] if [e'] is directly a delayed event (i.e.
      an event given to a fixing function). ")
  let fix: (event<'a> => (event<'a>, 'b)) => 'b
  @ocaml.doc(" {1:stdlib Stdlib types support} ") @ocaml.doc(" Option events ")
  module Option: {
    @ocaml.doc(" [on_some e] is [e] when [Some _] occurs:
        {ul
        {- \[[on_some e]\]{_t} [= Some v] if \[[e]\]{_t} [= Some (Some v)]}
        {- \[[on_some e]\]{_t} [= None] otherwise.}} ")
    let on_some: event<option<'a>> => event<'a>
    @ocaml.doc(" {1:lift Lifted {!Stdlib.Option} module} ")
    @ocaml.doc(" [some e] is [map (fun v -> Some v) e]. ")
    let some: event<'a> => event<option<'a>>
    @ocaml.doc(" [value e default] is [default] when [e] occurs with [None]:
        {ul
        {- \[[value e ~default]\]{_t} [= None] if \[[e]\]{_t} [= None]}
        {- \[[value e ~default]\]{_t} [= Some v]
           if \[[e]\]{_t} [= Some (Some v)]}
        {- \[[value e ~default]\]{_t} [=] \[[default]\]{_t} if \[[e]\]{_t}
            [= Some None]}} ")
    let value: (event<option<'a>>, ~default: signal<'a>) => event<'a>
    @ocaml.doc(" [get e] is [map get e]. ")
    let get: event<option<'a>> => event<'a>
    @ocaml.doc(" [bind e f] is [map (fun v -> Option.bind v f) e]. ")
    let bind: (event<option<'a>>, 'a => option<'b>) => event<option<'b>>
    @ocaml.doc(" [join e] is [map Option.join e]. ")
    let join: event<option<option<'a>>> => event<option<'a>>
    @ocaml.doc(" [map f e] is [map Option.map e]. ")
    let map: ('a => 'b, event<option<'a>>) => event<option<'b>>
    @ocaml.doc(" [is_none e] is [map is_none e]. ")
    let is_none: event<option<'a>> => event<bool>
    @ocaml.doc(" [is_some e] is [map is_some e]. ")
    let is_some: event<option<'a>> => event<bool>
  }
  @ocaml.doc(" Pair events. ")
  module Pair: {
    @ocaml.doc(" [fst e] is [map fst e]. ")
    let fst: event<('a, 'b)> => event<'a>
    @ocaml.doc(" [snd e] is [map snd e]. ")
    let snd: event<('a, 'b)> => event<'b>
    @ocaml.doc(" [v e0 e1] pair simultaneous occurences of [e0] and [e1]:
        {ul
        {- \[[v e0 e1]\]{_t} [= Some (v0, v1)] if \[[e0]\]{_t} [= Some v0]
           and \[[e1]\]{_t} [= Some v1]}
        {- \[[v e0 e1]\]{_t} [= None] otherwise.}} ")
    let v: (event<'a>, event<'b>) => event<('a, 'b)>
  }

  let dump_src_ids: (unit, event<'a>) => unit
}

@ocaml.doc(" Signals.

    A signal is a value that varies continuously over time. It has
    a value at every point in time. ")
module S: {
  @ocaml.doc(" {1:sig Signals} ") @ocaml.doc(" The type for signals of type ['a]. ")
  type t<'a> = signal<'a>
  @ocaml.doc(" The type for functions setting signal values of type ['a].
      See {!create}.")
  type set<'a> = (~step: Step.t=?, 'a) => unit
  @ocaml.doc(" [obs s] is an observation for [s]. ")
  let obs: t<'a> => Logr.obs<'a>
  @ocaml.doc(" [log ?now s f] is [Logr.(create ~now (const f $ obs s))]. ")
  let log: (~now: bool=?, signal<'a>, 'a => unit) => Logr.t
  @ocaml.doc(" [create v] is a primitive signal set to the value [v] and a
      [set] function. The function [set] is such that:
      {ul
      {- [set v] sets the signal's value to [v] at the time it is called.}
      {- [set ~step v] sets the signal value to [v] at the time it is called
         and schedules an update at time [step].}}

      {b Warning.} [set] must not be used in the definition of signals
      or events. ")
  let create: (~eq: ('a, 'a) => bool=?, 'a) => (signal<'a>, set<'a>)
  @ocaml.doc(" [eq s] is [s]'s equality function. ")
  let eq: (signal<'a>, 'a, 'a) => bool
  @ocaml.doc(" [with_eq eq s] is [s] with equality function [eq]. ")
  let with_eq: (('a, 'a) => bool, signal<'a>) => signal<'a>
  @ocaml.doc(" [value s] is the current value of [s], \[[s]\]{_t} ")
  let value: signal<'a> => 'a
  @ocaml.doc(" [rough_value s] is the current value of [s], but in contrast to
      {!value} it might not be exactly \[[s]\]{_t}. ")
  let rough_value: signal<'a> => 'a
  @ocaml.doc(" [const v] is always [v], \[[const v]\]{_t} [= v]. ")
  let const: (~eq: ('a, 'a) => bool=?, 'a) => signal<'a>
  @ocaml.doc(" [hold i e] has the value of [e]'s last occurrence or the
      value of [i] provides the signal value at creation time if
      there's no event at that time.
      {ul
      {- \[[hold i e]\]{_t} [= i] if \[[e]\]{_<=t} [= None]}
      {- \[[hold i e]\]{_t} [= v] if \[[e]\]{_<=t} [= Some v]}} ")
  let hold: (~eq: ('a, 'a) => bool=?, 'a, event<'a>) => signal<'a>
  @ocaml.doc(" [bind s f] is the signal that results from applying [f] to
      [s], \[[bind s f]\]{_ t} [=] \[f\[[s]\]{_t}\]{_t}. ")
  let bind: (signal<'a>, 'a => signal<'b>) => signal<'b>
  @ocaml.doc(" [join ss] is [bind ss (fun s -> s)]. ")
  let join: signal<signal<'a>> => signal<'a>
  @ocaml.doc(" [swap s se] is [join (hold ~eq:( == ) s se)] that is the values of
      [s] followed by values of the last signal that occured on [se]. ")
  let swap: (signal<'a>, event<signal<'a>>) => signal<'a>
  @ocaml.doc(" [changes s] occurs with the value of [s] whenever it changes.
      {ul
      {- \[[changes s]\]{_t} [= Some v]
      if \[[s]\]{_t} [= v] and \[[s]\]{_t-dt} [= v'] and [eq v v' = false].}
      {- \[[changes s]\]{_t} [= None] otherwise.}}

      {b Warning.} By definition no event occurs if [s] changes at
      creation time ([0 - dt] is undefined). ")
  let changes: signal<'a> => event<'a>
  @ocaml.doc(" [map f s] is [s] transformed by [f],
      \[[map f s]\]{_t} = [f] \[[s]\]{_t}. ")
  let map: (~eq: ('b, 'b) => bool=?, 'a => 'b, signal<'a>) => signal<'b>
  @ocaml.doc(" [app sf s] holds the value of [sf] applied to the value of [s],
      \[[app sf s]\]{_t} [=] \[[sf]\]{_t} \[[s]\]{_t}. ")
  let app: (~eq: ('b, 'b) => bool=?, signal<'a => 'b>, signal<'a>) => signal<'b>
  @ocaml.doc(" [sample s ~on f] samples [s] at [e]'s occurrences.
      {ul
      {- \[[sample s ~on f]\]{_t} [= Some (f sv ev)] if \[[on]\]{_t} [= Some ev]
         and  \[[s]\]{_t} [= sv].}
      {- \[[sample s ~on f]\]{_t} [= None] otherwise.}} ")
  let sample: (signal<'b>, ~on: event<'a>, ('b, 'a) => 'c) => event<'c>
  @ocaml.doc(" [sample_filter s on f] is [E.Option.on_some (sample s ~on f)]. ")
  let sample_filter: (signal<'b>, ~on: event<'a>, ('b, 'a) => option<'c>) => event<'c>
  @ocaml.doc(" [snapshot ~on s] is [sample (fun v _ -> v) ~on s].

      {b TODO.} Candidate for deletion. ")
  let snapshot: (signal<'b>, ~on: event<'a>) => event<'b>
  /*
     val active : on:bool signal -> 'a signal -> 'a signal
     (** [active ~on s] is has the value of [s] at creation
         time and then mirrors [s] whenever [on] is [true].
         When [on] is false holds the last value [s] had when [on]
         was true.
         {ul
         {- \[[active ~on s]\]{_0} [=] \[[s]\]{_0}}
         {- \[[active ~on s]\]{_t} [=] \[[s]\]{_t} if \[[on]\]{_t} [= true]}
         {- \[[active ~on s]\]{_t} [=] \[[s]\]{_t'} if \[[on]\]{_t} [= false]
            where t' is the greatest 0 < t' < t with \[[on]\]{_t'} [= true].}} *)
 */
  @ocaml.doc(" [accum i e] is [hold i (E.accum i e)]. ")
  let accum: (~eq: ('a, 'a) => bool=?, 'a, event<'a => 'a>) => signal<'a>
  @ocaml.doc(" [until ~limit ~init ~next s] is [s] until [next] occurs, after
      which the value [s] had just before ([limit] is [false], default)
      or whenever [next] occurs ([limit] is [true]) is kept forever.
      {ul
      {- \[[until ~limit ~init ~next s]\]{_t} [=] \[[s]\]{_t}
         if \[[next]\]{_<=t} [= None]}
      {- \[[until ~limit ~init ~next s]\]{_t}
         [= init] if \[[next]\]{_0} [= Some _]}
      {- \[[until ~limit:false ~init ~next s]\]{_t} [=] \[[s]\]{_t'- dt}
         if \[[next]\]{_t'} [= Some _] and \[[next]\]{_<t'} [= None].}
      {- \[[until ~limit:true ~init ~next s]\]{_t} [=] \[[s]\]{_t'}
         if \[[next]\]{_t'} [= Some _] and \[[next]\]{_<t'} [= None].}}
      [init] defaults to [value s]. ")
  let until: (~limit: bool=?, ~init: 'b=?, ~next: event<'a>, signal<'b>) => signal<'b>
  @ocaml.doc(" [follow ~init s ~on] is [s] whenever [on] is [true] and the last
      value of [s] when [on] was [true] if [on] is [false]. If [on] is
      [false] at creation time [init] is used (defaults to [S.value
      s]).
      {ul
      {- \[[follow ~init s ~on]\]{_0} [=] \[[s]\]{_0}
         if \[[on]\]{_0} [= true]}
      {- \[[follow ~init s ~on]\]{_0} [=] \[[init]\]{_0}
         if \[[on]\]{_0} [= false]}
      {- \[[follow ~init s ~on]\]{_t} [=] \[[s]\]{_t}
         if \[[on]\]{_t} [= true]}
      {- \[[follow ~init s ~on]\]{_t} [=] \[[follow ~init s ~on]\]{_t'}
         if \[[on]\]{_t} [= false] where t' is the
         greatest t' < t with \[[on]\]{_t'} [= true] or [0] if there
         is no such time.}} ")
  let follow: (~init: 'a=?, signal<'a>, ~on: signal<bool>) => signal<'a>
  @ocaml.doc(" [defer s] is [s] delayed by an infinitesimal amount of time.
      At creation time [init] is used (defaults to [S.value s]).
      {ul
      {- \[[defer s]\]{_ t} [=] [init] for t = 0. }
      {- \[[defer s]\]{_ t} [=] \[[s]\]{_t-dt} otherwise.}} ")
  let defer: (~init: 'a=?, signal<'a>) => signal<'a>
  @ocaml.doc(" [delay i (lazy s)] is the value [s] had an infinitesimal amount
      of time before:
      {ul
      {- \[[delay i (lazy s)]\]{_ t} [=] [i] for t = 0. }
      {- \[[delay i (lazy s)]\]{_ t} [=] \[[s']\]{_t-dt} otherwise.}} ")
  let delay: ('a, Lazy.t<signal<'a>>) => signal<'a>
  @ocaml.doc(" In [fix sf], [sf] is called with a signal [s] that represents

      the signal returned by [sf] delayed by an infinitesimal amount
      time. If [s', r = sf s] then [r] is returned by [fix] and [s]
      is such that :
      {ul
      {- \[[s]\]{_ t} [=] [i] for t = 0. }
      {- \[[s]\]{_ t} [=] \[[s']\]{_t-dt} otherwise.}} ")
  let fix: (~eq: ('a, 'a) => bool=?, 'a, signal<'a> => (signal<'a>, 'b)) => 'b
  @ocaml.doc(" {1:lifting Lifting}
     Lifting combinators. For a given [n] the semantics is :
     \[[ln f a1] ... [an]\]{_t} = f \[[a1]\]{_t} ... \[[an]\]{_t} ")
  let l1: (~eq: ('b, 'b) => bool=?, 'a => 'b, signal<'a>) => signal<'b>
  let l2: (~eq: ('c, 'c) => bool=?, ('a, 'b) => 'c, signal<'a>, signal<'b>) => signal<'c>
  let l3: (
    ~eq: ('d, 'd) => bool=?,
    ('a, 'b, 'c) => 'd,
    signal<'a>,
    signal<'b>,
    signal<'c>,
  ) => signal<'d>
  @ocaml.doc(" {1:stdlib Stdlib types support} ") @ocaml.doc(" Boolean signals ")
  module Bool: {
    @ocaml.doc(" [false'] is [const false]. ")
    let false': signal<bool>
    @ocaml.doc(" [true'] is [const true]. ")
    let true': signal<bool>
    @ocaml.doc(" [not s] is [map not s]. ")
    let not: signal<bool> => signal<bool>
    @ocaml.doc(" [s0 && s1] is [l2 ( && ) s1 s2]. ")
    let \"&&": (signal<bool>, signal<bool>) => signal<bool>
    @ocaml.doc(" [s0 || s1] is [l2 ( || ) s1 s2]. ")
    let \"||": (signal<bool>, signal<bool>) => signal<bool>
    @ocaml.doc(" [edge s] is [changes s]. ")
    let edge: signal<bool> => event<bool>
    @ocaml.doc(" [rise s] is
        [E.filter_map (function true -> Some b | false -> None) (edge s)]. ")
    let rise: signal<bool> => event<unit>
    @ocaml.doc(" [fall s] is
       [E.filter_map (function true -> None | None -> Some b) (edge s)] ")
    let fall: signal<bool> => event<unit>
    @ocaml.doc(" [flip ~init e] is a signal whose boolean value flips each time
        [e] occurs. [init] provides the signal value at creation time.
          {ul
          {- \[[flip b e]\]{_0} [= not b] if \[[e]\]{_0} [= Some _]}
          {- \[[flip b e]\]{_t} [=] init if \[[e]\]{_<=t} [= None]}
          {- \[[flip b e]\]{_t} [=] [not] \[[flip b e]\]{_t-dt}
             if \[[e]\]{_t} [= Some _]}} ")
    let flip: (~init: bool, event<'a>) => signal<bool>
  }
  @ocaml.doc(" Option signals ")
  module Option: {
    @ocaml.doc(" [eq f] derives an equality function on options using [f] for
        testing [Some _]. ")
    let eq: (('a, 'a) => bool, option<'a>, option<'a>) => bool
    @ocaml.doc(" [hold_value i s] is the last [Some _] value of [s] or
        [i] if there was no such value:
        {ul
        {- \[[hold_some i s]\]{_t} [= i] if \[[s]\]{_<t} [= None]}
        {- \[[hold_some i s]\]{_t} [= v] if \[[s]\]{_<=t} [= Some v]}}
        Uses [s]'s equality on [Some _]. ")
    let hold_value: ('a, signal<option<'a>>) => signal<'a>
    @ocaml.doc(" {1:lift Lifted {!Stdlib.Option} module} ") @ocaml.doc(" [none] is [const None]. ")
    let none: signal<option<'a>>
    @ocaml.doc(" [some s] is [map (fun v -> Some v) s] and uses [s]'s equality
        function to derive the obvious one on options. ")
    let some: signal<'a> => signal<option<'a>>
    @ocaml.doc(" [value s ~default] is [default] when [s] is [None]:
        {ul
        {- \[[value s ~default]\]{_t} [= v] if \[[s]\]{_t} [= Some v]}
        {- \[[value s ~default]\]{_t} [=]
           \[[default]\]{_t} if \[[s]\]{_t} [= None]}}
        [default]'s equality function is used for the resulting signal. ")
    let value: (signal<option<'a>>, ~default: signal<'a>) => signal<'a>
    @ocaml.doc(" [get s] is [map ~eq Option.get s]. ")
    let get: (~eq: ('a, 'a) => bool=?, signal<option<'a>>) => signal<'a>
    @ocaml.doc(" [bind ~eq s f] is [map ~eq (fun v -> Option.bind v f) s]. ")
    let bind: (
      ~eq: (option<'b>, option<'b>) => bool=?,
      signal<option<'a>>,
      'a => option<'b>,
    ) => signal<option<'b>>
    @ocaml.doc(" [join ~eq oos] is [map ~eq Option.join oos]. ")
    let join: (
      ~eq: (option<'a>, option<'a>) => bool=?,
      signal<option<option<'a>>>,
    ) => signal<option<'a>>
    @ocaml.doc(" [map ~eq f s] is [map ~eq Option.map s]. ")
    let map: (
      ~eq: (option<'b>, option<'b>) => bool=?,
      'a => 'b,
      signal<option<'a>>,
    ) => signal<option<'b>>
    @ocaml.doc(" [is_none s] is [map Option.is_none s]. ")
    let is_none: signal<option<'a>> => signal<bool>
    @ocaml.doc(" [is_some s] is [map Option.is_some s]. ")
    let is_some: signal<option<'a>> => signal<bool>
  }
  @ocaml.doc(" Pair signals. ")
  module Pair: {
    @ocaml.doc(" [fst ?eq s] is [map ?eq fst s]. ")
    let fst: (~eq: ('a, 'a) => bool=?, signal<('a, 'b)>) => signal<'a>
    @ocaml.doc(" [snd ?eq e] is [map ?eq snd e]. ")
    let snd: (~eq: ('b, 'b) => bool=?, signal<('a, 'b)>) => signal<'b>
    @ocaml.doc(" [v s0 s1] is [l2 (fun x y -> (x, y) s0 s1]. ")
    let v: (signal<'a>, signal<'b>) => signal<('a, 'b)>
  }

  let dump_src_ids: (unit, signal<'a>) => unit
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
