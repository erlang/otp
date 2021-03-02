\*
\* %CopyrightBegin%
\*
\* Copyright Ericsson AB 2021. All Rights Reserved.
\*
\* Licensed under the Apache License, Version 2.0 (the "License");
\* you may not use this file except in compliance with the License.
\* You may obtain a copy of the License at
\*
\*     http://www.apache.org/licenses/LICENSE-2.0
\*
\* Unless required by applicable law or agreed to in writing, software
\* distributed under the License is distributed on an "AS IS" BASIS,
\* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
\* See the License for the specific language governing permissions and
\* limitations under the License.
\*
\* %CopyrightEnd%
\*

(*
    --- Model of the new link protocol introduced in Erlang/OTP 23.3 ---

    The protocol is documented in the ERTS User's Guide ->
    Distribution Protocol -> Protocol Between Connected Nodes ->
    Link Protocol -> New Link Protocol

    This model only models a link between two processes. This since a link
    between one pair of processes is completely independent of links between
    other pairs of processes. This model also assumes that the connection
    between the processes does not fail. In the real world a connection can
    of course fail. This is however taken care of by clearing the link
    information on both ends when a connection fails, and tracking which
    instantiation of a connection between the nodes signals arrive on and
    ignoring signals from old instantiations of connections. That is,
    connection loss is trivially taken care of since we just start over
    again from scratch if the connection is lost.

    The documentation of the protocol talks about "process local information
    about links". This information is stored in the process state record below.
    The 'other' field contains the identifier of the other process. The 'type'
    field acts as "active flag". The link is active when 'type' equals
    "linked" and not active when 'type' equals "unlinked". If the
    'wait_unlink_ack' field contains a value larger than or equal to zero
    it is the "unlink id" of an unlink request we have issued and are waiting
    for to get acknowledged. If the 'wait_unlink_ack' field contains -1 we
    are not waiting for an acknowledgment. When 'type' equals "unlinked" and
    'wait_unlink_ack' equals -1, we would in the documented protocol have
    removed the "process local information about the link". In this model we,
    however, keep the state, but in this state instead of removing it.
    Messages are tagged with a message number in order to model the signal
    order of Erlang. The message number of the unlink signal is also used as
    "unlink id".

    The model has been checked with the following parameters:

      Declared constants:
        PROCS <- {"a", "b"}

      Temporal formula:
        FairSpec

      Deadlock:
        disabled

      Invariants:
        TypeOK
        ValidState

      State Constraint:
        /\ \E proc \in PROCS : procState[proc].next_send =< 15
        /\ Cardinality(msgs) =< 10

    That is, we have checked all states where processes send up to 15 signals
    with at most 10 outstanding signals.

    Deadlock checking has been disabled since we intentionally stop when
    we have no outstanding signals (in 'Next') in order to avoid checking
    signal sequences equivalent to sequences we already have checked.

*)
------------------------------ MODULE NewLinking ------------------------------
EXTENDS Integers, TLC, FiniteSets
CONSTANTS PROCS \* PROCS should be a set of exactly two process names

VARIABLES
    procState, \* Set of process states; procState[proc] is state of proc
    msgs \* Set of messages sent

vars == <<procState, msgs>>

\* Set of possible process states...
procStateRec ==
    [self : PROCS,
     other: PROCS,
     type : {"linked", "unlinked"},
     wait_unlink_ack : Nat \cup {-1},
     next_send : Nat,
     next_recv : Nat]

\* Set of possible messages...
Messages ==
  [type : {"link", "unlink", "unlink_ack"}, from : PROCS, to : PROCS, msg_no : Nat, ack : Nat \cup {-1}]

TypeOK ==
   /\ procState \in [PROCS -> procStateRec]
   /\ msgs \subseteq Messages

Init ==
    /\ msgs = {}
    /\ procState = [p \in PROCS |-> [self |-> p,
                                     other |-> CHOOSE p2 \in PROCS : p2 /= p,
                                     type |-> "unlinked",
                                     next_send |-> 0,
                                     next_recv |-> 0,
                                     wait_unlink_ack |-> -1]]

MkMsg(self, mtype, accnr) ==
  [type |-> mtype,
   from |-> procState[self].self,
   to |-> procState[self].other,
   msg_no |-> procState[self].next_send,
   ack |-> accnr]

Link(self) ==
  /\ procState[self].type = "unlinked"
  /\ msgs' = msgs \cup {MkMsg(self, "link", -1)}
  /\ procState' = [procState EXCEPT ![self].type = "linked",
                                    ![self].next_send = @ + 1,
                                    ![self].wait_unlink_ack = -1]

Unlink(self) ==
  /\ procState[self].type = "linked"
  /\ msgs' = msgs \cup {MkMsg(self, "unlink", -1)}
  /\ procState' = [procState EXCEPT ![self].type = "unlinked",
                                    ![self].next_send = @ + 1,
                                    ![self].wait_unlink_ack = procState[self].next_send]

RecvLink(self, msg) ==
   LET type == IF procState[self].wait_unlink_ack /= -1
               THEN "unlinked"
               ELSE "linked"
   IN /\ msgs' = msgs \ {msg}
      /\ procState' = [procState EXCEPT ![self].type = type,
                                        ![self].next_recv = @ + 1]

RecvUnlink(self, msg) ==
  /\ msgs' = (msgs \ {msg}) \cup {MkMsg(self,
                                        "unlink_ack",
                                        procState[self].next_recv)}
  /\ procState' = [procState EXCEPT ![self].type = "unlinked",
                                    ![self].next_recv = @ + 1,
                                    ![self].next_send = @ + 1]

RecvUnlinkAck(self, msg) ==
   LET wack == IF procState[self].wait_unlink_ack = msg.ack
               THEN -1
               ELSE procState[self].wait_unlink_ack
   IN /\ msgs' = msgs \ {msg}
      /\ procState' = [procState EXCEPT ![self].next_recv = @ + 1,
                                        ![self].wait_unlink_ack = wack]

Recv(self) ==
  /\ \E m \in msgs : /\ m.to = self
                     /\ m.msg_no = procState[self].next_recv
  /\ LET msg == CHOOSE m \in msgs : /\ m.to = self
                                    /\ m.msg_no = procState[self].next_recv
     IN CASE msg.type = "link" -> RecvLink(self, msg)
          [] msg.type = "unlink" -> RecvUnlink(self, msg)
          [] msg.type = "unlink_ack" -> RecvUnlinkAck(self, msg)

(*
   If we have no outstanding messages; both processes should
   have the same view about whether they are linked or not...
*)
ValidState ==
  IF msgs /= {}
  THEN TRUE
  ELSE \A p \in PROCS : \A p2 \in PROCS : procState[p].type = procState[p2].type

Next ==
    /\ (msgs /= {} \/ \A p \in PROCS : procState[p].next_send = 0)
    /\ \E p \in PROCS : \/ Recv(p)
                        \/ Link(p)
                        \/ Unlink(p)
                     
Spec == Init /\ [][Next]_vars

FairSpec == Spec /\ WF_vars(Next)

=============================================================================
\* Modification History
\* Last modified Mon Jan 25 11:26:06 CET 2021 by rickard.green
\* Created Wed Jan 20 13:11:46 CET 2021 by rickard.green
