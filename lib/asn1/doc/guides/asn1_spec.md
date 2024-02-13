<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# Specialized Decodes

[](){: #SpecializedDecodes }

When performance is of highest priority and one is interested in a limited part
of the ASN.1 encoded message before deciding what to do with the rest of it, an
option is to decode only a part of the message. This situation can be a server
that has to decide the addressee of a message. The addressee can be interested
in the entire message, but the server can be a bottleneck that you want to spare
any unnecessary load.

Instead of making two _complete decodes_ (the normal case of decode), one in the
server and one in the addressee, it is only necessary to make one _specialized
decode_ (in the server) and another complete decode (in the addressee). This
section describes the following specialized decode functionality:

- _Exclusive decode_
- _Selected decode_

This functionality is only provided when using `BER` (option `ber`).

## Exclusive Decode

The basic idea with exclusive decode is to specify which parts of the message
you want to exclude from being decoded. These parts remain encoded and are
returned in the value structure as binaries. The undecoded parts can be decoded
later by calling the `decode_part/2` function.

### Procedure

To perform an exclusive decode:

- _Step 1:_ Decide the name of the function for the exclusive decode.
- _Step 2:_ Include the following instructions in a configuration file:

  - The name of the exclusive decode function
  - The name of the ASN.1 specification
  - A notation that tells which parts of the message structure to be excluded
    from decode

- _Step 3_ Compile with the additional option `asn1config`. The compiler
  searches for a configuration file with the same name as the ASN.1
  specification but with extension `.asn1config`. This configuration file is not
  the same as used for compilation of a set of files. See Section
  [Writing an Exclusive Decode Instruction.](asn1_spec.md#UndecodedPart)

### User Interface

The runtime user interface for exclusive decode consists of the following two
functions:

- A function for an exclusive decode, whose name the user decides in the
  configuration file
- The compiler generates a `decode_part/2` function when exclusive decode is
  chosen. This function decodes the parts that were left undecoded during the
  exclusive decode.

Both functions are described in the following.

If the exclusive decode function has, for example, the name `decode_exclusive`
and an ASN.1 encoded message `Bin` is to be exclusive decoded, the call is as
follows:

```erlang
{ok,ExclMessage} = 'MyModule':decode_exclusive(Bin)
```

[](){: #UndecodedPart } The result `ExclMessage` has the same structure as a
complete decode would have, except for the parts of the top type that were not
decoded. The undecoded parts are on their places in the structure on format
`{TypeKey,UndecodedValue}`.

Each undecoded part that is to be decoded must be fed into function
`decode_part/2` as follows:

```erlang
{ok,PartMessage} = 'MyModule':decode_part(TypeKey, UndecodedValue)
```

[](){: #Exclusive-Instruction }

### Writing an Exclusive Decode Instruction

This instruction is written in the configuration file in the following format:

```erlang
ExclusiveDecodeInstruction = {exclusive_decode,{ModuleName,DecodeInstructions}}.

ModuleName = atom()

DecodeInstructions = [DecodeInstruction]+

DecodeInstruction = {ExclusiveDecodeFunctionName,TypeList}

ExclusiveDecodeFunctionName = atom()

TypeList = [TopType,ElementList]

ElementList = [Element]+

Element = {Name,parts} |
          {Name,undecoded} |
          {Name,ElementList}

TopType = atom()

Name = atom()
```

The instruction must be a valid Erlang term ended by a dot.

In `TypeList` the path from the top type to each undecoded subcomponent is
described. `TopType` is the name of a top-level type in the ASN.1 specification.
The action for each component in `ElementList` is described by one of:

- `{Name,parts}`
- `{Name,undecoded}`
- `{Name,ElementList}`

The use and effect of the actions are as follows:

- **`{Name,undecoded}`** - Leaves the element undecoded. The type of `Name` can
  be any ASN.1 type. The value of element `Name` is returned as a tuple (as
  mentioned in the previous section) in the value structure of the top type.

- **`{Name,parts}`** - The type of `Name` must be either `SEQUENCE OF` or
  `SET OF`. The action implies that the different components of `Name` are left
  undecoded. The value of `Name` is returned as a tuple (as mentioned in the
  previous section) where the second element is a list of binaries. This is
  because the representation of a `SEQUENCE OF` or a `SET OF` in Erlang is a
  list of its internal type. Any of the elements in this list or the entire list
  can be decoded by function `decodepart`.

- **`{Name,ElementList}`** - This action is used when one or more of the
  subtypes of `Name` is exclusively decoded.

`Name` in these actions can be a component name of a `SEQUENCE OF` or a
`SET OF`, or a name of an alternative in a `CHOICE`.

### Example

In this examples, the definitions from the following ASN.1 specification are
used:

```text

GUI DEFINITIONS AUTOMATIC TAGS ::= BEGIN

  Action ::= SEQUENCE {
    number  INTEGER DEFAULT 15,
    handle  Handle DEFAULT {number 12, on TRUE}
  }

  Key ::= Button
  Handle ::= Key

  Button ::= SEQUENCE {
    number INTEGER,
    on     BOOLEAN
  }

  Window ::= CHOICE {
    vsn INTEGER,
    status Status
  }

  Status ::= SEQUENCE {
    state INTEGER,
    buttonList SEQUENCE OF Button,
    enabled BOOLEAN OPTIONAL,
    actions CHOICE {
      possibleActions SEQUENCE OF Action,
      noOfActions INTEGER
    }
  }

END
```

{: #Asn1spec }

If `Button` is a top type and it is needed to exclude component `number` from
decode, `TypeList` in the instruction in the configuration file is
`['Button',[{number,undecoded}]]`. If you call the decode function
`decode_Button_exclusive`, `DecodeInstruction` is
`{decode_Button_exclusive,['Button',[{number,undecoded}]]}`.

Another top type is `Window` whose subcomponent actions in type `Status` and the
parts of component `buttonList` are to be left undecoded. For this type, the
function is named `decode__Window_exclusive`. The complete
`Exclusive_Decode_Instruction` configuration is as follows:

```text

{exclusive_decode,
 {'GUI',
  [{decode_Window_exclusive,
    ['Window',[{status,[{buttonList,parts},{actions,undecoded}]}]]},
   {decode_Button_exclusive,
    ['Button',[{number,undecoded}]]}]}}.
```

The following figure shows the bytes of a `Window:status` message. The
components `buttonList` and `actions` are excluded from decode. Only `state` and
`enabled` are decoded when `decode__Window_exclusive` is called.

![Bytes of a Window:status Message](assets/exclusive_Win_But.gif "Bytes of a Window:status Message")

Here follows an example of how the module. Note that option `no_ok_wrapper` is
used to make the make example more concise.

```erlang
1> asn1ct:compile('GUI', [ber,asn1config,no_ok_wrapper]).
ok
2> rr('GUI').
['Action','Button','Status']
3> ButtonMsg = #'Button'{number=123,on=true}.
#'Button'{number = 123,on = true}
4> ButtonBytes = 'GUI':encode('Button', ButtonMsg).
<<48,6,128,1,123,129,1,255>>
5> ExclusiveMsgButton = 'GUI':decode_Button_exclusive(ButtonBytes).
#'Button'{number = {'Button_number',<<128,1,123>>},
          on = true}
6> {UndecKey,UndecBytes} = ExclusiveMsgButton#'Button'.number.
{'Button_number',<<128,1,123>>}
7> 'GUI':decode_part(UndecKey, UndecBytes).
123
8> WindowMsg =
{status,{'Status',35,
   [{'Button',3,true},
    {'Button',4,false},
    {'Button',5,true},
    {'Button',6,true},
    {'Button',7,false}],
   false,
   {possibleActions,[{'Action',16,{'Button',17,true}}]}}}.
{status,#'Status'{state = 35,
        buttonList = [#'Button'{number = 3,on = true},
                      #'Button'{number = 4,on = false},
                      #'Button'{number = 5,on = true},
                      #'Button'{number = 6,on = true},
                      #'Button'{number = 7,on = false}],
        enabled = false,
        actions = {possibleActions,[#'Action'{number = 16,
                                              handle = #'Button'{number = 17,on = true}}]}}}
9> WindowBytes = 'GUI':encode('Window', WindowMsg).
<<161,65,128,1,35,161,40,48,6,128,1,3,129,1,255,48,6,128,
  1,4,129,1,0,48,6,128,1,5,129,...>>
10> {status,#'Status'{buttonList={UndecWindowKey,UndecWindowParts}}} =
'GUI':decode_Window_exclusive(WindowBytes).
{status,#'Status'{state = 35,
                  buttonList = {'Status_buttonList',[<<48,6,128,1,3,129,1,
                                                       255>>,
                                                     <<48,6,128,1,4,129,1,0>>,
                                                     <<48,6,128,1,5,129,1,255>>,
                                                     <<48,6,128,1,6,129,1,255>>,
                                                     <<48,6,128,1,7,129,1,0>>]},
                  enabled = false,
                  actions = {'Status_actions',<<163,15,160,13,48,11,128,
                                                1,16,161,6,128,1,17,129,
                                                1,255>>}}}
11> 'GUI':decode_part(UndecWindowKey, UndecWindowParts).
[#'Button'{number = 3,on = true},
 #'Button'{number = 4,on = false},
 #'Button'{number = 5,on = true},
 #'Button'{number = 6,on = true},
 #'Button'{number = 7,on = false}]
12> 'GUI':decode_part(UndecWindowKey, hd(UndecWindowParts)).
#'Button'{number = 3,on = true}
13> {status,#'Status'{actions={ChoiceKey,ChoiceUndec}}} = v(10).
{status,#'Status'{state = 35,
                  buttonList = {'Status_buttonList',[<<48,6,128,1,3,129,1,
                                                       255>>,
                                                     <<48,6,128,1,4,129,1,0>>,
                                                     <<48,6,128,1,5,129,1,255>>,
                                                     <<48,6,128,1,6,129,1,255>>,
                                                     <<48,6,128,1,7,129,1,0>>]},
                  enabled = false,
                  actions = {'Status_actions',<<163,15,160,13,48,11,128,
                                                1,16,161,6,128,1,17,129,
                                                1,255>>}}}
14> 'GUI':decode_part(ChoiceKey, ChoiceUndec).
{possibleActions,[#'Action'{number = 16,
                            handle = #'Button'{number = 17,on = true}}]}
```

## Selective Decode

Selective decode decodes a single subtype of a constructed value. This is the
fastest method to extract a subvalue. Selective decode is typically used when
one want to inspect, for example, a version number to be able to decide what to
do with the entire value.

### Procedure

To perform a selective decode:

- _Step 1:_ Include the following instructions in the configuration file:

  - The name of the user function
  - The name of the ASN.1 specification
  - A notation that tells which part of the type to be decoded

- _Step 2:_ Compile with the additional option `asn1config`. The compiler
  searches for a configuration file with the same name as the ASN.1
  specification, but with extension `.asn1config`. In the same file you can also
  provide configuration specifications for exclusive decode. The generated
  Erlang module has the usual functionality for encode/decode preserved and the
  specialized decode functionality added.

### User Interface

The only new user interface function is the one provided by the user in the
configuration file.

For example, if the configuration file includes the specification
`{selective_decode,{'ModuleName',[{selected_decode_Window,TypeList}]}}` do the
selective decode by
`{ok,Result} = 'ModuleName':selected_decode_Window(EncodedBinary).`

[](){: #Selective-Instruction }

### Writing a Selective Decode Instruction

One or more selective decode functions can be described in a configuration file.
Use the following notation:

```erlang
SelectiveDecodeInstruction = {selective_decode,{ModuleName,DecodeInstructions}}.

ModuleName = atom()

DecodeInstructions = [DecodeInstruction]+

DecodeInstruction = {SelectiveDecodeFunctionName,TypeList}

SelectiveDecodeFunctionName = atom()

TypeList = [TopType|ElementList]

ElementList = Name|ListSelector

Name = atom()

ListSelector = [integer()]
```

The instruction must be a valid Erlang term ended by a dot.

- `ModuleName` is the same as the name of the ASN.1 specification, but without
  the extension.
- `DecodeInstruction` is a tuple with your chosen function name and the
  components from the top type that leads to the single type you want to decode.
  Ensure to choose a name of your function that is not the same as any of the
  generated functions.
- The first element of `TypeList` is the top type of the encoded message. In
  `ElementList`, it is followed by each of the component names that leads to
  selected type.
- Each name in `ElementList` must be a constructed type except the last name,
  which can be any type.
- `ListSelector` makes it possible to choose one of the encoded components in a
  `SEQUENCE OF` or a `SET OF`. It is also possible to go further in that
  component and pick a subtype of that to decode. So, in the `TypeList`:
  `['Window',status,buttonList,[1],number]`, component `buttonList` must be of
  type `SEQUENCE OF` or `SET OF`.

In the example, component `number` of the first of the encoded elements in the
`SEQUENCE OF` `buttonList` is selected. This applies on the ASN.1 specification
in Section [Writing an Exclusive Decode Instruction](asn1_spec.md#Asn1spec).

### Example

In this example, the same ASN.1 specification as in Section
[Writing an Exclusive Decode Instruction](asn1_spec.md#Asn1spec) is used. The
following is a valid selective decode instruction:

```erlang
{selective_decode,
    {'GUI',
        [{selected_decode_Window1,
            ['Window',status,buttonList,
             [1],
             number]},
 {selected_decode_Action,
     ['Action',handle,number]},
 {selected_decode_Window2,
     ['Window',
      status,
      actions,
      possibleActions,
      [1],
      handle,number]}]}}.
```

The first instruction,
`{selected_decode_Window1,['Window',status,buttonList,[1],number]}` is described
in the previous section.

The second instruction, `{selected_decode_Action,['Action',handle,number]}`,
takes component `number` in the `handle` component of type `Action`. If the
value is `ValAction = {'Action',17,{'Button',4711,false}}`, the internal value
4711 is to be picked by `selected_decode_Action`. In an Erlang terminal it looks
as follows:

```erlang
1> asn1ct:compile('GUI', [ber,asn1config,no_ok_wrapper]).
ok
2> ValAction = {'Action',17,{'Button',4711,false}}.
{'Action',17,{'Button',4711,false}}
3> Bytes = 'GUI':encode('Action',ValAction).
<<48,18,2,1,17,160,13,172,11,171,9,48,7,128,2,18,103,129,1,0>>
4> 'GUI':selected_decode_Action(Bytes).
4711
```

The third instruction,
`['Window',status,actions,possibleActions,[1],handle,number]`, works as follows:

- _Step 1:_ Starts with type `Window`.
- _Step 2:_ Takes component `status` of `Window` that is of type `Status`.
- _Step 3:_ Takes _actions_ of type `Status`.
- _Step 4:_ Takes `possibleActions` of the internally defined `CHOICE` type.
- _Step 5:_ Goes into the first component of `SEQUENCE OF` by `[1]`. That
  component is of type `Action`.
- _Step 6:_ Takes component `handle`.
- _Step 7:_ Takes component `number` of type `Button`.

The following figure shows which components are in `TypeList`
`['Window',status,actions,possibleActions,[1],handle,number]`:

![Elements Specified in Configuration File for Selective Decode of a Subvalue in a Window Message](assets/selective_TypeList.gif "Elements Specified in Configuration File for Selective Decode of a Subvalue in a Window Message")

In the following figure, only the marked element is decoded by
`selected_decode_Window2`:

![Bytes of a Window:status Message](assets/selective_Window2.gif "Bytes of a Window:status Message")

With the following example, you can examine that both `selected_decode_Window2`
and `selected_decode_Window1` decodes the intended subvalue of value `Val`:

```erlang
1> Val = {status,{'Status',12,
                    [{'Button',13,true},
                     {'Button',14,false},
                     {'Button',15,true},
                     {'Button',16,false}],
                    true,
                    {possibleActions,[{'Action',17,{'Button',18,false}},
                                      {'Action',19,{'Button',20,true}},
                                      {'Action',21,{'Button',22,false}}]}}}.
2> Bin = 'GUI':encode('Window',Val).
<<161,89,128,1,12,161,32,48,6,128,1,13,129,1,255,48,6,128,
  1,14,129,1,0,48,6,128,1,15,129,...>>
4> 'GUI':selected_decode_Window1(Bin).
13
5> 'GUI':selected_decode_Window2(Bin).
18
```
