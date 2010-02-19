PDUs DEFINITIONS ::=

-- Search for 'org' to find changes for erlang.

-- SnmpMgmtCom and PDUs only for dbg.


BEGIN
EXPORTS SnmpPrivMsg, SnmpAuthMsg, SnmpMgmtCom, PDUs;

-- From RFC 1442

          -- names of objects

          ObjectName ::=
              OBJECT IDENTIFIER


          -- syntax of objects

          ObjectSyntax ::=
              CHOICE {
                  simple
                      SimpleSyntax,

                    -- note that SEQUENCEs for conceptual tables and
                    -- rows are not mentioned here...

                  applicationWide
                      ApplicationSyntax
              }


          -- built-in ASN.1 types

          SimpleSyntax ::=
              CHOICE {
                  -- INTEGERs with a more restrictive range
                  -- may also be used
                  integerValue
                      INTEGER, 

                  stringValue
                      OCTET STRING,

                  objectIDValue
                      OBJECT IDENTIFIER,

                  -- only the enumerated form is allowed
                  bitValue
                      BIT STRING
              }


          -- indistinguishable from INTEGER, but never needs more than
          -- 32Bits for a two's complement representation
          Integer32 ::=
              [UNIVERSAL 2]
                  IMPLICIT INTEGER (-2147483648..2147483647)


          -- applicationWide types

          ApplicationSyntax ::=
              CHOICE {
                  ipAddressValue
                      IpAddress,

                  counterValue
                      Counter32,

                  gaugeValue
                      Gauge32,

                  timeticksValue
                      TimeTicks,

                  arbitraryValue
                      Opaque,

                  nsapAddressValue
                      NsapAddress,

                  bigCounterValue
                      Counter64,

                  unsignedIntegerValue
                      UInteger32
              }

          -- in networkByte order
          -- (this is a tagged type for historical reasons)
          IpAddress ::=
              [APPLICATION 0]
                  IMPLICIT OCTET STRING (SIZE (4))




          -- this wraps
          Counter32 ::=
              [APPLICATION 1]
                  IMPLICIT INTEGER (0..4294967295)

          -- this doesn't wrap
          Gauge32 ::=
              [APPLICATION 2]
                  IMPLICIT INTEGER (0..4294967295) 

          -- hundredths of seconds since an epoch
          TimeTicks ::=
              [APPLICATION 3]
                  IMPLICIT INTEGER (0..4294967295)

          -- for backwardCompatibility only
          Opaque ::=
              [APPLICATION 4]
                  IMPLICIT OCTET STRING

          -- for OSI NSAP addresses
          -- (this is a tagged type for historical reasons)
          NsapAddress ::=
              [APPLICATION 5]
-- org:                  IMPLICIT OCTET STRING (SIZE (1 | 4..21))
                  IMPLICIT OCTET STRING

          -- for counters that wrap in less than one hour with only 32 bits
          Counter64 ::=
              [APPLICATION 6]
                  IMPLICIT INTEGER (0..18446744073709551615)

          -- an unsigned 32Bit quantity
          UInteger32 ::=
              [APPLICATION 7]
                  IMPLICIT INTEGER (0..4294967295)


-- From RFC 1445

          SnmpPrivMsg ::= [1] IMPLICIT SEQUENCE {
             privDst
                OBJECT IDENTIFIER,
             privData
                [1] IMPLICIT OCTET STRING
           }

           SnmpAuthMsg ::= [1] IMPLICIT SEQUENCE {
             authInfo
                ANY, -- defined by authentication protocol
             authData
                SnmpMgmtCom
           }

           SnmpMgmtCom ::= [2] IMPLICIT SEQUENCE {
             dstParty
                OBJECT IDENTIFIER,
             srcParty
                OBJECT IDENTIFIER,
             context
                OBJECT IDENTIFIER,
             pdu
                PDUs
           }


-- From RFC 1448

                -- org: no tag at all. we need a tag to test 'PDUs'. 
               PDUs ::= [PRIVATE 1]
                   -- remove tag when 'PDUs' only is used in another type.
                   CHOICE {
                       getRequest
                           GetRequestPdu,

                       getNextRequest
                           GetNextRequestPdu,

                       getBulkRequest
                           GetBulkRequestPdu,

                       response
                           ResponsePdu,

                       setRequest
                           SetRequestPdu,

                       informRequest
                           InformRequestPdu,

                       snmpV2Trap
                           SNMPv2TrapPdu
                   }

               -- PDUs

               GetRequestPdu ::=
                   [0]
                       IMPLICIT PDU

               GetNextRequestPdu ::=
                   [1]
                       IMPLICIT PDU

               ResponsePdu ::=
                   [2]
                       IMPLICIT PDU

               SetRequestPdu ::=
                   [3]
                       IMPLICIT PDU

               -- [4] is obsolete

               GetBulkRequestPdu ::=
                   [5]
                       IMPLICIT BulkPDU

               InformRequestPdu ::=
                   [6]
                       IMPLICIT PDU

               SNMPv2TrapPdu ::=
                   [7]
                       IMPLICIT PDU


               maxBindings
                   INTEGER ::= 2147483647

               PDU ::=
                   SEQUENCE {
                       requestId
                           Integer32,

                       errorStatus            -- sometimes ignored
                           INTEGER {
                               noError(0),
                               tooBig(1),
                               noSuchName(2),   -- for proxy compatibility
                               badValue(3),     -- for proxy compatibility
                               readOnly(4),     -- for proxy compatibility
                               genErr(5),
                               noAccess(6),
                               wrongType(7),
                               wrongLength(8),
                               wrongEncoding(9),
                               wrongValue(10),
                               noCreation(11),
                               inconsistentValue(12),
                               resourceUnavailable(13),
                               commitFailed(14),
                               undoFailed(15),
                               authorizationError(16),
                               notWritable(17),
                               inconsistentName(18)
                           },

                       errorIndex            -- sometimes ignored
                           INTEGER (0..maxBindings),

                       variableBindings   -- values are sometimes ignored
                           VarBindList
                   }


               BulkPDU ::=                     -- MUST be identical in
                   SEQUENCE {                  -- structure to PDU
                       requestId
                           Integer32,

                       nonRepeaters
                           INTEGER (0..maxBindings),

                       maxRepetitions
                           INTEGER (0..maxBindings),

                       variableBindings       -- values are ignored
                           VarBindList
                   }


               VarBind ::=
                   SEQUENCE {
                       name
                           ObjectName,

                       data CHOICE {
                           value
                               ObjectSyntax,

                           unSpecified         -- in retrieval requests
                                   NULL,

                                               -- exceptions in responses
                           noSuchObject[0]
                                   IMPLICIT NULL,

                           noSuchInstance[1]
                                   IMPLICIT NULL,

                           endOfMibView[2]
                                   IMPLICIT NULL
                       }
                   }


               -- variableBinding list

               VarBindList ::=
                   SEQUENCE OF VarBind

-- org:
--               VarBindList ::=
--                   SEQUENCE (SIZE (0..maxBindings)) OF
--                       VarBind

END








