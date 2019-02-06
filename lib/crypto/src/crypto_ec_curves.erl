-module(crypto_ec_curves).

-export([curve/1, curves/0]).

curves() ->
    proplists:get_value(curves,  crypto:supports()) -- [x25519,x448].

curve(secp112r1) ->
    {
     {prime_field, <<16#DB7C2ABF62E35E668076BEAD208B:112>>},                                        %% Prime
     {<<16#DB7C2ABF62E35E668076BEAD2088:112>>,                                                      %% A
      <<16#659EF8BA043916EEDE8911702B22:112>>,                                                      %% B
      <<16#00F50B028E4D696E676875615175290472783FB1:160>>},                                         %% Seed
      <<16#04:8,
        16#09487239995A5EE76B55F9C2F098:112,                                                        %% X(p0)
        16#A89CE5AF8724C0A23E0E0FF77500:112>>,                                                      %% Y(p0)
      <<16#DB7C2ABF62E35E7628DFAC6561C5:112>>,                                                      %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(secp112r2) ->
    {
     {prime_field, <<16#DB7C2ABF62E35E668076BEAD208B:112>>},                                        %% Prime
     {<<16#6127C24C05F38A0AAAF65C0EF02C:112>>,                                                      %% A
      <<16#51DEF1815DB5ED74FCC34C85D709:112>>,                                                      %% B
      <<16#002757A1114D696E6768756151755316C05E0BD4:160>>},                                         %% Seed
      <<16#04:8,
        16#4BA30AB5E892B4E1649DD0928643:112,                                                        %% X(p0)
        16#ADCD46F5882E3747DEF36E956E97:112>>,                                                      %% Y(p0)
      <<16#36DF0AAFD8B8D7597CA10520D04B:112>>,                                                      %% Order
      <<16#04:8>>                                                                                   %% CoFactor
    };

curve(secp128r1) ->
    {
     {prime_field, <<16#FFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFF:128>>},                                    %% Prime
     {<<16#FFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFC:128>>,                                                  %% A
      <<16#E87579C11079F43DD824993C2CEE5ED3:128>>,                                                  %% B
      <<16#000E0D4D696E6768756151750CC03A4473D03679:160>>},                                         %% Seed
      <<16#04:8,
        16#161FF7528B899B2D0C28607CA52C5B86:128,                                                    %% X(p0)
        16#CF5AC8395BAFEB13C02DA292DDED7A83:128>>,                                                  %% Y(p0)
      <<16#FFFFFFFE0000000075A30D1B9038A115:128>>,                                                  %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(secp128r2) ->
    {
     {prime_field, <<16#FFFFFFFDFFFFFFFFFFFFFFFFFFFFFFFF:128>>},                                    %% Prime
     {<<16#D6031998D1B3BBFEBF59CC9BBFF9AEE1:128>>,                                                  %% A
      <<16#5EEEFCA380D02919DC2C6558BB6D8A5D:128>>,                                                  %% B
      <<16#004D696E67687561517512D8F03431FCE63B88F4:160>>},                                         %% Seed
      <<16#04:8,
        16#7B6AA5D85E572983E6FB32A7CDEBC140:128,                                                    %% X(p0)
        16#27B6916A894D3AEE7106FE805FC34B44:128>>,                                                  %% Y(p0)
      <<16#3FFFFFFF7FFFFFFFBE0024720613B5A3:128>>,                                                  %% Order
      <<16#04:8>>                                                                                   %% CoFactor
    };

curve(secp160k1) ->
    {
     {prime_field, <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFAC73:160>>},                            %% Prime
     {<<16#00:8>>,                                                                                  %% A
      <<16#07:8>>,                                                                                  %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#3B4C382CE37AA192A4019E763036F4F5DD4D7EBB:160,                                            %% X(p0)
        16#938CF935318FDCED6BC28286531733C3F03C4FEE:160>>,                                          %% Y(p0)
      <<16#0100000000000000000001B8FA16DFAB9ACA16B6B3:168>>,                                        %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(secp160r1) ->
    {
     {prime_field, <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7FFFFFFF:160>>},                            %% Prime
     {<<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7FFFFFFC:160>>,                                          %% A
      <<16#1C97BEFC54BD7A8B65ACF89F81D4D4ADC565FA45:160>>,                                          %% B
      <<16#1053CDE42C14D696E67687561517533BF3F83345:160>>},                                         %% Seed
      <<16#04:8,
        16#4A96B5688EF573284664698968C38BB913CBFC82:160,                                            %% X(p0)
        16#23A628553168947D59DCC912042351377AC5FB32:160>>,                                          %% Y(p0)
      <<16#0100000000000000000001F4C8F927AED3CA752257:168>>,                                        %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(secp160r2) ->
    {
     {prime_field, <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFAC73:160>>},                            %% Prime
     {<<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFAC70:160>>,                                          %% A
      <<16#B4E134D3FB59EB8BAB57274904664D5AF50388BA:160>>,                                          %% B
      <<16#B99B99B099B323E02709A4D696E6768756151751:160>>},                                         %% Seed
      <<16#04:8,
        16#52DCB034293A117E1F4FF11B30F7199D3144CE6D:160,                                            %% X(p0)
        16#FEAFFEF2E331F296E071FA0DF9982CFEA7D43F2E:160>>,                                          %% Y(p0)
      <<16#0100000000000000000000351EE786A818F3A1A16B:168>>,                                        %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(secp192r1) ->
    {
     {prime_field, <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF:192>>},                    %% Prime
     {<<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFC:192>>,                                  %% A
      <<16#64210519E59C80E70FA7E9AB72243049FEB8DEECC146B9B1:192>>,                                  %% B
      <<16#3045AE6FC8422F64ED579528D38120EAE12196D5:160>>},                                         %% Seed
      <<16#04:8,
        16#188DA80EB03090F67CBF20EB43A18800F4FF0AFD82FF1012:192,                                    %% X(p0)
        16#07192B95FFC8DA78631011ED6B24CDD573F977A11E794811:192>>,                                  %% Y(p0)
      <<16#FFFFFFFFFFFFFFFFFFFFFFFF99DEF836146BC9B1B4D22831:192>>,                                  %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(secp192k1) ->
    {
     {prime_field, <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFEE37:192>>},                    %% Prime
     {<<16#00:8>>,                                                                                  %% A
      <<16#03:8>>,                                                                                  %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#DB4FF10EC057E9AE26B07D0280B7F4341DA5D1B1EAE06C7D:192,                                    %% X(p0)
        16#9B2F2F6D9C5628A7844163D015BE86344082AA88D95E2F9D:192>>,                                  %% Y(p0)
      <<16#FFFFFFFFFFFFFFFFFFFFFFFE26F2FC170F69466A74DEFD8D:192>>,                                  %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(secp224k1) ->
    {
     {prime_field, <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFE56D:224>>},            %% Prime
     {<<16#00:8>>,                                                                                  %% A
      <<16#05:8>>,                                                                                  %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#A1455B334DF099DF30FC28A169A467E9E47075A90F7E650EB6B7A45C:224,                            %% X(p0)
        16#7E089FED7FBA344282CAFBD6F7E319F7C0B0BD59E2CA4BDB556D61A5:224>>,                          %% Y(p0)
      <<16#010000000000000000000000000001DCE8D2EC6184CAF0A971769FB1F7:232>>,                        %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(secp224r1) ->
    {
     {prime_field, <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000001:224>>},            %% Prime
     {<<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFE:224>>,                          %% A
      <<16#B4050A850C04B3ABF54132565044B0B7D7BFD8BA270B39432355FFB4:224>>,                          %% B
      <<16#BD71344799D5C7FCDC45B59FA3B9AB8F6A948BC5:160>>},                                         %% Seed
      <<16#04:8,
        16#B70E0CBD6BB4BF7F321390B94A03C1D356C21122343280D6115C1D21:224,                            %% X(p0)
        16#BD376388B5F723FB4C22DFE6CD4375A05A07476444D5819985007E34:224>>,                          %% Y(p0)
      <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFF16A2E0B8F03E13DD29455C5C2A3D:224>>,                          %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(secp256k1) ->
    {
     {prime_field, <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F:256>>},    %% Prime
     {<<16#00:8>>,                                                                                  %% A
      <<16#07:8>>,                                                                                  %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798:256,                    %% X(p0)
        16#483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8:256>>,                  %% Y(p0)
      <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141:256>>,                  %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(secp256r1) ->
    {
     {prime_field, <<16#FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF:256>>},    %% Prime
     {<<16#FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFC:256>>,                  %% A
      <<16#5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B:256>>,                  %% B
      <<16#C49D360886E704936A6678E1139D26B7819F7E90:160>>},                                         %% Seed
      <<16#04:8,
        16#6B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296:256,                    %% X(p0)
        16#4FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5:256>>,                  %% Y(p0)
      <<16#FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551:256>>,                  %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(secp384r1) ->
    {
     {prime_field, <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE:256,       %% Prime
                     16#FFFFFFFF0000000000000000FFFFFFFF:128>>},
     {<<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE:256,                    %% A
        16#FFFFFFFF0000000000000000FFFFFFFC:128>>,
      <<16#B3312FA7E23EE7E4988E056BE3F82D19181D9C6EFE8141120314088F5013875A:256,                    %% B
        16#C656398D8A2ED19D2A85C8EDD3EC2AEF:128>>,
      <<16#A335926AA319A27A1D00896A6773A4827ACDAC73:160>>},                                         %% Seed
      <<16#04:8,
        16#AA87CA22BE8B05378EB1C71EF320AD746E1D3B628BA79B9859F741E082542A38:256,                    %% X(p0)
        16#5502F25DBF55296C3A545E3872760AB7:128,
        16#3617DE4A96262C6F5D9E98BF9292DC29F8F41DBD289A147CE9DA3113B5F0B8C0:256,                    %% Y(p0)
        16#0A60B1CE1D7E819D7A431D7C90EA0E5F:128>>,
      <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC7634D81F4372DDF:256,                    %% Order
        16#581A0DB248B0A77AECEC196ACCC52973:128>>,
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(secp521r1) ->
    {
     {prime_field, <<16#01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF:256,       %% Prime
                     16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF:256,
                     16#FFFF:16>>},
     {<<16#01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF:256,                    %% A
        16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF:256,
        16#FFFC:16>>,
      <<16#51953EB9618E1C9A1F929A21A0B68540EEA2DA725B99B315F3B8B489918EF109:256,                    %% B
        16#E156193951EC7E937B1652C0BD3BB1BF073573DF883D2C34F1EF451FD46B503F:256,
        16#00:8>>,
      <<16#D09E8800291CB85396CC6717393284AAA0DA64BA:160>>},                                         %% Seed
      <<16#04:8,
        16#00C6858E06B70404E9CD9E3ECB662395B4429C648139053FB521F828AF606B4D:256,                    %% X(p0)
        16#3DBAA14B5E77EFE75928FE1DC127A2FFA8DE3348B3C1856A429BF97E7E31C2E5:256,
        16#BD66:16,
        16#011839296A789A3BC0045C8A5FB42C7D1BD998F54449579B446817AFBD17273E:256,                    %% Y(p0)
        16#662C97EE72995EF42640C550B9013FAD0761353C7086A272C24088BE94769FD1:256,
        16#6650:16>>,
      <<16#01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF:256,                    %% Order
        16#FFFA51868783BF2F966B7FCC0148F709A5D03BB5C9B8899C47AEBB6FB71E9138:256,
        16#6409:16>>,
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(prime192v1) ->
    {
     {prime_field, <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF:192>>},                    %% Prime
     {<<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFC:192>>,                                  %% A
      <<16#64210519E59C80E70FA7E9AB72243049FEB8DEECC146B9B1:192>>,                                  %% B
      <<16#3045AE6FC8422F64ED579528D38120EAE12196D5:160>>},                                         %% Seed
      <<16#04:8,
        16#188DA80EB03090F67CBF20EB43A18800F4FF0AFD82FF1012:192,                                    %% X(p0)
        16#07192B95FFC8DA78631011ED6B24CDD573F977A11E794811:192>>,                                  %% Y(p0)
      <<16#FFFFFFFFFFFFFFFFFFFFFFFF99DEF836146BC9B1B4D22831:192>>,                                  %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(prime192v2) ->
    {
     {prime_field, <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF:192>>},                    %% Prime
     {<<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFC:192>>,                                  %% A
      <<16#CC22D6DFB95C6B25E49C0D6364A4E5980C393AA21668D953:192>>,                                  %% B
      <<16#31A92EE2029FD10D901B113E990710F0D21AC6B6:160>>},                                         %% Seed
      <<16#04:8,
        16#EEA2BAE7E1497842F2DE7769CFE9C989C072AD696F48034A:192,                                    %% X(p0)
        16#6574D11D69B6EC7A672BB82A083DF2F2B0847DE970B2DE15:192>>,                                  %% Y(p0)
      <<16#FFFFFFFFFFFFFFFFFFFFFFFE5FB1A724DC80418648D8DD31:192>>,                                  %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(prime192v3) ->
    {
     {prime_field, <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFF:192>>},                    %% Prime
     {<<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFC:192>>,                                  %% A
      <<16#22123DC2395A05CAA7423DAECCC94760A7D462256BD56916:192>>,                                  %% B
      <<16#C469684435DEB378C4B65CA9591E2A5763059A2E:160>>},                                         %% Seed
      <<16#04:8,
        16#7D29778100C65A1DA1783716588DCE2B8B4AEE8E228F1896:192,                                    %% X(p0)
        16#38A90F22637337334B49DCB66A6DC8F9978ACA7648A943B0:192>>,                                  %% Y(p0)
      <<16#FFFFFFFFFFFFFFFFFFFFFFFF7A62D031C83F4294F640EC13:192>>,                                  %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(prime239v1) ->
    {
     {prime_field, <<16#7FFFFFFFFFFFFFFFFFFFFFFF7FFFFFFFFFFF8000000000007FFFFFFFFFFF:240>>},        %% Prime
     {<<16#7FFFFFFFFFFFFFFFFFFFFFFF7FFFFFFFFFFF8000000000007FFFFFFFFFFC:240>>,                      %% A
      <<16#6B016C3BDCF18941D0D654921475CA71A9DB2FB27D1D37796185C2942C0A:240>>,                      %% B
      <<16#E43BB460F0B80CC0C0B075798E948060F8321B7D:160>>},                                         %% Seed
      <<16#04:8,
        16#0FFA963CDCA8816CCC33B8642BEDF905C3D358573D3F27FBBD3B3CB9AAAF:240,                        %% X(p0)
        16#7DEBE8E4E90A5DAE6E4054CA530BA04654B36818CE226B39FCCB7B02F1AE:240>>,                      %% Y(p0)
      <<16#7FFFFFFFFFFFFFFFFFFFFFFF7FFFFF9E5E9A9F5D9071FBD1522688909D0B:240>>,                      %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(prime239v2) ->
    {
     {prime_field, <<16#7FFFFFFFFFFFFFFFFFFFFFFF7FFFFFFFFFFF8000000000007FFFFFFFFFFF:240>>},        %% Prime
     {<<16#7FFFFFFFFFFFFFFFFFFFFFFF7FFFFFFFFFFF8000000000007FFFFFFFFFFC:240>>,                      %% A
      <<16#617FAB6832576CBBFED50D99F0249C3FEE58B94BA0038C7AE84C8C832F2C:240>>,                      %% B
      <<16#E8B4011604095303CA3B8099982BE09FCB9AE616:160>>},                                         %% Seed
      <<16#04:8,
        16#38AF09D98727705120C921BB5E9E26296A3CDCF2F35757A0EAFD87B830E7:240,                        %% X(p0)
        16#5B0125E4DBEA0EC7206DA0FC01D9B081329FB555DE6EF460237DFF8BE4BA:240>>,                      %% Y(p0)
      <<16#7FFFFFFFFFFFFFFFFFFFFFFF800000CFA7E8594377D414C03821BC582063:240>>,                      %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(prime239v3) ->
    {
     {prime_field, <<16#7FFFFFFFFFFFFFFFFFFFFFFF7FFFFFFFFFFF8000000000007FFFFFFFFFFF:240>>},        %% Prime
     {<<16#7FFFFFFFFFFFFFFFFFFFFFFF7FFFFFFFFFFF8000000000007FFFFFFFFFFC:240>>,                      %% A
      <<16#255705FA2A306654B1F4CB03D6A750A30C250102D4988717D9BA15AB6D3E:240>>,                      %% B
      <<16#7D7374168FFE3471B60A857686A19475D3BFA2FF:160>>},                                         %% Seed
      <<16#04:8,
        16#6768AE8E18BB92CFCF005C949AA2C6D94853D0E660BBF854B1C9505FE95A:240,                        %% X(p0)
        16#1607E6898F390C06BC1D552BAD226F3B6FCFE48B6E818499AF18E3ED6CF3:240>>,                      %% Y(p0)
      <<16#7FFFFFFFFFFFFFFFFFFFFFFF7FFFFF975DEB41B3A6057C3C432146526551:240>>,                      %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(prime256v1) ->
    {
     {prime_field, <<16#FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF:256>>},    %% Prime
     {<<16#FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFC:256>>,                  %% A
      <<16#5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B:256>>,                  %% B
      <<16#C49D360886E704936A6678E1139D26B7819F7E90:160>>},                                         %% Seed
      <<16#04:8,
        16#6B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296:256,                    %% X(p0)
        16#4FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5:256>>,                  %% Y(p0)
      <<16#FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551:256>>,                  %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(sect113r1) ->
    {
     {characteristic_two_field, 113, {tpbasis,9}},
     {<<16#3088250CA6E7C7FE649CE85820F7:112>>,                                                      %% A
      <<16#E8BEE4D3E2260744188BE0E9C723:112>>,                                                      %% B
      <<16#10E723AB14D696E6768756151756FEBF8FCB49A9:160>>},                                         %% Seed
      <<16#04:8,
        16#009D73616F35F4AB1407D73562C10F:120,                                                      %% X(p0)
        16#00A52830277958EE84D1315ED31886:120>>,                                                    %% Y(p0)
      <<16#0100000000000000D9CCEC8A39E56F:120>>,                                                    %% Order
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(sect113r2) ->
    {
     {characteristic_two_field, 113, {tpbasis,9}},
     {<<16#689918DBEC7E5A0DD6DFC0AA55C7:112>>,                                                      %% A
      <<16#95E9A9EC9B297BD4BF36E059184F:112>>,                                                      %% B
      <<16#10C0FB15760860DEF1EEF4D696E676875615175D:160>>},                                         %% Seed
      <<16#04:8,
        16#01A57A6A7B26CA5EF52FCDB8164797:120,                                                      %% X(p0)
        16#00B3ADC94ED1FE674C06E695BABA1D:120>>,                                                    %% Y(p0)
      <<16#010000000000000108789B2496AF93:120>>,                                                    %% Order
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(sect131r1) ->
    {
     {characteristic_two_field, 131, {ppbasis,2,3,8}},
     {<<16#07A11B09A76B562144418FF3FF8C2570B8:136>>,                                                %% A
      <<16#0217C05610884B63B9C6C7291678F9D341:136>>,                                                %% B
      <<16#4D696E676875615175985BD3ADBADA21B43A97E2:160>>},                                         %% Seed
      <<16#04:8,
        16#0081BAF91FDF9833C40F9C181343638399:136,                                                  %% X(p0)
        16#078C6E7EA38C001F73C8134B1B4EF9E150:136>>,                                                %% Y(p0)
      <<16#0400000000000000023123953A9464B54D:136>>,                                                %% Order
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(sect131r2) ->
    {
     {characteristic_two_field, 131, {ppbasis,2,3,8}},
     {<<16#03E5A88919D7CAFCBF415F07C2176573B2:136>>,                                                %% A
      <<16#04B8266A46C55657AC734CE38F018F2192:136>>,                                                %% B
      <<16#985BD3ADBAD4D696E676875615175A21B43A97E3:160>>},                                         %% Seed
      <<16#04:8,
        16#0356DCD8F2F95031AD652D23951BB366A8:136,                                                  %% X(p0)
        16#0648F06D867940A5366D9E265DE9EB240F:136>>,                                                %% Y(p0)
      <<16#0400000000000000016954A233049BA98F:136>>,                                                %% Order
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(sect163k1) ->
    {
     {characteristic_two_field, 163, {ppbasis,3,6,7}},
     {<<16#01:8>>,                                                                                  %% A
      <<16#01:8>>,                                                                                  %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#02FE13C0537BBC11ACAA07D793DE4E6D5E5C94EEE8:168,                                          %% X(p0)
        16#0289070FB05D38FF58321F2E800536D538CCDAA3D9:168>>,                                        %% Y(p0)
      <<16#04000000000000000000020108A2E0CC0D99F8A5EF:168>>,                                        %% Order
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(sect163r1) ->
    {
     {characteristic_two_field, 163, {ppbasis,3,6,7}},
     {<<16#07B6882CAAEFA84F9554FF8428BD88E246D2782AE2:168>>,                                        %% A
      <<16#0713612DCDDCB40AAB946BDA29CA91F73AF958AFD9:168>>,                                        %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#0369979697AB43897789566789567F787A7876A654:168,                                          %% X(p0)
        16#00435EDB42EFAFB2989D51FEFCE3C80988F41FF883:168>>,                                        %% Y(p0)
      <<16#03FFFFFFFFFFFFFFFFFFFF48AAB689C29CA710279B:168>>,                                        %% Order
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(sect163r2) ->
    {
     {characteristic_two_field, 163, {ppbasis,3,6,7}},
     {<<16#01:8>>,                                                                                  %% A
      <<16#020A601907B8C953CA1481EB10512F78744A3205FD:168>>,                                        %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#03F0EBA16286A2D57EA0991168D4994637E8343E36:168,                                          %% X(p0)
        16#00D51FBC6C71A0094FA2CDD545B11C5C0C797324F1:168>>,                                        %% Y(p0)
      <<16#040000000000000000000292FE77E70C12A4234C33:168>>,                                        %% Order
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(sect193r1) ->
    {
     {characteristic_two_field, 193, {tpbasis,15}},
     {<<16#17858FEB7A98975169E171F77B4087DE098AC8A911DF7B01:192>>,                                  %% A
      <<16#FDFB49BFE6C3A89FACADAA7A1E5BBC7CC1C2E5D831478814:192>>,                                  %% B
      <<16#103FAEC74D696E676875615175777FC5B191EF30:160>>},                                         %% Seed
      <<16#04:8,
        16#01F481BC5F0FF84A74AD6CDF6FDEF4BF6179625372D8C0C5E1:200,                                  %% X(p0)
        16#0025E399F2903712CCF3EA9E3A1AD17FB0B3201B6AF7CE1B05:200>>,                                %% Y(p0)
      <<16#01000000000000000000000000C7F34A778F443ACC920EBA49:200>>,                                %% Order
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(sect193r2) ->
    {
     {characteristic_two_field, 193, {tpbasis,15}},
     {<<16#0163F35A5137C2CE3EA6ED8667190B0BC43ECD69977702709B:200>>,                                %% A
      <<16#C9BB9E8927D4D64C377E2AB2856A5B16E3EFB7F61D4316AE:192>>,                                  %% B
      <<16#10B7B4D696E676875615175137C8A16FD0DA2211:160>>},                                         %% Seed
      <<16#04:8,
        16#00D9B67D192E0367C803F39E1A7E82CA14A651350AAE617E8F:200,                                  %% X(p0)
        16#01CE94335607C304AC29E7DEFBD9CA01F596F927224CDECF6C:200>>,                                %% Y(p0)
      <<16#010000000000000000000000015AAB561B005413CCD4EE99D5:200>>,                                %% Order
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(sect233k1) ->
    {
     {characteristic_two_field, 233, {tpbasis,74}},
     {<<16#00:8>>,                                                                                  %% A
      <<16#01:8>>,                                                                                  %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#017232BA853A7E731AF129F22FF4149563A419C26BF50A4C9D6EEFAD6126:240,                        %% X(p0)
        16#01DB537DECE819B7F70F555A67C427A8CD9BF18AEB9B56E0C11056FAE6A3:240>>,                      %% Y(p0)
      <<16#8000000000000000000000000000069D5BB915BCD46EFB1AD5F173ABDF:232>>,                        %% Order
      <<16#04:8>>                                                                                   %% CoFactor
    };

curve(sect233r1) ->
    {
     {characteristic_two_field, 233, {tpbasis,74}},
     {<<16#01:8>>,                                                                                  %% A
      <<16#66647EDE6C332C7F8C0923BB58213B333B20E9CE4281FE115F7D8F90AD:232>>,                        %% B
      <<16#74D59FF07F6B413D0EA14B344B20A2DB049B50C3:160>>},                                         %% Seed
      <<16#04:8,
        16#00FAC9DFCBAC8313BB2139F1BB755FEF65BC391F8B36F8F8EB7371FD558B:240,                        %% X(p0)
        16#01006A08A41903350678E58528BEBF8A0BEFF867A7CA36716F7E01F81052:240>>,                      %% Y(p0)
      <<16#01000000000000000000000000000013E974E72F8A6922031D2603CFE0D7:240>>,                      %% Order
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(sect239k1) ->
    {
     {characteristic_two_field, 239, {tpbasis,158}},
     {<<16#00:8>>,                                                                                  %% A
      <<16#01:8>>,                                                                                  %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#29A0B6A887A983E9730988A68727A8B2D126C44CC2CC7B2A6555193035DC:240,                        %% X(p0)
        16#76310804F12E549BDB011C103089E73510ACB275FC312A5DC6B76553F0CA:240>>,                      %% Y(p0)
      <<16#2000000000000000000000000000005A79FEC67CB6E91F1C1DA800E478A5:240>>,                      %% Order
      <<16#04:8>>                                                                                   %% CoFactor
    };

curve(sect283k1) ->
    {
     {characteristic_two_field, 283, {ppbasis,5,7,12}},
     {<<16#00:8>>,                                                                                  %% A
      <<16#01:8>>,                                                                                  %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#0503213F78CA44883F1A3B8162F188E553CD265F23C1567A16876913B0C2AC24:256,                    %% X(p0)
        16#58492836:32,
        16#01CCDA380F1C9E318D90F95D07E5426FE87E45C0E8184698E45962364E341161:256,                    %% Y(p0)
        16#77DD2259:32>>,
      <<16#01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9AE2ED07577265DFF7F94451E06:256,                    %% Order
        16#1E163C61:32>>,
      <<16#04:8>>                                                                                   %% CoFactor
    };

curve(sect283r1) ->
    {
     {characteristic_two_field, 283, {ppbasis,5,7,12}},
     {<<16#01:8>>,                                                                                  %% A
      <<16#027B680AC8B8596DA5A4AF8A19A0303FCA97FD7645309FA2A581485AF6263E31:256,                    %% B
        16#3B79A2F5:32>>,
      <<16#77E2B07370EB0F832A6DD5B62DFC88CD06BB84BE:160>>},                                         %% Seed
      <<16#04:8,
        16#05F939258DB7DD90E1934F8C70B0DFEC2EED25B8557EAC9C80E2E198F8CDBECD:256,                    %% X(p0)
        16#86B12053:32,
        16#03676854FE24141CB98FE6D4B20D02B4516FF702350EDDB0826779C813F0DF45:256,                    %% Y(p0)
        16#BE8112F4:32>>,
      <<16#03FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEF90399660FC938A90165B042A7C:256,                    %% Order
        16#EFADB307:32>>,
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(sect409k1) ->
    {
     {characteristic_two_field, 409, {tpbasis,87}},
     {<<16#00:8>>,                                                                                  %% A
      <<16#01:8>>,                                                                                  %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#0060F05F658F49C1AD3AB1890F7184210EFD0987E307C84C27ACCFB8F9F67CC2:256,                    %% X(p0)
        16#C460189EB5AAAA62EE222EB1B35540CFE9023746:160,
        16#01E369050B7C4E42ACBA1DACBF04299C3460782F918EA427E6325165E9EA10E3:256,                    %% Y(p0)
        16#DA5F6C42E9C55215AA9CA27A5863EC48D8E0286B:160>>,
      <<16#7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE5F83B2D4EA20:256,                    %% Order
        16#400EC4557D5ED3E3E7CA5B4B5C83B8E01E5FCF:152>>,
      <<16#04:8>>                                                                                   %% CoFactor
    };

curve(sect409r1) ->
    {
     {characteristic_two_field, 409, {tpbasis,87}},
     {<<16#01:8>>,                                                                                  %% A
      <<16#21A5C2C8EE9FEB5C4B9A753B7B476B7FD6422EF1F3DD674761FA99D6AC27C8A9:256,                    %% B
        16#A197B272822F6CD57A55AA4F50AE317B13545F:152>>,
      <<16#4099B5A457F9D69F79213D094C4BCD4D4262210B:160>>},                                         %% Seed
      <<16#04:8,
        16#015D4860D088DDB3496B0C6064756260441CDE4AF1771D4DB01FFE5B34E59703:256,                    %% X(p0)
        16#DC255A868A1180515603AEAB60794E54BB7996A7:160,
        16#0061B1CFAB6BE5F32BBFA78324ED106A7636B9C5A7BD198D0158AA4F5488D08F:256,                    %% Y(p0)
        16#38514F1FDF4B4F40D2181B3681C364BA0273C706:160>>,
      <<16#010000000000000000000000000000000000000000000000000001E2AAD6A612:256,                    %% Order
        16#F33307BE5FA47C3C9E052F838164CD37D9A21173:160>>,
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(sect571k1) ->
    {
     {characteristic_two_field, 571, {ppbasis,2,5,10}},
     {<<16#00:8>>,                                                                                  %% A
      <<16#01:8>>,                                                                                  %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#026EB7A859923FBC82189631F8103FE4AC9CA2970012D5D46024804801841CA4:256,                    %% X(p0)
        16#4370958493B205E647DA304DB4CEB08CBBD1BA39494776FB988B47174DCA88C7:256,
        16#E2945283A01C8972:64,
        16#0349DC807F4FBF374F4AEADE3BCA95314DD58CEC9F307A54FFC61EFC006D8A2C:256,                    %% Y(p0)
        16#9D4979C0AC44AEA74FBEBBB9F772AEDCB620B01A7BA7AF1B320430C8591984F6:256,
        16#01CD4C143EF1C7A3:64>>,
      <<16#0200000000000000000000000000000000000000000000000000000000000000:256,                    %% Order
        16#00000000131850E1F19A63E4B391A8DB917F4138B630D84BE5D639381E91DEB4:256,
        16#5CFE778F637C1001:64>>,
      <<16#04:8>>                                                                                   %% CoFactor
    };

curve(sect571r1) ->
    {
     {characteristic_two_field, 571, {ppbasis,2,5,10}},
     {<<16#01:8>>,                                                                                  %% A
      <<16#02F40E7E2221F295DE297117B7F3D62F5C6A97FFCB8CEFF1CD6BA8CE4A9A18AD:256,                    %% B
        16#84FFABBD8EFA59332BE7AD6756A66E294AFD185A78FF12AA520E4DE739BACA0C:256,
        16#7FFEFF7F2955727A:64>>,
      <<16#2AA058F73A0E33AB486B0F610410C53A7F132310:160>>},                                         %% Seed
      <<16#04:8,
        16#0303001D34B856296C16C0D40D3CD7750A93D1D2955FA80AA5F40FC8DB7B2ABD:256,                    %% X(p0)
        16#BDE53950F4C0D293CDD711A35B67FB1499AE60038614F1394ABFA3B4C850D927:256,
        16#E1E7769C8EEC2D19:64,
        16#037BF27342DA639B6DCCFFFEB73D69D78C6C27A6009CBBCA1980F8533921E8A6:256,                    %% Y(p0)
        16#84423E43BAB08A576291AF8F461BB2A8B3531D2F0485C19B16E2F1516E23DD3C:256,
        16#1A4827AF1B8AC15B:64>>,
      <<16#03FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF:256,                    %% Order
        16#FFFFFFFFE661CE18FF55987308059B186823851EC7DD9CA1161DE93D5174D66E:256,
        16#8382E9BB2FE84E47:64>>,
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(c2pnb163v1) ->
    {
     {characteristic_two_field, 163, {ppbasis,1,2,8}},
     {<<16#072546B5435234A422E0789675F432C89435DE5242:168>>,                                        %% A
      <<16#C9517D06D5240D3CFF38C74B20B6CD4D6F9DD4D9:160>>,                                          %% B
      <<16#D2C0FB15760860DEF1EEF4D696E6768756151754:160>>},                                         %% Seed
      <<16#04:8,
        16#07AF69989546103D79329FCC3D74880F33BBE803CB:168,                                          %% X(p0)
        16#01EC23211B5966ADEA1D3F87F7EA5848AEF0B7CA9F:168>>,                                        %% Y(p0)
      <<16#0400000000000000000001E60FC8821CC74DAEAFC1:168>>,                                        %% Order
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(c2pnb163v2) ->
    {
     {characteristic_two_field, 163, {ppbasis,1,2,8}},
     {<<16#0108B39E77C4B108BED981ED0E890E117C511CF072:168>>,                                        %% A
      <<16#0667ACEB38AF4E488C407433FFAE4F1C811638DF20:168>>,                                        %% B
      <<16#53814C050D44D696E67687561517580CA4E29FFD:160>>},                                         %% Seed
      <<16#04:8,
        16#0024266E4EB5106D0A964D92C4860E2671DB9B6CC5:168,                                          %% X(p0)
        16#079F684DDF6684C5CD258B3890021B2386DFD19FC5:168>>,                                        %% Y(p0)
      <<16#03FFFFFFFFFFFFFFFFFFFDF64DE1151ADBB78F10A7:168>>,                                        %% Order
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(c2pnb163v3) ->
    {
     {characteristic_two_field, 163, {ppbasis,1,2,8}},
     {<<16#07A526C63D3E25A256A007699F5447E32AE456B50E:168>>,                                        %% A
      <<16#03F7061798EB99E238FD6F1BF95B48FEEB4854252B:168>>,                                        %% B
      <<16#50CBF1D95CA94D696E676875615175F16A36A3B8:160>>},                                         %% Seed
      <<16#04:8,
        16#02F9F87B7C574D0BDECF8A22E6524775F98CDEBDCB:168,                                          %% X(p0)
        16#05B935590C155E17EA48EB3FF3718B893DF59A05D0:168>>,                                        %% Y(p0)
      <<16#03FFFFFFFFFFFFFFFFFFFE1AEE140F110AFF961309:168>>,                                        %% Order
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(c2pnb176v1) ->
    {
     {characteristic_two_field, 176, {ppbasis,1,2,43}},
     {<<16#E4E6DB2995065C407D9D39B8D0967B96704BA8E9C90B:176>>,                                      %% A
      <<16#5DDA470ABE6414DE8EC133AE28E9BBD7FCEC0AE0FFF2:176>>,                                      %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#8D16C2866798B600F9F08BB4A8E860F3298CE04A5798:176,                                        %% X(p0)
        16#6FA4539C2DADDDD6BAB5167D61B436E1D92BB16A562C:176>>,                                      %% Y(p0)
      <<16#010092537397ECA4F6145799D62B0A19CE06FE26AD:168>>,                                        %% Order
      <<16#FF6E:16>>                                                                                %% CoFactor
    };

curve(c2tnb191v1) ->
    {
     {characteristic_two_field, 191, {tpbasis,9}},
     {<<16#2866537B676752636A68F56554E12640276B649EF7526267:192>>,                                  %% A
      <<16#2E45EF571F00786F67B0081B9495A3D95462F5DE0AA185EC:192>>,                                  %% B
      <<16#4E13CA542744D696E67687561517552F279A8C84:160>>},                                         %% Seed
      <<16#04:8,
        16#36B3DAF8A23206F9C4F299D7B21A9C369137F2C84AE1AA0D:192,                                    %% X(p0)
        16#765BE73433B3F95E332932E70EA245CA2418EA0EF98018FB:192>>,                                  %% Y(p0)
      <<16#40000000000000000000000004A20E90C39067C893BBB9A5:192>>,                                  %% Order
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(c2tnb191v2) ->
    {
     {characteristic_two_field, 191, {tpbasis,9}},
     {<<16#401028774D7777C7B7666D1366EA432071274F89FF01E718:192>>,                                  %% A
      <<16#0620048D28BCBD03B6249C99182B7C8CD19700C362C46A01:192>>,                                  %% B
      <<16#0871EF2FEF24D696E6768756151758BEE0D95C15:160>>},                                         %% Seed
      <<16#04:8,
        16#3809B2B7CC1B28CC5A87926AAD83FD28789E81E2C9E3BF10:192,                                    %% X(p0)
        16#17434386626D14F3DBF01760D9213A3E1CF37AEC437D668A:192>>,                                  %% Y(p0)
      <<16#20000000000000000000000050508CB89F652824E06B8173:192>>,                                  %% Order
      <<16#04:8>>                                                                                   %% CoFactor
    };

curve(c2tnb191v3) ->
    {
     {characteristic_two_field, 191, {tpbasis,9}},
     {<<16#6C01074756099122221056911C77D77E77A777E7E7E77FCB:192>>,                                  %% A
      <<16#71FE1AF926CF847989EFEF8DB459F66394D90F32AD3F15E8:192>>,                                  %% B
      <<16#E053512DC684D696E676875615175067AE786D1F:160>>},                                         %% Seed
      <<16#04:8,
        16#375D4CE24FDE434489DE8746E71786015009E66E38A926DD:192,                                    %% X(p0)
        16#545A39176196575D985999366E6AD34CE0A77CD7127B06BE:192>>,                                  %% Y(p0)
      <<16#155555555555555555555555610C0B196812BFB6288A3EA3:192>>,                                  %% Order
      <<16#06:8>>                                                                                   %% CoFactor
    };

curve(c2pnb208w1) ->
    {
     {characteristic_two_field, 208, {ppbasis,1,2,83}},
     {<<16#00:8>>,                                                                                  %% A
      <<16#C8619ED45A62E6212E1160349E2BFA844439FAFC2A3FD1638F9E:208>>,                              %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#89FDFBE4ABE193DF9559ECF07AC0CE78554E2784EB8C1ED1A57A:208,                                %% X(p0)
        16#0F55B51A06E78E9AC38A035FF520D8B01781BEB1A6BB08617DE3:208>>,                              %% Y(p0)
      <<16#0101BAF95C9723C57B6C21DA2EFF2D5ED588BDD5717E212F9D:200>>,                                %% Order
      <<16#FE48:16>>                                                                                %% CoFactor
    };

curve(c2tnb239v1) ->
    {
     {characteristic_two_field, 239, {tpbasis,36}},
     {<<16#32010857077C5431123A46B808906756F543423E8D27877578125778AC76:240>>,                      %% A
      <<16#790408F2EEDAF392B012EDEFB3392F30F4327C0CA3F31FC383C422AA8C16:240>>,                      %% B
      <<16#D34B9A4D696E676875615175CA71B920BFEFB05D:160>>},                                         %% Seed
      <<16#04:8,
        16#57927098FA932E7C0A96D3FD5B706EF7E5F5C156E16B7E7C86038552E91D:240,                        %% X(p0)
        16#61D8EE5077C33FECF6F1A16B268DE469C3C7744EA9A971649FC7A9616305:240>>,                      %% Y(p0)
      <<16#2000000000000000000000000000000F4D42FFE1492A4993F1CAD666E447:240>>,                      %% Order
      <<16#04:8>>                                                                                   %% CoFactor
    };

curve(c2tnb239v2) ->
    {
     {characteristic_two_field, 239, {tpbasis,36}},
     {<<16#4230017757A767FAE42398569B746325D45313AF0766266479B75654E65F:240>>,                      %% A
      <<16#5037EA654196CFF0CD82B2C14A2FCF2E3FF8775285B545722F03EACDB74B:240>>,                      %% B
      <<16#2AA6982FDFA4D696E676875615175D266727277D:160>>},                                         %% Seed
      <<16#04:8,
        16#28F9D04E900069C8DC47A08534FE76D2B900B7D7EF31F5709F200C4CA205:240,                        %% X(p0)
        16#5667334C45AFF3B5A03BAD9DD75E2C71A99362567D5453F7FA6E227EC833:240>>,                      %% Y(p0)
      <<16#1555555555555555555555555555553C6F2885259C31E3FCDF154624522D:240>>,                      %% Order
      <<16#06:8>>                                                                                   %% CoFactor
    };

curve(c2tnb239v3) ->
    {
     {characteristic_two_field, 239, {tpbasis,36}},
     {<<16#01238774666A67766D6676F778E676B66999176666E687666D8766C66A9F:240>>,                      %% A
      <<16#6A941977BA9F6A435199ACFC51067ED587F519C5ECB541B8E44111DE1D40:240>>,                      %% B
      <<16#9E076F4D696E676875615175E11E9FDD77F92041:160>>},                                         %% Seed
      <<16#04:8,
        16#70F6E9D04D289C4E89913CE3530BFDE903977D42B146D539BF1BDE4E9C92:240,                        %% X(p0)
        16#2E5A0EAF6E5E1305B9004DCE5C0ED7FE59A35608F33837C816D80B79F461:240>>,                      %% Y(p0)
      <<16#0CCCCCCCCCCCCCCCCCCCCCCCCCCCCCAC4912D2D9DF903EF9888B8A0E4CFF:240>>,                      %% Order
      <<16#0A:8>>                                                                                   %% CoFactor
    };

curve(c2pnb272w1) ->
    {
     {characteristic_two_field, 272, {ppbasis,1,3,56}},
     {<<16#91A091F03B5FBA4AB2CCF49C4EDD220FB028712D42BE752B2C40094DBACDB586:256,                    %% A
        16#FB20:16>>,
      <<16#7167EFC92BB2E3CE7C8AAAFF34E12A9C557003D7C73A6FAF003F99F6CC8482E5:256,                    %% B
        16#40F7:16>>,
      none},                                                                                        %% Seed
      <<16#04:8,
        16#6108BABB2CEEBCF787058A056CBE0CFE622D7723A289E08A07AE13EF0D10D171:256,                    %% X(p0)
        16#DD8D:16,
        16#10C7695716851EEF6BA7F6872E6142FBD241B830FF5EFCACECCAB05E02005DDE:256,                    %% Y(p0)
        16#9D23:16>>,
      <<16#0100FAF51354E0E39E4892DF6E319C72C8161603FA45AA7B998A167B8F1E6295:256,                    %% Order
        16#21:8>>,
      <<16#FF06:16>>                                                                                %% CoFactor
    };

curve(c2pnb304w1) ->
    {
     {characteristic_two_field, 304, {ppbasis,1,2,11}},
     {<<16#FD0D693149A118F651E6DCE6802085377E5F882D1B510B44160074C128807836:256,                    %% A
        16#5A0396C8E681:48>>,
      <<16#BDDB97E555A50A908E43B01C798EA5DAA6788F1EA2794EFCF57166B8C1403960:256,                    %% B
        16#1E55827340BE:48>>,
      none},                                                                                        %% Seed
      <<16#04:8,
        16#197B07845E9BE2D96ADB0F5F3C7F2CFFBD7A3EB8B6FEC35C7FD67F26DDF6285A:256,                    %% X(p0)
        16#644F740A2614:48,
        16#E19FBEB76E0DA171517ECF401B50289BF014103288527A9B416A105E80260B54:256,                    %% Y(p0)
        16#9FDC1B92C03B:48>>,
      <<16#0101D556572AABAC800101D556572AABAC8001022D5C91DD173F8FB561DA6899:256,                    %% Order
        16#164443051D:40>>,
      <<16#FE2E:16>>                                                                                %% CoFactor
    };

curve(c2tnb359v1) ->
    {
     {characteristic_two_field, 359, {tpbasis,68}},
     {<<16#5667676A654B20754F356EA92017D946567C46675556F19556A04616B567D223:256,                    %% A
        16#A5E05656FB549016A96656A557:104>>,
      <<16#2472E2D0197C49363F1FE7F5B6DB075D52B6947D135D8CA445805D39BC345626:256,                    %% B
        16#089687742B6329E70680231988:104>>,
      <<16#2B354920B724D696E67687561517585BA1332DC6:160>>},                                         %% Seed
      <<16#04:8,
        16#3C258EF3047767E7EDE0F1FDAA79DAEE3841366A132E163ACED4ED2401DF9C6B:256,                    %% X(p0)
        16#DCDE98E8E707C07A2239B1B097:104,
        16#53D7E08529547048121E9C95F3791DD804963948F34FAE7BF44EA82365DC7868:256,                    %% Y(p0)
        16#FE57E4AE2DE211305A407104BD:104>>,
      <<16#01AF286BCA1AF286BCA1AF286BCA1AF286BCA1AF286BC9FB8F6B85C556892C20:256,                    %% Order
        16#A7EB964FE7719E74F490758D3B:104>>,
      <<16#4C:8>>                                                                                   %% CoFactor
    };

curve(c2pnb368w1) ->
    {
     {characteristic_two_field, 368, {ppbasis,1,2,85}},
     {<<16#E0D2EE25095206F5E2A4F9ED229F1F256E79A0E2B455970D8D0D865BD94778C5:256,                    %% A
        16#76D62F0AB7519CCD2A1A906AE30D:112>>,
      <<16#FC1217D4320A90452C760A58EDCD30C8DD069B3C34453837A34ED50CB54917E1:256,                    %% B
        16#C2112D84D164F444F8F74786046A:112>>,
      none},                                                                                        %% Seed
      <<16#04:8,
        16#1085E2755381DCCCE3C1557AFA10C2F0C0C2825646C5B34A394CBCFA8BC16B22:256,                    %% X(p0)
        16#E7E789E927BE216F02E1FB136A5F:112,
        16#7B3EB1BDDCBA62D5D8B2059B525797FC73822C59059C623A45FF3843CEE8F87C:256,                    %% Y(p0)
        16#D1855ADAA81E2A0750B80FDA2310:112>>,
      <<16#010090512DA9AF72B08349D98A5DD4C7B0532ECA51CE03E2D10F3B7AC579BD87:256,                    %% Order
        16#E909AE40A6F131E9CFCE5BD967:104>>,
      <<16#FF70:16>>                                                                                %% CoFactor
    };

curve(c2tnb431r1) ->
    {
     {characteristic_two_field, 431, {tpbasis,120}},
     {<<16#1A827EF00DD6FC0E234CAF046C6A5D8A85395B236CC4AD2CF32A0CADBDC9DDF6:256,                    %% A
        16#20B0EB9906D0957F6C6FEACD615468DF104DE296CD8F:176>>,
      <<16#10D9B4A3D9047D8B154359ABFB1B7F5485B04CEB868237DDC9DEDA982A679A5A:256,                    %% B
        16#919B626D4E50A8DD731B107A9962381FB5D807BF2618:176>>,
      none},                                                                                        %% Seed
      <<16#04:8,
        16#120FC05D3C67A99DE161D2F4092622FECA701BE4F50F4758714E8A87BBF2A658:256,                    %% X(p0)
        16#EF8C21E7C5EFE965361F6C2999C0C247B0DBD70CE6B7:176,
        16#20D0AF8903A96F8D5FA2C255745D3C451B302C9346D9B7E485E7BCE41F6B591F:256,                    %% Y(p0)
        16#3E8F6ADDCBB0BC4C2F947A7DE1A89B625D6A598B3760:176>>,
      <<16#0340340340340340340340340340340340340340340340340340340323C313FA:256,                    %% Order
        16#B50589703B5EC68D3587FEC60D161CC149C1AD4A91:168>>,
      <<16#2760:16>>                                                                                %% CoFactor
    };

curve(wtls1) ->
    {
     {characteristic_two_field, 113, {tpbasis,9}},
     {<<16#01:8>>,                                                                                  %% A
      <<16#01:8>>,                                                                                  %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#01667979A40BA497E5D5C270780617:120,                                                      %% X(p0)
        16#00F44B4AF1ECC2630E08785CEBCC15:120>>,                                                    %% Y(p0)
      <<16#FFFFFFFFFFFFFFFDBF91AF6DEA73:112>>,                                                      %% Order
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(wtls3) ->
    {
     {characteristic_two_field, 163, {ppbasis,3,6,7}},
     {<<16#01:8>>,                                                                                  %% A
      <<16#01:8>>,                                                                                  %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#02FE13C0537BBC11ACAA07D793DE4E6D5E5C94EEE8:168,                                          %% X(p0)
        16#0289070FB05D38FF58321F2E800536D538CCDAA3D9:168>>,                                        %% Y(p0)
      <<16#04000000000000000000020108A2E0CC0D99F8A5EF:168>>,                                        %% Order
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(wtls4) ->
    {
     {characteristic_two_field, 113, {tpbasis,9}},
     {<<16#3088250CA6E7C7FE649CE85820F7:112>>,                                                      %% A
      <<16#E8BEE4D3E2260744188BE0E9C723:112>>,                                                      %% B
      <<16#10E723AB14D696E6768756151756FEBF8FCB49A9:160>>},                                         %% Seed
      <<16#04:8,
        16#009D73616F35F4AB1407D73562C10F:120,                                                      %% X(p0)
        16#00A52830277958EE84D1315ED31886:120>>,                                                    %% Y(p0)
      <<16#0100000000000000D9CCEC8A39E56F:120>>,                                                    %% Order
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(wtls5) ->
    {
     {characteristic_two_field, 163, {ppbasis,1,2,8}},
     {<<16#072546B5435234A422E0789675F432C89435DE5242:168>>,                                        %% A
      <<16#C9517D06D5240D3CFF38C74B20B6CD4D6F9DD4D9:160>>,                                          %% B
      <<16#D2C0FB15760860DEF1EEF4D696E6768756151754:160>>},                                         %% Seed
      <<16#04:8,
        16#07AF69989546103D79329FCC3D74880F33BBE803CB:168,                                          %% X(p0)
        16#01EC23211B5966ADEA1D3F87F7EA5848AEF0B7CA9F:168>>,                                        %% Y(p0)
      <<16#0400000000000000000001E60FC8821CC74DAEAFC1:168>>,                                        %% Order
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(wtls6) ->
    {
     {prime_field, <<16#DB7C2ABF62E35E668076BEAD208B:112>>},                                        %% Prime
     {<<16#DB7C2ABF62E35E668076BEAD2088:112>>,                                                      %% A
      <<16#659EF8BA043916EEDE8911702B22:112>>,                                                      %% B
      <<16#00F50B028E4D696E676875615175290472783FB1:160>>},                                         %% Seed
      <<16#04:8,
        16#09487239995A5EE76B55F9C2F098:112,                                                        %% X(p0)
        16#A89CE5AF8724C0A23E0E0FF77500:112>>,                                                      %% Y(p0)
      <<16#DB7C2ABF62E35E7628DFAC6561C5:112>>,                                                      %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(wtls7) ->
    {
     {prime_field, <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFAC73:160>>},                            %% Prime
     {<<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFAC70:160>>,                                          %% A
      <<16#B4E134D3FB59EB8BAB57274904664D5AF50388BA:160>>,                                          %% B
      <<16#B99B99B099B323E02709A4D696E6768756151751:160>>},                                         %% Seed
      <<16#04:8,
        16#52DCB034293A117E1F4FF11B30F7199D3144CE6D:160,                                            %% X(p0)
        16#FEAFFEF2E331F296E071FA0DF9982CFEA7D43F2E:160>>,                                          %% Y(p0)
      <<16#0100000000000000000000351EE786A818F3A1A16B:168>>,                                        %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(wtls8) ->
    {
     {prime_field, <<16#FFFFFFFFFFFFFFFFFFFFFFFFFDE7:112>>},                                        %% Prime
     {<<16#00:8>>,                                                                                  %% A
      <<16#03:8>>,                                                                                  %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#0000000000000000000000000001:112,                                                        %% X(p0)
        16#0000000000000000000000000002:112>>,                                                      %% Y(p0)
      <<16#0100000000000001ECEA551AD837E9:120>>,                                                    %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(wtls9) ->
    {
     {prime_field, <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC808F:160>>},                            %% Prime
     {<<16#00:8>>,                                                                                  %% A
      <<16#03:8>>,                                                                                  %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#0000000000000000000000000000000000000001:160,                                            %% X(p0)
        16#0000000000000000000000000000000000000002:160>>,                                          %% Y(p0)
      <<16#0100000000000000000001CDC98AE0E2DE574ABF33:168>>,                                        %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(wtls10) ->
    {
     {characteristic_two_field, 233, {tpbasis,74}},
     {<<16#00:8>>,                                                                                  %% A
      <<16#01:8>>,                                                                                  %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#017232BA853A7E731AF129F22FF4149563A419C26BF50A4C9D6EEFAD6126:240,                        %% X(p0)
        16#01DB537DECE819B7F70F555A67C427A8CD9BF18AEB9B56E0C11056FAE6A3:240>>,                      %% Y(p0)
      <<16#8000000000000000000000000000069D5BB915BCD46EFB1AD5F173ABDF:232>>,                        %% Order
      <<16#04:8>>                                                                                   %% CoFactor
    };

curve(wtls11) ->
    {
     {characteristic_two_field, 233, {tpbasis,74}},
     {<<16#01:8>>,                                                                                  %% A
      <<16#66647EDE6C332C7F8C0923BB58213B333B20E9CE4281FE115F7D8F90AD:232>>,                        %% B
      <<16#74D59FF07F6B413D0EA14B344B20A2DB049B50C3:160>>},                                         %% Seed
      <<16#04:8,
        16#00FAC9DFCBAC8313BB2139F1BB755FEF65BC391F8B36F8F8EB7371FD558B:240,                        %% X(p0)
        16#01006A08A41903350678E58528BEBF8A0BEFF867A7CA36716F7E01F81052:240>>,                      %% Y(p0)
      <<16#01000000000000000000000000000013E974E72F8A6922031D2603CFE0D7:240>>,                      %% Order
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(wtls12) ->
    {
     {prime_field, <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000001:224>>},            %% Prime
     {<<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFE:224>>,                          %% A
      <<16#B4050A850C04B3ABF54132565044B0B7D7BFD8BA270B39432355FFB4:224>>,                          %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#B70E0CBD6BB4BF7F321390B94A03C1D356C21122343280D6115C1D21:224,                            %% X(p0)
        16#BD376388B5F723FB4C22DFE6CD4375A05A07476444D5819985007E34:224>>,                          %% Y(p0)
      <<16#FFFFFFFFFFFFFFFFFFFFFFFFFFFF16A2E0B8F03E13DD29455C5C2A3D:224>>,                          %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(ipsec3) ->
    {
     {characteristic_two_field, 155, {tpbasis,62}},
     {<<16#00:8>>,                                                                                  %% A
      <<16#07338F:24>>,                                                                             %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#000000000000000000000000000000000000007B:160,                                            %% X(p0)
        16#00000000000000000000000000000000000001C8:160>>,                                          %% Y(p0)
      <<16#02AAAAAAAAAAAAAAAAAAC7F3C7881BD0868FA86C:160>>,                                          %% Order
      <<16#03:8>>                                                                                   %% CoFactor
    };

curve(ipsec4) ->
    {
     {characteristic_two_field, 185, {tpbasis,69}},
     {<<16#00:8>>,                                                                                  %% A
      <<16#1EE9:16>>,                                                                               %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#000000000000000000000000000000000000000000000018:192,                                    %% X(p0)
        16#00000000000000000000000000000000000000000000000D:192>>,                                  %% Y(p0)
      <<16#FFFFFFFFFFFFFFFFFFFFFFEDF97C44DB9F2420BAFCA75E:184>>,                                    %% Order
      <<16#02:8>>                                                                                   %% CoFactor
    };

curve(brainpoolP160r1) ->
    {
     {prime_field, <<16#E95E4A5F737059DC60DFC7AD95B3D8139515620F:160>>},                            %% Prime
     {<<16#340E7BE2A280EB74E2BE61BADA745D97E8F7C300:160>>,                                          %% A
      <<16#1E589A8595423412134FAA2DBDEC95C8D8675E58:160>>,                                          %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#BED5AF16EA3F6A4F62938C4631EB5AF7BDBCDBC3:160,                                            %% X(p0)
        16#1667CB477A1A8EC338F94741669C976316DA6321:160>>,                                          %% Y(p0)
      <<16#E95E4A5F737059DC60DF5991D45029409E60FC09:160>>,                                          %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(brainpoolP160t1) ->
    {
     {prime_field, <<16#E95E4A5F737059DC60DFC7AD95B3D8139515620F:160>>},                            %% Prime
     {<<16#E95E4A5F737059DC60DFC7AD95B3D8139515620C:160>>,                                          %% A
      <<16#7A556B6DAE535B7B51ED2C4D7DAA7A0B5C55F380:160>>,                                          %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#B199B13B9B34EFC1397E64BAEB05ACC265FF2378:160,                                            %% X(p0)
        16#ADD6718B7C7C1961F0991B842443772152C9E0AD:160>>,                                          %% Y(p0)
      <<16#E95E4A5F737059DC60DF5991D45029409E60FC09:160>>,                                          %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(brainpoolP192r1) ->
    {
     {prime_field, <<16#C302F41D932A36CDA7A3463093D18DB78FCE476DE1A86297:192>>},                    %% Prime
     {<<16#6A91174076B1E0E19C39C031FE8685C1CAE040E5C69A28EF:192>>,                                  %% A
      <<16#469A28EF7C28CCA3DC721D044F4496BCCA7EF4146FBF25C9:192>>,                                  %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#C0A0647EAAB6A48753B033C56CB0F0900A2F5C4853375FD6:192,                                    %% X(p0)
        16#14B690866ABD5BB88B5F4828C1490002E6773FA2FA299B8F:192>>,                                  %% Y(p0)
      <<16#C302F41D932A36CDA7A3462F9E9E916B5BE8F1029AC4ACC1:192>>,                                  %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(brainpoolP192t1) ->
    {
     {prime_field, <<16#C302F41D932A36CDA7A3463093D18DB78FCE476DE1A86297:192>>},                    %% Prime
     {<<16#C302F41D932A36CDA7A3463093D18DB78FCE476DE1A86294:192>>,                                  %% A
      <<16#13D56FFAEC78681E68F9DEB43B35BEC2FB68542E27897B79:192>>,                                  %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#3AE9E58C82F63C30282E1FE7BBF43FA72C446AF6F4618129:192,                                    %% X(p0)
        16#097E2C5667C2223A902AB5CA449D0084B7E5B3DE7CCC01C9:192>>,                                  %% Y(p0)
      <<16#C302F41D932A36CDA7A3462F9E9E916B5BE8F1029AC4ACC1:192>>,                                  %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(brainpoolP224r1) ->
    {
     {prime_field, <<16#D7C134AA264366862A18302575D1D787B09F075797DA89F57EC8C0FF:224>>},            %% Prime
     {<<16#68A5E62CA9CE6C1C299803A6C1530B514E182AD8B0042A59CAD29F43:224>>,                          %% A
      <<16#2580F63CCFE44138870713B1A92369E33E2135D266DBB372386C400B:224>>,                          %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#0D9029AD2C7E5CF4340823B2A87DC68C9E4CE3174C1E6EFDEE12C07D:224,                            %% X(p0)
        16#58AA56F772C0726F24C6B89E4ECDAC24354B9E99CAA3F6D3761402CD:224>>,                          %% Y(p0)
      <<16#D7C134AA264366862A18302575D0FB98D116BC4B6DDEBCA3A5A7939F:224>>,                          %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(brainpoolP224t1) ->
    {
     {prime_field, <<16#D7C134AA264366862A18302575D1D787B09F075797DA89F57EC8C0FF:224>>},            %% Prime
     {<<16#D7C134AA264366862A18302575D1D787B09F075797DA89F57EC8C0FC:224>>,                          %% A
      <<16#4B337D934104CD7BEF271BF60CED1ED20DA14C08B3BB64F18A60888D:224>>,                          %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#6AB1E344CE25FF3896424E7FFE14762ECB49F8928AC0C76029B4D580:224,                            %% X(p0)
        16#0374E9F5143E568CD23F3F4D7C0D4B1E41C8CC0D1C6ABD5F1A46DB4C:224>>,                          %% Y(p0)
      <<16#D7C134AA264366862A18302575D0FB98D116BC4B6DDEBCA3A5A7939F:224>>,                          %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(brainpoolP256r1) ->
    {
     {prime_field, <<16#A9FB57DBA1EEA9BC3E660A909D838D726E3BF623D52620282013481D1F6E5377:256>>},    %% Prime
     {<<16#7D5A0975FC2C3057EEF67530417AFFE7FB8055C126DC5C6CE94A4B44F330B5D9:256>>,                  %% A
      <<16#26DC5C6CE94A4B44F330B5D9BBD77CBF958416295CF7E1CE6BCCDC18FF8C07B6:256>>,                  %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#8BD2AEB9CB7E57CB2C4B482FFC81B7AFB9DE27E1E3BD23C23A4453BD9ACE3262:256,                    %% X(p0)
        16#547EF835C3DAC4FD97F8461A14611DC9C27745132DED8E545C1D54C72F046997:256>>,                  %% Y(p0)
      <<16#A9FB57DBA1EEA9BC3E660A909D838D718C397AA3B561A6F7901E0E82974856A7:256>>,                  %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(brainpoolP256t1) ->
    {
     {prime_field, <<16#A9FB57DBA1EEA9BC3E660A909D838D726E3BF623D52620282013481D1F6E5377:256>>},    %% Prime
     {<<16#A9FB57DBA1EEA9BC3E660A909D838D726E3BF623D52620282013481D1F6E5374:256>>,                  %% A
      <<16#662C61C430D84EA4FE66A7733D0B76B7BF93EBC4AF2F49256AE58101FEE92B04:256>>,                  %% B
      none},                                                                                        %% Seed
      <<16#04:8,
        16#A3E8EB3CC1CFE7B7732213B23A656149AFA142C47AAFBC2B79A191562E1305F4:256,                    %% X(p0)
        16#2D996C823439C56D7F7B22E14644417E69BCB6DE39D027001DABE8F35B25C9BE:256>>,                  %% Y(p0)
      <<16#A9FB57DBA1EEA9BC3E660A909D838D718C397AA3B561A6F7901E0E82974856A7:256>>,                  %% Order
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(brainpoolP320r1) ->
    {
     {prime_field, <<16#D35E472036BC4FB7E13C785ED201E065F98FCFA6F6F40DEF4F92B9EC7893EC28:256,       %% Prime
                     16#FCD412B1F1B32E27:64>>},
     {<<16#3EE30B568FBAB0F883CCEBD46D3F3BB8A2A73513F5EB79DA66190EB085FFA9F4:256,                    %% A
        16#92F375A97D860EB4:64>>,
      <<16#520883949DFDBC42D3AD198640688A6FE13F41349554B49ACC31DCCD88453981:256,                    %% B
        16#6F5EB4AC8FB1F1A6:64>>,
      none},                                                                                        %% Seed
      <<16#04:8,
        16#43BD7E9AFB53D8B85289BCC48EE5BFE6F20137D10A087EB6E7871E2A10A599C7:256,                    %% X(p0)
        16#10AF8D0D39E20611:64,
        16#14FDD05545EC1CC8AB4093247F77275E0743FFED117182EAA9C77877AAAC6AC7:256,                    %% Y(p0)
        16#D35245D1692E8EE1:64>>,
      <<16#D35E472036BC4FB7E13C785ED201E065F98FCFA5B68F12A32D482EC7EE8658E9:256,                    %% Order
        16#8691555B44C59311:64>>,
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(brainpoolP320t1) ->
    {
     {prime_field, <<16#D35E472036BC4FB7E13C785ED201E065F98FCFA6F6F40DEF4F92B9EC7893EC28:256,       %% Prime
                     16#FCD412B1F1B32E27:64>>},
     {<<16#D35E472036BC4FB7E13C785ED201E065F98FCFA6F6F40DEF4F92B9EC7893EC28:256,                    %% A
        16#FCD412B1F1B32E24:64>>,
      <<16#A7F561E038EB1ED560B3D147DB782013064C19F27ED27C6780AAF77FB8A547CE:256,                    %% B
        16#B5B4FEF422340353:64>>,
      none},                                                                                        %% Seed
      <<16#04:8,
        16#925BE9FB01AFC6FB4D3E7D4990010F813408AB106C4F09CB7EE07868CC136FFF:256,                    %% X(p0)
        16#3357F624A21BED52:64,
        16#63BA3A7A27483EBF6671DBEF7ABB30EBEE084E58A0B077AD42A5A0989D1EE71B:256,                    %% Y(p0)
        16#1B9BC0455FB0D2C3:64>>,
      <<16#D35E472036BC4FB7E13C785ED201E065F98FCFA5B68F12A32D482EC7EE8658E9:256,                    %% Order
        16#8691555B44C59311:64>>,
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(brainpoolP384r1) ->
    {
     {prime_field, <<16#8CB91E82A3386D280F5D6F7E50E641DF152F7109ED5456B412B1DA197FB71123:256,       %% Prime
                     16#ACD3A729901D1A71874700133107EC53:128>>},
     {<<16#7BC382C63D8C150C3C72080ACE05AFA0C2BEA28E4FB22787139165EFBA91F90F:256,                    %% A
        16#8AA5814A503AD4EB04A8C7DD22CE2826:128>>,
      <<16#04A8C7DD22CE28268B39B55416F0447C2FB77DE107DCD2A62E880EA53EEB62D5:256,                    %% B
        16#7CB4390295DBC9943AB78696FA504C11:128>>,
      none},                                                                                        %% Seed
      <<16#04:8,
        16#1D1C64F068CF45FFA2A63A81B7C13F6B8847A3E77EF14FE3DB7FCAFE0CBD10E8:256,                    %% X(p0)
        16#E826E03436D646AAEF87B2E247D4AF1E:128,
        16#8ABE1D7520F9C2A45CB1EB8E95CFD55262B70B29FEEC5864E19C054FF9912928:256,                    %% Y(p0)
        16#0E4646217791811142820341263C5315:128>>,
      <<16#8CB91E82A3386D280F5D6F7E50E641DF152F7109ED5456B31F166E6CAC0425A7:256,                    %% Order
        16#CF3AB6AF6B7FC3103B883202E9046565:128>>,
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(brainpoolP384t1) ->
    {
     {prime_field, <<16#8CB91E82A3386D280F5D6F7E50E641DF152F7109ED5456B412B1DA197FB71123:256,       %% Prime
                     16#ACD3A729901D1A71874700133107EC53:128>>},
     {<<16#8CB91E82A3386D280F5D6F7E50E641DF152F7109ED5456B412B1DA197FB71123:256,                    %% A
        16#ACD3A729901D1A71874700133107EC50:128>>,
      <<16#7F519EADA7BDA81BD826DBA647910F8C4B9346ED8CCDC64E4B1ABD11756DCE1D:256,                    %% B
        16#2074AA263B88805CED70355A33B471EE:128>>,
      none},                                                                                        %% Seed
      <<16#04:8,
        16#18DE98B02DB9A306F2AFCD7235F72A819B80AB12EBD653172476FECD462AABFF:256,                    %% X(p0)
        16#C4FF191B946A5F54D8D0AA2F418808CC:128,
        16#25AB056962D30651A114AFD2755AD336747F93475B7A1FCA3B88F2B6A208CCFE:256,                    %% Y(p0)
        16#469408584DC2B2912675BF5B9E582928:128>>,
      <<16#8CB91E82A3386D280F5D6F7E50E641DF152F7109ED5456B31F166E6CAC0425A7:256,                    %% Order
        16#CF3AB6AF6B7FC3103B883202E9046565:128>>,
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(brainpoolP512r1) ->
    {
     {prime_field, <<16#AADD9DB8DBE9C48B3FD4E6AE33C9FC07CB308DB3B3C9D20ED6639CCA70330871:256,       %% Prime
                     16#7D4D9B009BC66842AECDA12AE6A380E62881FF2F2D82C68528AA6056583A48F3:256>>},
     {<<16#7830A3318B603B89E2327145AC234CC594CBDD8D3DF91610A83441CAEA9863BC:256,                    %% A
        16#2DED5D5AA8253AA10A2EF1C98B9AC8B57F1117A72BF2C7B9E7C1AC4D77FC94CA:256>>,
      <<16#3DF91610A83441CAEA9863BC2DED5D5AA8253AA10A2EF1C98B9AC8B57F1117A7:256,                    %% B
        16#2BF2C7B9E7C1AC4D77FC94CADC083E67984050B75EBAE5DD2809BD638016F723:256>>,
      none},                                                                                        %% Seed
      <<16#04:8,
        16#81AEE4BDD82ED9645A21322E9C4C6A9385ED9F70B5D916C1B43B62EEF4D0098E:256,                    %% X(p0)
        16#FF3B1F78E2D0D48D50D1687B93B97D5F7C6D5047406A5E688B352209BCB9F822:256,
        16#7DDE385D566332ECC0EABFA9CF7822FDF209F70024A57B1AA000C55B881F8111:256,                    %% Y(p0)
        16#B2DCDE494A5F485E5BCA4BD88A2763AED1CA2B2FA8F0540678CD1E0F3AD80892:256>>,
      <<16#AADD9DB8DBE9C48B3FD4E6AE33C9FC07CB308DB3B3C9D20ED6639CCA70330870:256,                    %% Order
        16#553E5C414CA92619418661197FAC10471DB1D381085DDADDB58796829CA90069:256>>,
      <<16#01:8>>                                                                                   %% CoFactor
    };

curve(brainpoolP512t1) ->
    {
     {prime_field, <<16#AADD9DB8DBE9C48B3FD4E6AE33C9FC07CB308DB3B3C9D20ED6639CCA70330871:256,       %% Prime
                     16#7D4D9B009BC66842AECDA12AE6A380E62881FF2F2D82C68528AA6056583A48F3:256>>},
     {<<16#AADD9DB8DBE9C48B3FD4E6AE33C9FC07CB308DB3B3C9D20ED6639CCA70330871:256,                    %% A
        16#7D4D9B009BC66842AECDA12AE6A380E62881FF2F2D82C68528AA6056583A48F0:256>>,
      <<16#7CBBBCF9441CFAB76E1890E46884EAE321F70C0BCB4981527897504BEC3E36A6:256,                    %% B
        16#2BCDFA2304976540F6450085F2DAE145C22553B465763689180EA2571867423E:256>>,
      none},                                                                                        %% Seed
      <<16#04:8,
        16#640ECE5C12788717B9C1BA06CBC2A6FEBA85842458C56DDE9DB1758D39C0313D:256,                    %% X(p0)
        16#82BA51735CDB3EA499AA77A7D6943A64F7A3F25FE26F06B51BAA2696FA9035DA:256,
        16#5B534BD595F5AF0FA2C892376C84ACE1BB4E3019B71634C01131159CAE03CEE9:256,                    %% Y(p0)
        16#D9932184BEEF216BD71DF2DADF86A627306ECFF96DBB8BACE198B61E00F8B332:256>>,
      <<16#AADD9DB8DBE9C48B3FD4E6AE33C9FC07CB308DB3B3C9D20ED6639CCA70330870:256,                    %% Order
        16#553E5C414CA92619418661197FAC10471DB1D381085DDADDB58796829CA90069:256>>,
      <<16#01:8>>                                                                                   %% CoFactor
    }.
