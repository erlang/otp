-module(spec_other_module).

%% OTP-15562 and ERL-845. Example provided by Kostis.

-type deep_list(A) :: [A | deep_list(A)].

-spec lists:flatten(deep_list(A)) -> [A].
