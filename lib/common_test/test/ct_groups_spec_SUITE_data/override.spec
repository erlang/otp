{merge_tests,false}.

{alias,dir,"."}.

{groups, dir, groups_spec_1_SUITE, {g1,default}}.
{groups, dir, groups_spec_1_SUITE, [{g1,[sequence]},
				    {g1,[parallel],[]}]}.

{groups, dir, groups_spec_1_SUITE, {g2,[],[]}}.
{groups, dir, groups_spec_1_SUITE, {g2,default,[{g3,[sequence]}]}}.
{groups, dir, groups_spec_1_SUITE, {g2,[],[{g4,[sequence],[{g5,[sequence]}]},
					   {g3,[sequence]}]}}.

{groups, dir, groups_spec_1_SUITE, {g1,[sequence]}, {cases,[t12,t13]}}.
{groups, dir, groups_spec_1_SUITE, {g5,[]}, {cases,[t53,t54]}}.
